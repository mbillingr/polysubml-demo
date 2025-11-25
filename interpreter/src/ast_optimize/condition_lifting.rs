use crate::ast_transform::AstTransformer;
use crate::runtime_ast as ast;
use std::collections::HashSet;

/// Move if expressions ouf of match expressions, duplicating the match in both arms.
pub struct ConditionLiftTransformer {
    changes: usize,
}

impl ConditionLiftTransformer {
    pub fn new() -> Self {
        Self { changes: 0 }
    }

    pub fn changes(&self) -> usize {
        self.changes
    }
}

impl AstTransformer for ConditionLiftTransformer {
    fn post_visit_expr(&mut self, expr: ast::Expr) -> ast::Expr {
        use ast::Expr::*;
        match expr {
            Match(m) => match *m.expr {
                If(if_) => {
                    let mut if_tags = HashSet::new();
                    if let Case(cx) = &*if_.then_expr {
                        if_tags.insert(cx.tag);
                    }
                    if let Case(cx) = &*if_.else_expr {
                        if_tags.insert(cx.tag);
                    }

                    let mut match_tags = HashSet::new();
                    for (pat, _) in &m.cases {
                        if let ast::LetPattern::Case(tag, _) = pat {
                            match_tags.insert(*tag);
                        }
                    }

                    if if_tags.is_disjoint(&match_tags) {
                        Match(ast::MatchExpr {
                            expr: Box::new(If(if_)),
                            ..m
                        })
                    } else {
                        If(ast::IfExpr {
                            cond: if_.cond,
                            then_expr: Box::new(Match(ast::MatchExpr {
                                expr: if_.then_expr,
                                cases: m.cases.clone(),
                            })),
                            else_expr: Box::new(Match(ast::MatchExpr {
                                expr: if_.else_expr,
                                cases: m.cases,
                            })),
                        })
                    }
                }
                _ => Match(m),
            },
            other => other,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast_transform::AstTransWalker;
    use crate::runtime_ast::builder::stmts;

    #[test]
    fn dont_move_arbitary_if() {
        let input = stmts("match if a then b else c with | `A _ -> 1 | `B _ -> 2");
        let expect = stmts("match if a then b else c with | `A _ -> 1 | `B _ -> 2");

        let output = input.transform(&mut ConditionLiftTransformer::new());
        assert_eq!(output, expect);
    }

    #[test]
    fn move_if_at_least_one_ifcase_occurs_in_match() {
        let input = stmts("match if a then `B b else c with | `A _ -> 1 | `B _ -> 2");
        let expect = stmts("if a then match `B b with | `A _ -> 1 | `B _ -> 2 else match c with | `A _ -> 1 | `B _ -> 2");

        let output = input.transform(&mut ConditionLiftTransformer::new());
        assert_eq!(output, expect);
    }
}

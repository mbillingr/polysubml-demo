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
                    for (tag, _, _) in &m.cases {
                        match_tags.insert(*tag);
                    }

                    if if_tags.is_disjoint(&match_tags) {
                        Match(ast::MatchExpr {
                            expr: Box::new(If(if_)),
                            ..m
                        })
                    } else {
                        self.changes += 1;
                        If(ast::IfExpr {
                            cond: if_.cond,
                            then_expr: Box::new(ast::match_(*if_.then_expr, m.cases.clone(), m.wildcard.clone())),
                            else_expr: Box::new(ast::match_(*if_.else_expr, m.cases, m.wildcard)),
                        })
                    }
                }

                Match(m2) => {
                    let mut if_tags = HashSet::new();
                    for (_, _, arm) in &m2.cases {
                        if let Case(cx) = arm {
                            if_tags.insert(cx.tag);
                        }
                    }

                    let mut match_tags = HashSet::new();
                    for (tag, _, _) in &m.cases {
                        match_tags.insert(*tag);
                    }

                    if if_tags.difference(&match_tags).count() > 1 {
                        // we would get more that one copy of the default branch
                        Match(ast::MatchExpr {
                            expr: Box::new(Match(m2)),
                            ..m
                        })
                    } else {
                        self.changes += 1;
                        Match(ast::MatchExpr {
                            expr: m2.expr,
                            cases: m2
                                .cases
                                .into_iter()
                                .map(|(t2, p2, x2)| (t2, p2, ast::match_(x2, m.cases.clone(), m.wildcard.clone())))
                                .collect(),
                            wildcard: m2
                                .wildcard
                                .map(|(var, expr)| (var, Box::new(ast::match_(*expr, m.cases.clone(), m.wildcard.clone())))),
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
    #[test]
    fn move_match_out_of_match() {
        let input = stmts("match match x with | `Foo _ -> `A a | `Bar _ -> `B b with | `A _ -> 1 | `B _ -> 2");
        let expect = stmts(
            "match x with \
                | `Foo _ -> (match `A a with | `A _ -> 1 | `B _ -> 2) \
                | `Bar _ -> (match `B b with | `A _ -> 1 | `B _ -> 2)",
        );

        let output = input.transform(&mut ConditionLiftTransformer::new());
        assert_eq!(output, expect);
    }
}

use crate::ast_transform::AstTransformer;
use crate::runtime_ast as ast;

/// Transform matching of known cases into a single arm
/// For example:
///     match `Foo 0 with | `Foo _ -> a | `Bar _ -> b
///   becomes
///     (let `Foo _ = `Foo 0; a)
pub struct KnownMatchTransformer {
    changes: usize,
}

impl KnownMatchTransformer {
    pub fn new() -> Self {
        Self { changes: 0 }
    }

    pub fn changes(&self) -> usize {
        self.changes
    }
}

impl AstTransformer for KnownMatchTransformer {
    fn post_visit_expr(&mut self, expr: ast::Expr) -> ast::Expr {
        use ast::Expr::*;
        use ast::Statement::*;
        match expr {
            Match(mx) if matches!(&*mx.expr, Case(_)) => {
                self.changes += 1;
                let Case(cx) = *mx.expr else { unimplemented!() };
                let mut wildcard = None;
                for (pat, arm) in mx.cases {
                    match pat {
                        ast::LetPattern::Case(tag, _) => {
                            if tag != cx.tag {
                                continue;
                            }
                            return ast::block(vec![LetDef(pat, Case(cx))], arm);
                        }
                        _ => wildcard = Some((pat, arm)),
                    }
                }
                let (pat, arm) = wildcard.unwrap();
                ast::block(vec![LetDef(pat, Case(cx))], arm)
            }
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
    fn transform_known_case() {
        let input = stmts("match `Foo 0 with | _ -> 0 | `Foo _ -> a | `Bar _ -> b");
        let expect = stmts("(let `Foo _ = `Foo 0; a)");

        let output = input.transform(&mut KnownMatchTransformer::new());
        assert_eq!(output, expect);
    }

    #[test]
    fn transform_wildcard_case() {
        let input = stmts("match `Bar 0 with | `Foo _ -> a | _ -> b");
        let expect = stmts("(let _ = `Bar 0; b)");

        let output = input.transform(&mut KnownMatchTransformer::new());
        assert_eq!(output, expect);
    }
}

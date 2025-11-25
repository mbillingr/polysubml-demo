use crate::ast_transform::AstTransformer;
use crate::runtime_ast as ast;

/// Move case expressions out of blocks and field access into blocks.
/// For example:
///     (..; `Foo x)  --> `Foo (..; x)
///     (..; r).x  --> (..; r.x)
pub struct CaseLiftAndFieldLowerTransformer {
    changes: usize,
}

impl CaseLiftAndFieldLowerTransformer {
    pub fn new() -> Self {
        Self { changes: 0 }
    }

    pub fn changes(&self) -> usize {
        self.changes
    }
}

impl AstTransformer for CaseLiftAndFieldLowerTransformer {
    fn post_visit_expr(&mut self, expr: ast::Expr) -> ast::Expr {
        use ast::Expr::*;
        match expr {
            Block(blk) => match *blk.expr {
                Case(ce) => {
                    self.changes += 1;
                    ast::case(ce.tag, ast::block(blk.statements, *ce.expr))
                }
                _ => Block(blk),
            },

            FieldAccess(fa) => match *fa.expr {
                Block(blk) => {
                    self.changes += 1;
                    ast::block(blk.statements, ast::field_access(fa.field, *blk.expr))
                }
                _ => FieldAccess(fa),
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
    fn move_case_out_of_block() {
        let input = stmts("(print 0; `Foo 1)");
        let expect = stmts("`Foo (print 0; 1)");

        let output = input.transform(&mut CaseLiftAndFieldLowerTransformer::new());
        assert_eq!(output, expect);
    }

    #[test]
    fn move_case_out_of_multiple_blocks() {
        let input = stmts("(print 1; (print 2; (print 3; `Foo 0)))");
        let expect = stmts("`Foo (print 1; (print 2; (print 3; 0)))");

        let output = input.transform(&mut CaseLiftAndFieldLowerTransformer::new());
        assert_eq!(output, expect);
    }

    #[test]
    fn move_field_into_block() {
        let input = stmts("(print 0; r).x");
        let expect = stmts("(print 0; r.x)");

        let output = input.transform(&mut CaseLiftAndFieldLowerTransformer::new());
        assert_eq!(output, expect);
    }

    #[test]
    fn move_field_into_multiple_blocks() {
        let input = stmts("(print 1; (print 2; (print 3; r))).x");
        let expect = stmts("(print 1; (print 2; (print 3; r.x)))");

        let output = input.transform(&mut CaseLiftAndFieldLowerTransformer::new());
        assert_eq!(output, expect);
    }
}

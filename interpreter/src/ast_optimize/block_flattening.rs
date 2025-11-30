use crate::ast_transform::{AstTransWalker, AstTransformer, TransformResult};
use crate::runtime_ast as ast;

pub struct BlockFlattener {
    changes: usize,
}

impl BlockFlattener {
    pub fn new() -> Self {
        Self { changes: 0 }
    }

    pub fn changes(&self) -> usize {
        self.changes
    }

    /// entry point for top level transformations
    pub fn transform_stmts(&mut self, stmts: Vec<ast::Statement>, toplevel: bool) -> Vec<ast::Statement> {
        use ast::Expr::*;
        use ast::LetPattern::*;
        use ast::Statement::*;

        if toplevel {
            return stmts.transform(self);
        }

        let mut out = vec![];
        for stmt in stmts {
            match stmt.transform(self) {
                Expr(Block(inner_blk)) => {
                    out.extend(inner_blk.statements);
                    out.push(LetDef(Var(ast::Variable(None)), *inner_blk.expr));
                }

                LetDef(pat, Block(val_blk)) => {
                    out.extend(val_blk.statements);
                    out.push(LetDef(pat, *val_blk.expr));
                }

                stmt => out.push(stmt),
            }
        }
        out
    }
}

impl AstTransformer for BlockFlattener {
    fn pre_visit_expr(&mut self, expr: ast::Expr) -> TransformResult<ast::Expr> {
        use ast::Expr::*;
        match expr {
            Block(blk) => {
                let statements = self.transform_stmts(blk.statements, false);
                let expr = (*blk.expr).transform(self);
                TransformResult::Break(ast::block(statements, expr))
            }
            _ => TransformResult::Continue(expr),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::builder::*;

    #[test]
    fn dont_expand_into_toplevel_block() {
        let input = stmts("print 1; begin print2; 0 end; begin print3; 0 end");
        let expect = stmts("print 1; begin print2; 0 end; begin print3; 0 end");

        let output = BlockFlattener::new().transform_stmts(input, true);
        assert_eq!(output, expect);
    }

    #[test]
    fn merge_block_in_expr_position() {
        // Haha! That's actually been done during block construction. We keep the test to make sure the behavior stays.
        let input = stmts("print begin print 1; begin print 2; 3 end end");
        let expect = stmts("print begin print 1; print 2; 3 end");

        let output = BlockFlattener::new().transform_stmts(input, false);
        assert_eq!(output, expect);
    }

    #[test]
    fn merge_nested_blocks() {
        // Important: This is only allowed on the alpha-converted AST!
        //            Otherwise, we might break variables with the same
        //            name outside and inside the inner block.
        let input = stmts("print 1; begin print 2; 0 end");
        let expect = stmts("print 1; print 2; let _ = 0");

        let output = BlockFlattener::new().transform_stmts(input, false);
        assert_eq!(output, expect);
    }

    #[test]
    fn dont_merge_nested_blocks_into_toplevel() {
        let input = stmts("print 1; begin print 2; 0 end");
        let expect = stmts("print 1; begin print 2; 0 end");

        let output = BlockFlattener::new().transform_stmts(input, true);
        assert_eq!(output, expect);
    }

    #[test]
    fn lift_let_bindings_out_of_inner_block() {
        // Important: This is only allowed on the alpha-converted AST!
        //            Otherwise, we might break variables with the same
        //            name outside and inside the inner block.
        let input = stmts("let x = begin let y=0; y end");
        let expect = stmts("let y=0; let x = y");

        let output = BlockFlattener::new().transform_stmts(input, false);
        assert_eq!(output, expect);
    }

    #[test]
    fn dont_lift_let_bindings_into_toplevel() {
        let input = stmts("let x = begin let y=0; y end");
        let expect = stmts("let x = begin let y=0; y end");

        let output = BlockFlattener::new().transform_stmts(input, true);
        assert_eq!(output, expect);
    }
}

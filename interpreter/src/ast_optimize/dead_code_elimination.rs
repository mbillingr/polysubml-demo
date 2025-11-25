use crate::ast_purity::is_pure;
use crate::ast_transform::{AstTransWalker, AstTransformer, TransformResult};
use crate::free_vars::free_vars;
use crate::runtime_ast as ast;
use compiler_lib::ast::StringId;
use std::collections::{HashSet, VecDeque};

pub struct DeadCodeTransformer {
    changes: usize,
}

impl DeadCodeTransformer {
    pub fn new() -> Self {
        Self { changes: 0 }
    }

    pub fn changes(&self) -> usize {
        self.changes
    }

    /// entry point for top level transformations
    pub fn transform_stmts(&mut self, stmts: Vec<ast::Statement>, toplevel: bool) -> Vec<ast::Statement> {
        if toplevel {
            stmts.transform(self)
        } else {
            self.transform_block(stmts, HashSet::new()).transform(self)
        }
    }

    fn transform_block(&mut self, mut stmts: Vec<ast::Statement>, mut fvs: HashSet<StringId>) -> Vec<ast::Statement> {
        use ast::LetPattern::*;
        use ast::Statement::*;

        let mut out = VecDeque::new();
        while let Some(current_stmt) = stmts.pop() {
            match current_stmt {
                LetDef(Var(Some(var)), expr) if !fvs.contains(&var) => {
                    if !is_pure(&expr) {
                        fvs.extend(free_vars(&expr));
                        out.push_front(LetDef(Var(None), expr.clone()));
                    }
                    self.changes += 1;
                }
                _ => {
                    fvs.extend(free_vars(&current_stmt));
                    out.push_front(current_stmt);
                    continue;
                }
            };
        }

        out.into()
    }
}

impl AstTransformer for DeadCodeTransformer {
    fn pre_visit_expr(&mut self, expr: ast::Expr) -> TransformResult<ast::Expr> {
        match expr {
            ast::Expr::Block(blk) => {
                let expr = (*blk.expr).transform(self);
                let stmts = self.transform_block(blk.statements, free_vars(&expr)).transform(self);
                TransformResult::Break(ast::block(stmts, expr))
            }
            _ => TransformResult::Continue(expr),
        }
    }

    fn post_visit_stmt(&mut self, stmt: ast::Statement) -> ast::Statement {
        use ast::LetPattern::*;
        match stmt {
            ast::Statement::LetDef(Var(None), val) if is_pure(&val) => {
                self.changes += 1;
                ast::Statement::Empty
            }
            _ => stmt,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime_ast::builder::stmts;

    #[test]
    fn dont_eliminate_used_variables() {
        let input = stmts("let x = 0 in x");
        let expect = stmts("let x = 0 in x");

        let output = DeadCodeTransformer::new().transform_stmts(input, false);
        assert_eq!(output, expect);
    }

    #[test]
    fn eliminate_unused_let() {
        let input = stmts("let _ = 0");
        let expect = stmts("");

        let output = DeadCodeTransformer::new().transform_stmts(input, false);
        assert_eq!(output, expect);
    }

    #[test]
    fn keep_side_effects() {
        let input = stmts("let _ = (print 0; 0)");
        let expect = stmts("let _ = (print 0; 0)");

        // todo: this could be transformed to `print 0`

        let output = DeadCodeTransformer::new().transform_stmts(input, false);
        assert_eq!(output, expect);
    }

    #[test]
    fn keep_unused_toplevel_bindings() {
        let input = stmts("let x = 0");
        let expect = stmts("let x = 0");
        let unexpect = stmts("");

        let output = DeadCodeTransformer::new().transform_stmts(input, true);
        assert_eq!(output, expect);
        assert_ne!(output, unexpect);
    }

    #[test]
    fn keep_variables_used_in_subsequent_statements() {
        let input = stmts("begin let x = 0; print x; 0 end");
        let expect = stmts("begin let x = 0; print x; 0 end");

        let output = DeadCodeTransformer::new().transform_stmts(input, false);
        assert_eq!(output, expect);
    }

    #[test]
    fn eliminate_unused_variables() {
        let input = stmts("let x = 0 in 0");
        let expect = stmts("begin ; 0 end");

        let output = DeadCodeTransformer::new().transform_stmts(input, true);
        assert_eq!(output, expect);
    }

    #[test]
    fn eliminate_unused_variables2() {
        let input = stmts("let x = 0");
        let expect = vec![];

        let output = DeadCodeTransformer::new().transform_stmts(input, false);
        assert_eq!(output, expect);
    }

    #[test]
    fn eliminate_indirectly_nested_unused_variables() {
        let input = stmts("begin let x = 1 in 1 + begin let y = x in 0 end end");
        let expect = stmts("1 + 0");
        println!("{:?}", input);

        let output = DeadCodeTransformer::new().transform_stmts(input, true);
        assert_eq!(output, expect);
    }

    #[test]
    fn eliminate_indirectly_chained_unused_variables() {
        let input = stmts("begin let x = 1; let y = x; 0 end");
        let expect = stmts("0");
        println!("{:?}", input);

        let output = DeadCodeTransformer::new().transform_stmts(input, true);
        assert_eq!(output, expect);
    }
}

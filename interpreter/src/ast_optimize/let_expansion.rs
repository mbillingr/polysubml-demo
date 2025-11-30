use crate::ast_transform::{AstTransWalker, AstTransformer, TransformResult};
use crate::runtime_ast as ast;
use std::collections::HashMap;

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
    pub fn transform_stmts(&mut self, stmts: Vec<ast::Statement>) -> Vec<ast::Statement> {
        use ast::Statement::*;
        let mut out = vec![];

        for stmt in stmts {
            match stmt {
                LetDef(pat, exp) => out.extend(self.expand_let(pat, exp)),
                _ => out.push(stmt),
            }
        }

        out.transform(self)
    }

    fn expand_let(&mut self, pat: ast::LetPattern, exp: ast::Expr) -> Vec<ast::Statement> {
        use ast::Expr;
        use ast::LetPattern::*;
        match (pat, exp) {
            (Case(_, pat), Expr::Case(ce)) => {
                self.changes += 1;
                self.expand_let(*pat, *ce.expr)
            }
            (Record(pats), Expr::Record(rec)) => {
                self.changes += 1;
                let mut pats: HashMap<_, _> = pats.into_iter().collect();
                let mut out = vec![];
                for (f, x, _) in rec.fields {
                    match pats.remove(&f) {
                        Some(p) => out.extend(self.expand_let(p, x)),
                        None => out.push(ast::Statement::LetDef(Var(ast::Variable(None)), x)),
                    }
                }
                out
            }
            (pat, exp) => vec![ast::Statement::LetDef(pat, exp)],
        }
    }
}

impl AstTransformer for BlockFlattener {
    fn pre_visit_expr(&mut self, expr: ast::Expr) -> TransformResult<ast::Expr> {
        match expr {
            ast::Expr::Block(blk) => {
                let statements = self.transform_stmts(blk.statements);
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
    fn expand_invariant() {
        let input = stmts("let x = 0");
        let expect = stmts("let x = 0");

        let output = BlockFlattener::new().transform_stmts(input);
        assert_eq!(output, expect);
    }

    #[test]
    fn expand_letcase() {
        let input = stmts("let `A x = `A 0");
        let expect = stmts("let x = 0");

        let output = BlockFlattener::new().transform_stmts(input);
        assert_eq!(output, expect);
    }

    #[test]
    fn expand_nested_letcase() {
        let input = stmts("let `A `B `C x = `A `B `C 0");
        let expect = stmts("let x = 0");

        let output = BlockFlattener::new().transform_stmts(input);
        assert_eq!(output, expect);
    }

    #[test]
    fn expand_simple_record() {
        let input = stmts("let {x} = {x=0}");
        let expect = stmts("let x = 0");

        let output = BlockFlattener::new().transform_stmts(input);
        assert_eq!(output, expect);
    }

    #[test]
    fn expand_record_field_order_follows_expression() {
        let input = stmts("let {x; y; z} = {y=2; z=0; x=1}");
        let expect = stmts("let y = 2; let z = 0; let x = 1");

        let output = BlockFlattener::new().transform_stmts(input);
        assert_eq!(output, expect);
    }

    #[test]
    fn expand_record_with_extra_fields() {
        let input = stmts("let {z} = {y=2; z=0; x=1}");
        let expect = stmts("let _ = 2; let z = 0; let _ = 1");

        let output = BlockFlattener::new().transform_stmts(input);
        assert_eq!(output, expect);
    }

    #[test]
    fn expand_deep_nest() {
        let input = stmts("let `A {x=`B y; y=`C {z}} = `A {x=`B 1; y=`C {z=2}}");
        let expect = stmts("let y = 1; let z = 2");

        let output = BlockFlattener::new().transform_stmts(input);
        assert_eq!(output, expect);
    }

    #[test]
    fn expand_inside_arbitrary_expr() {
        let input = stmts("1 + begin let {x} = {x=0}; x end");
        let expect = stmts("1 + begin let x = 0; x end");

        let output = BlockFlattener::new().transform_stmts(input);
        assert_eq!(output, expect);
    }
}

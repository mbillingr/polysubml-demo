use crate::ast_transform::AstTransformer;
use crate::runtime_ast as ast;

/// Transform direct function calls into let bindings.
/// For example:
///     `(fun x -> x + x) 21`
///   becomes
///     `(let x = 21 in x + x)`
pub struct DirectCallTransformer {
    changes: usize,
}

impl DirectCallTransformer {
    pub fn new() -> Self {
        Self { changes: 0 }
    }

    pub fn changes(&self) -> usize {
        self.changes
    }
}

impl AstTransformer for DirectCallTransformer {
    fn post_visit_expr(&mut self, expr: ast::Expr) -> ast::Expr {
        match expr {
            ast::Expr::Call(ast::CallExpr {
                func,
                arg,
                eval_arg_first,
            }) => match *func {
                ast::Expr::FuncDef(fd) => {
                    self.changes += 1;
                    let statements = vec![ast::Statement::LetDef(fd.param, *arg)];
                    ast::block(statements, *fd.body)
                }
                f => ast::Expr::Call(ast::CallExpr {
                    func: Box::new(f),
                    arg,
                    eval_arg_first,
                }),
            },

            other => other,
        }
    }
}

use crate::ast_transform::AstTransformer;
use crate::runtime_ast as ast;

/// Transform direct field access into the field definition.
/// For example:
///     `{a=1; b=2}.a`
///   becomes
///     `1`
/// Currently, the transform requires that the whole record definition is free of side effects.
pub struct DirectFieldAccessTransformer {
    changes: usize,
}

impl DirectFieldAccessTransformer {
    pub fn new() -> Self {
        Self { changes: 0 }
    }

    pub fn changes(&self) -> usize {
        self.changes
    }
}

impl AstTransformer for DirectFieldAccessTransformer {
    fn post_visit_expr(&mut self, expr: ast::Expr) -> ast::Expr {
        match expr {
            ast::Expr::FieldAccess(ast::FieldAccessExpr { expr, field }) => match *expr {
                ast::Expr::Record(rec) => {
                    self.changes += 1;
                    for (f, v, _) in rec.fields {
                        if f == field {
                            return v;
                        }
                    }
                    unreachable!();
                }
                r => ast::Expr::FieldAccess(ast::FieldAccessExpr {
                    expr: Box::new(r),
                    field,
                }),
            },
            other => other,
        }
    }
}

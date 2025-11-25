use crate::ast_purity::is_pure;
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
                ast::Expr::Record(rec) if rec.fields.iter().filter(|(f, _, _)| *f != field).all(|(_, v, _)| is_pure(v)) => {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast_transform::AstTransWalker;
    use crate::runtime_ast::builder::stmts;

    #[test]
    fn transform_simple_field_access() {
        let input = stmts("{x=1; y=2}.x");
        let expect = stmts("1");

        let output = input.transform(&mut DirectFieldAccessTransformer::new());
        assert_eq!(output, expect);
    }

    #[test]
    fn transform_access_to_field_with_side_effect() {
        let input = stmts("{x=(print 0; 1); y=2}.x");
        let expect = stmts("(print 0; 1)");

        let output = input.transform(&mut DirectFieldAccessTransformer::new());
        assert_eq!(output, expect);
    }

    #[test]
    fn transform_if_other_field_has_side_effect() {
        let input = stmts("{x=1; y=(print 0; 2)}.x");
        let expect = stmts("{x=1; y=(print 0; 2)}.x");

        // todo: this could be transformed to `begin let _ = (print 0; 2); 1 end`.
        //       maybe we could even transform all fields into `let _ = ...` statements and let dead code elimination do the rest?
        //       in any case, care needs to be taken to preserve the order of side-effects if the selected field has side effects.

        let output = input.transform(&mut DirectFieldAccessTransformer::new());
        assert_eq!(output, expect);
    }
}

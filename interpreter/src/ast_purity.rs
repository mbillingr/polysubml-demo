use crate::ast_visitor::{AstVisitor, AstWalker, VisitResult};
use crate::runtime_ast as ast;
use std::cmp::max;

pub fn is_pure(expr: &ast::Expr) -> bool {
    let mut pv = PurityVisitor(SideEffect::None);
    expr.walk_ast(&mut pv);
    pv.0 == SideEffect::None
}

struct PurityVisitor(SideEffect);

#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
enum SideEffect {
    /// Must be first (lowest)
    None,
    /// Side effect is not visible outside the block
    Local,
    /// Must be last (highest)
    Global,
}

impl SideEffect {
    fn update(&mut self, other: Self) {
        let mx = max(*self, other);
        *self = mx;
    }

    fn global(&mut self) {
        *self = SideEffect::Global;
    }

    fn local(&mut self) {
        self.update(SideEffect::Local);
    }

    fn restore_env(&mut self) {
        match self {
            SideEffect::None => {}
            SideEffect::Local => {
                *self = SideEffect::None;
            }
            SideEffect::Global => {}
        }
    }
}

impl AstVisitor for PurityVisitor {
    fn pre_visit_expr(&mut self, expr: &ast::Expr) -> VisitResult {
        match expr {
            ast::Expr::Call(_) => {
                // A function call might have any side effect
                self.0.global();
                VisitResult::Break
            }
            ast::Expr::FieldSet(_) => {
                // Assigning to a field IS a side effect
                self.0.global();
                VisitResult::Break
            }
            ast::Expr::FuncDef(_) => {
                // A function *definition* never has a side effect, regardless of the body
                VisitResult::Break
            }
            ast::Expr::Record(rec) => {
                let mutable = rec.fields.iter().any(|(_, _, m)| *m);
                if mutable {
                    // A record with mutable fields is not pure
                    // (in particular, we can't inline it because every access needs to see the same object.)
                    self.0.global();
                    VisitResult::Break
                } else {
                    VisitResult::Continue
                }
            }
            _ => VisitResult::Continue,
        }
    }

    fn post_visit_expr(&mut self, expr: &ast::Expr) {
        match expr {
            ast::Expr::Block(_) => self.0.restore_env(),
            _ => {}
        }
    }

    fn pre_visit_stmt(&mut self, stmt: &ast::Statement) -> VisitResult {
        match stmt {
            ast::Statement::Empty => VisitResult::Continue,
            ast::Statement::Expr(_) => VisitResult::Continue,
            ast::Statement::LetDef(_, _) => {
                self.0.local();
                VisitResult::Continue
            }
            ast::Statement::LetRecDef(_) => {
                self.0.local();
                VisitResult::Continue
            }
            ast::Statement::Println(_) => {
                self.0.global();
                VisitResult::Break
            }
        }
    }
}

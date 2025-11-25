use crate::ast_visitor::{AstVisitor, AstWalker, VisitResult};
use crate::runtime_ast as ast;
use compiler_lib::ast::StringId;
use std::collections::HashSet;

pub fn free_vars<T: AstWalker>(expr: &T) -> HashSet<StringId> {
    let mut fvv = FreeVarsVisitor::new();
    expr.walk_ast(&mut fvv);
    fvv.fvs
}

pub struct FreeVarsVisitor {
    fvs: HashSet<StringId>,
}

impl FreeVarsVisitor {
    pub fn new() -> Self {
        Self { fvs: HashSet::new() }
    }
}

pub struct BoundVarsVisitor<'a> {
    fvs: &'a mut HashSet<StringId>,
}

impl AstVisitor for FreeVarsVisitor {
    fn pre_visit_expr(&mut self, expr: &ast::Expr) -> VisitResult {
        match expr {
            ast::Expr::Block(blk) => {
                blk.expr.walk_ast(self);

                for stmt in blk.statements.iter().rev() {
                    match stmt {
                        ast::Statement::LetDef(pat, val) => {
                            pat.walk_ast(&mut BoundVarsVisitor { fvs: &mut self.fvs });
                            val.walk_ast(self)
                        }
                        _ => stmt.walk_ast(self),
                    }
                }

                VisitResult::Break
            }

            ast::Expr::Match(mx) => {
                for (pat, arm) in &mx.cases {
                    arm.walk_ast(self);
                    pat.walk_ast(&mut BoundVarsVisitor { fvs: &mut self.fvs });
                }
                mx.expr.walk_ast(self);
                VisitResult::Break
            }

            _ => VisitResult::Continue,
        }
    }

    fn post_visit_stmt(&mut self, stmt: &ast::Statement) {
        match stmt {
            ast::Statement::LetDef(_pat, expr) => {
                expr.walk_ast(self);
            }
            ast::Statement::LetRecDef(defs) => {
                for (bound, _) in defs {
                    self.fvs.remove(bound);
                }
            }
            _ => {}
        }
    }

    fn post_visit_expr(&mut self, expr: &ast::Expr) {
        match expr {
            ast::Expr::Variable(var) => {
                self.fvs.insert(var.name);
            }

            ast::Expr::FuncDef(fd) => {
                fd.param.walk_ast(&mut BoundVarsVisitor { fvs: &mut self.fvs });
            }

            _ => {}
        }
    }

    fn post_visit_pattern(&mut self, _pattern: &ast::LetPattern) {}
}

impl<'a> AstVisitor for BoundVarsVisitor<'a> {
    fn post_visit_stmt(&mut self, _stmt: &ast::Statement) {
        unimplemented!()
    }

    fn post_visit_expr(&mut self, _expr: &ast::Expr) {
        unimplemented!()
    }

    fn post_visit_pattern(&mut self, pattern: &ast::LetPattern) {
        match pattern {
            ast::LetPattern::Var(Some(var)) => {
                self.fvs.remove(var);
            }
            _ => {}
        }
    }
}

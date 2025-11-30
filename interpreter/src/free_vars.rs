use crate::ast_visitor::{AstVisitor, AstWalker, VisitResult};
use crate::runtime_ast as ast;
use compiler_lib::ast::StringId;
use std::collections::{HashMap, HashSet};

pub fn free_vars<T: AstWalker>(expr: &T) -> HashSet<StringId> {
    let mut fvv = FreeVarsVisitor::new();
    expr.walk_ast(&mut fvv);
    fvv.fvs
}

pub fn free_var_usage_counts<T: AstWalker>(stmts: &[T]) -> HashMap<StringId, usize> {
    let mut fvv = FreeVarsVisitor::new();
    stmts.walk_ast(&mut fvv);
    fvv.fvs
}

pub struct FreeVarsVisitor<FV: FreeVars> {
    fvs: FV,
}

impl<FV: FreeVars + Default> FreeVarsVisitor<FV> {
    pub fn new() -> Self {
        Self { fvs: Default::default() }
    }
}

pub struct BoundVarsVisitor<'a, FV: FreeVars> {
    fvs: &'a mut FV,
}

impl<FV: FreeVars> AstVisitor for FreeVarsVisitor<FV> {
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
                for (_, pat, arm) in &mx.cases {
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
                    self.fvs.make_unfree(*bound);
                }
            }
            _ => {}
        }
    }

    fn post_visit_expr(&mut self, expr: &ast::Expr) {
        match expr {
            ast::Expr::Variable(var) => {
                self.fvs.add_usage(var.name);
            }

            ast::Expr::FuncDef(fd) => {
                fd.param.walk_ast(&mut BoundVarsVisitor { fvs: &mut self.fvs });
            }

            _ => {}
        }
    }

    fn post_visit_pattern(&mut self, _pattern: &ast::LetPattern) {}
}

impl<'a, FV: FreeVars> AstVisitor for BoundVarsVisitor<'a, FV> {
    fn post_visit_stmt(&mut self, _stmt: &ast::Statement) {
        unimplemented!()
    }

    fn post_visit_expr(&mut self, _expr: &ast::Expr) {
        unimplemented!()
    }

    fn post_visit_pattern(&mut self, pattern: &ast::LetPattern) {
        match pattern {
            ast::LetPattern::Var(ast::Variable(Some(var))) => {
                self.fvs.make_unfree(*var);
            }
            _ => {}
        }
    }
}

pub trait FreeVars {
    fn add_usage(&mut self, var: StringId);
    fn make_unfree(&mut self, var: StringId);
}

impl FreeVars for HashSet<StringId> {
    fn add_usage(&mut self, var: StringId) {
        self.insert(var);
    }

    fn make_unfree(&mut self, var: StringId) {
        self.remove(&var);
    }
}

impl FreeVars for HashMap<StringId, usize> {
    fn add_usage(&mut self, var: StringId) {
        self.entry(var).and_modify(|x| *x += 1).or_insert(1);
    }

    fn make_unfree(&mut self, var: StringId) {
        self.remove(&var);
    }
}

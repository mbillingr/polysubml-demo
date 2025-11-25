use crate::ast_purity::is_pure;
use crate::ast_transform::{AstTransWalker, AstTransformer, TransformResult};
use crate::free_vars::free_vars;
use crate::runtime_ast as ast;
use compiler_lib::ast::StringId;
use im_rc::HashMap;

/// Inline known functions...
#[derive(Clone)]
pub struct InlineTransformer {
    changes: usize,
    env: HashMap<StringId, Var>,
}

#[derive(Clone, Debug)]
enum Var {
    Unknown,
    Variable(ast::VariableExpr),
    Literal(ast::LiteralExpr),
    Function(ast::FuncDefExpr),
    Case(ast::CaseExpr),
    Record(ast::RecordExpr),
}

impl InlineTransformer {
    pub fn new() -> Self {
        Self {
            changes: 0,
            env: HashMap::new(),
        }
    }

    pub fn changes(&self) -> usize {
        self.changes
    }

    fn bind(&mut self, pat: &ast::LetPattern, x: Var) {
        match pat {
            ast::LetPattern::Var(None) => {}
            ast::LetPattern::Var(Some(var)) => {
                self.env.insert(*var, x);
            }
            ast::LetPattern::Case(_, pat) => self.bind(pat, self.unwrap_case(&x)),
            ast::LetPattern::Record(fps) => {
                for (f, p) in fps {
                    self.bind(p, self.get_field(&x, *f))
                }
            }
        }
    }

    fn resolve_exp(&self, exp: &ast::Expr) -> Var {
        if !is_pure(exp) {
            return Var::Unknown;
        }

        match exp {
            ast::Expr::Variable(var) => self.lookup(&var.name).unwrap_or(Var::Variable(var.clone())),
            ast::Expr::Literal(x) => Var::Literal(x.clone()),
            ast::Expr::FuncDef(fd) => Var::Function(fd.clone()),
            ast::Expr::Case(cx) => Var::Case(cx.clone()),
            ast::Expr::Record(rec) => Var::Record(rec.clone()),
            ast::Expr::Block(blk) => {
                if is_pure(&blk.expr) && free_vars(&blk.expr).is_empty() {
                    self.resolve_exp(&blk.expr)
                } else {
                    Var::Unknown
                }
            }
            _ => Var::Unknown,
        }
    }

    fn inline(&self, var: StringId) -> Option<ast::Expr> {
        match self.lookup(&var).unwrap_or(Var::Unknown) {
            Var::Unknown => None,
            Var::Variable(vx) => Some(ast::Expr::Variable(vx)),
            Var::Literal(lit) => Some(ast::Expr::Literal(lit)),
            Var::Function(fd) => Some(ast::Expr::FuncDef(fd)),
            Var::Case(cx) => Some(ast::Expr::Case(cx)),
            Var::Record(rec) => Some(ast::Expr::Record(rec)),
        }
    }

    fn lookup(&self, var: &StringId) -> Option<Var> {
        self.env.get(&var).cloned()
    }

    fn unwrap_case(&self, v: &Var) -> Var {
        match v {
            Var::Unknown => Var::Unknown,
            Var::Variable(_) => Var::Unknown,
            Var::Literal(_) => unimplemented!(),
            Var::Function(_) => unimplemented!(),
            Var::Case(cx) => self.resolve_exp(&cx.expr),
            Var::Record(_) => unimplemented!(),
        }
    }
    fn get_field(&self, v: &Var, field_name: StringId) -> Var {
        match v {
            Var::Unknown => Var::Unknown,
            Var::Variable(_) => Var::Unknown,
            Var::Literal(_) => unimplemented!(),
            Var::Function(_) => unimplemented!(),
            Var::Case(_) => unimplemented!(),
            Var::Record(rec) => {
                for (f, v, m) in &rec.fields {
                    if *f == field_name {
                        if *m {
                            // We can't statically know the value of a mutable field
                            return Var::Unknown;
                        } else {
                            return self.resolve_exp(&v);
                        }
                    }
                }
                unreachable!()
            }
        }
    }
}

impl AstTransformer for InlineTransformer {
    fn pre_visit_expr(&mut self, expr: ast::Expr) -> TransformResult<ast::Expr> {
        match expr {
            ast::Expr::Block(blk) => {
                let mut local = self.clone();

                let mut stmts = vec![];

                for stmt in blk.statements {
                    stmts.push(stmt.transform(&mut local));
                }

                let expr = (*blk.expr).transform(&mut local);

                TransformResult::Break(ast::block(stmts, expr))
            }

            ast::Expr::Variable(ref var) => match self.inline(var.name) {
                None => TransformResult::Break(expr),
                Some(new) => {
                    self.changes += 1;
                    TransformResult::Break(new)
                }
            },

            other => TransformResult::Continue(other),
        }
    }

    fn post_visit_stmt(&mut self, stmt: ast::Statement) -> ast::Statement {
        match &stmt {
            ast::Statement::LetDef(pat, expr) => {
                self.bind(pat, self.resolve_exp(&expr));
            }
            _ => {}
        }
        stmt
    }
}

use crate::ast_transform::{AstTransWalker, AstTransformer, TransformResult};
use crate::runtime_ast as ast;
use compiler_lib::Rodeo;
use compiler_lib::ast::StringId;
use im_rc::HashMap;

/// Make all variable names unique
pub struct VariableRenamer<'a> {
    strings: Option<&'a mut Rodeo>,
    env: HashMap<StringId, StringId>,
    gensym_counter: usize,
    toplevel: bool,
}

impl<'a> VariableRenamer<'a> {
    pub fn new(toplevel: bool, strings: &'a mut Rodeo) -> Self {
        Self {
            strings: Some(strings),
            env: HashMap::new(),
            gensym_counter: 0,
            toplevel,
        }
    }

    fn in_scope<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let outer_env = self.env.clone();
        let outer_level = self.toplevel;

        self.toplevel = false;
        let result = f(self);

        self.toplevel = outer_level;
        self.env = outer_env;

        result
    }

    fn resolve(&self, var: StringId) -> StringId {
        self.env.get(&var).cloned().unwrap_or(var)
    }

    fn bind(&mut self, oldvar: StringId) -> StringId {
        let newvar = self.gensym(oldvar);
        self.env.insert(oldvar, newvar);
        newvar
    }

    fn gensym(&mut self, varname: StringId) -> StringId {
        if self.toplevel {
            // Don't rename toplevel variables
            return varname;
        }

        let s = self.strings.as_mut().unwrap();
        let prefix = s.resolve(&varname);

        if prefix.starts_with("__") {
            // Don't gensym variables that start with a dunder
            // (TODO: maybe prevent users from creating them?)
            return varname;
        }

        let unique_name = format!("{}'{}", prefix, self.gensym_counter);
        self.gensym_counter += 1;
        s.get_or_intern(unique_name)
    }
}

impl AstTransformer for VariableRenamer<'_> {
    fn pre_visit_expr(&mut self, expr: ast::Expr) -> TransformResult<ast::Expr> {
        match expr {
            ast::Expr::Block(blk) => self.in_scope(|local| {
                let mut stmts = vec![];

                for stmt in blk.statements {
                    stmts.push(stmt.transform(local));
                }

                let expr = blk.expr.transform(local);

                TransformResult::Break(ast::Expr::Block(ast::BlockExpr { statements: stmts, expr }))
            }),

            ast::Expr::Variable(ref var) => TransformResult::Break(ast::Expr::Variable(ast::VariableExpr {
                name: self.resolve(var.name),
            })),

            ast::Expr::FuncDef(fd) => self.in_scope(|local| {
                let param = fd.param.transform(local);
                let body = fd.body.transform(local);
                TransformResult::Break(ast::Expr::FuncDef(ast::FuncDefExpr { param, body }))
            }),

            other => TransformResult::Continue(other),
        }
    }

    fn pre_visit_stmt(&mut self, stmt: ast::Statement) -> TransformResult<ast::Statement> {
        match stmt {
            ast::Statement::LetDef(pat, val) => {
                // Better don't rely on the transformers default order to visit the value first.
                let val = val.transform(self);
                let pat = pat.transform(self);
                TransformResult::Break(ast::Statement::LetDef(pat, val))
            }
            ast::Statement::LetRecDef(defs) => {
                for (name, _) in &defs {
                    self.bind(*name);
                }

                let mut defs_ = vec![];
                for (name, def) in defs {
                    defs_.push((self.resolve(name), def.transform(self)));
                }

                TransformResult::Break(ast::Statement::LetRecDef(defs_))
            }
            _ => TransformResult::Continue(stmt),
        }
    }

    fn pre_visit_pattern(&mut self, pat: ast::LetPattern) -> TransformResult<ast::LetPattern> {
        match pat {
            ast::LetPattern::Var(Some(v)) => {
                let newvar = self.bind(v);
                TransformResult::Break(ast::LetPattern::Var(Some(newvar)))
            }
            _ => TransformResult::Continue(pat),
        }
    }
}

use compiler_lib::ast;
use compiler_lib::spans::{Spanned, SpannedError};
use std::collections::HashMap;
use std::sync::Arc;

/// Recursively expands type expressions of the form `<<tcons t1 t2 ...>>` into the type previously
/// defined by `let type <<tcons a b ...>> = ...`, substituting t1 for a, t2 for b, etc.
pub fn expand_types(stmts: Vec<ast::Statement>) -> Result<Vec<ast::Statement>, SpannedError> {
    let mut ctx = ExpansionContext {
        env: TypeTemplates::Empty,
    };

    stmts.expand(&mut ctx)
}

struct ExpansionContext {
    env: TypeTemplates,
}

#[derive(Clone)]
enum TypeTemplates {
    Empty,
    Entry(Arc<(ast::StringId, Vec<Spanned<ast::StringId>>, ast::TypeExpr, Self)>),
}

impl TypeTemplates {
    fn lookup_and_substitute(
        &self,
        (name, span): Spanned<ast::StringId>,
        args: Vec<Spanned<ast::TypeExpr>>,
    ) -> Result<ast::TypeExpr, SpannedError> {
        match self {
            TypeTemplates::Empty => Err(SpannedError::new1("Undefined type alias", span)),
            TypeTemplates::Entry(e) if e.0 != name => e.3.lookup_and_substitute((name, span), args),
            TypeTemplates::Entry(e) => {
                let params: HashMap<ast::StringId, ast::STypeExpr> =
                    e.1.iter().zip(args).map(|((n, _), t)| (*n, t)).collect();
                e.2.substitute(&params)
            }
        }
    }
}

trait Expandable: Sized {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError>;
}

impl Expandable for ast::Statement {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        match self {
            ast::Statement::Empty => Ok(self),
            ast::Statement::Expr(sx) => sx.expand(ctx).map(ast::Statement::Expr),
            ast::Statement::LetDef(def) => def.expand(ctx).map(ast::Statement::LetDef),
            ast::Statement::LetRecDef(defs) => defs.expand(ctx).map(ast::Statement::LetRecDef),
            ast::Statement::Println(args) => args.expand(ctx).map(ast::Statement::Println),
            ast::Statement::Import(_) => Ok(self),
            ast::Statement::TypeDef(((mut defpat, ty), _)) => {
                let (name, _) = defpat.remove(0);
                let params = defpat;
                let ty = ty.expand(ctx)?;
                ctx.env = TypeTemplates::Entry(Arc::new((name, params, ty, ctx.env.clone())));
                Ok(ast::Statement::Empty)
            }
        }
    }
}

impl Expandable for ast::TypeExpr {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        match self {
            ast::TypeExpr::Bot | ast::TypeExpr::Top | ast::TypeExpr::Hole | ast::TypeExpr::Ident(_) => Ok(self),
            ast::TypeExpr::Case(cases) => cases.expand(ctx).map(ast::TypeExpr::Case),
            ast::TypeExpr::Func(rator, rand) => Ok(ast::TypeExpr::Func(rator.expand(ctx)?, rand.expand(ctx)?)),
            ast::TypeExpr::Poly(params, body, kind) => Ok(ast::TypeExpr::Poly(params.expand(ctx)?, body.expand(ctx)?, kind)),
            ast::TypeExpr::Record(fields) => fields.expand(ctx).map(ast::TypeExpr::Record),
            ast::TypeExpr::RecursiveDef(name, body) => Ok(ast::TypeExpr::RecursiveDef(name, body.expand(ctx)?)),
            ast::TypeExpr::VarJoin(jk, tys) => Ok(ast::TypeExpr::VarJoin(jk, tys.expand(ctx)?)),

            ast::TypeExpr::TypeRef(((name, args), _)) => {
                let args = args.expand(ctx)?;
                ctx.env.lookup_and_substitute(name, args)
            }
            ast::TypeExpr::Container(tc, t) => t.expand(ctx).map(|t| ast::TypeExpr::Container(tc, t)),
        }
    }
}

impl Expandable for ast::LetPattern {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        match self {
            ast::LetPattern::Case(tag, sub) => Ok(ast::LetPattern::Case(tag, sub.expand(ctx)?)),
            ast::LetPattern::Record(fields) => fields.expand(ctx).map(ast::LetPattern::Record),
            ast::LetPattern::Var(name, ty) => Ok(ast::LetPattern::Var(name, ty.expand(ctx)?)),
        }
    }
}

impl Expandable for ast::FieldTypeDecl {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        match self {
            ast::FieldTypeDecl::Imm(ty) => Ok(ast::FieldTypeDecl::Imm(ty.expand(ctx)?)),
            ast::FieldTypeDecl::RWSame(ty) => Ok(ast::FieldTypeDecl::RWSame(ty.expand(ctx)?)),
            ast::FieldTypeDecl::RWPair(ty1, ty2) => Ok(ast::FieldTypeDecl::RWPair(ty1.expand(ctx)?, ty2.expand(ctx)?)),
        }
    }
}

impl Expandable for ast::Expr {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        match self {
            ast::Expr::BinOp(x) => x.expand(ctx).map(ast::Expr::BinOp),
            ast::Expr::Block(x) => {
                let outer_env = ctx.env.clone();
                let out = x.expand(ctx).map(ast::Expr::Block);
                ctx.env = outer_env;
                out
            }
            ast::Expr::Call(x) => x.expand(ctx).map(ast::Expr::Call),
            ast::Expr::Case(x) => x.expand(ctx).map(ast::Expr::Case),
            ast::Expr::FieldAccess(x) => x.expand(ctx).map(ast::Expr::FieldAccess),
            ast::Expr::FieldSet(x) => x.expand(ctx).map(ast::Expr::FieldSet),
            ast::Expr::FuncDef(x) => x.expand(ctx).map(ast::Expr::FuncDef),
            ast::Expr::If(x) => x.expand(ctx).map(ast::Expr::If),
            ast::Expr::InstantiateExist(x) => x.expand(ctx).map(ast::Expr::InstantiateExist),
            ast::Expr::InstantiateUni(x) => x.expand(ctx).map(ast::Expr::InstantiateUni),
            ast::Expr::Literal(x) => x.expand(ctx).map(ast::Expr::Literal),
            ast::Expr::Loop(x) => x.expand(ctx).map(ast::Expr::Loop),
            ast::Expr::Match(x) => x.expand(ctx).map(ast::Expr::Match),
            ast::Expr::Record(x) => x.expand(ctx).map(ast::Expr::Record),
            ast::Expr::Typed(x) => x.expand(ctx).map(ast::Expr::Typed),
            ast::Expr::Variable(x) => x.expand(ctx).map(ast::Expr::Variable),
            ast::Expr::Array(kind, xs) => xs.expand(ctx).map(|xs| ast::Expr::Array(kind, xs)),
            ast::Expr::Dict(kind, xs) => xs.expand(ctx).map(|xs| ast::Expr::Dict(kind, xs)),
        }
    }
}

impl Expandable for ast::expr::BinOpExpr {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(ast::expr::BinOpExpr {
            lhs: self.lhs.expand(ctx)?,
            rhs: self.rhs.expand(ctx)?,
            op: self.op,
            op_type: self.op_type,
        })
    }
}

impl Expandable for ast::expr::BlockExpr {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(ast::expr::BlockExpr {
            statements: self.statements.expand(ctx)?,
            expr: self.expr.expand(ctx)?,
        })
    }
}

impl Expandable for ast::expr::CallExpr {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(ast::expr::CallExpr {
            func: self.func.expand(ctx)?,
            arg: self.arg.expand(ctx)?,
            eval_arg_first: self.eval_arg_first,
        })
    }
}

impl Expandable for ast::expr::CaseExpr {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(ast::expr::CaseExpr {
            expr: self.expr.expand(ctx)?,
            tag: self.tag,
        })
    }
}

impl Expandable for ast::expr::FieldAccessExpr {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(ast::expr::FieldAccessExpr {
            expr: self.expr.expand(ctx)?,
            field: self.field,
        })
    }
}

impl Expandable for ast::expr::FieldSetExpr {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(ast::expr::FieldSetExpr {
            expr: self.expr.expand(ctx)?,
            value: self.value.expand(ctx)?,
            field: self.field,
        })
    }
}

impl Expandable for ast::expr::FuncDefExpr {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(ast::expr::FuncDefExpr {
            type_params: self.type_params,
            param: self.param.expand(ctx)?,
            return_type: self.return_type.expand(ctx)?,
            body: self.body.expand(ctx)?,
        })
    }
}

impl Expandable for ast::expr::IfExpr {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(ast::expr::IfExpr {
            cond: self.cond.expand(ctx)?,
            then_expr: self.then_expr.expand(ctx)?,
            else_expr: self.else_expr.expand(ctx)?,
        })
    }
}

impl Expandable for ast::expr::InstantiateExistExpr {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(ast::expr::InstantiateExistExpr {
            expr: self.expr.expand(ctx)?,
            types: self.types.expand(ctx)?,
            source: self.source,
        })
    }
}

impl Expandable for ast::expr::InstantiateUniExpr {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(ast::expr::InstantiateUniExpr {
            expr: self.expr.expand(ctx)?,
            types: self.types.expand(ctx)?,
            source: self.source,
        })
    }
}

impl Expandable for ast::expr::LiteralExpr {
    fn expand(self, _: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(self)
    }
}

impl Expandable for ast::expr::LoopExpr {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(ast::expr::LoopExpr {
            body: self.body.expand(ctx)?,
        })
    }
}

impl Expandable for ast::expr::MatchExpr {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(ast::expr::MatchExpr {
            expr: self.expr.expand(ctx)?,
            cases: self.cases.expand(ctx)?,
        })
    }
}

impl Expandable for ast::expr::RecordExpr {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(ast::expr::RecordExpr {
            fields: self.fields.expand(ctx)?,
        })
    }
}

impl Expandable for ast::expr::KeyPair {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok((self.0, self.1.expand(ctx)?, self.2, self.3.expand(ctx)?))
    }
}

impl Expandable for ast::expr::VariableExpr {
    fn expand(self, _: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(self)
    }
}

impl Expandable for ast::expr::TypedExpr {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(ast::expr::TypedExpr {
            expr: self.expr.expand(ctx)?,
            type_expr: self.type_expr.expand(ctx)?,
        })
    }
}

impl Expandable for ast::TypeParam {
    fn expand(self, _: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(self)
    }
}

impl<T: Expandable> Expandable for Vec<T> {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        self.into_iter().map(|x| x.expand(ctx)).collect()
    }
}

impl<A: Expandable, B: Expandable> Expandable for (A, B) {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        let (a, b) = self;
        Ok((a.expand(ctx)?, b.expand(ctx)?))
    }
}

impl<T: Expandable> Expandable for Box<T> {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        (*self).expand(ctx).map(Box::new)
    }
}

impl<T: Expandable> Expandable for Option<T> {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        self.map(|x| x.expand(ctx)).transpose()
    }
}

impl<T: Expandable> Expandable for Spanned<T> {
    fn expand(self, ctx: &mut ExpansionContext) -> Result<Self, SpannedError> {
        let (x, span) = self;
        x.expand(ctx).map(|x| (x, span))
    }
}

impl Expandable for ast::StringId {
    fn expand(self, _: &mut ExpansionContext) -> Result<Self, SpannedError> {
        Ok(self)
    }
}

trait Substitutable: Sized {
    fn substitute(&self, subs: &HashMap<ast::StringId, ast::STypeExpr>) -> Result<Self, SpannedError>;
}

impl Substitutable for ast::TypeExpr {
    fn substitute(&self, subs: &HashMap<ast::StringId, ast::STypeExpr>) -> Result<Self, SpannedError> {
        match self {
            ast::TypeExpr::Bot | ast::TypeExpr::Top | ast::TypeExpr::Hole => Ok(self.clone()),
            ast::TypeExpr::Case(cases) => cases.substitute(subs).map(ast::TypeExpr::Case),
            ast::TypeExpr::Func(rator, rand) => Ok(ast::TypeExpr::Func(rator.substitute(subs)?, rand.substitute(subs)?)),
            ast::TypeExpr::Poly(params, body, kind) => {
                Ok(ast::TypeExpr::Poly(params.clone(), body.substitute(subs)?, *kind))
            }
            ast::TypeExpr::Record(fields) => fields.substitute(subs).map(ast::TypeExpr::Record),
            ast::TypeExpr::RecursiveDef(name, body) => Ok(ast::TypeExpr::RecursiveDef(*name, body.substitute(subs)?)),
            ast::TypeExpr::VarJoin(jk, tys) => Ok(ast::TypeExpr::VarJoin(*jk, tys.substitute(subs)?)),

            ast::TypeExpr::TypeRef((_, span)) => Err(SpannedError::new1("Unexpanded type reference", *span)),
            ast::TypeExpr::Container(tc, t) => t.substitute(subs).map(|t| ast::TypeExpr::Container(*tc, t)),

            ast::TypeExpr::Ident(name) => Ok(subs.get(name).map(|(t, _)| t).unwrap_or(self).clone()),
        }
    }
}

impl Substitutable for ast::STypeExpr {
    fn substitute(&self, subs: &HashMap<ast::StringId, ast::STypeExpr>) -> Result<Self, SpannedError> {
        let (tx, span) = self;
        match tx {
            ast::TypeExpr::Ident(name) => Ok(subs.get(name).unwrap_or(self).clone()),
            tx => tx.substitute(subs).map(|x| (x, *span)),
        }
    }
}

impl<T: Substitutable> Substitutable for Vec<T> {
    fn substitute(&self, params: &HashMap<ast::StringId, ast::STypeExpr>) -> Result<Self, SpannedError> {
        self.into_iter().map(|x| x.substitute(params)).collect()
    }
}

impl<T: Substitutable> Substitutable for Box<T> {
    fn substitute(&self, subs: &HashMap<ast::StringId, ast::STypeExpr>) -> Result<Self, SpannedError> {
        (**self).substitute(subs).map(Box::new)
    }
}

impl<A: Substitutable, B: Substitutable> Substitutable for (A, B) {
    fn substitute(&self, subs: &HashMap<ast::StringId, ast::STypeExpr>) -> Result<Self, SpannedError> {
        Ok((self.0.substitute(subs)?, self.1.substitute(subs)?))
    }
}

impl Substitutable for ast::StringId {
    fn substitute(&self, _: &HashMap<ast::StringId, ast::STypeExpr>) -> Result<Self, SpannedError> {
        Ok(*self)
    }
}

impl Substitutable for Spanned<ast::StringId> {
    fn substitute(&self, _: &HashMap<ast::StringId, ast::STypeExpr>) -> Result<Self, SpannedError> {
        Ok(*self)
    }
}

impl Substitutable for ast::FieldTypeDecl {
    fn substitute(&self, subs: &HashMap<ast::StringId, ast::STypeExpr>) -> Result<Self, SpannedError> {
        match self {
            ast::FieldTypeDecl::Imm(ty) => Ok(ast::FieldTypeDecl::Imm(ty.substitute(subs)?)),
            ast::FieldTypeDecl::RWSame(ty) => Ok(ast::FieldTypeDecl::RWSame(ty.substitute(subs)?)),
            ast::FieldTypeDecl::RWPair(ty1, ty2) => {
                Ok(ast::FieldTypeDecl::RWPair(ty1.substitute(subs)?, ty2.substitute(subs)?))
            }
        }
    }
}

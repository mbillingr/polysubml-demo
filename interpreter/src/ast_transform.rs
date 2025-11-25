use crate::runtime_ast as ast;
use compiler_lib::ast::StringId;
use compiler_lib::spans::Spanned;

pub trait AstTransformer {
    fn pre_visit_stmt(&mut self, stmt: ast::Statement) -> TransformResult<ast::Statement> {
        TransformResult::Continue(stmt)
    }
    fn pre_visit_expr(&mut self, expr: ast::Expr) -> TransformResult<ast::Expr> {
        TransformResult::Continue(expr)
    }
    fn pre_visit_pattern(&mut self, pat: ast::LetPattern) -> TransformResult<ast::LetPattern> {
        TransformResult::Continue(pat)
    }

    fn post_visit_stmt(&mut self, stmt: ast::Statement) -> ast::Statement {
        stmt
    }
    fn post_visit_expr(&mut self, epr: ast::Expr) -> ast::Expr {
        epr
    }
    fn post_visit_pattern(&mut self, pat: ast::LetPattern) -> ast::LetPattern {
        pat
    }
}

pub enum TransformResult<T> {
    /// Continue transforming the Ast node's children
    Continue(T),
    /// Accept the ast node as it is and don't transform its children further
    Break(T),
}

pub trait AstTransWalker: Sized {
    fn transform(self, visitor: &mut impl AstTransformer) -> Self;
}

impl AstTransWalker for ast::Expr {
    fn transform(self, visitor: &mut impl AstTransformer) -> Self {
        let expr = match visitor.pre_visit_expr(self) {
            TransformResult::Continue(expr) => expr,
            TransformResult::Break(expr) => return expr,
        };

        let expr = match expr {
            ast::Expr::BinOp(boe) => ast::Expr::BinOp(ast::BinOpExpr {
                lhs: boe.lhs.transform(visitor),
                rhs: boe.rhs.transform(visitor),
                ..boe
            }),

            ast::Expr::Block(blk) => ast::block(blk.statements.transform(visitor), (*blk.expr).transform(visitor)),

            ast::Expr::Call(call) => ast::Expr::Call(ast::CallExpr {
                func: call.func.transform(visitor),
                arg: call.arg.transform(visitor),
                ..call
            }),

            ast::Expr::Case(case) => ast::Expr::Case(ast::CaseExpr {
                tag: case.tag.transform(visitor),
                expr: case.expr.transform(visitor),
            }),

            ast::Expr::FieldAccess(fae) => ast::Expr::FieldAccess(ast::FieldAccessExpr {
                expr: fae.expr.transform(visitor),
                field: fae.field.transform(visitor),
            }),

            ast::Expr::FieldSet(fse) => ast::Expr::FieldSet(ast::FieldSetExpr {
                expr: fse.expr.transform(visitor),
                field: fse.field.transform(visitor),
                value: fse.value.transform(visitor),
            }),

            ast::Expr::FuncDef(fd) => ast::Expr::FuncDef(ast::FuncDefExpr {
                param: fd.param.transform(visitor),
                body: fd.body.transform(visitor),
            }),

            ast::Expr::If(ie) => ast::Expr::If(ast::IfExpr {
                cond: ie.cond.transform(visitor),
                then_expr: ie.then_expr.transform(visitor),
                else_expr: ie.else_expr.transform(visitor),
            }),

            ast::Expr::Literal(lit) => ast::Expr::Literal(lit),

            ast::Expr::Loop(lx) => ast::Expr::Loop(ast::LoopExpr {
                body: lx.body.transform(visitor),
            }),

            ast::Expr::Match(mx) => ast::Expr::Match(ast::MatchExpr {
                expr: mx.expr.transform(visitor),
                cases: mx.cases.transform(visitor),
            }),

            ast::Expr::Record(rec) => ast::Expr::Record(ast::RecordExpr {
                fields: rec.fields.transform(visitor),
            }),

            ast::Expr::Variable(vx) => ast::Expr::Variable(ast::VariableExpr { name: vx.name }),

            ast::Expr::Array(xs) => ast::Expr::Array(xs.transform(visitor)),

            ast::Expr::Dict(xs) => ast::Expr::Dict(xs.transform(visitor)),
        };

        visitor.post_visit_expr(expr)
    }
}

impl AstTransWalker for ast::Statement {
    fn transform(self, visitor: &mut impl AstTransformer) -> Self {
        let stmt = match visitor.pre_visit_stmt(self) {
            TransformResult::Continue(stmt) => stmt,
            TransformResult::Break(stmt) => return stmt,
        };

        let stmt = match stmt {
            ast::Statement::Empty => ast::Statement::Empty,

            ast::Statement::Expr(expr) => ast::Statement::Expr(expr.transform(visitor)),

            ast::Statement::LetDef(pat, val) => {
                // We visit the value before the pattern for consistency with the language semantics,
                // where the value is evaluated before being matched.
                let val = val.transform(visitor);
                let pat = pat.transform(visitor);
                ast::Statement::LetDef(pat, val)
            }

            ast::Statement::LetRecDef(lrd) => ast::Statement::LetRecDef(lrd.transform(visitor)),

            ast::Statement::Println(xs) => ast::Statement::Println(xs.transform(visitor)),
        };

        visitor.post_visit_stmt(stmt)
    }
}

impl AstTransWalker for ast::LetPattern {
    fn transform(self, visitor: &mut impl AstTransformer) -> Self {
        let pat = match visitor.pre_visit_pattern(self) {
            TransformResult::Continue(pat) => pat,
            TransformResult::Break(pat) => return pat,
        };

        let pat = match pat {
            ast::LetPattern::Var(p) => ast::LetPattern::Var(p),
            ast::LetPattern::Record(r) => ast::LetPattern::Record(r.transform(visitor)),
            ast::LetPattern::Case(x, p) => ast::LetPattern::Case(x.transform(visitor), p.transform(visitor)),
        };

        visitor.post_visit_pattern(pat)
    }
}

impl AstTransWalker for StringId {
    fn transform(self, _: &mut impl AstTransformer) -> Self {
        // this trivial impl allows us to easily transform tuples that contain stringids and ast nodes
        self
    }
}

impl AstTransWalker for bool {
    fn transform(self, _: &mut impl AstTransformer) -> Self {
        // this trivial impl allows us to easily transform tuples that contain bools and ast nodes
        self
    }
}

impl<T: AstTransWalker> AstTransWalker for Spanned<T> {
    fn transform(mut self, visitor: &mut impl AstTransformer) -> Self {
        self.0 = self.0.transform(visitor);
        self
    }
}

impl<U: AstTransWalker, V: AstTransWalker> AstTransWalker for (U, V) {
    fn transform(self, visitor: &mut impl AstTransformer) -> Self {
        (self.0.transform(visitor), self.1.transform(visitor))
    }
}

impl<U: AstTransWalker, V: AstTransWalker, W: AstTransWalker> AstTransWalker for (U, V, W) {
    fn transform(self, visitor: &mut impl AstTransformer) -> Self {
        (
            self.0.transform(visitor),
            self.1.transform(visitor),
            self.2.transform(visitor),
        )
    }
}

impl<T: AstTransWalker> AstTransWalker for Vec<T> {
    fn transform(self, visitor: &mut impl AstTransformer) -> Self {
        self.into_iter().map(|x| x.transform(visitor)).collect()
    }
}

impl<T: AstTransWalker> AstTransWalker for Option<T> {
    fn transform(self, visitor: &mut impl AstTransformer) -> Self {
        self.map(|x| x.transform(visitor))
    }
}

impl<T: AstTransWalker> AstTransWalker for Box<T> {
    fn transform(mut self, visitor: &mut impl AstTransformer) -> Self {
        *self = (*self).transform(visitor);
        self
    }
}

use crate::runtime_ast as ast;
use compiler_lib::ast::StringId;
use compiler_lib::spans::Spanned;

pub trait AstVisitor {
    fn pre_visit_stmt(&mut self, _: &ast::Statement) -> VisitResult {
        VisitResult::Continue
    }
    fn pre_visit_expr(&mut self, _: &ast::Expr) -> VisitResult {
        VisitResult::Continue
    }
    fn pre_visit_pattern(&mut self, _: &ast::LetPattern) -> VisitResult {
        VisitResult::Continue
    }

    fn post_visit_stmt(&mut self, _: &ast::Statement) {}
    fn post_visit_expr(&mut self, _: &ast::Expr) {}
    fn post_visit_pattern(&mut self, _: &ast::LetPattern) {}
}

pub enum VisitResult {
    Continue,
    Break,
}

pub trait AstWalker {
    fn walk_ast(&self, visitor: &mut impl AstVisitor);
}

impl AstWalker for ast::Expr {
    fn walk_ast(&self, visitor: &mut impl AstVisitor) {
        match visitor.pre_visit_expr(self) {
            VisitResult::Continue => {}
            VisitResult::Break => return,
        }

        match self {
            ast::Expr::BinOp(boe) => {
                boe.lhs.walk_ast(visitor);
                boe.rhs.walk_ast(visitor);
            }

            ast::Expr::Block(blk) => {
                blk.statements.walk_ast(visitor);
                blk.expr.walk_ast(visitor);
            }

            ast::Expr::Call(call) => {
                call.func.walk_ast(visitor);
                call.arg.walk_ast(visitor);
            }

            ast::Expr::Case(case) => case.expr.walk_ast(visitor),

            ast::Expr::FieldAccess(fae) => fae.expr.walk_ast(visitor),

            ast::Expr::FieldSet(fse) => {
                fse.expr.walk_ast(visitor);
                fse.value.walk_ast(visitor);
            }

            ast::Expr::FuncDef(fd) => {
                fd.body.walk_ast(visitor);
                fd.param.walk_ast(visitor);
            }

            ast::Expr::If(ie) => {
                ie.cond.walk_ast(visitor);
                ie.then_expr.walk_ast(visitor);
                ie.else_expr.walk_ast(visitor);
            }

            ast::Expr::Literal(_) => {}

            ast::Expr::Loop(lx) => lx.body.walk_ast(visitor),

            ast::Expr::Match(mx) => {
                mx.expr.walk_ast(visitor);
                mx.cases.walk_ast(visitor);
                mx.wildcard.walk_ast(visitor);
            }

            ast::Expr::Record(rec) => rec.fields.walk_ast(visitor),

            ast::Expr::Variable(_) => {}

            ast::Expr::Array(xs) => xs.walk_ast(visitor),

            ast::Expr::Dict(xs) => xs.walk_ast(visitor),
        }

        visitor.post_visit_expr(self);
    }
}

impl AstWalker for ast::Statement {
    fn walk_ast(&self, visitor: &mut impl AstVisitor) {
        match visitor.pre_visit_stmt(self) {
            VisitResult::Continue => {}
            VisitResult::Break => return,
        }

        match self {
            ast::Statement::Empty => {}
            ast::Statement::Expr(expr) => expr.walk_ast(visitor),
            ast::Statement::LetDef(pat, val) => {
                pat.walk_ast(visitor);
                val.walk_ast(visitor);
            }
            ast::Statement::LetRecDef(rec) => rec.walk_ast(visitor),
            ast::Statement::Println(xs) => xs.walk_ast(visitor),
        }

        visitor.post_visit_stmt(self);
    }
}

impl AstWalker for ast::LetPattern {
    fn walk_ast(&self, visitor: &mut impl AstVisitor) {
        match visitor.pre_visit_pattern(self) {
            VisitResult::Continue => {}
            VisitResult::Break => return,
        }

        match self {
            ast::LetPattern::Case(_, pat) => pat.walk_ast(visitor),
            ast::LetPattern::Var(_) => {}
            ast::LetPattern::Record(fields) => fields.walk_ast(visitor),
        }

        visitor.post_visit_pattern(self);
    }
}

impl AstWalker for ast::Variable {
    fn walk_ast(&self, _: &mut impl AstVisitor) {}
}

impl AstWalker for StringId {
    fn walk_ast(&self, _: &mut impl AstVisitor) {
        // this trivial impl allows us to walk tuples that contain stringids and ast nodes
    }
}

impl AstWalker for bool {
    fn walk_ast(&self, _: &mut impl AstVisitor) {
        // this trivial impl allows us to walk tuples that contain bools and ast nodes
    }
}

impl<T: AstWalker> AstWalker for Spanned<T> {
    fn walk_ast(&self, visitor: &mut impl AstVisitor) {
        self.0.walk_ast(visitor)
    }
}

impl<U: AstWalker, V: AstWalker> AstWalker for (U, V) {
    fn walk_ast(&self, visitor: &mut impl AstVisitor) {
        self.0.walk_ast(visitor);
        self.1.walk_ast(visitor);
    }
}

impl<U: AstWalker, V: AstWalker, W: AstWalker> AstWalker for (U, V, W) {
    fn walk_ast(&self, visitor: &mut impl AstVisitor) {
        self.0.walk_ast(visitor);
        self.1.walk_ast(visitor);
        self.2.walk_ast(visitor);
    }
}

impl<T: AstWalker> AstWalker for Vec<T> {
    fn walk_ast(&self, visitor: &mut impl AstVisitor) {
        for x in self {
            x.walk_ast(visitor);
        }
    }
}

impl<T: AstWalker> AstWalker for [T] {
    fn walk_ast(&self, visitor: &mut impl AstVisitor) {
        for x in self {
            x.walk_ast(visitor);
        }
    }
}

impl<T: AstWalker> AstWalker for Option<T> {
    fn walk_ast(&self, visitor: &mut impl AstVisitor) {
        if let Some(x) = self {
            x.walk_ast(visitor);
        }
    }
}

impl<T: AstWalker> AstWalker for Box<T> {
    fn walk_ast(&self, visitor: &mut impl AstVisitor) {
        (**self).walk_ast(visitor);
    }
}

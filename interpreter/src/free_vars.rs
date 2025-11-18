use compiler_lib::ast;
use compiler_lib::ast::StringId;
use compiler_lib::spans::Spanned;
use std::collections::HashSet;

pub fn free_vars(expr: &ast::Expr) -> HashSet<StringId> {
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

impl AstVisitor for FreeVarsVisitor {
    fn pre_visit_expr(&mut self, expr: &ast::Expr) -> VisitResult {
        match expr {
            ast::Expr::Block(blk) => {
                blk.expr.walk_ast(self);

                for stmt in blk.statements.iter().rev() {
                    match stmt {
                        ast::Statement::LetDef((pat, val)) => {
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
            ast::Statement::LetDef((_pat, _)) => {
                unimplemented!()
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

    fn post_visit_type(&mut self, _expr: &ast::TypeExpr) {}
}

pub struct BoundVarsVisitor<'a> {
    fvs: &'a mut HashSet<StringId>,
}

impl<'a> AstVisitor for BoundVarsVisitor<'a> {
    fn post_visit_stmt(&mut self, _stmt: &ast::Statement) {
        unimplemented!()
    }

    fn post_visit_expr(&mut self, _expr: &ast::Expr) {
        unimplemented!()
    }

    fn post_visit_pattern(&mut self, pattern: &ast::LetPattern) {
        match dbg!(pattern) {
            ast::LetPattern::Var((Some(var), _), _) => {
                self.fvs.remove(var);
            }
            _ => {}
        }
    }

    fn post_visit_type(&mut self, _expr: &ast::TypeExpr) {}
}

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
    fn pre_visit_type(&mut self, _: &ast::TypeExpr) -> VisitResult {
        VisitResult::Continue
    }

    fn post_visit_stmt(&mut self, _: &ast::Statement) {}
    fn post_visit_expr(&mut self, _: &ast::Expr) {}
    fn post_visit_pattern(&mut self, _: &ast::LetPattern) {}
    fn post_visit_type(&mut self, _: &ast::TypeExpr) {}
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
                fd.return_type.walk_ast(visitor);
                fd.body.walk_ast(visitor);
                fd.param.walk_ast(visitor);
            }

            ast::Expr::If(ie) => {
                ie.cond.walk_ast(visitor);
                ie.then_expr.walk_ast(visitor);
                ie.else_expr.walk_ast(visitor);
            }

            ast::Expr::InstantiateExist(ie) => {
                ie.expr.walk_ast(visitor);
                ie.types.walk_ast(visitor);
            }

            ast::Expr::InstantiateUni(ie) => {
                ie.expr.walk_ast(visitor);
                ie.types.walk_ast(visitor);
            }

            ast::Expr::Literal(_) => {}

            ast::Expr::Loop(lx) => lx.body.walk_ast(visitor),

            ast::Expr::Match(mx) => {
                mx.expr.walk_ast(visitor);
                mx.cases.walk_ast(visitor);
            }

            ast::Expr::Record(rec) => rec.fields.walk_ast(visitor),

            ast::Expr::Typed(tx) => {
                tx.expr.walk_ast(visitor);
                tx.type_expr.walk_ast(visitor);
            }

            ast::Expr::Variable(_) => {}

            ast::Expr::Array(_, xs) => xs.walk_ast(visitor),

            ast::Expr::Dict(_, xs) => xs.walk_ast(visitor),
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
            ast::Statement::LetDef(def) => def.walk_ast(visitor),
            ast::Statement::LetRecDef(rec) => rec.walk_ast(visitor),
            ast::Statement::Println(xs) => xs.walk_ast(visitor),
            ast::Statement::Import(_) => unimplemented!(),
            ast::Statement::TypeDef(td) => td.walk_ast(visitor),
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
            ast::LetPattern::Var(_, ty) => ty.walk_ast(visitor),
            ast::LetPattern::Record(((_, fields), _)) => fields.walk_ast(visitor),
        }

        visitor.post_visit_pattern(self);
    }
}

impl AstWalker for ast::TypeExpr {
    fn walk_ast(&self, visitor: &mut impl AstVisitor) {
        match visitor.pre_visit_type(self) {
            VisitResult::Continue => {}
            VisitResult::Break => return,
        }

        match self {
            ast::TypeExpr::Bot => {}
            ast::TypeExpr::Case(cases) => cases.walk_ast(visitor),
            ast::TypeExpr::Func(a, r) => {
                r.walk_ast(visitor);
                a.walk_ast(visitor);
            }
            ast::TypeExpr::Hole => {}
            ast::TypeExpr::Ident(_) => {}
            ast::TypeExpr::Poly(_, ty, _) => ty.walk_ast(visitor),
            ast::TypeExpr::Record(fields) => fields.walk_ast(visitor),
            ast::TypeExpr::RecursiveDef(_, ty) => ty.walk_ast(visitor),
            ast::TypeExpr::Top => {}
            ast::TypeExpr::VarJoin(_, ts) => ts.walk_ast(visitor),
            ast::TypeExpr::TypeRef(((_, ts), _)) => ts.walk_ast(visitor),
            ast::TypeExpr::Container(_, ts) => ts.walk_ast(visitor),
        }

        visitor.post_visit_type(self);
    }
}

impl AstWalker for ast::FieldTypeDecl {
    fn walk_ast(&self, visitor: &mut impl AstVisitor) {
        match self {
            ast::FieldTypeDecl::Imm(t) => t.walk_ast(visitor),
            ast::FieldTypeDecl::RWSame(t) => t.walk_ast(visitor),
            ast::FieldTypeDecl::RWPair(r, w) => {
                r.walk_ast(visitor);
                w.walk_ast(visitor);
            }
        }
    }
}

impl AstWalker for ast::expr::KeyPair {
    fn walk_ast(&self, visitor: &mut impl AstVisitor) {
        self.1.walk_ast(visitor);
        self.3.walk_ast(visitor);
    }
}

impl AstWalker for StringId {
    fn walk_ast(&self, _: &mut impl AstVisitor) {
        // this trivial impl allows us to walk tuples that contain stringids and ast nodes
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

impl<T: AstWalker> AstWalker for Vec<T> {
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

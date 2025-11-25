pub use compiler_lib::ast::{Literal, Op, OpType, StringId};
use compiler_lib::spans::Spanned;

pub fn simplify(script: Vec<compiler_lib::ast::Statement>) -> Vec<Statement> {
    to_stmts(script)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Empty,
    Expr(Expr),
    LetDef(LetPattern, Expr),
    LetRecDef(Vec<(StringId, Expr)>),
    Println(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LetPattern {
    Case(StringId, Box<LetPattern>),
    Record(Vec<(StringId, LetPattern)>),
    Var(Option<StringId>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    BinOp(BinOpExpr),
    Block(BlockExpr),
    Call(CallExpr),
    Case(CaseExpr),
    FieldAccess(FieldAccessExpr),
    FieldSet(FieldSetExpr),
    FuncDef(FuncDefExpr),
    If(IfExpr),
    Literal(LiteralExpr),
    Loop(LoopExpr),
    Match(MatchExpr),
    Record(RecordExpr),
    Variable(VariableExpr),
    Array(Vec<Expr>),
    Dict(Vec<(Expr, Expr)>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOpExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op_type: OpType,
    pub op: Op,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockExpr {
    pub statements: Vec<Statement>,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub func: Box<Expr>,
    pub arg: Box<Expr>,
    pub eval_arg_first: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CaseExpr {
    pub tag: StringId,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccessExpr {
    pub expr: Box<Expr>,
    pub field: StringId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldSetExpr {
    pub expr: Box<Expr>,
    pub field: StringId,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDefExpr {
    pub param: LetPattern,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub cond: Box<Expr>,
    pub then_expr: Box<Expr>,
    pub else_expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralExpr {
    pub lit_type: Literal,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoopExpr {
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchExpr {
    pub expr: Box<Expr>,
    pub cases: Vec<(LetPattern, Expr)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordExpr {
    pub fields: Vec<(StringId, Expr, bool)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableExpr {
    pub name: StringId,
}

pub fn block(stmts: impl IntoIterator<Item = Statement>, mut expr: Expr) -> Expr {
    let mut stmts: Vec<_> = stmts.into_iter().filter(|stmt| !matches!(stmt, Statement::Empty)).collect();
    if stmts.is_empty() {
        return expr;
    }

    if let Expr::Block(blk) = expr {
        stmts.extend(blk.statements);
        expr = *blk.expr;
    }

    Expr::Block(BlockExpr {
        statements: stmts,
        expr: Box::new(expr),
    })
}

pub fn case(tag: StringId, expr: Expr) -> Expr {
    Expr::Case(CaseExpr {
        tag,
        expr: Box::new(expr),
    })
}

pub fn field_access(field: StringId, expr: Expr) -> Expr {
    Expr::FieldAccess(FieldAccessExpr {
        expr: Box::new(expr),
        field,
    })
}

trait ToExpr {
    fn to_expr(self) -> Expr;
}

trait ToStmt {
    fn to_stmt(self) -> Statement;
}

trait ToPattern {
    fn to_pat(self) -> LetPattern;
}

fn to_stmts<T: ToStmt>(stmts: impl IntoIterator<Item = T>) -> Vec<Statement> {
    stmts.into_iter().map(T::to_stmt).collect()
}

impl ToExpr for compiler_lib::ast::Expr {
    fn to_expr(self) -> Expr {
        match self {
            compiler_lib::ast::Expr::BinOp(bxe) => Expr::BinOp(BinOpExpr {
                lhs: bxe.lhs.to_expr().into(),
                rhs: bxe.rhs.to_expr().into(),
                op_type: bxe.op_type,
                op: bxe.op,
            }),

            compiler_lib::ast::Expr::Block(be) => block(to_stmts(be.statements), be.expr.to_expr()),

            compiler_lib::ast::Expr::Call(ce) => Expr::Call(CallExpr {
                func: ce.func.to_expr().into(),
                arg: ce.arg.to_expr().into(),
                eval_arg_first: ce.eval_arg_first,
            }),

            compiler_lib::ast::Expr::Case(ce) => Expr::Case(CaseExpr {
                tag: ce.tag.0,
                expr: ce.expr.to_expr().into(),
            }),

            compiler_lib::ast::Expr::FieldAccess(fae) => Expr::FieldAccess(FieldAccessExpr {
                expr: fae.expr.to_expr().into(),
                field: fae.field.0,
            }),

            compiler_lib::ast::Expr::FieldSet(fse) => Expr::FieldSet(FieldSetExpr {
                expr: fse.expr.to_expr().into(),
                field: fse.field.0,
                value: fse.value.to_expr().into(),
            }),

            compiler_lib::ast::Expr::FuncDef(fde) => Expr::FuncDef(FuncDefExpr {
                param: fde.param.to_pat(),
                body: fde.body.to_expr().into(),
            }),

            compiler_lib::ast::Expr::If(ie) => Expr::If(IfExpr {
                cond: ie.cond.to_expr().into(),
                then_expr: ie.then_expr.to_expr().into(),
                else_expr: ie.else_expr.to_expr().into(),
            }),

            compiler_lib::ast::Expr::InstantiateExist(ie) => ie.expr.to_expr(),

            compiler_lib::ast::Expr::InstantiateUni(ie) => ie.expr.to_expr(),

            compiler_lib::ast::Expr::Literal(le) => Expr::Literal(LiteralExpr {
                lit_type: le.lit_type,
                value: le.value.0,
            }),

            compiler_lib::ast::Expr::Loop(le) => Expr::Loop(LoopExpr {
                body: le.body.to_expr().into(),
            }),

            compiler_lib::ast::Expr::Match(me) => Expr::Match(MatchExpr {
                expr: me.expr.to_expr().into(),
                cases: me.cases.into_iter().map(|(p, e)| (p.to_pat(), e.to_expr())).collect(),
            }),

            compiler_lib::ast::Expr::Record(re) => Expr::Record(RecordExpr {
                fields: re.fields.into_iter().map(|((f, _), e, m, _)| (f, e.to_expr(), m)).collect(),
            }),

            compiler_lib::ast::Expr::Typed(te) => te.expr.to_expr(),

            compiler_lib::ast::Expr::Variable(ve) => Expr::Variable(VariableExpr { name: ve.name }),

            compiler_lib::ast::Expr::Array(_, arr) => Expr::Array(arr.into_iter().map(|e| e.to_expr()).collect()),

            compiler_lib::ast::Expr::Dict(_, dict) => {
                Expr::Dict(dict.into_iter().map(|(k, v)| (k.to_expr(), v.to_expr())).collect())
            }
        }
    }
}

impl ToStmt for compiler_lib::ast::Statement {
    fn to_stmt(self) -> Statement {
        match self {
            compiler_lib::ast::Statement::Empty => Statement::Empty,
            compiler_lib::ast::Statement::Expr(e) => Statement::Expr(e.to_expr()),
            compiler_lib::ast::Statement::LetDef((pat, expr)) => Statement::LetDef(pat.to_pat(), expr.to_expr()),
            compiler_lib::ast::Statement::LetRecDef(defs) => {
                Statement::LetRecDef(defs.into_iter().map(|(n, e)| (n, e.to_expr())).collect())
            }
            compiler_lib::ast::Statement::Println(exprs) => {
                Statement::Println(exprs.into_iter().map(|e| e.to_expr()).collect())
            }
            compiler_lib::ast::Statement::Import(_) => unimplemented!(),
            compiler_lib::ast::Statement::TypeDef(_) => unimplemented!(),
        }
    }
}

impl ToPattern for compiler_lib::ast::LetPattern {
    fn to_pat(self) -> LetPattern {
        match self {
            compiler_lib::ast::LetPattern::Case((tag, _), pat) => LetPattern::Case(tag, pat.to_pat().into()),
            compiler_lib::ast::LetPattern::Record(((_, fields), _)) => {
                LetPattern::Record(fields.into_iter().map(|((f, _), p)| (f, p.to_pat())).collect())
            }
            compiler_lib::ast::LetPattern::Var((var, _), _) => LetPattern::Var(var),
        }
    }
}

impl<T: ToExpr> ToExpr for Spanned<T> {
    fn to_expr(self) -> Expr {
        self.0.to_expr()
    }
}

impl<T: ToExpr> ToExpr for Box<T> {
    fn to_expr(self) -> Expr {
        (*self).to_expr()
    }
}

impl<T: ToPattern> ToPattern for Spanned<T> {
    fn to_pat(self) -> LetPattern {
        self.0.to_pat()
    }
}

/// Builder functions for constructing AST nodes without a source. Used for tests and such.
#[cfg(test)]
pub mod builder {
    use super::*;
    use compiler_lib::Rodeo;
    use compiler_lib::grammar::ScriptParser;
    use compiler_lib::spans::SpanManager;
    use std::cell::RefCell;

    thread_local! {
        static STRINGS: RefCell<Rodeo> = RefCell::new(Rodeo::new());
    }

    pub fn stmts(src: &str) -> Vec<Statement> {
        STRINGS.with(|s| {
            let mut strings = s.borrow_mut();
            let mut span_mgr = SpanManager::default();
            let span_maker = span_mgr.add_source(src.to_owned());
            let mut ctx = compiler_lib::ast::ParserContext {
                span_maker,
                strings: &mut *strings,
            };
            let parser = ScriptParser::new();
            let ast = parser.parse(&mut ctx, src).unwrap();
            simplify(ast)
        })
    }
}

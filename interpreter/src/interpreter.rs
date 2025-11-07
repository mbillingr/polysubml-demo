use crate::value::Builtin;
use crate::value::Value;
use compiler_lib::ast::StringId;
use compiler_lib::{Rodeo, ast};
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;
use im::{OrdMap, Vector};
use crate::builtins::define_builtins;

pub struct Context<'a> {
    pub static_env: StaticEnv,
    pub strings: &'a mut Rodeo,
}

#[derive(Clone)]
pub struct StaticEnv(Vector<StringId>);

impl StaticEnv {
    pub fn new() -> Self {
        StaticEnv(Vector::new())
    }

    fn lookup(&self, name: StringId) -> CompiledBinding {
        if let Some(index) = self.0.iter().rposition(|&x| x == name) {
            return CompiledBinding(index)
        }
        panic!("binding not found")
    }

    pub fn push_binding(&mut self, name: StringId) -> CompiledBinding {
        let index = self.0.len();
        self.0.push_back(name);
        CompiledBinding(index)
    }
}

impl std::fmt::Debug for StaticEnv {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.iter().enumerate().collect::<Vec<_>>().fmt(f)
    }
}

pub struct Runtime<'a> {
    pub env: Env,
    pub strings: &'a mut Rodeo,
}

impl<'a> Runtime<'a> {
}

pub struct CompiledStatement(Box<dyn Fn(&mut Runtime)>);
#[derive(Clone)]
pub struct CompiledExpr(Arc<dyn Fn(&mut Runtime) -> Value>);

#[derive(Debug, Clone, Copy)]
pub struct CompiledBinding(usize);

#[derive(Debug, Clone)]
pub enum CompiledPattern {
    Case(StringId, Box<CompiledPattern>),
    Record(Vec<(StringId, CompiledPattern)>),
    Var(CompiledBinding),
    Wildcard,
}

impl<'a> Runtime<'a> {
    pub fn exec(&mut self, stmt: &CompiledStatement) {
        stmt.0(self)
    }

    pub fn eval(&mut self, expr: &CompiledExpr) -> Value {
        expr.0(self)
    }
}

impl<'a> Context<'a> {
    pub fn new(static_env: StaticEnv, strings: &'a mut Rodeo) -> Self {
        Context { static_env, strings }
    }

    pub fn push_binding(&mut self, name: StringId) -> CompiledBinding {
        self.static_env.push_binding(name)
    }

    pub fn analyze_stmts(&mut self, stmts: &[ast::Statement]) -> CompiledStatement {
        let mut stmt = CompiledStatement::nop();
        for st in stmts {
            stmt = stmt.chain(self.analyze_stmt(st));
        }
        stmt
    }

    pub fn analyze_stmt(&mut self, stmt: &ast::Statement) -> CompiledStatement {
        match stmt {
            ast::Statement::Empty => CompiledStatement::nop(),
            ast::Statement::Expr(expr) => {
                let x = self.analyze_expr(&expr.0);
                CompiledStatement::ignore_value(x)
            }
            ast::Statement::LetDef((pat, expr)) => {
                // need to analyze val before pat extends the environment
                let val = self.analyze_expr(&expr.0);
                let pat =self.compile_let_pattern(pat);

                CompiledStatement(Box::new(move |rt|{
                    let val = rt.eval(&val);
                    destructure_pattern(&pat, Cow::Owned(val), &mut rt.env)
                }))
            }
            ast::Statement::LetRecDef(defs) => {
                let mut stmt = CompiledStatement::nop();
                for (name, _) in defs {
                    let bnd = self.push_binding(*name);
                    stmt = stmt.chain(CompiledStatement::store_placeholder(bnd))
                }

                for (name, expr) in defs {
                    let val = self.analyze_expr(&expr.0);
                    stmt = stmt.chain(CompiledStatement::initialize_placeholder(self.static_env.lookup(*name), val))
                }
                stmt
            }
            ast::Statement::Println(exprs) => {
                let mut stmt = CompiledStatement::println();
                for x in exprs.iter().rev() {
                    let x_ = self.analyze_expr(&x.0);
                    stmt = CompiledStatement::print(x_).chain(stmt);
                }
                stmt
            }
            ast::Statement::Import(_) => unimplemented!(),
            ast::Statement::TypeDef(_) => unimplemented!(),
        }
    }

    pub fn analyze_expr(&mut self, expr: &ast::Expr) -> CompiledExpr {
        match expr {
            ast::Expr::BinOp(bop) => {
                let lhs = self.analyze_expr(&bop.lhs.0);
                let rhs = self.analyze_expr(&bop.rhs.0);
                CompiledExpr::binop(bop.op.clone(), lhs, rhs)
            }

            ast::Expr::Block(block) => {
                let outer_env = self.static_env.clone();

                let mut stmts = CompiledStatement::nop();

                for stmt in &block.statements {
                    stmts = stmts.chain(self.analyze_stmt(stmt));
                }

                let expr = self.analyze_expr(&block.expr.0);
                self.static_env = outer_env;
                CompiledExpr::block(stmts, expr)
            }

            ast::Expr::Call(call) => {
                let func;
                let arg;
                if call.eval_arg_first {
                    arg = self.analyze_expr(&call.arg.0);
                    func = self.analyze_expr(&call.func.0);
                } else {
                    func = self.analyze_expr(&call.func.0);
                    arg = self.analyze_expr(&call.arg.0);
                }
                CompiledExpr::call(func, arg, call.eval_arg_first)
            }

            ast::Expr::Case(case) => {
                CompiledExpr::make_case(case.tag.0, self.analyze_expr(&case.expr.0))
            },

            ast::Expr::FieldAccess(fa) => {
                let obj = self.analyze_expr(&fa.expr.0);
                CompiledExpr::get_field(fa.field.0, obj)
            }

            ast::Expr::FieldSet(fs) => {
                let obj = self.analyze_expr(&fs.expr.0);
                let val = self.analyze_expr(&fs.value.0);
                CompiledExpr::set_field(fs.field.0, val, obj)
            }

            ast::Expr::FuncDef(def) => {
                let outer_env = self.static_env.clone();
                let pat = self.compile_let_pattern(&def.param.0);
                println!("{:?}", self.static_env);
                let body = self.analyze_expr(&def.body.0);
                self.static_env = outer_env;
                CompiledExpr::make_func(pat, body)
            },

            ast::Expr::If(ifexp) => {
                let condition = self.analyze_expr(&ifexp.cond.0.0);
                let consequence = self.analyze_expr(&ifexp.then_expr.0);
                let alternative = self.analyze_expr(&ifexp.else_expr.0);
                CompiledExpr::branch(condition, consequence, alternative)
            }

            ast::Expr::InstantiateExist(iex) => self.analyze_expr(&iex.expr.0),

            ast::Expr::InstantiateUni(iun) => self.analyze_expr(&iun.expr.0),

            ast::Expr::Literal(lit) => CompiledExpr::constant(match lit.lit_type {
                ast::Literal::Bool => Value::bool(lit.value.0.parse().unwrap()),
                ast::Literal::Int => Value::int(lit.value.0.parse().unwrap()),
                ast::Literal::Float => Value::float(lit.value.0.parse().unwrap()),
                ast::Literal::Str => Value::str(&lit.value.0.strip_prefix('"').unwrap().strip_suffix('"').unwrap()),
            }),

            ast::Expr::Loop(lx) => {
                let brk = self.strings.get_or_intern_static("Break");
                let body = self.analyze_expr(&lx.body.0);
                CompiledExpr::make_loop(body, brk)
            }

            ast::Expr::Match(mx) => {
                let val = self.analyze_expr(&mx.expr.0.0);

                //let stmt = CompiledStatement::set_match_value(val);

                let mut ex = CompiledExpr::unreachable();

                for ((pat, _), arm) in mx.cases.iter().rev() {
                    let body = self.analyze_expr(&arm.0);
                    let p = self.compile_match_arm(pat, body);
                    //let cond = self.test_pattern(pat);
                    //let ex = CompiledExpr::branch(cond, go, ex);
                    todo!()
                }

                ex
            }

            ast::Expr::Record(rec) => {
                CompiledExpr::make_record(rec.fields.iter().map(|field| (field.0.0, self.analyze_expr(&field.1.0))).collect())
            },

            ast::Expr::Typed(tx) => self.analyze_expr(&tx.expr.0),

            ast::Expr::Variable(var) => {
                let bnd = self.static_env.lookup(var.name);
                println!("{:?} -> {:?}", var, bnd);
                CompiledExpr::get_var(bnd)
            },
        }
    }

    fn compile_let_pattern(&mut self, pat: &ast::LetPattern) -> CompiledPattern {
        match pat {
            ast::LetPattern::Var((None, _), _) => CompiledPattern::Wildcard,

            ast::LetPattern::Var((Some(var), _), _) => {
                let idx = self.push_binding(*var);
                CompiledPattern::Var(idx)
            },

            ast::LetPattern::Case((variant, _), inner_pat) => {
                let inner_pat = self.compile_let_pattern(&**inner_pat);
                CompiledPattern::Case(*variant, Box::new(inner_pat))
            }

            ast::LetPattern::Record(((_, field_patterns), _)) => {
                let ps = field_patterns.iter().map(|((field, _), inner_pat)|(*field, self.compile_let_pattern(inner_pat))).collect();
                CompiledPattern::Record(ps)
            }
        }
    }

    fn compile_match_arm(&mut self, pat: &ast::LetPattern, body: CompiledExpr) -> CompiledPattern {
        match pat {
            ast::LetPattern::Var((None, _), _) => todo!(),

            ast::LetPattern::Var((Some(var), _), _) => todo!(),

            ast::LetPattern::Case((variant, _), inner_pat) => {
                let inner_pat = self.compile_let_pattern(&**inner_pat);
                CompiledPattern::Case(*variant, Box::new(inner_pat))
            }

            ast::LetPattern::Record(((_, field_patterns), _)) => {
                let ps = field_patterns.iter().map(|((field, _), inner_pat)|(*field, self.compile_let_pattern(inner_pat))).collect();
                CompiledPattern::Record(ps)
            }
        }
    }
}

fn destructure_pattern(pat: &CompiledPattern, val: Cow<Value>, env: &mut Env) {
    match pat {
        CompiledPattern::Wildcard => { }
        CompiledPattern::Var(bnd) => {
            env.bind(*bnd, val.into_owned())
        },

        CompiledPattern::Case(_, inner_pat) => {
            let (_, inner_val) = val.as_case();
            destructure_pattern(&*inner_pat, Cow::Borrowed(inner_val), env)
        }

        CompiledPattern::Record(field_patterns) => {
            for (field, inner_pat) in field_patterns {
                destructure_pattern(inner_pat, Cow::Owned(val.get_field(*field)), env);
            }
        }
    }
}


impl CompiledStatement {

    fn chain(self, next: Self) -> Self {
        CompiledStatement(Box::new(move |rt|{
            rt.exec(&self);
            rt.exec(&next);
        }))
    }

    fn nop() -> Self {
        CompiledStatement(Box::new(|_| {}))
    }

    fn ignore_value(val: CompiledExpr) -> Self {
        CompiledStatement(Box::new(move |rt| {
            rt.eval(&val);
        }))
    }

    fn store_placeholder(bnd: CompiledBinding) -> Self {
        CompiledStatement(Box::new(move |rt|{
            rt.env.bind(bnd, Value::uninitialized());
        }))
    }

    fn initialize_placeholder(bnd: CompiledBinding, val: CompiledExpr) -> Self {
        CompiledStatement(Box::new(move |rt|{
            rt.env.lookup(bnd).initialize(rt.eval(&val));
        }))
    }

    fn println() -> Self {
        CompiledStatement(Box::new(|_| {println!()}))
    }

    fn print(val: CompiledExpr) -> Self {
        CompiledStatement(Box::new(move |rt| {
            print!("{} ", rt.eval(&val).show(rt.strings))
        }))
    }
}

impl CompiledExpr {
    fn unreachable() -> Self {
        CompiledExpr(Arc::new(move |_| panic!("unreachable")))
    }

    fn block(stmt: CompiledStatement, expr: CompiledExpr) -> Self {
        CompiledExpr(Arc::new(move |rt|{
            let old_env = rt.env.clone();
            rt.exec(&stmt);
            let out = rt.eval(&expr);
            rt.env = old_env;
            out
        }))
    }

    fn constant(val: Value) -> Self {
        CompiledExpr(Arc::new(move |_|val.clone()))
    }

    fn binop(op: ast::Op, lhs: Self, rhs: Self) -> Self {
        CompiledExpr(match op {
            ast::Op::Add => Arc::new(move |rt|rt.eval(&lhs) + rt.eval(&rhs)),
            ast::Op::Sub => Arc::new(move |rt|rt.eval(&lhs) - rt.eval(&rhs)),
            ast::Op::Mult => Arc::new(move |rt|rt.eval(&lhs) * rt.eval(&rhs)),
            ast::Op::Div => Arc::new(move |rt|rt.eval(&lhs) / rt.eval(&rhs)),
            ast::Op::Rem => Arc::new(move |rt|rt.eval(&lhs) % rt.eval(&rhs)),

            ast::Op::Lt => Arc::new(move |rt|Value::bool(rt.eval(&lhs) < rt.eval(&rhs))),
            ast::Op::Lte => Arc::new(move |rt|Value::bool(rt.eval(&lhs) <= rt.eval(&rhs))),
            ast::Op::Gt => Arc::new(move |rt|Value::bool(rt.eval(&lhs) > rt.eval(&rhs))),
            ast::Op::Gte => Arc::new(move |rt|Value::bool(rt.eval(&lhs) >= rt.eval(&rhs))),

            ast::Op::Eq => Arc::new(move |rt|Value::bool(rt.eval(&lhs) == rt.eval(&rhs))),
            ast::Op::Neq => Arc::new(move |rt|Value::bool(rt.eval(&lhs) != rt.eval(&rhs))),
        })
    }

    fn call(func: Self, arg: Self, eval_arg_first: bool) -> Self {
        CompiledExpr(Arc::new(move |rt| {
            let f;
            let a;
            if eval_arg_first {
                a = rt.eval(&arg);
                f = rt.eval(&func);
            } else {
                f = rt.eval(&func);
                a = rt.eval(&arg);
            }
            match &*f.initialized() {
                Value::Func(f) => {
                    let mut local_env = f.closure.clone();
                    destructure_pattern(&f.param, Cow::Owned(a), &mut local_env);
                    let old_env = std::mem::replace(&mut rt.env, local_env);
                    let result = rt.eval(&f.body);
                    rt.env = old_env;
                    result
                }
                Value::Builtin(Builtin(f)) => f(a, rt),
                _ => panic!("not callable: {:?}", func),
            }
        }))
    }

    fn make_case(tag: StringId, val: CompiledExpr) -> Self {
        CompiledExpr(Arc::new(move |rt|Value::case(tag, rt.eval(&val))))
    }

    fn make_record(field_values: Vec<(StringId, CompiledExpr)>) -> Self {
        CompiledExpr(Arc::new(move |rt|{
            Value::record(field_values.iter().map(|(name, val)|(*name, rt.eval(val))))
        }))
    }

    fn get_field(name: StringId, obj: CompiledExpr) -> Self {
        CompiledExpr(Arc::new(move |rt|rt.eval(&obj).get_field(name)))
    }

    fn set_field(name: StringId, val: CompiledExpr, obj: CompiledExpr) -> Self {
        CompiledExpr(Arc::new(move |rt|{
            let obj = rt.eval(&obj);
            let val = rt.eval(&val);
            obj.set_field(name, val)
        }))
    }

    fn make_func(pat: CompiledPattern, body: CompiledExpr) -> Self {
        CompiledExpr(Arc::new(move |rt|{
            Value::func(pat.clone(), body.clone(), rt.env.clone())
        }))
    }

    fn branch(condition: CompiledExpr, consequence: CompiledExpr, alterative: CompiledExpr) -> Self {
        CompiledExpr(Arc::new(move |rt|{
            if rt.eval(&condition).as_bool() {
                rt.eval(&consequence)
            } else {
                rt.eval(&alterative)
            }
        }))
    }

    fn make_loop(body: CompiledExpr, break_tag: StringId) -> Self {
        CompiledExpr(Arc::new(move |rt|{
            loop {
                let res = rt.eval(&body);
                let (tag, val) = res.as_case();
                if tag == break_tag {
                    return val.clone();
                }
            }
        }))
    }

    fn get_var(bnd: CompiledBinding) -> Self {
        CompiledExpr(Arc::new(move |rt| rt.env.lookup(bnd)))
    }
}

impl std::fmt::Debug for CompiledExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<compiled expr>")
    }
}

#[derive(Clone)]
pub struct Env(pub Vector<Value>);

impl Env {
    pub fn new() -> Self {
        Env(Vector::new())
    }

    pub fn bind(&mut self, bnd: CompiledBinding, value: Value) {
        self.0.push_back(value);
    }

    fn lookup(&self, bnd: CompiledBinding) -> Value {
        match self.0.get(bnd.0) {
            Some(val) => val.clone(),
            None => panic!("unbound variable: {}", bnd.0),
        }
    }
}

impl std::fmt::Debug for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.iter().enumerate().collect::<Vec<_>>().fmt(f)
    }
}
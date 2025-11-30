use crate::ast_processor::AstProcessor;
use crate::builtins;
use crate::runtime_ast as ast;
use crate::value::{Builtin, Func};
use crate::value::{ImHashMap, Value};
use compiler_lib::Rodeo;
use compiler_lib::ast::StringId;
use std::borrow::Cow;
use std::cell::RefCell;
use std::sync::Arc;

pub struct State {
    env: Env,
}

impl State {
    pub fn run_script(&mut self, script: &[ast::Statement], strings: &mut Rodeo) {
        let mut ctx = Context::new(self, strings);
        for stmt in script {
            ctx.exec(&stmt);
        }
    }
}

impl AstProcessor for State {
    fn process_script(&mut self, script: &[ast::Statement], strings: &mut Rodeo) {
        self.run_script(script, strings);
    }
}

pub struct Context<'a> {
    pub state: &'a mut State,
    pub strings: &'a mut Rodeo,
}

impl State {
    pub fn new(env: Env) -> Self {
        State { env }
    }
}

impl<'a> Context<'a> {
    pub fn new(state: &'a mut State, strings: &'a mut Rodeo) -> Self {
        Context { state, strings }
    }

    pub fn exec(&mut self, stmt: &ast::Statement) {
        match stmt {
            ast::Statement::Empty => {}
            ast::Statement::Expr(expr) => {
                self.eval(&expr);
            }
            ast::Statement::LetDef(pat, expr) => {
                let val = self.eval(&expr);
                assign(pat, Cow::Owned(val), &mut self.state.env);
            }
            ast::Statement::LetRecDef(defs) => {
                for (name, _) in defs {
                    self.state.env = self.state.env.bind_placeholder(*name);
                }

                for (name, expr) in defs {
                    let val = self.eval(&expr);
                    self.state.env.set_placeholder(*name, val);
                }
            }
            ast::Statement::Println(exprs) => {
                for x in exprs {
                    print!("{} ", self.eval(&x).show(self.strings));
                }
                println!()
            }
        }
    }

    pub fn eval(&mut self, expr: &ast::Expr) -> Value {
        match expr {
            ast::Expr::BinOp(bop) => {
                use compiler_lib::ast::Literal::*;
                use compiler_lib::ast::Op::*;
                let lhs = self.eval(&bop.lhs);
                let rhs = self.eval(&bop.rhs);
                match (&bop.op_type.0, &bop.op) {
                    (None, Eq) => Value::bool(lhs == rhs),
                    (None, Neq) => Value::bool(lhs != rhs),
                    (Some(Int), Lt) => Value::bool(lhs.as_int() < rhs.as_int()),
                    (Some(Int), Lte) => Value::bool(lhs.as_int() <= rhs.as_int()),
                    (Some(Int), Gt) => Value::bool(lhs.as_int() > rhs.as_int()),
                    (Some(Int), Gte) => Value::bool(lhs.as_int() >= rhs.as_int()),
                    (Some(Int), Add) => Value::int(lhs.as_int() + rhs.as_int()),
                    (Some(Int), Sub) => Value::int(lhs.as_int() - rhs.as_int()),
                    (Some(Int), Mult) => Value::int(lhs.as_int() * rhs.as_int()),
                    (Some(Int), Div) => Value::int(lhs.as_int() / rhs.as_int()),
                    (Some(Int), Rem) => Value::int(lhs.as_int() % rhs.as_int()),
                    (Some(Float), Lt) => Value::bool(lhs.as_float() < rhs.as_float()),
                    (Some(Float), Lte) => Value::bool(lhs.as_float() <= rhs.as_float()),
                    (Some(Float), Gt) => Value::bool(lhs.as_float() > rhs.as_float()),
                    (Some(Float), Gte) => Value::bool(lhs.as_float() >= rhs.as_float()),
                    (Some(Float), Add) => Value::float(lhs.as_float() + rhs.as_float()),
                    (Some(Float), Sub) => Value::float(lhs.as_float() - rhs.as_float()),
                    (Some(Float), Mult) => Value::float(lhs.as_float() * rhs.as_float()),
                    (Some(Float), Div) => Value::float(lhs.as_float() / rhs.as_float()),
                    (Some(Float), Rem) => Value::float(lhs.as_float() % rhs.as_float()),
                    (Some(Str), Add) => Value::string(lhs.as_str().to_string() + rhs.as_str()),
                    other => todo!("{other:?}"),
                }
            }

            ast::Expr::Block(block) => {
                let outer_env = self.state.env.clone();

                for stmt in &block.statements {
                    self.exec(stmt);
                }

                let result = self.eval(&block.expr);

                self.state.env = outer_env;

                result
            }

            ast::Expr::Call(call) => {
                let func;
                let arg;
                if call.eval_arg_first {
                    arg = self.eval(&call.arg);
                    func = self.eval(&call.func);
                } else {
                    func = self.eval(&call.func);
                    arg = self.eval(&call.arg);
                }
                match func.as_func() {
                    Func::Func(pat, body, cls) => {
                        let val = Cow::Owned(arg);
                        let local_env = match_pattern(pat, val, cls).unwrap();
                        let old_env = std::mem::replace(&mut self.state.env, local_env);
                        let result = self.eval(body);
                        self.state.env = old_env;
                        result
                    }
                    Func::Builtin(Builtin(f)) => f(arg, &mut builtins::Context { strings: self.strings }),
                    _ => panic!("not callable: {:?}", func),
                }
            }

            ast::Expr::Case(case) => Value::case(case.tag, self.eval(&case.expr)),

            ast::Expr::FieldAccess(fa) => {
                let obj = self.eval(&fa.expr);
                obj.get_field(fa.field)
            }

            ast::Expr::FieldSet(fs) => {
                let obj = self.eval(&fs.expr);
                let val = self.eval(&fs.value);
                obj.set_field(fs.field, val)
            }

            ast::Expr::FuncDef(def) => Value::func(def.param.clone(), (*def.body).clone(), self.state.env.clone()),

            ast::Expr::If(ifexp) => {
                let cond = self.eval(&ifexp.cond);
                if cond.as_bool() {
                    self.eval(&ifexp.then_expr)
                } else {
                    self.eval(&ifexp.else_expr)
                }
            }

            ast::Expr::Literal(lit) => match lit.lit_type {
                compiler_lib::ast::Literal::Bool => Value::bool(lit.value.parse().unwrap()),
                compiler_lib::ast::Literal::Int => Value::int(lit.value.parse().unwrap()),
                compiler_lib::ast::Literal::Float => Value::float(lit.value.parse().unwrap()),
                compiler_lib::ast::Literal::Str => {
                    Value::str(&lit.value.strip_prefix('"').unwrap().strip_suffix('"').unwrap())
                }
            },

            ast::Expr::Loop(lx) => {
                let brk = self.strings.get_or_intern_static("Break");
                loop {
                    let res = self.eval(&lx.body);
                    let (tag, val) = res.as_case();
                    if tag == brk {
                        return val.clone();
                    }
                }
            }

            ast::Expr::Match(mx) => {
                let val0 = self.eval(&mx.expr);
                let (tag, val) = val0.as_case();

                for (t, p, arm) in &mx.cases {
                    if *t != tag {
                        continue;
                    }
                    let env = match_pattern(&p, Cow::Borrowed(&val), &self.state.env).unwrap();
                    let old_env = std::mem::replace(&mut self.state.env, env);
                    let result = self.eval(&arm);
                    self.state.env = old_env;
                    return result;
                }

                let (var, arm) = mx.wildcard.as_ref().unwrap();
                let env = match_var(var, Cow::Owned(val0), &self.state.env).unwrap();
                let old_env = std::mem::replace(&mut self.state.env, env);
                let result = self.eval(arm);
                self.state.env = old_env;
                result
            }

            ast::Expr::Record(rec) => Value::record(rec.fields.iter().map(|field| (field.0, self.eval(&field.1), field.2))),

            ast::Expr::Variable(var) => self.state.env.lookup(var.name).unwrap(),

            ast::Expr::Array(items) => Value::vect(items.iter().map(|item| self.eval(&item)).collect::<Vec<_>>()),

            ast::Expr::Dict(items) => Value::dict(
                items
                    .iter()
                    .map(|item| (self.eval(&item.0), self.eval(&item.1)))
                    .collect::<ImHashMap<_, _>>(),
            ),
        }
    }
}

fn match_pattern(pat: &ast::LetPattern, val: Cow<Value>, env: &Env) -> Option<Env> {
    match pat {
        ast::LetPattern::Var(var) => match_var(var, val, env),

        ast::LetPattern::Case(tag, inner_pat) => {
            let (actual_tag, inner_val) = val.as_case();
            if *tag != actual_tag {
                return None;
            }
            match_pattern(&**inner_pat, Cow::Borrowed(inner_val), env)
        }

        ast::LetPattern::Record(field_patterns) => {
            let mut env_ = env.clone();
            for (field, inner_pat) in field_patterns {
                env_ = match_pattern(&*inner_pat, Cow::Owned(val.get_field(*field)), &env_)?;
            }
            Some(env_)
        }
    }
}

fn match_var(ast::Variable(var): &ast::Variable, val: Cow<Value>, env: &Env) -> Option<Env> {
    match var {
        None => Some(env.clone()),
        Some(var) => Some(env.bind(*var, val.into_owned())),
    }
}

fn assign(pat: &ast::LetPattern, val: Cow<Value>, env: &mut Env) {
    match pat {
        ast::LetPattern::Var(ast::Variable(None)) => {}
        ast::LetPattern::Var(ast::Variable(Some(var))) => {
            let bnd = env.bind(*var, val.into_owned());
            *env = bnd;
        }

        ast::LetPattern::Case(_, inner_pat) => {
            let (_, inner_val) = val.as_case();
            assign(&**inner_pat, Cow::Borrowed(inner_val), env)
        }

        ast::LetPattern::Record(field_patterns) => {
            for (field, inner_pat) in field_patterns {
                assign(&*inner_pat, Cow::Owned(val.get_field(*field)), env);
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Env {
    Empty,
    Entry(Arc<(StringId, Value, Env)>),
    Lazy(Arc<(StringId, RefCell<Option<Value>>, Env)>),
}

impl Env {
    pub fn new() -> Env {
        Env::Empty
    }

    pub fn bind(&self, name: StringId, value: Value) -> Env {
        Env::Entry(Arc::new((name, value, self.clone())))
    }

    fn lookup(&self, name: StringId) -> Option<Value> {
        let mut cursor = self;
        loop {
            match cursor {
                Env::Empty => return None,
                Env::Entry(entry) => {
                    let (n, v, c) = &**entry;
                    if *n == name {
                        return Some(v.clone());
                    }
                    cursor = c;
                }
                Env::Lazy(entry) => {
                    let (n, cov, c) = &**entry;
                    if *n == name {
                        if let Some(v) = &*cov.borrow() {
                            return Some(v.clone());
                        } else {
                            panic!("Uninitialized recursive value")
                        }
                    }
                    cursor = c;
                }
            }
        }
    }

    fn bind_placeholder(&self, name: StringId) -> Env {
        Env::Lazy(Arc::new((name, RefCell::new(None), self.clone())))
    }

    fn set_placeholder(&self, name: StringId, value: Value) {
        let mut cursor = self;
        loop {
            match cursor {
                Env::Empty => panic!("unbound name"),
                Env::Entry(entry) => {
                    let (n, _, c) = &**entry;
                    if *n == name {
                        panic!("immutable binding")
                    }
                    cursor = c;
                }
                Env::Lazy(entry) => {
                    let (n, cov, c) = &**entry;
                    if *n == name {
                        if cov.borrow_mut().replace(value.clone()).is_some() {
                            panic!("Placeholder assigned twice")
                        }
                        return;
                    }
                    cursor = c;
                }
            }
        }
    }
}

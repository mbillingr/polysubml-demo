use crate::value::Builtin;
use crate::value::Value;
use compiler_lib::ast::StringId;
use compiler_lib::{Rodeo, ast};
use num::ToPrimitive;
use std::borrow::Cow;
use std::cell::RefCell;
use std::io::BufRead;
use std::sync::Arc;

pub struct Context<'a> {
    state: &'a mut State,
    strings: &'a mut Rodeo,
}

pub struct State {
    env: Env,
}

impl State {
    pub fn with_builtins(strings: &mut Rodeo) -> Self {
        State {
            env: Env::builtins(strings),
        }
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
                self.eval(&expr.0);
            }
            ast::Statement::LetDef((pat, expr)) => {
                let val = self.eval(&expr.0);
                assign(pat, Cow::Owned(val), &mut self.state.env);
            }
            ast::Statement::LetRecDef(defs) => {
                for (name, _) in defs {
                    self.state.env = self.state.env.bind_placeholder(*name);
                }

                for (name, expr) in defs {
                    let val = self.eval(&expr.0);
                    self.state.env.set_placeholder(*name, val);
                }
            }
            ast::Statement::Println(exprs) => {
                for x in exprs {
                    print!("{} ", self.eval(&x.0).show(self.strings));
                }
                println!()
            }
            ast::Statement::Import(_) => unimplemented!(),
            ast::Statement::TypeDef(_) => unimplemented!(),
        }
    }

    pub fn eval(&mut self, expr: &ast::Expr) -> Value {
        match expr {
            ast::Expr::BinOp(bop) => {
                let lhs = self.eval(&bop.lhs.0);
                let rhs = self.eval(&bop.rhs.0);
                match bop.op {
                    ast::Op::Add => lhs + rhs,
                    ast::Op::Sub => lhs - rhs,
                    ast::Op::Mult => lhs * rhs,
                    ast::Op::Div => lhs / rhs,
                    ast::Op::Rem => lhs % rhs,

                    ast::Op::Lt => Value::bool(lhs < rhs),
                    ast::Op::Lte => Value::bool(lhs <= rhs),
                    ast::Op::Gt => Value::bool(lhs > rhs),
                    ast::Op::Gte => Value::bool(lhs >= rhs),

                    ast::Op::Eq => Value::bool(lhs == rhs),
                    ast::Op::Neq => Value::bool(lhs != rhs),
                }
            }

            ast::Expr::Block(block) => {
                for stmt in &block.statements {
                    self.exec(stmt);
                }

                self.eval(&block.expr.0)
            }

            ast::Expr::Call(call) => {
                let func;
                let arg;
                if call.eval_arg_first {
                    arg = self.eval(&call.arg.0);
                    func = self.eval(&call.func.0);
                } else {
                    func = self.eval(&call.func.0);
                    arg = self.eval(&call.arg.0);
                }
                match func {
                    Value::Func(f) => {
                        let pat = &f.0;
                        let cls = &f.2;
                        let val = Cow::Owned(arg);
                        let local_env = match_pattern(pat, val, cls).unwrap();
                        let old_env = std::mem::replace(&mut self.state.env, local_env);
                        let result = self.eval(&f.1);
                        self.state.env = old_env;
                        result
                    }
                    Value::Builtin(Builtin(f)) => f(arg, self),
                    _ => panic!("not callable: {:?}", func),
                }
            }

            ast::Expr::Case(case) => Value::case(case.tag.0, self.eval(&case.expr.0)),

            ast::Expr::FieldAccess(fa) => {
                let obj = self.eval(&fa.expr.0);
                obj.get_field(fa.field.0)
            }

            ast::Expr::FieldSet(fs) => {
                let obj = self.eval(&fs.expr.0);
                let val = self.eval(&fs.value.0);
                obj.set_field(fs.field.0, val)
            }

            ast::Expr::FuncDef(def) => Value::func(def.param.0.clone(), def.body.0.clone(), self.state.env.clone()),

            ast::Expr::If(ifexp) => {
                let cond = self.eval(&ifexp.cond.0.0);
                if cond.as_bool() {
                    self.eval(&ifexp.then_expr.0)
                } else {
                    self.eval(&ifexp.else_expr.0)
                }
            }

            ast::Expr::InstantiateExist(iex) => self.eval(&iex.expr.0),

            ast::Expr::InstantiateUni(iun) => self.eval(&iun.expr.0),

            ast::Expr::Literal(lit) => match lit.lit_type {
                ast::Literal::Bool => Value::bool(lit.value.0.parse().unwrap()),
                ast::Literal::Int => Value::int(lit.value.0.parse().unwrap()),
                ast::Literal::Float => Value::float(lit.value.0.parse().unwrap()),
                ast::Literal::Str => Value::str(&lit.value.0.strip_prefix('"').unwrap().strip_suffix('"').unwrap()),
            },

            ast::Expr::Loop(lx) => {
                let brk = self.strings.get_or_intern_static("Break");
                loop {
                    let res = self.eval(&lx.body.0);
                    let (tag, val) = res.as_case();
                    if tag == brk {
                        return val.clone();
                    }
                }
            }

            ast::Expr::Match(mx) => {
                let val = self.eval(&mx.expr.0.0);
                for (pat, arm) in &mx.cases {
                    if let Some(env) = match_pattern(&pat.0, Cow::Borrowed(&val), &self.state.env) {
                        let old_env = std::mem::replace(&mut self.state.env, env);
                        let result = self.eval(&arm.0);
                        self.state.env = old_env;
                        return result;
                    }
                }
                unimplemented!()
            }

            ast::Expr::Record(rec) => Value::record(rec.fields.iter().map(|field| (field.0.0, self.eval(&field.1.0)))),

            ast::Expr::Typed(tx) => self.eval(&tx.expr.0),

            ast::Expr::Variable(var) => self.state.env.lookup(var.name).unwrap(),
        }
    }
}

fn match_pattern(pat: &ast::LetPattern, val: Cow<Value>, env: &Env) -> Option<Env> {
    match pat {
        ast::LetPattern::Var((None, _), _) => Some(env.clone()),
        ast::LetPattern::Var((Some(var), _), _) => Some(env.bind(*var, val.into_owned())),

        ast::LetPattern::Case(tag, inner_pat) => {
            let (actual_tag, inner_val) = val.as_case();
            if tag.0 != actual_tag {
                return None;
            }
            match_pattern(&**inner_pat, Cow::Borrowed(inner_val), env)
        }

        ast::LetPattern::Record(((_, field_patterns), _)) => {
            let mut env_ = env.clone();
            for ((field, _), inner_pat) in field_patterns {
                env_ = match_pattern(&*inner_pat, Cow::Owned(val.get_field(*field)), &env_)?;
            }
            Some(env_)
        }
    }
}

fn assign(pat: &ast::LetPattern, val: Cow<Value>, env: &mut Env) {
    match pat {
        ast::LetPattern::Var((None, _), _) => {}
        ast::LetPattern::Var((Some(var), _), _) => {
            let bnd = env.bind(*var, val.into_owned());
            *env = bnd;
        }

        ast::LetPattern::Case((_, _), inner_pat) => {
            let (_, inner_val) = val.as_case();
            assign(&**inner_pat, Cow::Borrowed(inner_val), env)
        }

        ast::LetPattern::Record(((_, field_patterns), _)) => {
            for ((field, _), inner_pat) in field_patterns {
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
    fn new() -> Env {
        Env::Empty
    }

    fn builtins(strings: &mut Rodeo) -> Env {
        let mut env = Env::new();

        let ok = strings.get_or_intern_static("Ok");
        let err = strings.get_or_intern_static("Err");
        let some = strings.get_or_intern_static("Some");

        let eof_ = Value::case(strings.get_or_intern_static("Eof"), Value::int(0.into()));
        let none_ = Value::case(strings.get_or_intern_static("None"), Value::Nothing);

        let name = strings.get_or_intern_static("panic");
        let value = Value::builtin(|msg, ctx| panic!("{}", msg.show(ctx.strings)));
        env = env.bind(name, value);

        let name = strings.get_or_intern_static("__read_line");
        let eof = eof_.clone();
        let value = Value::builtin(move |_, _| {
            let mut s = String::new();
            match std::io::stdin().lock().read_line(&mut s) {
                Ok(0) => eof.clone(),
                Ok(_) => {
                    if s.ends_with('\n') {
                        s.pop();
                    }
                    Value::case(ok, Value::string(s))
                }
                Err(e) => Value::case(err, Value::string(e.to_string())),
            }
        });
        env = env.bind(name, value);

        let name = strings.get_or_intern_static("__write_str");
        let value = Value::builtin(|s, _| {
            print!("{}", s.as_str());
            Value::Nothing
        });
        env = env.bind(name, value);

        let name = strings.get_or_intern_static("__chars");
        let none = none_.clone();
        let value = Value::builtin(move |s, _| {
            let none = none.clone();
            let chars = RefCell::new(s.as_str().chars().rev().collect::<Vec<_>>());
            Value::builtin(move |_, _| match chars.borrow_mut().pop() {
                None => none.clone(),
                Some(ch) => Value::case(some, Value::string(ch.to_string())),
            })
        });
        env = env.bind(name, value);

        let name = strings.get_or_intern_static("__split");
        let none = none_.clone();
        let value = Value::builtin(move |s, _| {
            let none = none.clone();
            let parts = RefCell::new(s.as_str().split_whitespace().rev().map(str::to_string).collect::<Vec<_>>());
            Value::builtin(move |_, _| match parts.borrow_mut().pop() {
                None => none.clone(),
                Some(ch) => Value::case(some, Value::string(ch.to_string())),
            })
        });
        env = env.bind(name, value);

        let name = strings.get_or_intern_static("__escape");
        let value =
            Value::builtin(move |s, _| Value::string(String::from_utf8(escape_bytes::escape(s.as_str().bytes())).unwrap()));
        env = env.bind(name, value);

        let name = strings.get_or_intern_static("__unescape");
        let value = Value::builtin(move |s, _| {
            Value::string(String::from_utf8(escape_bytes::unescape(s.as_str().bytes()).unwrap()).unwrap())
        });
        env = env.bind(name, value);

        let name = strings.get_or_intern_static("__int_to_float");
        let vaue = Value::builtin(move |x, _| Value::float(x.as_int().to_f64().unwrap()));
        env = env.bind(name, vaue);

        let name = strings.get_or_intern_static("__float_to_int");
        let vaue = Value::builtin(move |x, _| Value::int((x.as_float() as i64).into()));
        env = env.bind(name, vaue);

        let name = strings.get_or_intern_static("__str_to_int");
        let none = none_.clone();
        let vaue = Value::builtin(move |x, _| match x.as_str().parse::<_>() {
            Ok(x) => Value::case(some, Value::int(x)),
            Err(_) => none.clone(),
        });
        env = env.bind(name, vaue);

        let name = strings.get_or_intern_static("__str_to_float");
        let none = none_.clone();
        let vaue = Value::builtin(move |x, _| match x.as_str().parse::<_>() {
            Ok(x) => Value::case(some, Value::float(x)),
            Err(_) => none.clone(),
        });
        env = env.bind(name, vaue);

        let name = strings.get_or_intern_static("__int_to_str");
        let vaue = Value::builtin(move |x, _| Value::string(x.as_int().to_string()));
        env = env.bind(name, vaue);

        let name = strings.get_or_intern_static("__float_to_str");
        let vaue = Value::builtin(move |x, _| Value::string(x.as_float().to_string()));
        env = env.bind(name, vaue);

        env
    }

    fn bind(&self, name: StringId, value: Value) -> Env {
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

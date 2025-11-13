use crate::compiler::Op;
use crate::interpreter::Env;
use crate::{builtins, vm};
use compiler_lib::ast::StringId;
use compiler_lib::{Rodeo, ast};
use im_rc::Vector;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::fmt::Formatter;
use std::rc::Rc;

pub type Int = num_bigint::BigInt;

#[derive(Clone, Debug)]
pub enum Value {
    Nothing,

    Bool(bool),
    Int(Rc<Int>),
    Float(f64),
    String(Rc<String>),

    Case(Rc<(StringId, Self)>),
    Record(Rc<FxHashMap<StringId, Field>>),

    Callable(Rc<Func>),

    Env(vm::Env),

    Vect(Vector<Value>),
}

#[derive(Clone, Debug)]
pub enum Field {
    Imm(Value),
    Mut(RefCell<Value>),
}

impl Field {
    fn immutable(val: Value) -> Self {
        Field::Imm(val)
    }

    fn mutable(val: Value) -> Self {
        Field::Mut(RefCell::new(val))
    }

    fn get(&self) -> Value {
        match self {
            Field::Imm(v) => v.clone(),
            Field::Mut(v) => v.borrow().clone(),
        }
    }

    fn set(&self, val: Value) -> Value {
        match self {
            Field::Imm(_) => panic!("Cannot set immutable field"),
            Field::Mut(v) => std::mem::replace(&mut *v.borrow_mut(), val),
        }
    }
}

#[derive(Debug)]
pub enum Func {
    Func(ast::LetPattern, ast::Expr, Env),
    Func2(Rc<Vec<Op>>, vm::Env),
    Builtin(Builtin),
}

impl Value {
    pub fn nothing() -> Value {
        Value::Nothing
    }

    pub fn bool(b: bool) -> Value {
        Value::Bool(b)
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => unimplemented!(),
        }
    }

    pub fn int(i: Int) -> Value {
        Value::Int(Rc::new(i))
    }

    pub fn usize(i: usize) -> Value {
        Value::Int(Rc::new(i.into()))
    }

    pub fn as_int(&self) -> &Int {
        match self {
            Value::Int(i) => i,
            _ => unimplemented!(),
        }
    }

    pub fn float(f: f64) -> Value {
        Value::Float(f)
    }

    pub fn as_float(&self) -> f64 {
        match self {
            Value::Float(f) => *f,
            _ => unimplemented!(),
        }
    }

    pub fn str(s: &str) -> Value {
        Value::string(s.to_string())
    }

    pub fn string(s: String) -> Value {
        Value::rc_str(Rc::new(s))
    }

    pub fn rc_str(s: Rc<String>) -> Value {
        Value::String(s)
    }

    pub fn as_str(&self) -> &str {
        match self {
            Value::String(s) => s.as_str(),
            _ => unimplemented!(),
        }
    }

    pub fn case(tag: StringId, val: Self) -> Value {
        Value::Case(Rc::new((tag, val)))
    }

    pub fn record(fields: impl Iterator<Item = (StringId, Value, bool)>) -> Value {
        Value::Record(Rc::new(
            fields
                .map(|(k, v, m)| if m { (k, Field::mutable(v)) } else { (k, Field::immutable(v)) })
                .collect(),
        ))
    }

    pub fn func(param: ast::LetPattern, expr: ast::Expr, env: Env) -> Value {
        Value::Callable(Rc::new(Func::Func(param, expr, env)))
    }

    pub fn func2(body: Rc<Vec<Op>>, env: vm::Env) -> Value {
        Value::Callable(Rc::new(Func::Func2(body, env)))
    }

    pub fn builtin(f: impl Fn(Value, &mut builtins::Context) -> Value + 'static) -> Value {
        Value::Callable(Rc::new(Func::Builtin(Builtin(Box::new(f)))))
    }

    pub fn as_func(&self) -> &Func {
        match self {
            Value::Callable(f) => f,
            _ => unimplemented!(),
        }
    }

    pub fn env(env: vm::Env) -> Self {
        Value::Env(env)
    }

    pub fn into_env(self) -> vm::Env {
        match self {
            Value::Env(env) => env,
            _ => unimplemented!(),
        }
    }

    pub fn vect(data: impl Into<Vector<Value>>) -> Value {
        Value::Vect(data.into())
    }

    pub fn as_vect(&self) -> &Vector<Value> {
        match self {
            Value::Vect(v) => v,
            _ => unimplemented!(),
        }
    }
}

impl Value {
    pub fn as_case(&self) -> (StringId, &Value) {
        match self {
            Value::Case(cs) => (cs.0, &cs.1),
            _ => unimplemented!(),
        }
    }

    pub fn get_field(&self, field: StringId) -> Value {
        match self {
            Value::Record(rec) => rec.get(&field).unwrap().get(),
            _ => unimplemented!(),
        }
    }

    pub fn set_field(&self, field: StringId, val: Value) -> Value {
        match self {
            Value::Record(rec) => rec.get(&field).unwrap().set(val),
            _ => unimplemented!(),
        }
    }
}

impl Value {
    pub fn show(&self, r: &Rodeo) -> String {
        match self {
            Value::Nothing => "()".to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Int(i) => i.to_string(),
            Value::Float(x) => x.to_string(),
            Value::String(s) => format!("{s:?}"),
            Value::Case(a) => format!("`{} {}", r.resolve(&a.0), a.1.show(r)),
            Value::Record(fields) => {
                let mut s = String::new();
                s.push('{');
                for (n, v) in fields.iter() {
                    s.push_str(&format!("{}={}; ", r.resolve(n), v.get().show(r)));
                }
                s.push('}');
                s
            }

            Value::Callable(f) => f.to_string(),

            Value::Env(_) => "<env>".to_string(),

            Value::Vect(v) => {
                let mut s = String::new();
                s.push('[');
                for (i, v) in v.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&v.show(r));
                }
                s.push(']');
                s
            }
        }
    }
}

impl std::fmt::Display for Func {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Func::Func(_, _, _) => write!(f, "<fun>"),
            Func::Func2(_, _) => write!(f, "<fun>"),
            Func::Builtin(_) => write!(f, "<builtin function>"),
        }
    }
}
impl std::cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Value::*;
        match (self, other) {
            (Int(a), Int(b)) => a.partial_cmp(b),
            (Float(a), Float(b)) => a.partial_cmp(b),
            (String(a), String(b)) => a.partial_cmp(b),
            _ => unimplemented!(),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (Bool(a), Bool(b)) => a.eq(b),
            (Int(a), Int(b)) => a.eq(b),
            (Float(a), Float(b)) => a.eq(b),
            (String(a), String(b)) => Rc::ptr_eq(a, b) || a.eq(b),
            (Case(a), Case(b)) => Rc::ptr_eq(a, b),
            (Record(a), Record(b)) => Rc::ptr_eq(a, b),
            (Callable(a), Callable(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

pub struct Builtin(pub Box<dyn Fn(Value, &mut builtins::Context) -> Value>);

impl std::fmt::Debug for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<builtin function>")
    }
}

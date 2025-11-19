use crate::compiler::Op;
use crate::interpreter::Env;
use crate::{builtins, vm};
use compiler_lib::ast::StringId;
use compiler_lib::{Rodeo, ast};
pub use im_rc::{HashMap as ImHashMap, Vector};
use indicatif::ProgressBar;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

pub type Int = num_bigint::BigInt;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Value(*mut u8);

#[derive(Debug)]
pub enum Object {
    Int(Int),
    String(String),

    Case(StringId, Value),
    Record(HashMap<StringId, Value>),

    Callable(Func),

    Env(vm::Env),

    Vect(Vector<Value>),
    Dict(ImHashMap<Value, Value>),

    PBar(ProgressBar),
}

#[derive(Debug)]
pub enum Func {
    Func(ast::LetPattern, ast::Expr, Env),
    Func2(Rc<Vec<Op>>, vm::Env),
    Builtin(Builtin),
}

impl Value {
    pub fn nothing() -> Value {
        Value(std::ptr::null_mut())
    }

    fn new_ptr(x: Object) -> Value {
        Value(Box::leak(Box::new(x)) as *mut _ as _)
    }

    fn as_obj(&self) -> &Object {
        unsafe { &*(self.0 as *const Object) }
    }

    fn as_obj_mut(&self) -> &mut Object {
        unsafe { &mut *(self.0 as *mut Object) }
    }

    pub fn bool(b: bool) -> Value {
        Value(std::ptr::without_provenance_mut(b as usize))
    }

    pub fn as_bool(&self) -> bool {
        self.0 as usize != 0
    }

    pub fn int(i: Int) -> Value {
        Value::new_ptr(Object::Int(i))
    }

    pub fn usize(i: usize) -> Value {
        Value::new_ptr(Object::Int(i.into()))
    }

    pub fn as_int(&self) -> &Int {
        match self.as_obj() {
            Object::Int(i) => i,
            _ => unimplemented!(),
        }
    }

    pub fn float(f: f64) -> Value {
        unsafe { Value(std::mem::transmute(f)) }
    }

    pub fn as_float(&self) -> f64 {
        unsafe { std::mem::transmute(self.0) }
    }

    pub fn str(s: &str) -> Value {
        Value::string(s.to_string())
    }

    pub fn string(s: String) -> Value {
        Value::new_ptr(Object::String(s))
    }

    pub fn as_str(&self) -> &str {
        match self.as_obj() {
            Object::String(s) => s.as_str(),
            _ => unimplemented!(),
        }
    }

    pub fn case(tag: StringId, val: Self) -> Value {
        Value::new_ptr(Object::Case(tag, val))
    }

    pub fn record(fields: impl IntoIterator<Item = (StringId, Value, bool)>) -> Value {
        Value::new_ptr(Object::Record(fields.into_iter().map(|(k, v, _)| (k, v)).collect()))
    }

    pub fn func(param: ast::LetPattern, expr: ast::Expr, env: Env) -> Value {
        Value::new_ptr(Object::Callable(Func::Func(param, expr, env)))
    }

    pub fn func2(body: Rc<Vec<Op>>, env: vm::Env) -> Value {
        Value::new_ptr(Object::Callable(Func::Func2(body, env)))
    }

    pub fn builtin(f: impl Fn(Value, &mut builtins::Context) -> Value + 'static) -> Value {
        Value::new_ptr(Object::Callable(Func::Builtin(Builtin(Arc::new(f)))))
    }

    pub fn as_func(&self) -> &Func {
        match self.as_obj() {
            Object::Callable(f) => f,
            _ => unimplemented!(),
        }
    }

    pub fn env(env: vm::Env) -> Self {
        Value::new_ptr(Object::Env(env))
    }

    pub fn into_env(self) -> vm::Env {
        match self.as_obj() {
            Object::Env(env) => env.clone(),
            _ => unimplemented!(),
        }
    }

    pub fn vect(data: impl Into<Vector<Value>>) -> Value {
        Value::new_ptr(Object::Vect(data.into()))
    }

    pub fn as_vect(&self) -> &Vector<Value> {
        match self.as_obj() {
            Object::Vect(v) => v,
            _ => unimplemented!(),
        }
    }

    pub fn dict(data: impl Into<ImHashMap<Value, Value>>) -> Value {
        Value::new_ptr(Object::Dict(data.into()))
    }

    pub fn as_dict(&self) -> &ImHashMap<Value, Value> {
        match self.as_obj() {
            Object::Dict(d) => d,
            _ => unimplemented!(),
        }
    }
}

impl Value {
    pub fn as_case(&self) -> (StringId, &Value) {
        match self.as_obj() {
            Object::Case(tag, val) => (*tag, val),
            _ => unimplemented!(),
        }
    }

    pub fn get_field(&self, field: StringId) -> Value {
        match self.as_obj() {
            Object::Record(rec) => rec.get(&field).cloned().unwrap(),
            _ => unimplemented!(),
        }
    }

    pub fn set_field(&self, field: StringId, val: Value) -> Value {
        match self.as_obj_mut() {
            Object::Record(rec) => rec.insert(field, val).unwrap(),
            _ => unimplemented!(),
        }
    }
}

impl Value {
    pub fn pbar(n: Option<u64>) -> Value {
        Value::new_ptr(Object::PBar(if let Some(n) = n {
            ProgressBar::new(n)
        } else {
            ProgressBar::no_length()
        }))
    }

    pub fn as_pbar(&self) -> &ProgressBar {
        match self.as_obj() {
            Object::PBar(p) => p,
            _ => unimplemented!(),
        }
    }
}

impl Value {
    pub fn show(&self, _r: &Rodeo) -> String {
        format!("{:?}", self)
    }
}

#[derive(Clone)]
pub struct Builtin(pub Arc<dyn Fn(Value, &mut builtins::Context) -> Value>);

impl std::fmt::Debug for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<builtin function>")
    }
}

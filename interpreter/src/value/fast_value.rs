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
pub enum Func {
    Func(ast::LetPattern, ast::Expr, Env),
    Func2(Rc<Vec<Op>>, vm::Env),
    Builtin(Builtin),
}

impl Value {
    pub fn nothing() -> Value {
        Value(std::ptr::null_mut())
    }

    fn new_ptr<T>(x: T) -> Value {
        Self::new_boxed(Box::new(x))
    }

    fn new_boxed<T>(x: Box<T>) -> Value {
        Value(Box::leak(x) as *mut _ as _)
    }

    fn as_<T>(&self) -> &T {
        unsafe { &*(self.0 as *const T) }
    }

    fn as_mut<T>(&self) -> &mut T {
        unsafe { &mut *(self.0 as *mut T) }
    }

    pub fn bool(b: bool) -> Value {
        Value(std::ptr::without_provenance_mut(b as usize))
    }

    pub fn as_bool(&self) -> bool {
        self.0 as usize != 0
    }

    pub fn int(i: Int) -> Value {
        Value::new_ptr(i)
    }

    pub fn usize(i: usize) -> Value {
        Value::int(i.into())
    }

    pub fn as_int(&self) -> &Int {
        self.as_()
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
        Value::new_ptr(s.into_boxed_str())
    }

    pub fn as_str(&self) -> &str {
        self.as_::<Box<str>>().as_ref()
    }

    pub fn case(tag: StringId, val: Self) -> Value {
        Value::new_ptr((tag, val))
    }

    pub fn record(fields: impl IntoIterator<Item = (StringId, Value, bool)>) -> Value {
        let rec: HashMap<StringId, Value> = fields.into_iter().map(|(k, v, _)| (k, v)).collect();
        Value::new_ptr(rec)
    }

    pub fn func(param: ast::LetPattern, expr: ast::Expr, env: Env) -> Value {
        Value::new_ptr(Func::Func(param, expr, env))
    }

    pub fn func2(body: Rc<Vec<Op>>, env: vm::Env) -> Value {
        Value::new_ptr(Func::Func2(body, env))
    }

    pub fn builtin(f: impl Fn(Value, &mut builtins::Context) -> Value + 'static) -> Value {
        Value::new_ptr(Func::Builtin(Builtin(Arc::new(f))))
    }

    pub fn as_func(&self) -> &Func {
        self.as_()
    }

    pub fn env(env: vm::Env) -> Self {
        Value::new_ptr(env)
    }

    pub fn into_env(self) -> vm::Env {
        self.as_::<vm::Env>().clone()
    }

    pub fn vect(data: impl Into<Vector<Value>>) -> Value {
        Value::new_ptr(data.into())
    }

    pub fn as_vect(&self) -> &Vector<Value> {
        self.as_()
    }

    pub fn dict(data: impl Into<ImHashMap<Value, Value>>) -> Value {
        Value::new_ptr(data.into())
    }

    pub fn as_dict(&self) -> &ImHashMap<Value, Value> {
        self.as_()
    }
}

impl Value {
    pub fn as_case(&self) -> (StringId, &Value) {
        let (tag, val) = self.as_::<(StringId, Value)>();
        (*tag, val)
    }

    pub fn get_field(&self, field: StringId) -> Value {
        self.as_::<HashMap<StringId, Value>>().get(&field).cloned().unwrap()
    }

    pub fn set_field(&self, field: StringId, val: Value) -> Value {
        self.as_mut::<HashMap<StringId, Value>>().insert(field, val).unwrap()
    }
}

impl Value {
    pub fn pbar(n: Option<u64>) -> Value {
        Value::new_ptr(if let Some(n) = n {
            ProgressBar::new(n)
        } else {
            ProgressBar::no_length()
        })
    }

    pub fn as_pbar(&self) -> &ProgressBar {
        self.as_()
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

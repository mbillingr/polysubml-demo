/*!

These are the definitions required for running code compiled to Rust.

*/

use crate::{_Field, _Tag};
use im_rc::{HashMap as ImHashMap, Vector};
use indicatif::ProgressBar;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Formatter;
use std::hash::Hasher;
use std::rc::Rc;

pub type Int = num_bigint::BigInt;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Value(*mut u8);

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

    pub fn int_literal(i: i64) -> Value {
        Value::int(i.into())
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

    pub fn case(tag: _Tag, val: Self) -> Value {
        Value::new_ptr((tag, val))
    }

    pub fn record(fields: impl IntoIterator<Item = (_Field, Value, bool)>) -> Value {
        let rec: HashMap<_Field, Value> = fields.into_iter().map(|(k, v, _)| (k, v)).collect();
        Value::new_ptr(rec)
    }

    pub fn func(f: impl Fn(Value) -> Value + 'static) -> Value {
        Value::new_ptr(Box::new(f))
    }

    pub fn apply(&self, arg: Value) -> Value {
        self.as_::<Box<dyn Fn(Value) -> Value>>()(arg)
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
    pub fn as_case(&self) -> (_Tag, &Value) {
        let (tag, val) = self.as_::<(_Tag, Value)>();
        (*tag, val)
    }

    pub fn get_field(&self, field: _Field) -> Value {
        self.as_::<HashMap<_Field, Value>>().get(&field).cloned().unwrap()
    }

    pub fn set_field(&self, field: _Field, val: Value) -> Value {
        self.as_mut::<HashMap<_Field, Value>>().insert(field, val).unwrap()
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

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:p}", self.0)
    }
}

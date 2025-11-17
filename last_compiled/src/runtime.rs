/*!

These are the definitions required for running code compiled to Rust.

*/

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Formatter;
use std::hash::Hasher;
use std::rc::Rc;
use im_rc::{HashMap as ImHashMap, Vector};
use crate::{_Field, _Tag};

pub type Int = num_bigint::BigInt;

#[derive(Clone)]
pub enum Value {
    Nothing,

    Bool(bool),
    Int(Rc<Int>),
    Float(f64),
    String(Rc<String>),

    Cell(Rc<RefCell<Value>>),

    Case(Rc<(_Tag, Self)>),
    Record(Rc<HashMap<_Field, Field>>),

    Callable(Rc<dyn Fn(Value) -> Value>),

    Vect(Vector<Value>),
    Dict(ImHashMap<Value, Value>),
}

#[derive(Clone)]
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

impl Value {
    pub fn nothing() -> Value {
        Value::Nothing
    }

    pub fn cell(x: Value) -> Value {
        Value::Cell(Rc::new(RefCell::new(x)))
    }

    pub fn update_cell(&self, x: Value) {
        match self {
            Value::Cell(c) => *c.borrow_mut() = x,
            _ => unimplemented!(),
        }
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

    pub fn int_literal(i: i64) -> Value {
        Value::Int(Rc::new(i.into()))
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

    pub fn case(tag: _Tag, val: Self) -> Value {
        Value::Case(Rc::new((tag, val)))
    }

    pub fn record(fields: impl IntoIterator<Item = (_Field, Value, bool)>) -> Value {
        Value::Record(Rc::new(
            fields
                .into_iter()
                .map(|(_k, _v, _m)| if _m { (_k, Field::mutable(_v)) } else { (_k, Field::immutable(_v)) })
                .collect(),
        ))
    }

    pub fn func(f: impl Fn(Value) -> Value + 'static) -> Value {
        Value::Callable(Rc::new(f))
    }

    pub fn apply(&self, arg: Value) -> Value {
        match self {
            Value::Cell(c) => c.borrow().apply(arg),
            Value::Callable(f) => f(arg),
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

    pub fn dict(data: impl Into<ImHashMap<Value, Value>>) -> Value {
        Value::Dict(data.into())
    }

    pub fn as_dict(&self) -> &ImHashMap<Value, Value> {
        match self {
            Value::Dict(d) => d,
            _ => unimplemented!(),
        }
    }
}

impl Value {
    pub fn as_case(&self) -> (_Tag, &Value) {
        match self {
            Value::Case(cs) => (cs.0, &cs.1),
            _ => unimplemented!(),
        }
    }

    pub fn get_field(&self, field: _Field) -> Value {
        match self {
            Value::Record(rec) => rec.get(&field).unwrap().get(),
            _ => unimplemented!(),
        }
    }

    pub fn set_field(&self, field: _Field, val: Value) -> Value {
        match self {
            Value::Record(rec) => rec.get(&field).unwrap().set(val),
            _ => unimplemented!(),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Cell(c) => write!(f, "{}", c.borrow()),
            Value::Nothing => write!(f, "()"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Int(i) => write!(f, "{i}"),
            Value::Float(x) => write!(f, "{x}"),
            Value::String(s) => write!(f, "{s:?}"),
            Value::Case(a) => write!(f, "`{:?} {}", a.0, a.1),
            Value::Record(fields) => {
                let mut s = String::new();
                s.push('{');
                for (_n, _v) in fields.iter() {
                    s.push_str(&format!("{:?}={}; ", _n, _v.get()));
                }
                s.push('}');
                write!(f, "{s}")
            }

            Value::Callable(_) => write!(f, "<func>"),

            Value::Vect(v) => {
                let mut s = "#[".to_string();
                for (i, v) in v.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&format!("{}", v));
                }
                s.push(']');
                write!(f, "{s}")
            }

            Value::Dict(v) => {
                let mut s = "#{".to_string();
                for (i, (k, v)) in v.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&format!("{}", k));
                    s.push_str(": ");
                    s.push_str(&format!("{}", v));
                }
                s.push('}');
                write!(f, "{s}")
            }
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
            (Cell(a), b) => a.borrow().eq(b),
            (a, Cell(b)) => a.eq(&*b.borrow()),
            (Bool(a), Bool(b)) => a.eq(b),
            (Int(a), Int(b)) => a.eq(b),
            (Float(a), Float(b)) => a.eq(b),
            (String(a), String(b)) => Rc::ptr_eq(a, b) || a.eq(b),
            (Case(a), Case(b)) => Rc::ptr_eq(a, b) || a.0.eq(&b.0) && a.1.eq(&b.1),
            (Record(a), Record(b)) => Rc::ptr_eq(a, b),
            (Callable(a), Callable(b)) => Rc::ptr_eq(a, b),
            (Vect(a), Vect(b)) => false,
            (Dict(a), Dict(b)) => false,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl std::hash::Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Nothing => ().hash(state),
            Value::Cell(x) => x.borrow().hash(state),
            Value::Bool(b) => b.hash(state),
            Value::Int(i) => i.hash(state),
            Value::Float(f) => f.to_bits().hash(state),
            Value::String(s) => s.hash(state),
            Value::Case(a) => a.0.hash(state),
            Value::Record(a) => Rc::as_ptr(a).hash(state),
            Value::Callable(a) => Rc::as_ptr(a).hash(state),
            Value::Vect(v) => v.hash(state),
            Value::Dict(d) => todo!(),
        }
    }
}

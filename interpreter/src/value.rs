use crate::interpreter::{Context, Env};
use compiler_lib::ast::StringId;
use compiler_lib::{Rodeo, ast};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

type Int = num_bigint::BigInt;

#[derive(Clone, Debug)]
pub enum Value {
    Nothing,

    Bool(bool),
    Int(Int),
    Float(f64),
    String(Arc<String>),

    Case(Arc<(StringId, Self)>),
    Record(Arc<RwLock<HashMap<StringId, Self>>>),

    Func(Arc<(ast::LetPattern, ast::Expr, Env)>),
    Builtin(Builtin),
}

impl Value {
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
        Value::Int(i)
    }

    pub fn float(f: f64) -> Value {
        Value::Float(f)
    }

    pub fn str(s: &str) -> Value {
        Value::string(s.to_string())
    }

    pub fn string(s: String) -> Value {
        Value::arc_str(Arc::new(s))
    }

    pub fn arc_str(s: Arc<String>) -> Value {
        Value::String(s)
    }

    pub fn as_str(&self) -> &str {
        match self {
            Value::String(s) => s.as_str(),
            _ => unimplemented!(),
        }
    }

    pub fn case(tag: StringId, val: Self) -> Value {
        Value::Case(Arc::new((tag, val)))
    }

    pub fn record(fields: impl Iterator<Item = (StringId, Value)>) -> Value {
        Value::Record(Arc::new(RwLock::new(fields.collect())))
    }

    pub fn func(param: ast::LetPattern, expr: ast::Expr, env: Env) -> Value {
        Value::Func(Arc::new((param, expr, env)))
    }

    pub fn builtin(f: impl Fn(Value, &mut Context) -> Value + 'static) -> Value {
        Value::Builtin(Builtin(Arc::new(f)))
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
            Value::Record(rec) => rec.read().unwrap().get(&field).cloned().unwrap(),
            _ => unimplemented!(),
        }
    }

    pub fn set_field(&self, field: StringId, val: Value) -> Value {
        match self {
            Value::Record(rec) => {
                let mut lock = rec.write().unwrap();
                std::mem::replace(lock.get_mut(&field).unwrap(), val)
            }
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
                for (n, v) in fields.read().unwrap().iter() {
                    s.push_str(&format!("{}={}; ", r.resolve(n), v.show(r)));
                }
                s.push('}');
                s
            }
            Value::Func(_) => "<fun>".to_string(),
            //Value::Func(f) => format!("{:?}", f),
            Value::Builtin(_) => "<builtin function>".to_string(),
        }
    }
}

macro_rules! impl_binop {
    ($tr:ident, $op:ident, $($vs:ident),*) => {
        impl std::ops::$tr for Value {
            type Output = Value;

            fn $op(self, rhs: Self) -> Self::Output {
                use Value::*;
                match (self, rhs) {
                    $(
                        ($vs(a), $vs(b)) => $vs(a.$op(b)),
                    )*
                    _ => unimplemented!(),
                }
            }
        }
    }
}

// Don't use the macro for + because we need to handle string concatenation
impl std::ops::Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Int(a), Int(b)) => Int(a.add(b)),
            (Float(a), Float(b)) => Float(a.add(b)),
            (String(a), String(b)) => Value::string((*a).clone().add(&b)),
            _ => unimplemented!(),
        }
    }
}

impl_binop!(Sub, sub, Int, Float);
impl_binop!(Mul, mul, Int, Float);
impl_binop!(Div, div, Int, Float);
impl_binop!(Rem, rem, Int, Float);

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

impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (Bool(a), Bool(b)) => a.eq(b),
            (Int(a), Int(b)) => a.eq(b),
            (Float(a), Float(b)) => a.eq(b),
            (String(a), String(b)) => Arc::ptr_eq(a, b) || a.eq(b),
            (Case(a), Case(b)) => Arc::ptr_eq(a, b) || a.eq(b),
            (Record(a), Record(b)) => Arc::ptr_eq(a, b) || a.read().unwrap().eq(&*b.read().unwrap()),
            (Func(a), Func(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct Builtin(pub Arc<dyn Fn(Value, &mut Context) -> Value>);

impl std::fmt::Debug for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<builtin function>")
    }
}

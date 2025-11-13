use crate::compiler::Op;
use crate::value::{Func, Value};
use compiler_lib::ast::StringId;
use compiler_lib::{Rodeo, ast};
use std::cell::RefCell;
use std::rc::Rc;

pub fn run_script(script: &[Op], env: &mut Env, strings: &Rodeo) {
    let mut stack = vec![];
    run(script, &mut stack, env, strings);
    if !stack.is_empty() {
        panic!("Stack not empty: {:?}", stack);
    }
}

fn run(ops: &[Op], stack: &mut Vec<Value>, env: &mut Env, strings: &Rodeo) {
    let mut ip = 0;
    while ip < ops.len() {
        match &ops[ip] {
            Op::Return => return,

            Op::PushConstant(val) => stack.push(val.clone()),

            Op::Drop => {
                stack.pop();
            }

            Op::Dup => {
                let val = stack.last().unwrap().clone();
                stack.push(val);
            }

            Op::Swap => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(a);
                stack.push(b);
            }

            Op::PushVar(var) => {
                let val = env.lookup(*var).unwrap();
                stack.push(val);
            }

            Op::BindVar(var) => {
                let val = stack.pop().unwrap();
                *env = env.bind(*var, val);
            }

            Op::BindPlaceholder(var) => {
                *env = env.bind_placeholder(*var);
            }

            Op::InitializePlaceholder(var) => {
                let val = stack.pop().unwrap();
                env.set_placeholder(*var, val);
            }

            Op::PushEnv => stack.push(Value::env(env.clone())),

            Op::PopEnv => *env = stack.pop().unwrap().into_env(),

            Op::MakeCase(tag) => {
                let val = stack.pop().unwrap();
                stack.push(Value::case(*tag, val))
            }

            Op::UnwrapCase => {
                let val = stack.pop().unwrap();
                let (_, inner) = val.as_case();
                stack.push(inner.clone())
            }

            Op::MakeRecord(fields) => {
                let rec = Value::record(fields.iter().rev().map(|(f, m)| (*f, stack.pop().unwrap(), *m)));
                stack.push(rec);
            }

            Op::GetField(field) => {
                let rec = stack.pop().unwrap();
                let val = rec.get_field(*field);
                stack.push(val);
            }

            Op::SetField(field) => {
                let val = stack.pop().unwrap();
                let rec = stack.pop().unwrap();
                let old = rec.set_field(*field, val);
                stack.push(old);
            }

            Op::Jump(offset) => ip = ip.wrapping_add_signed(*offset),

            Op::JumpWhenFalse(offset) => {
                let cond = stack.pop().unwrap().as_bool();
                if !cond {
                    ip = ip.wrapping_add_signed(*offset);
                }
            }

            Op::JumpAndPopWhenTag(tag, offset) => {
                let val = stack.pop().unwrap();
                let (actual_tag, _) = val.as_case();
                if actual_tag == *tag {
                    ip = ip.wrapping_add_signed(*offset);
                } else {
                    stack.push(val);
                }
            }

            Op::PeekAndJumpNotTag(tag, offset) => {
                let val = stack.last().unwrap();
                let (actual_tag, _) = val.as_case();
                if actual_tag != *tag {
                    ip = ip.wrapping_add_signed(*offset);
                }
            }

            Op::AnyOp(op) => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                let c = match op {
                    ast::Op::Eq => Value::bool(a == b),
                    ast::Op::Neq => Value::bool(a != b),
                    _ => unimplemented!("{:?}", op),
                };
                stack.push(c);
            }

            Op::IntOp(op) => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                let (a, b) = (a.as_int(), b.as_int());
                let c = match op {
                    ast::Op::Add => Value::int(a + b),
                    ast::Op::Sub => Value::int(a - b),
                    ast::Op::Mult => Value::int(a * b),
                    ast::Op::Div => Value::int(a / b),
                    ast::Op::Rem => Value::int(a % b),
                    ast::Op::Lt => Value::bool(a < b),
                    ast::Op::Lte => Value::bool(a <= b),
                    ast::Op::Gt => Value::bool(a > b),
                    ast::Op::Gte => Value::bool(a >= b),
                    _ => unimplemented!("{:?}", op),
                };
                stack.push(c);
            }

            Op::FloatOp(op) => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                let (a, b) = (a.as_float(), b.as_float());
                let c = match op {
                    ast::Op::Add => Value::float(a + b),
                    ast::Op::Sub => Value::float(a - b),
                    ast::Op::Mult => Value::float(a * b),
                    ast::Op::Div => Value::float(a / b),
                    ast::Op::Rem => Value::float(a % b),
                    ast::Op::Lt => Value::bool(a < b),
                    ast::Op::Lte => Value::bool(a <= b),
                    ast::Op::Gt => Value::bool(a > b),
                    ast::Op::Gte => Value::bool(a >= b),
                    _ => unimplemented!("{:?}", op),
                };
                stack.push(c);
            }

            Op::BoolOp(op) => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                let (a, b) = (a.as_bool(), b.as_bool());
                let c = match op {
                    ast::Op::Add => Value::bool(a | b),
                    ast::Op::Mult => Value::bool(a & b),
                    _ => unimplemented!("{:?}", op),
                };
                stack.push(c);
            }

            Op::StrOp(op) => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                let (a, b) = (a.as_str(), b.as_str());
                let c = match op {
                    ast::Op::Add => Value::string(a.to_string() + b),
                    _ => unimplemented!("{:?}", op),
                };
                stack.push(c);
            }

            Op::Call => {
                // stack: [... fun arg]
                let fun = stack.swap_remove(stack.len() - 2);
                // stack: [... arg]
                match fun.as_func() {
                    Func::Func2(f, e) => run(&f, stack, &mut e.clone(), strings),
                    Func::Builtin(f) => {
                        let arg = stack.pop().unwrap();
                        let result = f.0(arg, &mut strings.into());
                        stack.push(result);
                    }
                    other => unimplemented!("{:?}", other),
                }
            }

            Op::MakeClosure(body) => stack.push(Value::func2(body.clone(), env.clone())),

            Op::Println(n) => {
                let values = stack.split_off(stack.len() - *n);
                let values = values.into_iter().map(|v| v.show(strings)).collect::<Vec<_>>();
                println!("{}", values.join(" "));
            }
        }
        ip = ip.wrapping_add(1);
    }
}

#[derive(Clone, Debug)]
pub enum Env {
    Empty,
    Entry(Rc<(StringId, Value, Env)>),
    Lazy(Rc<(StringId, RefCell<Option<Value>>, Env)>),
}

impl Env {
    pub fn new() -> Env {
        Env::Empty
    }

    pub fn bind(&self, name: StringId, value: Value) -> Env {
        Env::Entry(Rc::new((name, value, self.clone())))
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
        Env::Lazy(Rc::new((name, RefCell::new(None), self.clone())))
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

use crate::compiler::Op;
use crate::value::Value;
use compiler_lib::Rodeo;
use escape_bytes::unescape;

pub fn to_rust(ops: &[Op], strings: &Rodeo) {
    for op in ops {
        println!("// {:?}", op);
        match op {
            Op::Return => println!("return stack.0"),

            Op::PushConstant(val) => println!("let stack = ({}, stack);", val.to_rust_literal()),

            Op::Drop => println!("let (_, stack) = stack;"),

            Op::Dup => println!("let (x, stack) = stack; let stack = (x.clone(), (x, stack));"),

            Op::Swap => println!("let (x, (y, stack)) = stack; let stack = (y, (x, stack));"),

            Op::PushVar(var) => println!("let stack = (env_{}, stack);", strings.resolve(var)),

            Op::BindVar(var) => println!("let (env_{}, stack) = stack;", strings.resolve(var)),

            Op::BindPlaceholder(var) => println!("let env_{} = RefCell::new(Value::nothing());", strings.resolve(var)),

            Op::InitializePlaceholder(var) => {
                println!("*env_{}.borrow_mut() = stack.0; let stack = stack.1;", strings.resolve(var))
            }

            Op::PushEnv => println!("let stack = {{ let stack = ((), stack);"),

            Op::PopEnv => println!("stack.1}};"),

            Op::MakeCase(tag) => println!(
                "let stack = (Value::case(Tag::{}, stack.0), stack); let stack = stack.1;",
                strings.resolve(tag)
            ),

            Op::UnwrapCase => println!("let stack = (stack.0.as_case().1, stack);"),

            Op::MakeRecord(fields) => todo!(),

            Op::GetField(field) => println!("let stack = (stack.0.get_field({}), stack);", strings.resolve(field)),

            Op::SetField(field) => println!(
                "let (val, (rec, stack)) = stack; let stack = (rec.set_field({}, val), stack);",
                strings.resolve(field)
            ),

            _ => todo!(),
        }
    }
}

impl Value {
    fn to_rust_literal(&self) -> String {
        match self {
            Value::Bool(i) => format!("Value::bool({})", i),
            Value::Int(i) => format!("Value::int({})", i),
            Value::Float(i) => format!("Value::float({})", i),
            Value::String(i) => format!(
                "Value::str({:?})",
                String::from_utf8(unescape(i.as_bytes()).unwrap()).unwrap()
            ),
            _ => unimplemented!("{:?}", self),
        }
    }
}

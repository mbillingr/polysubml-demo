mod builtins;
mod compiler;
mod expand;
mod expand_imports;
mod expand_types;
mod free_vars;
mod interpreter;
mod optimize;
mod to_python;
mod to_rust;
mod value;
mod vm;

use crate::compiler::CompilationContext;
use crate::expand::expand_syntax;
use compiler_lib::ast::{LetPattern, StringId};
use compiler_lib::spans::{Spanned, SpannedError};
use compiler_lib::{Rodeo, State};
use std::collections::HashMap;
use std::io::Write;

//#[global_allocator]
//static GLOBAL_ALLOCATOR: bdwgc_alloc::Allocator = bdwgc_alloc::Allocator;

fn main() {
    //unsafe { bdwgc_alloc::Allocator::initialize() }

    let mut state = State::new();
    state.add_builtins();

    let (env, mut vm_env) = builtins::define_builtins(interpreter::Env::new(), vm::Env::new(), &mut state.strings);

    let mut interpreter_state = interpreter::State::new(env);

    let mut known_modules = Default::default();

    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        let src = std::fs::read_to_string(&args[1]).unwrap();
        match exec(&src, &mut state, &mut interpreter_state, &mut vm_env, &mut known_modules) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("ERROR\n{}", state.err_to_str(&e));
            }
        }
        return;
    }

    let mut src = String::new();
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        src.clear();
        std::io::stdin().read_line(&mut src).unwrap();

        match exec(&src, &mut state, &mut interpreter_state, &mut vm_env, &mut known_modules) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("ERROR\n{}", state.err_to_str(&e));
                continue;
            }
        }
    }
}

fn exec(
    src: &str,
    state: &mut State,
    interpreter_state: &mut interpreter::State,
    vm_env: &mut vm::Env,
    known_modules: &mut HashMap<StringId, Option<StringId>>,
) -> Result<(), SpannedError> {
    let t0 = std::time::Instant::now();
    let ast = state.parse(&src)?;

    let t1 = std::time::Instant::now();
    let ast = expand_syntax(
        ast,
        &std::env::current_dir().unwrap(),
        &mut state.spans,
        &mut state.strings,
        known_modules,
    )?;

    for stmt in &ast {
        println!("{};", stmt.simple_print(0, &state.strings));
    }

    let t2 = std::time::Instant::now();
    state.check(&ast)?;

    let t3 = std::time::Instant::now();
    let mut cmp = CompilationContext {
        strings: &mut state.strings,
    };
    let ops = cmp.compile_script(ast.clone());

    let t4 = std::time::Instant::now();
    let ops = optimize::optimize(ops);

    let t5 = std::time::Instant::now();
    std::fs::write(
        "last_compiled/src/main.rs",
        to_rust::CompilationContext::new(&mut state.strings).compile_script(ast.clone()),
    )
    .unwrap();

    /*let py = to_python::CompilationContext::new(&mut state.strings).compile_script(ast.clone());
    println!("{}", dbg!(py).into_python_src(0, false, &mut state.strings));*/

    let t6 = std::time::Instant::now();
    vm::run_script(&ops, vm_env, &state.strings);

    let t7 = std::time::Instant::now();
    let mut ctx = interpreter::Context::new(interpreter_state, &mut state.strings);
    for stmt in ast {
        ctx.exec(&stmt);
    }

    let t8 = std::time::Instant::now();

    let t_total = t7 - t0;
    let t_parse = t1 - t0;
    let t_expand = t2 - t1;
    let t_check = t3 - t2;
    let t_compile = t4 - t3;
    let t_opt = t5 - t4;
    let t_rust = t6 - t5;
    let t_exec = t7 - t6;
    let t_interpret = t8 - t7;
    eprintln!(
        "{t_total:?} : (parse: {t_parse:?}, expand: {t_expand:?}, type-check: {t_check:?}, compile: {t_compile:?}, opt: {t_opt:?}, rust: {t_rust:?}, exec: {t_exec:?}, interpret: {t_interpret:?})"
    );
    Ok(())
}

trait SimplePrint {
    fn simple_print(&self, indent: usize, strings: &compiler_lib::Rodeo) -> String;
}

impl SimplePrint for compiler_lib::ast::Statement {
    fn simple_print(&self, indent: usize, strings: &Rodeo) -> String {
        use compiler_lib::ast::Statement::*;
        match self {
            Empty => "<nop>".to_string(),
            Expr(x) => x.simple_print(indent, strings),
            LetDef((pat, val)) => format!(
                "let {} = {}",
                pat.simple_print(indent, strings),
                val.simple_print(indent, strings)
            ),
            LetRecDef(defs) => {
                let mut out = "let rec\n".to_string();
                let let_indent = indent + 1;
                for (name, expr) in defs {
                    out += &make_indent(let_indent);
                    out += strings.resolve(name);
                    out += " = ";
                    out += &expr.simple_print(let_indent, strings);
                    out += "\n";
                }
                out += &make_indent(indent);
                out
            }
            Println(exprs) => format!(
                "print {}",
                exprs
                    .iter()
                    .map(|x| x.simple_print(indent, strings))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Import((lib, _)) => format!("import {lib:?}"),
            TypeDef(_) => todo!(),
        }
    }
}

impl SimplePrint for compiler_lib::ast::Expr {
    fn simple_print(&self, indent: usize, strings: &Rodeo) -> String {
        use compiler_lib::ast::Expr::*;
        use compiler_lib::ast::Op;
        match self {
            BinOp(bop) => {
                let op = match bop.op {
                    Op::Add => "+",
                    Op::Sub => "-",
                    Op::Mult => "*",
                    Op::Div => "/",
                    Op::Rem => "%",
                    Op::Lt => "<",
                    Op::Lte => "<=",
                    Op::Gt => ">",
                    Op::Gte => ">=",
                    Op::Eq => "==",
                    Op::Neq => "!=",
                };
                format!(
                    "({} {} {})",
                    bop.lhs.simple_print(indent, strings),
                    op,
                    bop.rhs.simple_print(indent, strings)
                )
            }

            Block(blk) => {
                let mut out = "begin\n".to_string();
                for stmt in &blk.statements {
                    out += &make_indent(indent + 1);
                    out += &stmt.simple_print(indent + 1, strings);
                    out += ";\n";
                }
                out += &make_indent(indent + 1);
                out += &blk.expr.simple_print(indent + 1, strings);
                out += "\n";
                out += &make_indent(indent);
                out += "end";
                out
            }

            Call(call) => format!(
                "({} {})",
                call.func.simple_print(indent, strings),
                call.arg.simple_print(indent, strings)
            ),

            Case(cx) => format!("`{} {}", strings.resolve(&cx.tag.0), cx.expr.simple_print(indent, strings)),

            FieldAccess(fa) => format!("{}.{}", fa.expr.simple_print(indent, strings), strings.resolve(&fa.field.0)),

            FieldSet(fs) => format!(
                "({}.{} <- {})",
                fs.expr.simple_print(indent, strings),
                strings.resolve(&fs.field.0),
                fs.value.simple_print(indent, strings)
            ),

            FuncDef(fd) => format!(
                "(fun {} -> {})",
                fd.param.simple_print(indent, strings),
                fd.body.simple_print(indent, strings)
            ),

            If(if_) => {
                let mut out = format!("if {}\n", if_.cond.simple_print(indent, strings));
                out += &make_indent(indent + 1);
                out += "then ";
                out += &if_.then_expr.simple_print(indent + 1, strings);
                out += "\n";
                out += &make_indent(indent + 1);
                out += "else ";
                out += &if_.else_expr.simple_print(indent + 1, strings);
                out += "\n";
                out += &make_indent(indent);
                out
            }

            InstantiateExist(ie) => ie.expr.simple_print(indent, strings),
            InstantiateUni(iu) => iu.expr.simple_print(indent, strings),

            Literal(lit) => lit.value.0.clone(),

            Loop(lp) => {
                let mut out = "loop\n".to_string();
                out += &make_indent(indent + 1);
                out += &lp.body.simple_print(indent + 1, strings);
                out += "\n";
                out += &make_indent(indent);
                out
            }

            Match(mtch) => {
                let mut out = format!("match {} with\n", mtch.expr.simple_print(indent, strings));

                for (pat, expr) in &mtch.cases {
                    out += &make_indent(indent + 1);
                    out += &pat.simple_print(indent + 1, strings);
                    out += " -> ";
                    out += &expr.simple_print(indent + 1, strings);
                    out += "\n";
                }
                out += &make_indent(indent);
                out
            }

            Record(rec) => format!(
                "{{{}}}",
                rec.fields
                    .iter()
                    .map(|(k, v, m, _)| {
                        format!(
                            "{}{}={}",
                            if *m { "mut " } else { "" },
                            strings.resolve(&k.0),
                            v.simple_print(indent, strings)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("; ")
            ),

            Typed(tx) => tx.expr.simple_print(indent, strings),

            Variable(v) => strings.resolve(&v.name).to_string(),

            Array(_, elems) => format!(
                "#[{}]",
                elems
                    .iter()
                    .map(|x| x.simple_print(indent, strings))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),

            Dict(_, elems) => format!(
                "#{{{}}}",
                elems
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k.simple_print(indent, strings), v.simple_print(indent, strings)))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl SimplePrint for LetPattern {
    fn simple_print(&self, indent: usize, strings: &Rodeo) -> String {
        use compiler_lib::ast::LetPattern::*;
        match self {
            Var((None, _), _) => "_".to_string(),
            Var((Some(v), _), _) => strings.resolve(v).to_string(),
            Case((tag, _), pat) => format!("`{} {}", strings.resolve(tag), pat.simple_print(indent, strings)),
            Record(((_, fields), _)) => format!(
                "{{{}}}",
                fields
                    .iter()
                    .map(|((f, _), p)| format!("{}={}", strings.resolve(f), p.simple_print(indent, strings)))
                    .collect::<Vec<_>>()
                    .join("; ")
            ),
        }
    }
}

impl<T: SimplePrint> SimplePrint for Spanned<T> {
    fn simple_print(&self, indent: usize, strings: &Rodeo) -> String {
        self.0.simple_print(indent, strings)
    }
}

impl<T: SimplePrint> SimplePrint for Box<T> {
    fn simple_print(&self, indent: usize, strings: &Rodeo) -> String {
        (**self).simple_print(indent, strings)
    }
}

fn make_indent(indent: usize) -> String {
    " ".repeat(indent * 4)
}

mod builtins;
mod compiler;
mod expand;
mod expand_imports;
mod expand_types;
mod interpreter;
mod optimize;
mod value;
mod vm;

use crate::compiler::CompilationContext;
use crate::expand::expand_syntax;
use compiler_lib::State;
use compiler_lib::ast::StringId;
use compiler_lib::spans::SpannedError;
use std::collections::HashMap;
use std::io::Write;

fn main() {
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
    let ast = expand_syntax(ast, &mut state.spans, &mut state.strings, known_modules)?;

    let t2 = std::time::Instant::now();
    state.check(&ast)?;

    let t3 = std::time::Instant::now();
    let mut cmp = CompilationContext {
        strings: &mut state.strings,
    };
    let ops = cmp.compile_script(ast.clone());

    let t4 = std::time::Instant::now();
    let ops = optimize::optimize(ops);

    println!("{:#?}", ops);

    let t5 = std::time::Instant::now();
    vm::run_script(&ops, vm_env, &state.strings);

    let t6 = std::time::Instant::now();
    let mut ctx = interpreter::Context::new(interpreter_state, &mut state.strings);
    for stmt in ast {
        ctx.exec(&stmt);
    }

    let t7 = std::time::Instant::now();

    let t_total = t7 - t0;
    let t_parse = t1 - t0;
    let t_expand = t2 - t1;
    let t_check = t3 - t2;
    let t_compile = t4 - t3;
    let t_opt = t5 - t4;
    let t_exec = t6 - t5;
    let t_interpret = t7 - t6;
    eprintln!(
        "{t_total:?} : (parse: {t_parse:?}, expand: {t_expand:?}, type-check: {t_check:?}, compile: {t_compile:?}, opt: {t_opt:?}, exec: {t_exec:?}, interpret: {t_interpret:?})"
    );
    Ok(())
}

mod expand;
mod expand_imports;
mod expand_types;
mod interpreter;
mod value;
mod builtins;

use crate::expand::expand_syntax;
use compiler_lib::State;
use compiler_lib::ast::StringId;
use compiler_lib::spans::SpannedError;
use std::collections::HashMap;
use std::io::Write;

fn main() {
    let mut state = State::new();
    state.add_builtins();

    let mut interpreter_state = interpreter::State::with_builtins(&mut state.strings);

    let mut known_modules = Default::default();

    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        let src = std::fs::read_to_string(&args[1]).unwrap();
        match exec(&src, &mut state, &mut interpreter_state, &mut known_modules) {
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

        match exec(&src, &mut state, &mut interpreter_state, &mut known_modules) {
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
    known_modules: &mut HashMap<StringId, Option<StringId>>,
) -> Result<(), SpannedError> {
    let t0 = std::time::Instant::now();
    let ast = state.parse(&src)?;

    let t1 = std::time::Instant::now();
    let ast = expand_syntax(ast, &mut state.spans, &mut state.strings, known_modules)?;

    let t2 = std::time::Instant::now();
    state.check(&ast)?;

    let t3 = std::time::Instant::now();
    let mut ctx = interpreter::Context::new(interpreter_state, &mut state.strings);
    for stmt in ast {
        ctx.exec(&stmt);
    }

    let t4 = std::time::Instant::now();

    let t_total = t4 - t0;
    let t_parse = t1 - t0;
    let t_expand = t2 - t1;
    let t_check = t3 - t2;
    let t_exec = t4 - t3;
    eprintln!("{t_total:?} : (parse: {t_parse:?}, expand: {t_expand:?}, type-check: {t_check:?}, exec: {t_exec:?})");
    Ok(())
}

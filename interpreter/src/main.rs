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
use crate::builtins::define_builtins;

fn main() {
    let mut state = State::new();
    state.add_builtins();

    let mut static_env = interpreter::StaticEnv::new();
    let mut runtime_env = interpreter::Env::new();

    define_builtins(&mut static_env, &mut runtime_env, &mut state.strings);

    let mut known_modules = Default::default();

    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        let src = std::fs::read_to_string(&args[1]).unwrap();
        match exec(&src, &mut state, &mut runtime_env, &mut static_env, &mut known_modules) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("ERROR\n{}", state.err_to_str(&e));
            }
        }
        return;
    }

    let mut src = String::new();
    loop {
        println!("{:?}", static_env);
        println!("{:?}", runtime_env);
        print!("> ");
        std::io::stdout().flush().unwrap();
        src.clear();
        std::io::stdin().read_line(&mut src).unwrap();

        match exec(&src, &mut state, &mut runtime_env, &mut static_env, &mut known_modules) {
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
    runtime_env: &mut interpreter::Env,
    static_env: &mut interpreter::StaticEnv,
    known_modules: &mut HashMap<StringId, Option<StringId>>,
) -> Result<(), SpannedError> {
    let t0 = std::time::Instant::now();
    let ast = state.parse(&src)?;

    let t1 = std::time::Instant::now();
    let ast = expand_syntax(ast, &mut state.spans, &mut state.strings, known_modules)?;

    let t2 = std::time::Instant::now();
    state.check(&ast)?;

    let t3 = std::time::Instant::now();
    let mut ctx = interpreter::Context::new(static_env.clone(), &mut state.strings);
    let prog = ctx.analyze_stmts(&ast);
    *static_env = ctx.static_env;

    let t4 = std::time::Instant::now();
    let mut rt = interpreter::Runtime{env: runtime_env.clone(), strings: &mut state.strings};
    rt.exec(&prog);
    *runtime_env = rt.env;

    let t5 = std::time::Instant::now();

    let t_total = t4 - t0;
    let t_parse = t1 - t0;
    let t_expand = t2 - t1;
    let t_check = t3 - t2;
    let t_analyze = t4 - t3;
    let t_exec = t5 - t4;
    eprintln!("{t_total:?} : (parse: {t_parse:?}, expand: {t_expand:?}, type-check: {t_check:?}, analyze: {t_analyze:?}, exec: {t_exec:?})");
    Ok(())
}

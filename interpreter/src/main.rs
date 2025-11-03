mod expand_imports;
mod interpreter;
mod value;

use crate::expand_imports::expand_imports;
use compiler_lib::State;
use std::io::Write;

fn main() {
    let mut state = State::new();
    state.add_builtins();

    let mut interpreter_state = interpreter::State::with_builtins(&mut state.strings);

    let mut known_modules = Default::default();

    let mut src = String::new();
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        src.clear();
        std::io::stdin().read_line(&mut src).unwrap();

        let t0 = std::time::Instant::now();

        let prep = state.parse(&src);
        let t1 = std::time::Instant::now();
        let prep = prep.and_then(|ast| {
            expand_imports(
                ast,
                std::env::current_dir().unwrap(),
                &mut state.spans,
                &mut state.strings,
                &mut known_modules,
            )
        });
        let t2 = std::time::Instant::now();
        let prep = prep.and_then(|ast| state.check(&ast).map(|_| ast));
        let t3 = std::time::Instant::now();

        let ast = match prep {
            Ok(ast) => ast,
            Err(e) => {
                eprintln!("ERROR\n{}", state.err_to_str(&e));
                continue;
            }
        };

        let mut ctx = interpreter::Context::new(&mut interpreter_state, &mut state.strings);
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
    }
}

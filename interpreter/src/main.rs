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

        let prep = state.parse(&src);
        let prep = prep.and_then(|ast| {
            expand_imports(
                ast,
                std::env::current_dir().unwrap(),
                &mut state.spans,
                &mut state.strings,
                &mut known_modules,
            )
        });
        let prep = prep.and_then(|ast| state.check(&ast).map(|_| ast));

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
    }
}

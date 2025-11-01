mod interpreter;
mod value;

use compiler_lib::State;
use std::io::Write;

fn main() {
    let mut state = State::new();
    state.add_builtins();

    let mut interpreter_state = interpreter::State::with_builtins(&mut state.strings);

    let mut src = String::new();
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        src.clear();
        std::io::stdin().read_line(&mut src).unwrap();

        let ast = match state.parse(&src) {
            Ok(ast) => ast,
            Err(e) => {
                eprintln!("ERROR\n{}", state.err_to_str(&e));
                continue;
            }
        };

        if let Err(e) = state.check(&ast) {
            eprintln!("ERROR\n{}", state.err_to_str(&e));
            continue;
        }

        let mut ctx = interpreter::Context::new(&mut interpreter_state, &mut state.strings);
        for stmt in ast {
            ctx.exec(&stmt);
        }
    }
}

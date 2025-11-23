use crate::ast_processor::AstProcessor;
use crate::bytecode_interpreter::compiler::CompilationContext;
use crate::runtime_ast as ast;
use compiler_lib::Rodeo;

pub mod compiler;
pub mod optimize;
pub mod vm;

pub struct State {
    vm_env: vm::Env,
}

impl State {
    pub fn new(vm_env: vm::Env) -> Self {
        State { vm_env }
    }

    pub fn run_script(&mut self, script: &[ast::Statement], strings: &mut Rodeo) {
        let mut cmp = CompilationContext { strings };
        let ops = cmp.compile_script(script.to_vec());
        let ops = optimize::optimize(ops);
        vm::run_script(&ops, &mut self.vm_env, strings);
    }
}

impl AstProcessor for State {
    fn process_script(&mut self, script: &[ast::Statement], strings: &mut Rodeo) {
        self.run_script(script, strings);
    }
}

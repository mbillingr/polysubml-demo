use crate::runtime_ast as ast;
use compiler_lib::Rodeo;

pub trait AstProcessor {
    fn process_script(&mut self, script: &[ast::Statement], strings: &mut Rodeo);
}

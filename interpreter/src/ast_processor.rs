use compiler_lib::{Rodeo, ast};

pub trait AstProcessor {
    fn process_script(&mut self, script: &[ast::Statement], strings: &mut Rodeo);
}

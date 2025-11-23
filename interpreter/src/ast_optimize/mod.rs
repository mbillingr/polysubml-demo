use crate::ast_optimize::alpha_conversion::VariableRenamer;
use crate::ast_optimize::beta_reduction::DirectCallTransformer;
use crate::ast_optimize::inliner::InlineTransformer;
use crate::ast_transform::AstTransWalker;
use crate::runtime_ast;
use compiler_lib::Rodeo;

mod alpha_conversion;
mod beta_reduction;
mod betarec_reduction;
mod inliner;

pub fn optimize_script(
    mut script: Vec<runtime_ast::Statement>,
    toplevel: bool,
    strings: &mut Rodeo,
) -> Vec<runtime_ast::Statement> {
    script = script.transform(&mut VariableRenamer::new(toplevel, strings));

    let mut changes = 1;
    while changes > 0 {
        changes = 0;

        let mut inliner = InlineTransformer::new();
        script = script.transform(&mut inliner);
        changes += inliner.changes();

        let mut betarec_reducer = betarec_reduction::DirectFieldAccessTransformer::new();
        script = script.transform(&mut betarec_reducer);
        changes += betarec_reducer.changes();

        let mut beta_reducer = DirectCallTransformer::new();
        script = script.transform(&mut beta_reducer);
        changes += beta_reducer.changes();

        eprintln!("Optimization pass made {changes} rewrites")
    }

    script
}

use crate::ast_optimize::alpha_conversion::VariableRenamer;
use crate::ast_optimize::beta_reduction::DirectCallTransformer;
use crate::ast_optimize::dead_code_elimination::DeadCodeTransformer;
use crate::ast_optimize::inliner::InlineTransformer;
use crate::ast_transform::AstTransWalker;
use crate::runtime_ast;
use compiler_lib::Rodeo;

mod alpha_conversion;
mod beta_reduction;
mod betacase_reduction;
mod betarec_reduction;
mod block_flattening;
mod case_field_mover;
mod condition_lifting;
mod dead_code_elimination;
mod inliner;
mod let_expansion;

pub fn optimize_script(
    mut script: Vec<runtime_ast::Statement>,
    toplevel: bool,
    strings: &mut Rodeo,
) -> Vec<runtime_ast::Statement> {
    let mut changes = 1;
    while changes > 0 {
        changes = 0;

        // Run alpha conversion before every pass, because transformations that duplicate code may have broken the variable name assumption.
        script = script.transform(&mut VariableRenamer::new(toplevel, strings));

        let mut inliner = InlineTransformer::new();
        script = script.transform(&mut inliner);
        changes += inliner.changes();

        // alpha-convert duplicate bindings introduced by inlining
        script = script.transform(&mut VariableRenamer::new(toplevel, strings));

        let mut betarec_reducer = betarec_reduction::DirectFieldAccessTransformer::new();
        script = script.transform(&mut betarec_reducer);
        changes += betarec_reducer.changes();

        // Expand directly called functions, which are often a result of inlining.
        let mut beta_reducer = DirectCallTransformer::new();
        script = script.transform(&mut beta_reducer);
        changes += beta_reducer.changes();

        // Beta reduction produces `let {...} = {...}` forms, which are optimized here.
        let mut let_expander = let_expansion::BlockFlattener::new();
        script = let_expander.transform_stmts(script);
        changes += let_expander.changes();

        // Flatten blocks to enable further optimizations.
        let mut block_flattener = block_flattening::BlockFlattener::new();
        script = block_flattener.transform_stmts(script, toplevel);
        changes += block_flattener.changes();

        // Move case expressions out of blocks and field access into blocks.
        let mut cfm = case_field_mover::CaseLiftAndFieldLowerTransformer::new();
        script = script.transform(&mut cfm);
        changes += cfm.changes();

        let mut clifter = condition_lifting::ConditionLiftTransformer::new();
        script = script.transform(&mut clifter);
        changes += clifter.changes();

        let mut betacase_reducer = betacase_reduction::KnownMatchTransformer::new();
        script = script.transform(&mut betacase_reducer);
        changes += betacase_reducer.changes();

        let mut eliminator = DeadCodeTransformer::new();
        script = eliminator.transform_stmts(script, toplevel);
        changes += eliminator.changes();

        eprintln!("Optimization pass made {changes} rewrites");
    }

    script
}

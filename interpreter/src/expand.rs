use crate::expand_imports::expand_imports;
use crate::expand_types::expand_types;
use compiler_lib::spans::{SpanManager, SpannedError};
use compiler_lib::{Rodeo, ast};
use std::collections::HashMap;

pub fn expand_syntax(
    ast: Vec<ast::Statement>,
    current_path: &std::path::Path,
    spans: &mut SpanManager,
    strings: &mut Rodeo,
    known_modules: &mut HashMap<ast::StringId, Option<ast::StringId>>,
) -> Result<Vec<ast::Statement>, SpannedError> {
    let ast = expand_types(ast)?;

    let mut module_defs = vec![];
    let ast = expand_imports(ast, current_path, &mut module_defs, spans, strings, known_modules)?;
    Ok(module_defs.into_iter().chain(ast).collect())
}

pub fn expand_syntax_inner(
    ast: Vec<ast::Statement>,
    current_path: &std::path::Path,
    module_defs: &mut Vec<ast::Statement>,
    spans: &mut SpanManager,
    strings: &mut Rodeo,
    known_modules: &mut HashMap<ast::StringId, Option<ast::StringId>>,
) -> Result<Vec<ast::Statement>, SpannedError> {
    let ast = expand_types(ast)?;

    let ast = expand_imports(ast, current_path, module_defs, spans, strings, known_modules)?;

    Ok(ast)
}

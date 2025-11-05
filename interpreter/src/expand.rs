use crate::expand_imports::expand_imports;
use crate::expand_types::expand_types;
use compiler_lib::spans::{SpanManager, SpannedError};
use compiler_lib::{Rodeo, ast};
use std::collections::HashMap;

pub fn expand_syntax(
    ast: Vec<ast::Statement>,
    spans: &mut SpanManager,
    strings: &mut Rodeo,
    known_modules: &mut HashMap<ast::StringId, Option<ast::StringId>>,
) -> Result<Vec<ast::Statement>, SpannedError> {
    let ast = expand_types(ast)?;

    let ast = expand_imports(ast, std::env::current_dir().unwrap(), spans, strings, known_modules)?;

    Ok(ast)
}

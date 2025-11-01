use compiler_lib::grammar::ModuleParser;
use compiler_lib::spans::{Span, SpanManager, SpannedError};
use compiler_lib::{Rodeo, ast, convert_parse_error};
use std::collections::HashMap;
use std::path::PathBuf;

/// Recursively expands `import "path/module.ml"` into
/// `let <priv.module> = <content of foo/module.ml>` and `let module = <priv.module>`.
/// The first binding is inserted only once for each file, even in the presence of multiple nested
/// imports. This is possible because <priv.module> is a private name of the module, which cannot
/// be shadowed by any grammatical variable names.
pub fn expand_imports(
    stmts: Vec<ast::Statement>,
    current_path: PathBuf,
    spans: &mut SpanManager,
    strings: &mut Rodeo,
    known_modules: &mut HashMap<ast::StringId, Option<ast::StringId>>,
) -> Result<Vec<ast::Statement>, SpannedError> {
    let mut ctx = ImportExpansion {
        current_path,
        module_defs: vec![],
        spans,
        strings,
        known_modules,
    };
    let out = ctx.expand_stmts(stmts)?;
    Ok(ctx.module_defs.into_iter().chain(out).collect())
}

struct ImportExpansion<'a> {
    current_path: PathBuf,
    module_defs: Vec<ast::Statement>,
    spans: &'a mut SpanManager,
    strings: &'a mut Rodeo,
    known_modules: &'a mut HashMap<ast::StringId, Option<ast::StringId>>,
}

impl<'a> ImportExpansion<'a> {
    fn expand_stmts(&mut self, stmts: Vec<ast::Statement>) -> Result<Vec<ast::Statement>, SpannedError> {
        let mut out = vec![];

        for stmt in stmts {
            match stmt {
                ast::Statement::Import((raw_path, path_span)) => {
                    let path = self
                        .current_path
                        .join(std::path::Path::new(&raw_path.trim_start_matches('"').trim_end_matches('"')));
                    let path = path.canonicalize().map_err(|_| {
                        SpannedError::new1(format!("Could not resolve import path {}", path.display()), path_span)
                    })?;
                    // construct a name from the path and a leading space to create an
                    // identifier that cannot be shadowed by any other variable name.
                    let module_private_name = self.strings.get_or_intern(&format!(" {}", path.display()));

                    match self.known_modules.get(&module_private_name) {
                        Some(None) => return Err(SpannedError::new1("cyclic import", path_span)),
                        Some(Some(module_name)) => {
                            let let_val = make_lookup(module_private_name);
                            out.push(make_let(path_span, *module_name, let_val))
                        }
                        None => {
                            eprintln!("Loading module {}", path.display());
                            let mod_name = path.file_prefix().unwrap().to_str().unwrap();
                            self.known_modules.insert(module_private_name, None);

                            let src = std::fs::read_to_string(&path)
                                .map_err(|_| SpannedError::new1("Could read module file", path_span))?;

                            let span_maker = self.spans.add_source(src.to_owned());
                            let mut ctx = ast::ParserContext {
                                span_maker,
                                strings: self.strings,
                            };
                            let module_ast = ModuleParser::new()
                                .parse(&mut ctx, &src)
                                .map_err(|e| convert_parse_error(ctx.span_maker, e))?;

                            let old_path = std::mem::replace(&mut self.current_path, path.parent().unwrap().to_owned());
                            let module_ast = self.expand_expr(module_ast)?;
                            self.current_path = old_path;

                            let module_name = self.strings.get_or_intern(mod_name);
                            self.known_modules.insert(module_private_name, Some(module_name));

                            self.module_defs.push(make_let(path_span, module_private_name, module_ast));
                            out.push(make_let(path_span, module_name, make_lookup(module_private_name)));
                        }
                    }
                }

                _ => {
                    out.push(stmt);
                }
            }
        }
        Ok(out)
    }

    fn expand_expr(&mut self, expr: ast::Expr) -> Result<ast::Expr, SpannedError> {
        match expr {
            ast::Expr::Block(blk) => {
                let stmts = self.expand_stmts(blk.statements)?;
                let expr = self.expand_expr(blk.expr.0)?;
                Ok(ast::Expr::Block(ast::expr::BlockExpr {
                    statements: stmts,
                    expr: Box::new((expr, blk.expr.1)),
                }))
            }
            _ => Ok(expr),
        }
    }
}

fn make_lookup(name: ast::StringId) -> ast::Expr {
    ast::Expr::Variable(ast::expr::VariableExpr { name })
}

fn make_let(span: Span, var: ast::StringId, let_val: ast::Expr) -> ast::Statement {
    let let_lhs = ast::LetPattern::Var((Some(var), span), None);
    let stmt = ast::Statement::LetDef((let_lhs, Box::new((let_val, span))));
    stmt
}

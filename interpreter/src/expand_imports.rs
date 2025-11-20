use crate::expand::expand_syntax_inner;
use compiler_lib::grammar::ModuleParser;
use compiler_lib::spans::{Span, SpanManager, SpannedError};
use compiler_lib::{Rodeo, ast, convert_parse_error};
use std::collections::HashMap;
use std::path::Path;

/// Recursively expands `import "path/module.ml"` into
/// `let <priv.module> = <content of foo/module.ml>` and `let module = <priv.module>`.
/// The first binding is inserted only once for each file, even in the presence of multiple nested
/// imports. This is possible because <priv.module> is a private name of the module, which cannot
/// be shadowed by any grammatical variable names.
pub fn expand_imports(
    stmts: Vec<ast::Statement>,
    current_path: &Path,
    module_defs: &mut Vec<ast::Statement>,
    spans: &mut SpanManager,
    strings: &mut Rodeo,
    known_modules: &mut HashMap<ast::StringId, Option<ast::StringId>>,
) -> Result<Vec<ast::Statement>, SpannedError> {
    let mut ctx = ImportExpansion {
        current_path,
        module_defs,
        spans,
        strings,
        known_modules,
    };
    ctx.expand_stmts(stmts)
}

struct ImportExpansion<'a> {
    current_path: &'a Path,
    module_defs: &'a mut Vec<ast::Statement>,
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
                    /*
                    // construct a name from the path and a leading space to create an
                    // identifier that cannot be shadowed by any other variable name.
                    let module_private_name = self.strings.get_or_intern(&format!(" {}", path.display()));
                    */

                    // use a valid rust identifier
                    let module_private_name = self.strings.get_or_intern(
                        &format!("{}", path.display())
                            .replace("/", "_")
                            .replace("-", "_")
                            .replace(".", "_"),
                    );

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

                            let mut module_ast = expand_syntax_inner(
                                module_ast,
                                path.parent().unwrap(),
                                self.module_defs,
                                self.spans,
                                self.strings,
                                self.known_modules,
                            )?;

                            let module_name = self.strings.get_or_intern(mod_name);
                            self.known_modules.insert(module_private_name, Some(module_name));

                            let last_expr = match module_ast.pop() {
                                Some(ast::Statement::Expr(expr)) => expr,
                                _ => panic!("Module file must end with an expression"),
                            };

                            let module_expr = if module_ast.is_empty() {
                                last_expr.0
                            } else {
                                ast::Expr::Block(ast::expr::BlockExpr {
                                    statements: module_ast,
                                    expr: Box::new(last_expr),
                                })
                            };

                            self.module_defs.push(make_let(path_span, module_private_name, module_expr));
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
}

fn make_lookup(name: ast::StringId) -> ast::Expr {
    ast::Expr::Variable(ast::expr::VariableExpr { name })
}

fn make_let(span: Span, var: ast::StringId, let_val: ast::Expr) -> ast::Statement {
    let let_lhs = ast::LetPattern::Var((Some(var), span), None);
    let stmt = ast::Statement::LetDef((let_lhs, Box::new((let_val, span))));
    stmt
}

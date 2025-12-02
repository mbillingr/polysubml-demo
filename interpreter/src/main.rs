mod ast_interpreter;
mod ast_optimize;
mod ast_processor;
mod ast_purity;
mod ast_transform;
mod ast_visitor;
mod builtins;
mod bytecode_interpreter;
mod expand;
mod expand_imports;
mod expand_types;
mod free_vars;
mod runtime_ast;
mod to_rust;
mod value;

use crate::ast_processor::AstProcessor;
use crate::expand::expand_syntax;
use bytecode_interpreter::vm;
use clap::{Parser, ValueEnum};
use compiler_lib::ast::{LetPattern, StringId};
use compiler_lib::spans::{Spanned, SpannedError};
use compiler_lib::{Rodeo, State};
use std::collections::HashMap;
use std::io::Write;

//#[global_allocator]
//static GLOBAL_ALLOCATOR: bdwgc_alloc::Allocator = bdwgc_alloc::Allocator;

#[derive(Debug, Parser)]
struct Cli {
    #[arg(short, long, default_value_t = Backend::Bytecode)]
    target: Backend,

    #[arg(short, long)]
    script: bool,

    files: Vec<String>,
}

#[derive(Clone, Debug, ValueEnum)]
enum Backend {
    Bytecode,
    Ast,
    Rust,
}

impl ToString for Backend {
    fn to_string(&self) -> String {
        format!("{:?}", self).to_lowercase()
    }
}

fn main() {
    //unsafe { bdwgc_alloc::Allocator::initialize() }

    let args = Cli::parse();
    println!("{:?}", args);

    println!("{}", build_info::format!("{}", $));

    let mut state = GlobalState::new(args.target);

    for file in &args.files {
        let src = std::fs::read_to_string(file).unwrap();
        state.run_script(&src);
    }

    if !args.script {
        state.repl();
    }
}

struct GlobalState {
    type_checker: State,
    backend: Box<dyn AstProcessor>,
    known_modules: HashMap<StringId, Option<StringId>>,
}

impl GlobalState {
    fn new(backend: Backend) -> Self {
        let mut type_checker = State::new();
        type_checker.add_builtins();

        let (env, vm_env) =
            builtins::define_builtins(ast_interpreter::Env::new(), vm::Env::new(), &mut type_checker.strings);

        let backend: Box<dyn AstProcessor> = match backend {
            Backend::Rust => Box::new(to_rust::State),
            Backend::Bytecode => Box::new(bytecode_interpreter::State::new(vm_env)),
            Backend::Ast => Box::new(ast_interpreter::State::new(env)),
        };

        GlobalState {
            type_checker,
            backend,
            known_modules: Default::default(),
        }
    }

    fn strings(&mut self) -> &mut Rodeo {
        &mut self.type_checker.strings
    }

    fn err_to_str(&self, err: &SpannedError) -> String {
        err.print(&self.type_checker.spans)
    }

    pub fn repl(&mut self) {
        let mut src = String::new();
        loop {
            print!("> ");
            std::io::stdout().flush().unwrap();
            src.clear();
            std::io::stdin().read_line(&mut src).unwrap();

            self.exec_(&src, true);
        }
    }

    pub fn run_script(&mut self, src: &str) {
        self.exec_(src, false);
    }

    fn exec_(&mut self, src: &str, in_repl: bool) {
        match self.exec(src, in_repl) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("ERROR\n{}", self.err_to_str(&e));
            }
        }
    }

    fn exec(&mut self, src: &str, in_repl: bool) -> Result<(), SpannedError> {
        let mut times = vec![];

        let t0 = std::time::Instant::now();
        let ast = self.type_checker.parse(&src)?;

        let t1 = std::time::Instant::now();
        times.push(("parse", t1 - t0));

        let ast = expand_syntax(
            ast,
            &std::env::current_dir().unwrap(),
            &mut self.type_checker.spans,
            &mut self.type_checker.strings,
            &mut self.known_modules,
        )?;

        let t2 = std::time::Instant::now();
        times.push(("expand", t1 - t0));

        self.type_checker.check(&ast)?;

        let t3 = std::time::Instant::now();
        times.push(("type-check", t3 - t2));

        let ast = runtime_ast::simplify(ast);

        // only consider the REPL to be top-level
        let ast = ast_optimize::optimize_script(ast, in_repl, &mut self.type_checker.strings);

        let mut t4 = std::time::Instant::now();
        times.push(("optimize ast", t4 - t3));

        for stmt in &ast {
            println!("{};", stmt.simple_print(0, self.strings()));
        }

        self.backend.process_script(&ast, &mut self.type_checker.strings);

        let t5 = std::time::Instant::now();
        times.push(("backend", t5 - t4));
        t4 = t5;

        times.push(("total", t4 - t0));

        eprintln!(
            "{}",
            times
                .into_iter()
                .map(|(name, t)| format!("{}: {:?}", name, t))
                .collect::<Vec<_>>()
                .join(", ")
        );

        println!("it: {:?}", self.type_checker.strings.get_or_intern_static("it"));

        Ok(())
    }
}

trait SimplePrint {
    fn simple_print(&self, indent: usize, strings: &compiler_lib::Rodeo) -> String;
}

impl SimplePrint for runtime_ast::Statement {
    fn simple_print(&self, indent: usize, strings: &Rodeo) -> String {
        use runtime_ast::Statement::*;
        match self {
            Empty => "<nop>".to_string(),
            Expr(x) => x.simple_print(indent, strings),
            LetDef(pat, val) => format!(
                "let {} = {}",
                pat.simple_print(indent, strings),
                val.simple_print(indent, strings)
            ),
            LetRecDef(defs) => {
                let mut out = "let rec\n".to_string();
                let let_indent = indent + 1;
                for (name, expr) in defs {
                    out += &make_indent(let_indent);
                    out += strings.resolve(name);
                    out += " = ";
                    out += &expr.simple_print(let_indent, strings);
                    out += "\n";
                }
                out += &make_indent(indent);
                out
            }
            Println(exprs) => format!(
                "print {}",
                exprs
                    .iter()
                    .map(|x| x.simple_print(indent, strings))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl SimplePrint for runtime_ast::Expr {
    fn simple_print(&self, indent: usize, strings: &Rodeo) -> String {
        use runtime_ast::Expr::*;
        use runtime_ast::Op;
        match self {
            BinOp(bop) => {
                let op = match bop.op {
                    Op::Add => "+",
                    Op::Sub => "-",
                    Op::Mult => "*",
                    Op::Div => "/",
                    Op::Rem => "%",
                    Op::Lt => "<",
                    Op::Lte => "<=",
                    Op::Gt => ">",
                    Op::Gte => ">=",
                    Op::Eq => "==",
                    Op::Neq => "!=",
                };
                format!(
                    "({} {} {})",
                    bop.lhs.simple_print(indent, strings),
                    op,
                    bop.rhs.simple_print(indent, strings)
                )
            }

            Block(blk) => {
                let mut out = "begin\n".to_string();
                for stmt in &blk.statements {
                    out += &make_indent(indent + 1);
                    out += &stmt.simple_print(indent + 1, strings);
                    out += ";\n";
                }
                out += &make_indent(indent + 1);
                out += &blk.expr.simple_print(indent + 1, strings);
                out += "\n";
                out += &make_indent(indent);
                out += "end";
                out
            }

            Call(call) => format!(
                "({} {})",
                call.func.simple_print(indent, strings),
                call.arg.simple_print(indent, strings)
            ),

            Case(cx) => format!("`{} {}", strings.resolve(&cx.tag), cx.expr.simple_print(indent, strings)),

            FieldAccess(fa) => format!("{}.{}", fa.expr.simple_print(indent, strings), strings.resolve(&fa.field)),

            FieldSet(fs) => format!(
                "({}.{} <- {})",
                fs.expr.simple_print(indent, strings),
                strings.resolve(&fs.field),
                fs.value.simple_print(indent, strings)
            ),

            FuncDef(fd) => format!(
                "(fun {} -> {})",
                fd.param.simple_print(indent, strings),
                fd.body.simple_print(indent, strings)
            ),

            If(if_) => {
                let mut out = format!("if {}\n", if_.cond.simple_print(indent, strings));
                out += &make_indent(indent + 1);
                out += "then ";
                out += &if_.then_expr.simple_print(indent + 1, strings);
                out += "\n";
                out += &make_indent(indent + 1);
                out += "else ";
                out += &if_.else_expr.simple_print(indent + 1, strings);
                out += "\n";
                out += &make_indent(indent);
                out
            }

            Literal(lit) => lit.value.clone(),

            Loop(lp) => {
                let mut out = "loop\n".to_string();
                out += &make_indent(indent + 1);
                out += &lp.body.simple_print(indent + 1, strings);
                out += "\n";
                out += &make_indent(indent);
                out
            }

            Match(mtch) => {
                let mut out = format!("match {} with\n", mtch.expr.simple_print(indent, strings));

                for (tag, pat, expr) in &mtch.cases {
                    out += &make_indent(indent + 1);
                    out += strings.resolve(tag);
                    out += " ";
                    out += &pat.simple_print(indent + 1, strings);
                    out += " -> ";
                    out += &expr.simple_print(indent + 1, strings);
                    out += "\n";
                }

                if let Some((var, expr)) = &mtch.wildcard {
                    out += &make_indent(indent + 1);
                    out += &var.simple_print(indent + 1, strings);
                    out += " -> ";
                    out += &expr.simple_print(indent + 1, strings);
                    out += "\n";
                }

                out += &make_indent(indent);
                out
            }

            Record(rec) => format!(
                "{{{}}}",
                rec.fields
                    .iter()
                    .map(|(k, v, m)| {
                        format!(
                            "{}{}={}",
                            if *m { "mut " } else { "" },
                            strings.resolve(k),
                            v.simple_print(indent, strings)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("; ")
            ),

            Variable(v) => strings.resolve(&v.name).to_string(),

            Array(elems) => format!(
                "#[{}]",
                elems
                    .iter()
                    .map(|x| x.simple_print(indent, strings))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),

            Dict(elems) => format!(
                "#{{{}}}",
                elems
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k.simple_print(indent, strings), v.simple_print(indent, strings)))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl SimplePrint for runtime_ast::LetPattern {
    fn simple_print(&self, indent: usize, strings: &Rodeo) -> String {
        use runtime_ast::LetPattern::*;
        match self {
            Var(v) => v.simple_print(indent, strings),
            Case(tag, pat) => format!("`{} {}", strings.resolve(tag), pat.simple_print(indent, strings)),
            Record(fields) => format!(
                "{{{}}}",
                fields
                    .iter()
                    .map(|(f, p)| format!("{}={}", strings.resolve(f), p.simple_print(indent, strings)))
                    .collect::<Vec<_>>()
                    .join("; ")
            ),
        }
    }
}

impl SimplePrint for runtime_ast::Variable {
    fn simple_print(&self, indent: usize, strings: &Rodeo) -> String {
        match &self.0 {
            None => "_".to_string(),
            Some(v) => strings.resolve(v).to_string(),
        }
    }
}

impl SimplePrint for compiler_lib::ast::Statement {
    fn simple_print(&self, indent: usize, strings: &Rodeo) -> String {
        use compiler_lib::ast::Statement::*;
        match self {
            Empty => "<nop>".to_string(),
            Expr(x) => x.simple_print(indent, strings),
            LetDef((pat, val)) => format!(
                "let {} = {}",
                pat.simple_print(indent, strings),
                val.simple_print(indent, strings)
            ),
            LetRecDef(defs) => {
                let mut out = "let rec\n".to_string();
                let let_indent = indent + 1;
                for (name, expr) in defs {
                    out += &make_indent(let_indent);
                    out += strings.resolve(name);
                    out += " = ";
                    out += &expr.simple_print(let_indent, strings);
                    out += "\n";
                }
                out += &make_indent(indent);
                out
            }
            Println(exprs) => format!(
                "print {}",
                exprs
                    .iter()
                    .map(|x| x.simple_print(indent, strings))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Import((lib, _)) => format!("import {lib:?}"),
            TypeDef(_) => todo!(),
        }
    }
}

impl SimplePrint for compiler_lib::ast::Expr {
    fn simple_print(&self, indent: usize, strings: &Rodeo) -> String {
        use compiler_lib::ast::Expr::*;
        use compiler_lib::ast::Op;
        match self {
            BinOp(bop) => {
                let op = match bop.op {
                    Op::Add => "+",
                    Op::Sub => "-",
                    Op::Mult => "*",
                    Op::Div => "/",
                    Op::Rem => "%",
                    Op::Lt => "<",
                    Op::Lte => "<=",
                    Op::Gt => ">",
                    Op::Gte => ">=",
                    Op::Eq => "==",
                    Op::Neq => "!=",
                };
                format!(
                    "({} {} {})",
                    bop.lhs.simple_print(indent, strings),
                    op,
                    bop.rhs.simple_print(indent, strings)
                )
            }

            Block(blk) => {
                let mut out = "begin\n".to_string();
                for stmt in &blk.statements {
                    out += &make_indent(indent + 1);
                    out += &stmt.simple_print(indent + 1, strings);
                    out += ";\n";
                }
                out += &make_indent(indent + 1);
                out += &blk.expr.simple_print(indent + 1, strings);
                out += "\n";
                out += &make_indent(indent);
                out += "end";
                out
            }

            Call(call) => format!(
                "({} {})",
                call.func.simple_print(indent, strings),
                call.arg.simple_print(indent, strings)
            ),

            Case(cx) => format!("`{} {}", strings.resolve(&cx.tag.0), cx.expr.simple_print(indent, strings)),

            FieldAccess(fa) => format!("{}.{}", fa.expr.simple_print(indent, strings), strings.resolve(&fa.field.0)),

            FieldSet(fs) => format!(
                "({}.{} <- {})",
                fs.expr.simple_print(indent, strings),
                strings.resolve(&fs.field.0),
                fs.value.simple_print(indent, strings)
            ),

            FuncDef(fd) => format!(
                "(fun {} -> {})",
                fd.param.simple_print(indent, strings),
                fd.body.simple_print(indent, strings)
            ),

            If(if_) => {
                let mut out = format!("if {}\n", if_.cond.simple_print(indent, strings));
                out += &make_indent(indent + 1);
                out += "then ";
                out += &if_.then_expr.simple_print(indent + 1, strings);
                out += "\n";
                out += &make_indent(indent + 1);
                out += "else ";
                out += &if_.else_expr.simple_print(indent + 1, strings);
                out += "\n";
                out += &make_indent(indent);
                out
            }

            InstantiateExist(ie) => ie.expr.simple_print(indent, strings),
            InstantiateUni(iu) => iu.expr.simple_print(indent, strings),

            Literal(lit) => lit.value.0.clone(),

            Loop(lp) => {
                let mut out = "loop\n".to_string();
                out += &make_indent(indent + 1);
                out += &lp.body.simple_print(indent + 1, strings);
                out += "\n";
                out += &make_indent(indent);
                out
            }

            Match(mtch) => {
                let mut out = format!("match {} with\n", mtch.expr.simple_print(indent, strings));

                for (pat, expr) in &mtch.cases {
                    out += &make_indent(indent + 1);
                    out += &pat.simple_print(indent + 1, strings);
                    out += " -> ";
                    out += &expr.simple_print(indent + 1, strings);
                    out += "\n";
                }
                out += &make_indent(indent);
                out
            }

            Record(rec) => format!(
                "{{{}}}",
                rec.fields
                    .iter()
                    .map(|(k, v, m, _)| {
                        format!(
                            "{}{}={}",
                            if *m { "mut " } else { "" },
                            strings.resolve(&k.0),
                            v.simple_print(indent, strings)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("; ")
            ),

            Typed(tx) => tx.expr.simple_print(indent, strings),

            Variable(v) => strings.resolve(&v.name).to_string(),

            Array(_, elems) => format!(
                "#[{}]",
                elems
                    .iter()
                    .map(|x| x.simple_print(indent, strings))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),

            Dict(_, elems) => format!(
                "#{{{}}}",
                elems
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k.simple_print(indent, strings), v.simple_print(indent, strings)))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl SimplePrint for LetPattern {
    fn simple_print(&self, indent: usize, strings: &Rodeo) -> String {
        use compiler_lib::ast::LetPattern::*;
        match self {
            Var((None, _), _) => "_".to_string(),
            Var((Some(v), _), _) => strings.resolve(v).to_string(),
            Case((tag, _), pat) => format!("`{} {}", strings.resolve(tag), pat.simple_print(indent, strings)),
            Record(((_, fields), _)) => format!(
                "{{{}}}",
                fields
                    .iter()
                    .map(|((f, _), p)| format!("{}={}", strings.resolve(f), p.simple_print(indent, strings)))
                    .collect::<Vec<_>>()
                    .join("; ")
            ),
        }
    }
}

impl<T: SimplePrint> SimplePrint for Spanned<T> {
    fn simple_print(&self, indent: usize, strings: &Rodeo) -> String {
        self.0.simple_print(indent, strings)
    }
}

impl<T: SimplePrint> SimplePrint for Box<T> {
    fn simple_print(&self, indent: usize, strings: &Rodeo) -> String {
        (**self).simple_print(indent, strings)
    }
}

fn make_indent(indent: usize) -> String {
    " ".repeat(indent * 4)
}

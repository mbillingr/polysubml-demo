use crate::free_vars::free_vars;
use crate::value::Value;
use compiler_lib::ast::StringId;
use compiler_lib::{Rodeo, ast};
use std::collections::{BTreeSet, HashSet};
use std::sync::Arc;

pub struct CompilationContext<'a> {
    strings: &'a mut Rodeo,
    tags: BTreeSet<String>,
    fields: BTreeSet<String>,
    sym_counter: usize,
}

impl<'a> CompilationContext<'a> {
    pub fn new(strings: &'a mut Rodeo) -> Self {
        let mut tags = BTreeSet::new();
        for t in ["Err", "Ok", "Some", "None", "Eof", "Continue", "Break"] {
            tags.insert(t.to_string());
        }

        let mut fields = BTreeSet::new();
        for f in ["_0", "_1", "_2"] {
            fields.insert(f.to_string());
        }

        Self {
            strings,
            tags, fields,
            sym_counter: 0,
        }
    }

    fn gensym(&mut self, prefix: &str) -> String {
        let sym = format!("_{}_{}", prefix, self.sym_counter);
        self.sym_counter += 1;
        sym
    }

    pub fn compile_script(&mut self, stmts: Vec<ast::Statement>) -> String {
        let mut out = String::new();

        out += "mod runtime; use runtime::*; \n";
        out += "use num::ToPrimitive;\n";
        out += "fn main() {\n";
        out += include_str!("../../libs/builtins.rs");

        for stmt in stmts {
            out += &self.compile_statement(stmt);
            out += "\n";
        }
        out += "}";

        out = format!(
            "#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]\nenum _Tag {{ {} }}\n",
            self.tags.iter().map(|t| format!("{},", t)).collect::<Vec<_>>().join("")
        ) + &out;

        out = format!(
            "#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]\nenum _Field {{ {} }}\n",
            self.fields.iter().map(|t| format!("{},", t)).collect::<Vec<_>>().join("")
        ) + &out;
        out
    }

    fn compile_statement(&mut self, stmt: ast::Statement) -> String {
        match stmt {
            ast::Statement::Empty => String::new(),

            ast::Statement::Expr((expr, _)) => format!("let _ = {};\n", self.compile_expression(expr)),

            ast::Statement::LetDef((pat, expr)) => {
                let expr = self.compile_expression(expr.0);
                self.compile_pattern_assignment(pat, expr)
            }

            ast::Statement::LetRecDef(defs) => {
                let mut out = String::new();
                for (name, _) in &defs {
                    out += &format!("let {} = Value::cell(Value::nothing());\n", self.strings.resolve(name));
                }
                for (name, (expr, _)) in defs {
                    let val = self.compile_expression(expr);
                    out += &format!("{}.update_cell({});\n", self.strings.resolve(&name), val);
                }
                out
            }

            ast::Statement::Println(exprs) => {
                let mut out = String::new();
                out += "println!(\"";
                out += &vec!["{}"; exprs.len()].join(" ");
                out += "\", ";
                out += &exprs
                    .into_iter()
                    .map(|(e, _)| self.compile_expression(e))
                    .collect::<Vec<_>>()
                    .join(", ");
                out += ");\n";

                out
            }

            ast::Statement::Import(_) => unimplemented!(),
            ast::Statement::TypeDef(_) => unimplemented!(),
        }
    }

    fn compile_pattern_assignment(&mut self, pat: ast::LetPattern, expr: String) -> String {
        match pat {
            ast::LetPattern::Var((None, _), _) => format!("let _ = {};\n", expr),

            ast::LetPattern::Var((Some(var), _), _) => format!("let {} = {};\n", self.strings.resolve(&var), expr),

            ast::LetPattern::Case(_, inner_pat) => {
                self.compile_pattern_assignment(*inner_pat, format!("({}).as_case().1", expr))
            }

            ast::LetPattern::Record(((_, field_patterns), _)) => {
                let tmp = self.gensym("rec");
                let mut out = format!("let {tmp} = {};\n", expr);
                for ((field, _), inner_pat) in field_patterns.into_iter().rev() {
                    self.fields.insert(self.strings.resolve(&field).to_string());
                    out += &self.compile_pattern_assignment(
                        *inner_pat,
                        format!("{tmp}.get_field(_Field::{})", self.strings.resolve(&field)),
                    );
                }
                out
            }
        }
    }

    fn compile_expression(&mut self, expr: ast::Expr) -> String {
        use ast::Literal::*;
        match expr {
            ast::Expr::BinOp(binop) => {
                use ast::Literal::*;
                use ast::Op::*;
                let lhs = self.compile_expression(binop.lhs.0);
                let rhs = self.compile_expression(binop.rhs.0);
                match (binop.op_type.0, binop.op) {
                    (None, Eq) => format!("Value::bool(({lhs}) == ({rhs}))"),
                    (None, Neq) => format!("Value::bool(({lhs}) != ({rhs}))"),
                    (Some(Int), Lt) => format!("Value::bool(({lhs}).as_int() < ({rhs}).as_int())"),
                    (Some(Int), Lte) => format!("Value::bool(({lhs}).as_int() <= ({rhs}).as_int())"),
                    (Some(Int), Gt) => format!("Value::bool(({lhs}).as_int() > ({rhs}).as_int())"),
                    (Some(Int), Gte) => format!("Value::bool(({lhs}).as_int() >= ({rhs}).as_int())"),
                    (Some(Int), Add) => format!("Value::int(({lhs}).as_int() + ({rhs}).as_int())"),
                    (Some(Int), Sub) => format!("Value::int(({lhs}).as_int() - ({rhs}).as_int())"),
                    (Some(Int), Mult) => format!("Value::int(({lhs}).as_int() * ({rhs}).as_int())"),
                    (Some(Int), Div) => format!("Value::int(({lhs}).as_int() / ({rhs}).as_int())"),
                    (Some(Int), Rem) => format!("Value::int(({lhs}).as_int() % ({rhs}).as_int())"),
                    (Some(Float), Lt) => format!("Value::bool(({lhs}).as_float() < ({rhs}).as_float())"),
                    (Some(Float), Lte) => format!("Value::bool(({lhs}).as_float() <= ({rhs}).as_float())"),
                    (Some(Float), Gt) => format!("Value::bool(({lhs}).as_float() > ({rhs}).as_float())"),
                    (Some(Float), Gte) => format!("Value::bool(({lhs}).as_float() >= ({rhs}).as_float())"),
                    (Some(Float), Add) => format!("Value::float(({lhs}).as_float() + ({rhs}).as_float())"),
                    (Some(Float), Sub) => format!("Value::float(({lhs}).as_float() - ({rhs}).as_float())"),
                    (Some(Float), Mult) => format!("Value::float(({lhs}).as_float() * ({rhs}).as_float())"),
                    (Some(Float), Div) => format!("Value::float(({lhs}).as_float() / ({rhs}).as_float())"),
                    (Some(Float), Rem) => format!("Value::float(({lhs}).as_float() % ({rhs}).as_float())"),
                    (Some(Str), Add) => format!("Value::string(({lhs}).as_str().to_string() + ({rhs}).as_str())"),
                    other => todo!("{other:?}"),
                }
            }

            ast::Expr::Block(block) => {
                let mut out = "{\n".to_string();
                for stmt in block.statements {
                    out += &self.compile_statement(stmt);
                }
                out += &self.compile_expression(block.expr.0);
                out += "\n}";
                out
            }

            ast::Expr::Call(call) => {
                let f = self.compile_expression(call.func.0);
                let arg = self.compile_expression(call.arg.0);
                if call.eval_arg_first {
                    format!("{{let _arg = {{ {arg} }}; ({f})(_arg)}}")
                } else {
                    format!("({f}).apply({arg})")
                }
            }

            ast::Expr::Case(case) => {
                self.tags.insert(self.strings.resolve(&case.tag.0).to_string());
                let inner = self.compile_expression(case.expr.0);
                format!("Value::case(_Tag::{}, {})", self.strings.resolve(&case.tag.0), inner)
            }

            ast::Expr::FieldAccess(field_access) => {
                self.fields.insert(self.strings.resolve(&field_access.field.0).to_string());
                let obj = self.compile_expression(field_access.expr.0);
                format!("({}).get_field(_Field::{})", obj, self.strings.resolve(&field_access.field.0))
            }

            ast::Expr::FieldSet(field_access) => {
                self.fields.insert(self.strings.resolve(&field_access.field.0).to_string());
                let obj = self.compile_expression(field_access.expr.0);
                let val = self.compile_expression(field_access.value.0);
                format!(
                    "({}).set_field(_Field::{}, {})",
                    obj,
                    self.strings.resolve(&field_access.field.0),
                    val
                )
            }

            ast::Expr::FuncDef(fndef) => {
                let tmp = ast::Expr::FuncDef(fndef);
                let cls_vars = free_vars(&tmp);
                let fndef = if let ast::Expr::FuncDef(fndef) = tmp {
                    fndef
                } else {
                    unreachable!()
                };

                let pat = self.compile_pattern_assignment(fndef.param.0, "arg".to_string());
                let body = self.compile_expression(fndef.body.0);
                let cls = cls_vars
                    .into_iter()
                    .map(|v| format!("let {x} = {x}.clone();", x = self.strings.resolve(&v)))
                    .collect::<Vec<_>>()
                    .join("; ");
                format!("Value::func({{ {cls} move|arg| {{ {pat} {body} }} }})")
            }

            ast::Expr::If(if_) => {
                let cond = self.compile_expression(if_.cond.0.0);
                let then_ = self.compile_expression(if_.then_expr.0);
                let else_ = self.compile_expression(if_.else_expr.0);
                format!("if ({cond}).as_bool() {{ {then_} }} else {{ {else_} }}")
            }

            ast::Expr::InstantiateExist(iex) => self.compile_expression(iex.expr.0),

            ast::Expr::InstantiateUni(iux) => self.compile_expression(iux.expr.0),

            ast::Expr::Literal(ast::expr::LiteralExpr {
                lit_type,
                value: (value, _),
            }) => match lit_type {
                Bool => format!("Value::bool({})", value),
                Int => format!("Value::int_literal({})", value),
                Float => format!("Value::float({})", value),
                Str => format!("Value::str({})", value),
            },

            ast::Expr::Loop(loop_) => {
                self.tags.insert("Break".to_string());
                self.tags.insert("Loop".to_string());
                let body = self.compile_expression(loop_.body.0);
                format!("loop {{ if let (_Tag::Break, res) = ({body}).as_case() {{ break res.clone() }} }}")
            }

            ast::Expr::Match(mx) => {
                let val = self.compile_expression(mx.expr.0.0);

                let mut wildcard_arm = None;
                let mut tag_arms = vec![];
                for ((pat, _), expr) in mx.cases {
                    match pat {
                        ast::LetPattern::Record(_) => unimplemented!(),
                        ast::LetPattern::Var(_, _) => wildcard_arm = Some((pat, expr.0)),
                        ast::LetPattern::Case((tag, _), inner_pat) => tag_arms.push((tag, *inner_pat, expr.0)),
                    }
                }

                let tmp = self.gensym("val");

                let mut out = String::new();
                out += "{\n";
                out += &format!("let {tmp} = {val};\n");
                out += &format!("match {tmp}.as_case() {{\n");

                for (tag, pat, expr) in tag_arms {
                    let pat = self.compile_pattern_assignment(pat, tmp.clone());
                    let body = self.compile_expression(expr);
                    let tag = self.strings.resolve(&tag);
                    self.tags.insert(tag.to_string());
                    out += &format!("(_Tag::{tag}, {tmp}) => {{ {pat} {body} }}\n");
                }

                if let Some((pat, expr)) = wildcard_arm {
                    out += &format!(
                        "_ => {{ {} {} }}",
                        self.compile_pattern_assignment(pat, tmp),
                        self.compile_expression(expr)
                    );
                } else {
                    out += "_ => unreachable!()";
                }

                out += "}}";
                out
            }

            ast::Expr::Record(rec) => {
                let mut out = "Value::record([".to_string();

                for ((fld, _), val, mutable, _) in rec.fields {
                    let val = self.compile_expression(val.0);
                    let fld = self.strings.resolve(&fld);
                    self.fields.insert(fld.to_string());
                    out += &format!("(_Field::{fld}, {val}, {mutable}),");
                }

                out += "])";
                out
            }

            ast::Expr::Typed(tx) => self.compile_expression(tx.expr.0),

            ast::Expr::Variable(var) => format!("{}.clone()", self.strings.resolve(&var.name)),

            ast::Expr::Array(_, _) => todo!(),
            ast::Expr::Dict(_, _) => todo!(),
        }
    }
}

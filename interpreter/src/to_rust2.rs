use crate::value::Value;
use compiler_lib::ast::StringId;
use compiler_lib::{Rodeo, ast};
use std::sync::Arc;

pub struct CompilationContext<'a> {
    pub strings: &'a mut Rodeo,
}

impl<'a> CompilationContext<'a> {
    pub fn compile_script(&mut self, stmts: Vec<ast::Statement>) -> String {
        let mut out = String::new();
        for stmt in stmts {
            out += &self.compile_statement(stmt);
            out += "\n";
        }
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
                    out += &format!("let mut {} = Value::nothing();\n", self.strings.resolve(name));
                }
                for (name, (expr, _)) in defs {
                    let val = self.compile_expression(expr);
                    out += &format!("{} = {}", self.strings.resolve(&name), val);
                }
                out
            }

            ast::Statement::Println(exprs) => {
                todo!()
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
                let mut out = format!("let _rec = {};\n", expr);
                for (i, ((field, _), inner_pat)) in field_patterns.into_iter().enumerate().rev() {
                    out += &self.compile_pattern_assignment(*inner_pat, format!("_rec.get_field({})", i));
                }
                out
            }
        }
    }

    fn compile_expression(&mut self, expr: ast::Expr) -> String {
        use ast::Literal::*;
        match expr {
            ast::Expr::BinOp(binop) => {
                /*let ops = self.compile_expression(binop.lhs.0);
                let ops = extend(ops, self.compile_expression(binop.rhs.0));

                match (binop.op, binop.op_type) {
                    (op, (Some(Int), _)) => append(ops, Op::IntOp(op)),
                    (op, (Some(Float), _)) => append(ops, Op::FloatOp(op)),
                    (op, (Some(Bool), _)) => append(ops, Op::BoolOp(op)),
                    (op, (Some(Str), _)) => append(ops, Op::StrOp(op)),
                    (op, (None, _)) => append(ops, Op::AnyOp(op)),
                }*/
                todo!()
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
                    format!("({f})({arg})")
                }
            }

            ast::Expr::Case(case) => {
                let inner = self.compile_expression(case.expr.0);
                format!("Value::case(Tag::{}, {})", self.strings.resolve(&case.tag.0), inner)
            }

            ast::Expr::FieldAccess(field_access) => {
                let obj = self.compile_expression(field_access.expr.0);
                format!("({}).get_field({})", obj, self.strings.resolve(&field_access.field.0))
            }

            ast::Expr::FieldSet(field_access) => {
                let obj = self.compile_expression(field_access.expr.0);
                let val = self.compile_expression(field_access.value.0);
                format!(
                    "({}).set_field({}, {})",
                    obj,
                    self.strings.resolve(&field_access.field.0),
                    val
                )
            }

            ast::Expr::FuncDef(fndef) => {
                todo!()
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
                Int => format!("Value::int({})", value),
                Float => format!("Value::float({})", value),
                Str => format!("Value::str({})", value),
            },

            ast::Expr::Loop(loop_) => {
                todo!()
            }

            ast::Expr::Match(mx) => {
                /*let ops = self.compile_expression(mx.expr.0.0);

                let mut wildcard_arm = None;
                let mut tag_arms = vec![];
                for ((pat, _), expr) in mx.cases {
                    match pat {
                        ast::LetPattern::Record(_) => unimplemented!(),
                        ast::LetPattern::Var(_, _) => wildcard_arm = Some((pat, expr.0)),
                        ast::LetPattern::Case((tag, _), inner_pat) => tag_arms.push((tag, *inner_pat, expr.0)),
                    }
                }

                let mut last_branch = vec![];

                if let Some((var, expr)) = wildcard_arm {
                    last_branch = extend(self.compile_pattern_assignment(var), self.compile_expression(expr));
                }

                for (tag, pat, expr) in tag_arms {
                    let out = last_branch.len() as isize;
                    let branch = vec![Op::UnwrapCase];
                    let branch = extend(branch, self.compile_pattern_assignment(pat));
                    let branch = extend(branch, self.compile_expression(expr));
                    let branch = append(branch, Op::Jump(out));
                    let skip = branch.len() as isize;
                    let branch = extend(vec![Op::PeekAndJumpNotTag(tag, skip)], branch);
                    last_branch = extend(branch, last_branch);
                }
                let ops = append(ops, Op::PushEnv);
                let ops = append(ops, Op::Swap);
                let ops = extend(ops, last_branch);
                let ops = append(ops, Op::Swap);
                let ops = append(ops, Op::PopEnv);
                ops*/
                todo!()
            }

            ast::Expr::Record(rec) => {
                /*let mut ops = vec![];
                let mut fields = Vec::with_capacity(rec.fields.len());
                for ((fld, _), val, _, _) in rec.fields {
                    fields.push(fld);
                    ops = extend(ops, self.compile_expression(val.0));
                }
                ops.push(Op::MakeRecord(fields));
                ops*/
                todo!()
            }

            ast::Expr::Typed(tx) => self.compile_expression(tx.expr.0),

            ast::Expr::Variable(var) => format!("{}", self.strings.resolve(&var.name)),

            ast::Expr::Array(_, _) => todo!(),
            ast::Expr::Dict(_, _) => todo!(),
        }
    }
}

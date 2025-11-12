use crate::value::Value;
use compiler_lib::ast::StringId;
use compiler_lib::{Rodeo, ast};
use std::sync::Arc;

pub struct CompilationContext<'a> {
    pub strings: &'a mut Rodeo,
}

impl<'a> CompilationContext<'a> {
    pub fn compile_script(&mut self, stmts: Vec<ast::Statement>) -> Vec<Op> {
        let mut ops = vec![];
        for stmt in stmts {
            ops = extend(ops, self.compile_statement(stmt));
        }
        ops = append(ops, Op::Return);
        ops
    }

    fn compile_statement(&mut self, stmt: ast::Statement) -> Vec<Op> {
        match stmt {
            ast::Statement::Empty => vec![],

            ast::Statement::Expr((expr, _)) => append(self.compile_expression(expr), Op::Drop),

            ast::Statement::LetDef((pat, expr)) => {
                extend(self.compile_expression(expr.0), self.compile_pattern_assignment(pat))
            }

            ast::Statement::LetRecDef(defs) => {
                let mut ops = vec![];
                for (name, _) in &defs {
                    ops.push(Op::BindPlaceholder(*name));
                }
                for (name, (expr, _)) in defs {
                    let val = self.compile_expression(expr);
                    ops = extend(ops, val);
                    ops.push(Op::InitializePlaceholder(name));
                }
                ops
            }

            ast::Statement::Println(exprs) => {
                let n = exprs.len();
                let mut ops = vec![];
                for (x, _) in exprs {
                    ops.extend(self.compile_expression(x));
                }
                ops.push(Op::Println(n));
                ops
            }

            ast::Statement::Import(_) => unimplemented!(),
            ast::Statement::TypeDef(_) => unimplemented!(),
        }
    }

    fn compile_pattern_assignment(&mut self, pat: ast::LetPattern) -> Vec<Op> {
        match pat {
            ast::LetPattern::Var((None, _), _) => vec![Op::Drop],

            ast::LetPattern::Var((Some(var), _), _) => vec![Op::BindVar(var)],

            ast::LetPattern::Case(_, inner_pat) => extend(vec![Op::UnwrapCase], self.compile_pattern_assignment(*inner_pat)),

            ast::LetPattern::Record(((_, field_patterns), _)) => {
                let mut ops = vec![];
                for (i, ((field, _), inner_pat)) in field_patterns.into_iter().enumerate().rev() {
                    if i > 0 {
                        // copy the record if not the last field
                        ops.push(Op::Dup);
                    }
                    ops.push(Op::GetField(field));
                    ops = extend(ops, self.compile_pattern_assignment(*inner_pat));
                }
                ops
            }
        }
    }

    fn compile_expression(&mut self, expr: ast::Expr) -> Vec<Op> {
        use ast::Literal::*;
        match expr {
            ast::Expr::BinOp(binop) => {
                let ops = self.compile_expression(binop.lhs.0);
                let ops = extend(ops, self.compile_expression(binop.rhs.0));

                match (binop.op, binop.op_type) {
                    (op, (Some(Int), _)) => append(ops, Op::IntOp(op)),
                    (op, (Some(Float), _)) => append(ops, Op::FloatOp(op)),
                    (op, (Some(Bool), _)) => append(ops, Op::BoolOp(op)),
                    (op, (Some(Str), _)) => append(ops, Op::StrOp(op)),
                    (op, (None, _)) => append(ops, Op::AnyOp(op)),
                }
            }

            ast::Expr::Block(block) => {
                let mut ops = vec![Op::PushEnv];
                for stmt in block.statements {
                    ops = extend(ops, self.compile_statement(stmt));
                }
                ops = extend(ops, self.compile_expression(block.expr.0));
                ops = append(ops, Op::Swap);
                ops = append(ops, Op::PopEnv);
                ops
            }

            ast::Expr::Call(call) => {
                let f = self.compile_expression(call.func.0);
                let args = self.compile_expression(call.arg.0);
                let ops = if call.eval_arg_first {
                    append(extend(args, f), Op::Swap)
                } else {
                    extend(f, args)
                };
                append(ops, Op::Call)
            }

            ast::Expr::Case(case) => {
                let inner = self.compile_expression(case.expr.0);
                append(inner, Op::MakeCase(case.tag.0))
            }

            ast::Expr::FieldAccess(field_access) => {
                let inner = self.compile_expression(field_access.expr.0);
                append(inner, Op::GetField(field_access.field.0))
            }

            ast::Expr::FieldSet(field_access) => {
                let ops = self.compile_expression(field_access.expr.0);
                let val = self.compile_expression(field_access.value.0);
                append(extend(ops, val), Op::SetField(field_access.field.0))
            }

            ast::Expr::FuncDef(fndef) => {
                let body = append(
                    extend(
                        self.compile_pattern_assignment(fndef.param.0),
                        self.compile_expression(fndef.body.0),
                    ),
                    Op::Return,
                );
                vec![Op::MakeClosure(Arc::new(body))]
            }

            ast::Expr::If(if_) => {
                let cond = self.compile_expression(if_.cond.0.0);
                let then_ = self.compile_expression(if_.then_expr.0);
                let else_ = self.compile_expression(if_.else_expr.0);
                let else_len = else_.len() as isize;
                let then_ = append(then_, Op::Jump(else_len));
                let then_len = then_.len() as isize;
                let ops = append(cond, Op::JumpWhenFalse(then_len));
                extend(extend(ops, then_), else_)
            }

            ast::Expr::InstantiateExist(iex) => self.compile_expression(iex.expr.0),

            ast::Expr::InstantiateUni(iux) => self.compile_expression(iux.expr.0),

            ast::Expr::Literal(ast::expr::LiteralExpr { lit_type, value }) => {
                vec![Op::PushConstant(match lit_type {
                    Bool => Value::bool(value.0.parse().unwrap()),
                    Int => Value::int(value.0.parse().unwrap()),
                    Float => Value::float(value.0.parse().unwrap()),
                    Str => Value::str(&value.0.strip_prefix('"').unwrap().strip_suffix('"').unwrap()),
                })]
            }

            ast::Expr::Loop(loop_) => {
                let body_ops = self.compile_expression(loop_.body.0);
                let offset = 1 + body_ops.len() as isize;
                let cont = self.strings.get_or_intern_static("Continue");
                let ops = append(body_ops, Op::JumpAndPopWhenTag(cont, -offset));
                let ops = append(ops, Op::UnwrapCase);
                ops
            }

            ast::Expr::Match(mx) => {
                let ops = self.compile_expression(mx.expr.0.0);

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
                ops
            }

            ast::Expr::Record(rec) => {
                let mut ops = vec![];
                let mut fields = Vec::with_capacity(rec.fields.len());
                for ((fld, _), val, _, _) in rec.fields {
                    fields.push(fld);
                    ops = extend(ops, self.compile_expression(val.0));
                }
                ops.push(Op::MakeRecord(fields));
                ops
            }

            ast::Expr::Typed(tx) => self.compile_expression(tx.expr.0),

            ast::Expr::Variable(var) => vec![Op::PushVar(var.name)],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    /// Leave the current function
    Return,

    /// Bush a constant value onto the stack
    PushConstant(Value),

    /// Drop the top of the stack
    Drop,

    /// Duplicates the top of the stack
    Dup,

    /// Swap the top two values on the stack
    Swap,

    /// Push the value of a variable onto the stack
    PushVar(StringId),

    /// Pop from the stack and bind to variable
    BindVar(StringId),

    /// Bind an uninitialized value to a variable
    BindPlaceholder(StringId),

    /// Initialize the uninitialized value in a variable
    InitializePlaceholder(StringId),

    /// Push the current environment onto the stack
    PushEnv,

    /// Set the current environment to the top of the stack, popping it
    PopEnv,

    /// Pop a value from the stack, wrap it in a case, and push
    MakeCase(StringId),

    /// Pop a case from the stack and push the inner value
    UnwrapCase,

    /// Pop a value for each field from the stack and push a record
    MakeRecord(Vec<StringId>),

    /// Pop a record from the stack and push the field's value
    GetField(StringId),

    /// Pop an object and a value from the stack (value on top), update, and push the old value back
    SetField(StringId),

    /// Unconditionally jump by offset
    Jump(isize),

    /// Pop a boolean from the stack and jump if false
    JumpWhenFalse(isize),

    /// Jump if the stack top does not match the tag (pops when jumping)
    JumpAndPopWhenTag(StringId, isize),

    /// Jump if the stack top does not match the tag (without popping!)
    PeekAndJumpNotTag(StringId, isize),

    /// Pop two integers from the stack and push the result back
    IntOp(ast::Op),

    /// Pop two floats from the stack and push the result back
    FloatOp(ast::Op),

    /// Pop two booleans from the stack and push the result back
    BoolOp(ast::Op),

    /// Pop two strings from the stack and push the result back
    StrOp(ast::Op),

    /// Pop two values from the stack and push the result back
    AnyOp(ast::Op),

    /// Pop a function and an argument (arg below f on top) from the stack and push the result back
    Call,

    /// Capture the current environment and push a function on the stack
    MakeClosure(Arc<Vec<Op>>),

    /// Pop n values from the stack and print them
    Println(usize),
}

fn append(a: Vec<Op>, b: Op) -> Vec<Op> {
    let mut result = a;
    result.push(b);
    result
}

fn extend(a: Vec<Op>, b: Vec<Op>) -> Vec<Op> {
    let mut result = a;
    result.extend(b);
    result
}

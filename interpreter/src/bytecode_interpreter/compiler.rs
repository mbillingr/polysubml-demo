use crate::runtime_ast as ast;
use crate::value::Value;
use compiler_lib::Rodeo;
use compiler_lib::ast::StringId;
use std::rc::Rc;

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

            ast::Statement::Expr(expr) => append(self.compile_expression(expr), Op::Drop),

            ast::Statement::LetDef(pat, expr) => extend(self.compile_expression(expr), self.compile_pattern_assignment(pat)),

            ast::Statement::LetRecDef(defs) => {
                let mut ops = vec![];
                for (name, _) in &defs {
                    ops.push(Op::BindPlaceholder(*name));
                }
                for (name, expr) in defs {
                    let val = self.compile_expression(expr);
                    ops = extend(ops, val);
                    ops.push(Op::InitializePlaceholder(name));
                }
                ops
            }

            ast::Statement::Println(exprs) => {
                let n = exprs.len();
                let mut ops = vec![];
                for x in exprs {
                    ops.extend(self.compile_expression(x));
                }
                ops.push(Op::Println(n));
                ops
            }
        }
    }

    fn compile_pattern_assignment(&mut self, pat: ast::LetPattern) -> Vec<Op> {
        match pat {
            ast::LetPattern::Var(None) => vec![Op::Drop],

            ast::LetPattern::Var(Some(var)) => vec![Op::BindVar(var)],

            ast::LetPattern::Case(_, inner_pat) => extend(vec![Op::UnwrapCase], self.compile_pattern_assignment(*inner_pat)),

            ast::LetPattern::Record(field_patterns) => {
                let mut ops = vec![];
                for (i, (field, inner_pat)) in field_patterns.into_iter().enumerate().rev() {
                    if i > 0 {
                        // copy the record if not the last field
                        ops.push(Op::Dup);
                    }
                    ops.push(Op::GetField(field));
                    ops = extend(ops, self.compile_pattern_assignment(inner_pat));
                }
                ops
            }
        }
    }

    fn compile_expression(&mut self, expr: ast::Expr) -> Vec<Op> {
        use compiler_lib::ast::Literal::*;
        match expr {
            ast::Expr::BinOp(binop) => {
                let ops = self.compile_expression(*binop.lhs);
                let ops = extend(ops, self.compile_expression(*binop.rhs));

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
                ops = extend(ops, self.compile_expression(*block.expr));
                ops = append(ops, Op::Swap);
                ops = append(ops, Op::PopEnv);
                ops
            }

            ast::Expr::Call(call) => {
                let f = self.compile_expression(*call.func);
                let args = self.compile_expression(*call.arg);
                let ops = if call.eval_arg_first {
                    append(extend(args, f), Op::Swap)
                } else {
                    extend(f, args)
                };
                append(ops, Op::Call)
            }

            ast::Expr::Case(case) => {
                let inner = self.compile_expression(*case.expr);
                append(inner, Op::MakeCase(case.tag))
            }

            ast::Expr::FieldAccess(field_access) => {
                let inner = self.compile_expression(*field_access.expr);
                append(inner, Op::GetField(field_access.field))
            }

            ast::Expr::FieldSet(field_access) => {
                let ops = self.compile_expression(*field_access.expr);
                let val = self.compile_expression(*field_access.value);
                append(extend(ops, val), Op::SetField(field_access.field))
            }

            ast::Expr::FuncDef(fndef) => {
                let body = append(
                    extend(
                        self.compile_pattern_assignment(fndef.param),
                        self.compile_expression(*fndef.body),
                    ),
                    Op::Return,
                );
                vec![Op::MakeClosure(Rc::new(body))]
            }

            ast::Expr::If(if_) => {
                let cond = self.compile_expression(*if_.cond);
                let then_ = self.compile_expression(*if_.then_expr);
                let else_ = self.compile_expression(*if_.else_expr);
                let else_len = else_.len() as isize;
                let then_ = append(then_, Op::Jump(else_len));
                let then_len = then_.len() as isize;
                let ops = append(cond, Op::JumpWhenFalse(then_len));
                extend(extend(ops, then_), else_)
            }

            ast::Expr::Literal(ast::LiteralExpr { lit_type, value }) => {
                vec![Op::PushConstant(match lit_type {
                    Bool => Value::bool(value.parse().unwrap()),
                    Int => Value::int(value.parse().unwrap()),
                    Float => Value::float(value.parse().unwrap()),
                    Str => Value::str(&value.strip_prefix('"').unwrap().strip_suffix('"').unwrap()),
                })]
            }

            ast::Expr::Loop(loop_) => {
                let body_ops = self.compile_expression(*loop_.body);
                let offset = 1 + body_ops.len() as isize;
                let cont = self.strings.get_or_intern_static("Continue");
                let ops = append(body_ops, Op::JumpAndPopWhenTag(cont, -offset));
                let ops = append(ops, Op::UnwrapCase);
                ops
            }

            ast::Expr::Match(mx) => {
                let ops = self.compile_expression(*mx.expr);

                let mut wildcard_arm = None;
                let mut tag_arms = vec![];
                for (pat, expr) in mx.cases {
                    match pat {
                        ast::LetPattern::Record(_) => unimplemented!(),
                        ast::LetPattern::Var(_) => wildcard_arm = Some((pat, expr)),
                        ast::LetPattern::Case(tag, inner_pat) => tag_arms.push((tag, *inner_pat, expr)),
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
                for (fld, val, is_mut) in rec.fields {
                    fields.push((fld, is_mut));
                    ops = extend(ops, self.compile_expression(val));
                }
                ops.push(Op::MakeRecord(fields));
                ops
            }

            ast::Expr::Variable(var) => vec![Op::PushVar(var.name)],

            ast::Expr::Array(items) => {
                let n = items.len();
                let mut ops = vec![];
                for item in items {
                    ops = extend(ops, self.compile_expression(item));
                }
                ops.push(Op::MakeVector(n));
                ops
            }

            ast::Expr::Dict(items) => {
                let n = items.len();
                let mut ops = vec![];
                for (key, val) in items {
                    ops = extend(ops, self.compile_expression(key));
                    ops = extend(ops, self.compile_expression(val));
                }
                ops.push(Op::MakeDict(n));
                ops
            }
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
    MakeRecord(Vec<(StringId, bool)>),

    /// Pop n values from the stack and push a vector
    MakeVector(usize),

    /// Pop n*2 values from the stack and push a dict
    MakeDict(usize),

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

    /// Pop a function and an argument (f below arg on top) from the stack and push the result back
    Call,

    /// Capture the current environment and push a function on the stack
    MakeClosure(Rc<Vec<Op>>),

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

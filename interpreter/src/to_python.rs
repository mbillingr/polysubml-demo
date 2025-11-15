use compiler_lib::ast::StringId;
use compiler_lib::{Rodeo, ast};

pub struct CompilationContext<'a> {
    pub strings: &'a mut Rodeo,
    local_defs: Vec<PyStmt>,
    symctr: usize,
}

impl<'a> CompilationContext<'a> {
    pub fn new(strings: &'a mut Rodeo) -> Self {
        Self {
            strings,
            local_defs: vec![],
            symctr: 0,
        }
    }

    pub fn compile_script(&mut self, stmts: Vec<ast::Statement>) -> PyStmt {
        self.compile_statements(stmts)
    }

    fn compile_statement(&mut self, stmt: ast::Statement) -> PyStmt {
        let py = match stmt {
            ast::Statement::Empty => PyStmt::Nop,

            ast::Statement::Expr((expr, _)) => PyStmt::Expr(self.compile_expression(expr)),

            ast::Statement::LetDef((pat, expr)) => {
                let expr = self.compile_expression(expr.0);
                self.compile_pattern_assignment(pat, expr)
            }

            ast::Statement::LetRecDef(defs) => {
                let mut py = PyStmt::Nop;
                for (name, (expr, _)) in defs {
                    match expr {
                        ast::Expr::FuncDef(fndef) => {
                            py = py.chain(PyStmt::def(name, self.compile_function(fndef.param.0, fndef.body.0)))
                        }
                        _ => unimplemented!(),
                    }
                }
                py
            }

            ast::Statement::Println(exprs) => {
                let print = self.strings.get_or_intern_static("print");
                PyStmt::Expr(PyExpr::call(
                    PyExpr::var(print),
                    exprs.into_iter().map(|(x, _)| self.compile_expression(x)),
                ))
            }

            ast::Statement::Import(_) => unimplemented!(),
            ast::Statement::TypeDef(_) => unimplemented!(),
        };
        self.insert_local_defs(py)
    }

    fn insert_local_defs(&mut self, py: PyStmt) -> PyStmt {
        let mut defs = PyStmt::Nop;
        for def in self.local_defs.drain(..) {
            defs = defs.chain(def);
        }
        defs.chain(py)
    }

    fn compile_function(&mut self, param: ast::LetPattern, body: ast::Expr) -> PyStmt {
        let arg = PyExpr::Var(self.strings.get_or_intern_static("arg"));
        let preamble = self.compile_pattern_assignment(param, arg);
        let body = self.compile_thunk(body);
        preamble.chain(body)
    }

    fn compile_thunk(&mut self, body: ast::Expr) -> PyStmt {
        let mut py_body = PyStmt::Nop;
        let final_expr = match body {
            ast::Expr::Block(blk) => {
                py_body = self.compile_statements(blk.statements);
                blk.expr.0
            }
            other => other,
        };
        py_body = py_body.chain(PyStmt::return_(Some(self.compile_expression(final_expr))));
        self.insert_local_defs(py_body)
    }

    fn compile_pattern_assignment(&mut self, pat: ast::LetPattern, expr: PyExpr) -> PyStmt {
        match pat {
            ast::LetPattern::Var((None, _), _) => PyStmt::Nop,

            ast::LetPattern::Var((Some(var), _), _) => PyStmt::assign(PyExpr::var(var), expr),

            ast::LetPattern::Case(_, inner_pat) => self.compile_pattern_assignment(*inner_pat, expr.unwrap_case()),

            ast::LetPattern::Record(((_, field_patterns), _)) => {
                let recid = self.gensym("recpat");
                let mut py = PyStmt::assign(PyExpr::var(recid), expr);
                for ((field, _), inner_pat) in field_patterns.into_iter() {
                    let p = self.compile_pattern_assignment(*inner_pat, PyExpr::var(recid).get_attr(field));
                    py = py.chain(p);
                }
                py
            }
        }
    }

    fn compile_block(&mut self, statements: Vec<ast::Statement>, expr: ast::Expr) -> PyStmt {
        let py = self.compile_statements(statements);
        let py = py.chain(PyStmt::return_(Some(self.compile_expression(expr))));
        self.insert_local_defs(py)
    }

    fn compile_statements(&mut self, statements: Vec<ast::Statement>) -> PyStmt {
        let mut py = PyStmt::Nop;
        for stmt in statements {
            py = py.chain(self.compile_statement(stmt));
        }

        self.insert_local_defs(py)
    }

    fn compile_expression(&mut self, expr: ast::Expr) -> PyExpr {
        match expr {
            ast::Expr::BinOp(binop) => {
                let lhs = self.compile_expression(binop.lhs.0);
                let rhs = self.compile_expression(binop.rhs.0);
                PyExpr::binop(binop.op, lhs, rhs)
            }

            ast::Expr::Block(block) => {
                let block_id = self.gensym("aux_block");
                let py = self.compile_block(block.statements, block.expr.0);
                self.local_defs.push(PyStmt::def_thunk(block_id, py));
                PyExpr::var(block_id).call(vec![])
            }

            ast::Expr::Call(call) => {
                let f = self.compile_expression(call.func.0);
                let arg = self.compile_expression(call.arg.0);
                PyExpr::call(f, vec![arg])
            }

            ast::Expr::Case(case) => {
                let inner = self.compile_expression(case.expr.0);
                inner.wrap_case(case.tag.0)
            }

            ast::Expr::FieldAccess(field_access) => {
                let obj = self.compile_expression(field_access.expr.0);
                obj.get_attr(field_access.field.0)
            }

            ast::Expr::FieldSet(field_access) => {
                let obj = self.compile_expression(field_access.expr.0);
                let val = self.compile_expression(field_access.value.0);
                obj.set_attr(field_access.field.0, val)
            }

            ast::Expr::FuncDef(fndef) => {
                let body = self.compile_function(fndef.param.0, fndef.body.0);
                let ident = self.gensym("fun");
                self.local_defs.push(PyStmt::def(ident, body));
                PyExpr::var(ident)
            }

            ast::Expr::If(if_) => {
                let cond = self.compile_expression(if_.cond.0.0);
                let then_ = self.compile_expression(if_.then_expr.0);
                let else_ = self.compile_expression(if_.else_expr.0);
                PyExpr::if_(cond, then_, else_)
            }

            ast::Expr::InstantiateExist(iex) => self.compile_expression(iex.expr.0),

            ast::Expr::InstantiateUni(iux) => self.compile_expression(iux.expr.0),

            ast::Expr::Literal(ast::expr::LiteralExpr { value: (value, _), .. }) => PyExpr::Literal(value),

            ast::Expr::Loop(loop_) => {
                let ident = self.gensym("loop");
                let break_tag = self.strings.get_or_intern_static("Break");
                let mut body_stmt = PyStmt::Nop;
                let final_expr = match loop_.body.0 {
                    ast::Expr::Block(blk) => {
                        body_stmt = self.compile_statements(blk.statements);
                        blk.expr.0
                    }
                    other => other,
                };
                let body_stmt = body_stmt.chain(PyStmt::assign(PyExpr::Var(ident), self.compile_expression(final_expr)));
                let body_stmt = body_stmt.chain(PyStmt::if_(
                    PyExpr::var(ident).has_tag(break_tag),
                    PyStmt::return_(Some(PyExpr::var(ident).unwrap_case())),
                ));
                self.local_defs.push(PyStmt::def_thunk(ident, PyStmt::loop_(body_stmt)));
                PyExpr::call(PyExpr::var(ident), vec![])
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
                if rec.fields.is_empty() {
                    PyExpr::var(self.strings.get_or_intern_static("EMPTY_RECORD"))
                } else {
                    let ident = self.gensym("Record");
                    let cls = PyStmt::DefRecord(ident, rec.fields.iter().map(|((f, _), _, _, _)| *f).collect());
                    self.local_defs.push(cls);
                    PyExpr::call(
                        PyExpr::var(ident),
                        rec.fields.into_iter().map(|(_, v, _, _)| self.compile_expression(v.0)),
                    )
                }
            }

            ast::Expr::Typed(tx) => self.compile_expression(tx.expr.0),

            ast::Expr::Variable(var) => PyExpr::var(var.name),
        }
    }

    fn gensym(&mut self, prefix: &str) -> StringId {
        let name = format!("_{prefix}_{}", self.symctr);
        self.symctr += 1;
        self.strings.get_or_intern(name)
    }
}

#[derive(Debug)]
pub enum PyStmt {
    Nop,
    Comment(String, Box<PyStmt>),
    Expr(PyExpr),
    Chain(Box<PyStmt>, Box<PyStmt>),
    /// def name(arg): ...
    Def(StringId, Box<PyStmt>),
    /// def name(): ...
    DefThunk(StringId, Box<PyStmt>),
    /// class name: def __init__(self, a, b, ...): ...
    DefRecord(StringId, Vec<StringId>),
    Assign(PyExpr, PyExpr),
    Return(Option<PyExpr>),
    If(PyExpr, Box<PyStmt>),
    Loop(Box<PyStmt>),
}

#[derive(Debug)]
pub enum PyExpr {
    Literal(String),
    StaticString(StringId),
    Var(StringId),
    Tuple(Vec<PyExpr>),
    /// obj[i]
    ConstIndexed(usize, Box<PyExpr>),
    /// obj.x
    GetAttr(StringId, Box<PyExpr>),
    /// _set_attr(obj, "x", newval)
    SetAttr(Box<PyExpr>, StringId, Box<PyExpr>),
    /// func(...)
    Call(Box<PyExpr>, Vec<PyExpr>),
    /// b if a else c
    If(Box<PyExpr>, Box<PyExpr>, Box<PyExpr>),
    BinOp(ast::Op, Box<PyExpr>, Box<PyExpr>),
}

impl PyStmt {
    fn chain(self, other: Self) -> Self {
        match (self, other) {
            (PyStmt::Nop, b) => b,
            (a, PyStmt::Nop) => a,
            (a, b) => PyStmt::Chain(Box::new(a), Box::new(b)),
        }
    }

    fn def(name: StringId, body: PyStmt) -> Self {
        PyStmt::Def(name, Box::new(body))
    }

    fn def_thunk(name: StringId, body: PyStmt) -> Self {
        PyStmt::DefThunk(name, Box::new(body))
    }

    fn assign(target: PyExpr, value: PyExpr) -> Self {
        PyStmt::Assign(target, value)
    }

    fn return_(value: Option<PyExpr>) -> Self {
        PyStmt::Return(value)
    }

    fn if_(cond: PyExpr, then_: Self) -> Self {
        Self::If(cond, Box::new(then_))
    }

    fn loop_(body: Self) -> Self {
        PyStmt::Loop(Box::new(body))
    }

    pub fn into_python_src(self, indent: usize, skip_indent: bool, strings: &mut Rodeo) -> String {
        let mut out = String::new();
        if !skip_indent {
            out += &indentate(indent);
        }
        match self {
            PyStmt::Nop => out += "pass",

            PyStmt::Comment(txt, stmt) => {
                out += "# ";
                out += &txt;
                out += "\n";
                out += &stmt.into_python_src(indent, false, strings);
            }

            PyStmt::Expr(x) => out += &x.into_python_src(strings),

            PyStmt::Chain(first, next) => {
                out += &first.into_python_src(indent, true, strings);
                out += &next.into_python_src(indent, false, strings);
                return out; // newline was already added by next
            }

            PyStmt::Def(name, body) => {
                out += &format!("def {}(arg):\n", strings.resolve(&name));
                out += &body.into_python_src(indent + 1, false, strings);
            }

            PyStmt::DefThunk(name, body) => {
                out += &format!("def {}():\n", strings.resolve(&name));
                out += &body.into_python_src(indent + 1, false, strings);
            }

            PyStmt::DefRecord(name, fields) => {
                let fields: Vec<_> = fields.into_iter().map(|f| strings.resolve(&f)).collect();
                out += &format!("class {}:\n", strings.resolve(&name));
                let indent = indent + 1;
                out += &indentate(indent);
                out += &format!("def __init__(self, {}):\n", fields.join(", "));
                let indent = indent + 1;
                for f in fields {
                    out += &indentate(indent);
                    out += &format!("self.{f} = {f}\n")
                }
            }

            PyStmt::Assign(tgt, val) => {
                out += &format!("{} = {}", tgt.into_python_src(strings), val.into_python_src(strings))
            }

            PyStmt::Return(None) => out += "return",
            PyStmt::Return(Some(expr)) => out += &format!("return {}", expr.into_python_src(strings)),

            PyStmt::If(cond, then_) => {
                out += &format!("if {}:\n", cond.into_python_src(strings));
                out += &then_.into_python_src(indent + 1, false, strings);
                return out; // newline was already added by branch
            }

            PyStmt::Loop(body) => {
                out += "while True:\n";
                out += &body.into_python_src(indent + 1, false, strings);
                return out; // newline was already added by body
            }
        }
        out + "\n"
    }
}

impl PyExpr {
    fn var(name: StringId) -> Self {
        Self::Var(name)
    }

    fn wrap_case(self, tag: StringId) -> Self {
        Self::Tuple(vec![Self::StaticString(tag), self])
    }

    fn unwrap_case(self) -> Self {
        Self::ConstIndexed(1, Box::new(self))
    }

    fn unwrap_tag(self) -> Self {
        Self::ConstIndexed(0, Box::new(self))
    }

    fn has_tag(self, tag: StringId) -> Self {
        Self::binop(ast::Op::Eq, self.unwrap_tag(), Self::StaticString(tag))
    }

    fn get_attr(self, name: StringId) -> Self {
        Self::GetAttr(name, Box::new(self))
    }

    fn set_attr(self, name: StringId, value: Self) -> Self {
        Self::SetAttr(Box::new(self), name, Box::new(value))
    }

    fn call(self, args: impl IntoIterator<Item = Self>) -> Self {
        Self::Call(Box::new(self), args.into_iter().collect())
    }

    fn if_(cond: Self, then_: Self, else_: Self) -> Self {
        Self::If(Box::new(cond), Box::new(then_), Box::new(else_))
    }

    fn binop(op: ast::Op, lhs: Self, rhs: Self) -> Self {
        Self::BinOp(op, Box::new(lhs), Box::new(rhs))
    }

    pub fn into_python_src(self, strings: &mut Rodeo) -> String {
        match self {
            PyExpr::Literal(txt) => txt,
            PyExpr::StaticString(s) => format!("\"{}\"", strings.resolve(&s)),
            PyExpr::Var(v) => strings.resolve(&v).to_string(),

            PyExpr::Tuple(fields) => format!(
                "({})",
                fields
                    .into_iter()
                    .map(|f| f.into_python_src(strings))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),

            PyExpr::ConstIndexed(idx, obj) => format!("({})[{}]", obj.into_python_src(strings), idx),

            PyExpr::GetAttr(name, rec) => {
                format!("({}).{}", rec.into_python_src(strings), strings.resolve(&name))
            }

            PyExpr::SetAttr(rec, name, val) => {
                let r = rec.into_python_src(strings);
                let v = val.into_python_src(strings);
                format!("_setattr({}, \"{}\", {})", r, strings.resolve(&name), v)
            }

            PyExpr::Call(fun, args) => format!(
                "({})({})",
                fun.into_python_src(strings),
                args.into_iter()
                    .map(|arg| arg.into_python_src(strings))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),

            PyExpr::If(c, t, e) => format!(
                "{} if {} else {}",
                t.into_python_src(strings),
                c.into_python_src(strings),
                e.into_python_src(strings)
            ),

            PyExpr::BinOp(op, lhs, rhs) => {
                let op = match op {
                    ast::Op::Add => "+",
                    ast::Op::Sub => "-",
                    ast::Op::Mult => "*",
                    ast::Op::Div => "/",
                    ast::Op::Rem => "%",
                    ast::Op::Lt => "<",
                    ast::Op::Gt => ">",
                    ast::Op::Lte => "<=",
                    ast::Op::Gte => ">=",
                    ast::Op::Eq => "==",
                    ast::Op::Neq => "!=",
                };
                format!("({}) {} ({})", lhs.into_python_src(strings), op, rhs.into_python_src(strings))
            }
        }
    }
}

fn indentate(indent: usize) -> String {
    " ".repeat(indent * 4)
}

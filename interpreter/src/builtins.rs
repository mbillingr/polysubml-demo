use crate::ast_interpreter::Env;
use crate::bytecode_interpreter::vm;
use crate::value::Value;
use compiler_lib::Rodeo;
use compiler_lib::ast::StringId;
use num::ToPrimitive;
use std::cell::RefCell;

pub struct Context<'a> {
    pub strings: &'a Rodeo,
}

impl<'a> From<&'a Rodeo> for Context<'a> {
    fn from(strings: &'a Rodeo) -> Self {
        Context { strings }
    }
}

pub fn define_builtins(env: Env, vm_env: vm::Env, strings: &mut Rodeo) -> (Env, vm::Env) {
    let ok = strings.get_or_intern_static("Ok");
    let err = strings.get_or_intern_static("Err");

    let eof_ = Value::case(strings.get_or_intern_static("Eof"), Value::int(0.into()));

    let idx0 = strings.get_or_intern_static("_0");
    let idx1 = strings.get_or_intern_static("_1");
    let idx2 = strings.get_or_intern_static("_2");

    let mut bb = BuiltinBuilder::new(env, vm_env, strings);

    bb.bind("panic", |msg, ctx| panic!("{}", msg.show(ctx.strings)));

    let eof = eof_.clone();
    bb.bind("__read_line", move |_, _| {
        use std::io::BufRead;
        let mut s = String::new();
        match std::io::stdin().lock().read_line(&mut s) {
            Ok(0) => eof.clone(),
            Ok(_) => {
                if s.ends_with('\n') {
                    s.pop();
                }
                Value::case(ok, Value::string(s))
            }
            Err(e) => Value::case(err, Value::string(e.to_string())),
        }
    });

    bb.bind("__write_str", |s, _| {
        print!("{}", s.as_str());
        Value::nothing()
    });

    let none = bb.val_none.clone();
    let some = bb.tag_some;
    bb.bind("__chars", move |s, _| {
        let chars = RefCell::new(s.as_str().chars().rev().collect::<Vec<_>>());

        let none = none.clone();
        Value::builtin(move |_, _| match chars.borrow_mut().pop() {
            None => none.clone(),
            Some(x) => Value::case(some, Value::string(x)),
        })
    });

    let none = bb.val_none.clone();
    let some = bb.tag_some;
    bb.bind("__split", move |s, _| {
        let parts = RefCell::new(s.as_str().split_whitespace().rev().map(str::to_string).collect::<Vec<_>>());

        let none = none.clone();
        Value::builtin(move |_, _| match parts.borrow_mut().pop() {
            None => none.clone(),
            Some(x) => Value::case(some, Value::string(x)),
        })
    });

    bb.bind("__char_to_num", move |s, _| {
        Value::int(s.as_str().chars().next().map(|x| x as i64).unwrap_or(-1).into())
    });

    bb.bind_opt("__num_to_char", move |i, _| {
        i.as_int().to_u32().and_then(char::from_u32).map(|ch| Value::string(ch))
    });

    bb.bind("__escape", move |s, _| {
        Value::string(String::from_utf8(escape_bytes::escape(s.as_str().bytes())).unwrap())
    });

    bb.bind("__unescape", move |s, _| {
        Value::string(String::from_utf8(escape_bytes::unescape(s.as_str().bytes()).unwrap()).unwrap())
    });

    bb.bind("__int_to_float", |x, _| Value::float(x.as_int().to_f64().unwrap()));
    bb.bind("__float_to_int", |x, _| Value::int((x.as_float() as i64).into()));
    bb.bind_opt("__str_to_int", |x, _| x.as_str().parse::<_>().map(Value::int).ok());
    bb.bind_opt("__str_to_float", |x, _| x.as_str().parse::<_>().map(Value::float).ok());
    bb.bind("__int_to_str", |x, _| Value::string(x.as_int().to_string()));
    bb.bind("__float_to_str", |x, _| Value::string(x.as_float().to_string()));

    bb.bind("__vec_new", |_, _| Value::vect(vec![]));
    bb.bind("__vec_length", move |vec, _| Value::usize(vec.as_vect().len().into()));
    bb.bind("__vec_push_back", move |args, _| {
        let mut v = args.get_field(idx0).as_vect().clone();
        let x = args.get_field(idx1);
        v.push_back(x);
        Value::vect(v)
    });
    bb.bind("__vec_pop_back", move |vec, _| {
        let mut v = vec.as_vect().clone();
        v.pop_back();
        Value::vect(v)
    });
    bb.bind_opt("__vec_peek_back", move |vec, _| vec.as_vect().clone().back().cloned());
    bb.bind("__vec_push_front", move |args, _| {
        let mut v = args.get_field(idx0).as_vect().clone();
        let x = args.get_field(idx1);
        v.push_front(x);
        Value::vect(v)
    });
    bb.bind("__vec_pop_front", move |vec, _| {
        let mut v = vec.as_vect().clone();
        v.pop_front();
        Value::vect(v)
    });
    bb.bind_opt("__vec_peek_front", move |vec, _| vec.as_vect().clone().front().cloned());
    bb.bind_opt("__vec_get", move |args, _| {
        let v = args.get_field(idx0).as_vect().clone();
        let idx = args.get_field(idx1).as_int().to_usize();
        idx.and_then(|i| v.get(i).cloned())
    });
    bb.bind_opt("__vec_set", move |args, _| {
        let mut v = args.get_field(idx0).as_vect().clone();
        let idx = args.get_field(idx1).as_int().to_usize();
        let val = args.get_field(idx2);
        let x = idx.and_then(|i| v.get_mut(i));
        let x = x.map(|x| *x = val);
        x.map(|_| Value::vect(v))
    });
    bb.bind("__vec_split", move |args, _| {
        let mut v = args.get_field(idx0).as_vect().clone();
        let idx = args.get_field(idx1).as_int().to_usize().unwrap();
        let w = v.split_off(idx);
        Value::record(vec![(idx0, Value::vect(v), false), (idx1, Value::vect(w), false)])
    });
    let none = bb.val_none.clone();
    let some = bb.tag_some;
    bb.bind("__vec_iter", move |args, _| {
        let d = args.as_vect().clone();
        let it = RefCell::new(d.into_iter());
        let none = none.clone();
        Value::builtin(move |_, _| match it.borrow_mut().next() {
            None => none.clone(),
            Some(x) => Value::case(some, x),
        })
    });
    let none = bb.val_none.clone();
    let some = bb.tag_some;
    bb.bind("__vec_iter_rev", move |args, _| {
        let d = args.as_vect().clone();
        let it = RefCell::new(d.into_iter().rev());
        let none = none.clone();
        Value::builtin(move |_, _| match it.borrow_mut().next() {
            None => none.clone(),
            Some(x) => Value::case(some, x),
        })
    });

    bb.bind("__dict_new", |_, _| Value::dict(vec![]));
    bb.bind("__dict_length", |dict, _| Value::usize(dict.as_dict().len().into()));
    bb.bind("__dict_insert", move |args, _| {
        let mut d = args.get_field(idx0).as_dict().clone();
        let k = args.get_field(idx1);
        let v = args.get_field(idx2);
        d.insert(k, v);
        Value::dict(d)
    });
    bb.bind("__dict_contains", move |args, _| {
        let d = args.get_field(idx0).as_dict().clone();
        let k = args.get_field(idx1);
        Value::bool(d.contains_key(&k))
    });
    bb.bind("__dict_remove", move |args, _| {
        let mut d = args.get_field(idx0).as_dict().clone();
        let k = args.get_field(idx1);
        d.remove(&k);
        Value::dict(d)
    });
    bb.bind_opt("__dict_get", move |args, _| {
        let d = args.get_field(idx0).as_dict().clone();
        let k = args.get_field(idx1);
        d.get(&k).cloned()
    });
    let none = bb.val_none.clone();
    let some = bb.tag_some;
    bb.bind("__dict_iter", move |args, _| {
        let d = args.as_dict().clone();
        let it = RefCell::new(d.into_iter());
        let none = none.clone();
        Value::builtin(move |_, _| match it.borrow_mut().next() {
            None => none.clone(),
            Some((k, v)) => Value::case(some, Value::record([(idx0, k.clone(), false), (idx1, v.clone(), false)])),
        })
    });

    bb.bind("__progress_bar_new", |n, _| Value::pbar(n.as_int().to_u64()));
    bb.bind("__progress_bar_step", move |args, _| {
        let pb = args.get_field(idx0);
        let pb = pb.as_pbar();
        if let Some(delta) = args.get_field(idx1).as_int().to_i64() {
            if delta < 0 {
                pb.dec((-delta) as u64);
            } else {
                pb.inc(delta as u64);
            }
        }
        Value::nothing()
    });
    bb.bind("__progress_bar_setlen", move |args, _| {
        let pb = args.get_field(idx0);
        let pb = pb.as_pbar();
        if let Some(n) = args.get_field(idx1).as_int().to_u64() {
            pb.set_length(n)
        }
        Value::nothing()
    });
    bb.bind("__progress_bar_finish", |pb, _| {
        pb.as_pbar().finish();
        Value::nothing()
    });

    (bb.env, bb.vm_env)
}

struct BuiltinBuilder<'a> {
    env: Env,
    vm_env: vm::Env,
    strings: &'a mut Rodeo,

    tag_some: StringId,
    val_none: Value,
}

impl<'a> BuiltinBuilder<'a> {
    fn new(env: Env, vm_env: vm::Env, strings: &'a mut Rodeo) -> Self {
        BuiltinBuilder {
            env,
            vm_env,
            tag_some: strings.get_or_intern_static("Some"),
            val_none: Value::case(strings.get_or_intern_static("None"), Value::nothing()),
            strings,
        }
    }

    fn bind(&mut self, name: &'static str, f: impl Fn(Value, &mut Context) -> Value + 'static) {
        let name = self.strings.get_or_intern_static(name);
        let value = Value::builtin(f);
        self.env = self.env.bind(name, value.clone());
        self.vm_env = self.vm_env.bind(name, value);
    }

    fn bind_opt(&mut self, name: &'static str, f: impl Fn(Value, &mut Context) -> Option<Value> + 'static) {
        let some = self.tag_some;
        let none = self.val_none.clone();
        self.bind(name, move |arg, ctx| match f(arg, ctx) {
            Some(x) => Value::case(some, x),
            None => none.clone(),
        })
    }
}

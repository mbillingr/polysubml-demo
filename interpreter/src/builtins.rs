use crate::interpreter::Env;
use crate::value::Value;
use crate::vm;
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

    bb.bind_opt("__chars", |s, _| {
        let mut chars = s.as_str().chars().rev().collect::<Vec<_>>();
        chars.pop().map(|ch| Value::string(ch.to_string()))
    });

    bb.bind_opt("__split", |s, _| {
        let mut parts = s.as_str().split_whitespace().rev().map(str::to_string).collect::<Vec<_>>();
        parts.pop().map(Value::string)
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

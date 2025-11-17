#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum _Field { _0,_1,_2,and_then,f2i,f2s,i2f,i2s,lines,map,map_err,read_expected_line,read_line,s2f,s2i,unwrap,write_str, }
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum _Tag { Break,Continue,Eof,Err,Left,None,Ok,Right,Some, }
mod runtime; use runtime::*; 
use num::ToPrimitive;
fn main() {
let (panic, __read_line, __write_str, __chars, __split, __escape, __unescape, __int_to_float, __float_to_int, __str_to_int, __str_to_float, __int_to_str, __float_to_str, __vec_new, __vec_length, __vec_push_back, __vec_pop_back, __vec_peek_back, __vec_push_front, __vec_pop_front, __vec_peek_front, __vec_get, __vec_set, __vec_split, __dict_new, __dict_length, __dict_insert, __dict_contains, __dict_remove, __dict_get) = {
    fn optfn(f: impl Fn(Value) -> Option<Value> + 'static) -> Value {
        Value::func(move |arg| match f(arg) {
            Some(x) => Value::case(_Tag::Some, x),
            None => Value::case(_Tag::None, Value::nothing())
        })
    }

    let ok = _Tag::Ok;
    let err = _Tag::Err;

    let eof_ = Value::case(_Tag::Eof, Value::nothing());

    let idx0 = _Field::_0;
    let idx1 = _Field::_1;
    let idx2 = _Field::_2;

    let __panic = Value::func(move |msg| panic!("{}", msg));

    let eof = eof_.clone();
    let __read_line = Value::func(move |_| {
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

    let __write_str = Value::func(move |s| {
        print!("{}", s.as_str());
        Value::nothing()
    });

    let __chars = optfn(move |s| {
        let mut chars = s.as_str().chars().rev().collect::<Vec<_>>();
        chars.pop().map(|ch| Value::string(ch.to_string()))
    });

    let __split = optfn(move |s| {
        let mut parts = s.as_str().split_whitespace().rev().map(str::to_string).collect::<Vec<_>>();
        parts.pop().map(Value::string)
    });

    let __escape = Value::func(move |s| {
        Value::string(String::from_utf8(escape_bytes::escape(s.as_str().bytes())).unwrap())
    });

    let __unescape = Value::func(move |s| {
        Value::string(String::from_utf8(escape_bytes::unescape(s.as_str().bytes()).unwrap()).unwrap())
    });

    let __int_to_float = Value::func(move |x| Value::float(x.as_int().to_f64().unwrap()));
    let __float_to_int = Value::func(move |x| Value::int((x.as_float() as i64).into()));
    let __str_to_int = optfn(|x| x.as_str().parse::<_>().map(Value::int).ok());
    let __str_to_float = optfn(|x| x.as_str().parse::<_>().map(Value::float).ok());
    let __int_to_str = Value::func(move |x| Value::string(x.as_int().to_string()));
    let __float_to_str = Value::func(move |x| Value::string(x.as_float().to_string()));

    let __vec_new = Value::func(|_| Value::vect(vec![]));
    let __vec_length = Value::func(move |vec| Value::usize(vec.as_vect().len().into()));
    let __vec_push_back = Value::func(move |args| {
        let mut v = args.get_field(idx0).as_vect().clone();
        let x = args.get_field(idx1);
        v.push_back(x);
        Value::vect(v)
    });
    let __vec_pop_back = Value::func(move |vec| {
        let mut v = vec.as_vect().clone();
        v.pop_back();
        Value::vect(v)
    });
    let __vec_peek_back = optfn(|vec| vec.as_vect().clone().back().cloned());
    let __vec_push_front = Value::func(move |args| {
        let mut v = args.get_field(idx0).as_vect().clone();
        let x = args.get_field(idx1);
        v.push_front(x);
        Value::vect(v)
    });
    let __vec_pop_front = Value::func(move |vec| {
        let mut v = vec.as_vect().clone();
        v.pop_front();
        Value::vect(v)
    });
    let __vec_peek_front = optfn(|vec| vec.as_vect().clone().front().cloned());
    let __vec_get = optfn(move |args| {
        let v = args.get_field(idx0).as_vect().clone();
        let idx = args.get_field(idx1).as_int().to_usize();
        idx.and_then(|i| v.get(i).cloned())
    });
    let __vec_set = optfn(move |args| {
        let mut v = args.get_field(idx0).as_vect().clone();
        let idx = args.get_field(idx1).as_int().to_usize();
        let val = args.get_field(idx2);
        let x = idx.and_then(|i| v.get_mut(i));
        let x = x.map(|x| *x = val);
        x.map(|_| Value::vect(v))
    });
    let __vec_split = Value::func(move|args|{
        let mut v = args.get_field(idx0).as_vect().clone();
        let idx = args.get_field(idx1).as_int().to_usize().unwrap();
        let w = v.split_off(idx);
        Value::record(vec![(idx0, Value::vect(v), false), (idx1, Value::vect(w), false)])
    });

    let __dict_new = Value::func(|_| Value::dict(vec![]));
    let __dict_length = Value::func(move |dict| Value::usize(dict.as_dict().len().into()));
    let __dict_insert = Value::func(move |args| {
        let mut d = args.get_field(idx0).as_dict().clone();
        let k = args.get_field(idx1);
        let v = args.get_field(idx2);
        d.insert(k, v);
        Value::dict(d)
    });
    let __dict_contains = Value::func(move |args| {
        let d = args.get_field(idx0).as_dict().clone();
        let k = args.get_field(idx1);
        Value::bool(d.contains_key(&k))
    });
    let __dict_remove = Value::func(move |args| {
        let mut d = args.get_field(idx0).as_dict().clone();
        let k = args.get_field(idx1);
        d.remove(&k);
        Value::dict(d)
    });
    let __dict_get = optfn(move |args| {
        let d = args.get_field(idx0).as_dict().clone();
        let k = args.get_field(idx1);
        d.get(&k).cloned()
    });

    (__panic, __read_line, __write_str, __chars, __split, __escape, __unescape, __int_to_float, __float_to_int, __str_to_int, __str_to_float, __int_to_str, __float_to_str, __vec_new, __vec_length, __vec_push_back, __vec_pop_back, __vec_peek_back, __vec_push_front, __vec_pop_front, __vec_peek_front, __vec_get, __vec_set, __vec_split, __dict_new, __dict_length, __dict_insert, __dict_contains, __dict_remove, __dict_get)
};let _home_martin_jj_working_polysubml_demo_libs_io_ml = Value::record([(_Field::lines, Value::func({ let panic = panic.clone();; let __read_line = __read_line.clone(); move|arg| { let x = arg;
 {
let _val_0 = (__read_line.clone()).apply(x.clone());
match _val_0.as_case() {
(_Tag::Err, _val_0) => { let e = _val_0;
 (panic.clone()).apply(e.clone()) }
(_Tag::Eof, _val_0) => { let _ = _val_0;
 Value::case(_Tag::None, Value::record([])) }
(_Tag::Ok, _val_0) => { let l = _val_0;
 Value::case(_Tag::Some, l.clone()) }
_ => unreachable!()}} } }), false),(_Field::read_line, __read_line.clone(), false),(_Field::write_str, __write_str.clone(), false),(_Field::read_expected_line, Value::func({ let panic = panic.clone();; let __read_line = __read_line.clone(); move|arg| { let x = arg;
 {
let _val_1 = (__read_line.clone()).apply(x.clone());
match _val_1.as_case() {
(_Tag::Ok, _val_1) => { let l = _val_1;
 l.clone() }
_ => { let _ = _val_1;
 (panic.clone()).apply(Value::str("could not read line")) }}} } }), false),]);

let _home_martin_jj_working_polysubml_demo_libs_num_ml = Value::record([(_Field::i2f, __int_to_float.clone(), false),(_Field::f2i, __float_to_int.clone(), false),(_Field::s2i, __str_to_int.clone(), false),(_Field::s2f, __str_to_float.clone(), false),(_Field::i2s, __int_to_str.clone(), false),(_Field::f2s, __float_to_str.clone(), false),]);

let _home_martin_jj_working_polysubml_demo_libs_option_ml = {
let map = Value::cell(Value::nothing());
let map_err = Value::cell(Value::nothing());
let and_then = Value::cell(Value::nothing());
let unwrap = Value::cell(Value::nothing());
map.update_cell(Value::func({  move|arg| { let _rec_2 = arg;
let val = _rec_2.get_field(_Field::_1);
let f = _rec_2.get_field(_Field::_0);
 {
let _val_3 = val.clone();
match _val_3.as_case() {
(_Tag::Some, _val_3) => { let x = _val_3;
 Value::case(_Tag::Some, (f.clone()).apply(x.clone())) }
(_Tag::Ok, _val_3) => { let x = _val_3;
 Value::case(_Tag::Ok, (f.clone()).apply(x.clone())) }
(_Tag::Right, _val_3) => { let x = _val_3;
 Value::case(_Tag::Right, (f.clone()).apply(x.clone())) }
_ => { let other = _val_3;
 other.clone() }}} } }));
map_err.update_cell(Value::func({  move|arg| { let _rec_4 = arg;
let val = _rec_4.get_field(_Field::_1);
let f = _rec_4.get_field(_Field::_0);
 {
let _val_5 = val.clone();
match _val_5.as_case() {
(_Tag::None, _val_5) => { let x = _val_5;
 Value::case(_Tag::None, (f.clone()).apply(x.clone())) }
(_Tag::Err, _val_5) => { let x = _val_5;
 Value::case(_Tag::Err, (f.clone()).apply(x.clone())) }
(_Tag::Left, _val_5) => { let x = _val_5;
 Value::case(_Tag::Left, (f.clone()).apply(x.clone())) }
_ => { let other = _val_5;
 other.clone() }}} } }));
and_then.update_cell(Value::func({  move|arg| { let _rec_6 = arg;
let val = _rec_6.get_field(_Field::_1);
let f = _rec_6.get_field(_Field::_0);
 {
let _val_7 = val.clone();
match _val_7.as_case() {
(_Tag::Some, _val_7) => { let x = _val_7;
 (f.clone()).apply(x.clone()) }
(_Tag::Ok, _val_7) => { let x = _val_7;
 (f.clone()).apply(x.clone()) }
(_Tag::Right, _val_7) => { let x = _val_7;
 (f.clone()).apply(x.clone()) }
_ => { let other = _val_7;
 other.clone() }}} } }));
unwrap.update_cell(Value::func({ let panic = panic.clone(); move|arg| { let val = arg;
 {
let _val_8 = val.clone();
match _val_8.as_case() {
(_Tag::Some, _val_8) => { let x = _val_8;
 x.clone() }
(_Tag::Ok, _val_8) => { let x = _val_8;
 x.clone() }
(_Tag::Right, _val_8) => { let x = _val_8;
 x.clone() }
_ => { let _ = _val_8;
 (panic.clone()).apply(Value::str("unwrapped a fault value")) }}} } }));
Value::record([(_Field::map, map.clone(), false),(_Field::map_err, map_err.clone(), false),(_Field::unwrap, unwrap.clone(), false),(_Field::and_then, and_then.clone(), false),])
};

let io = _home_martin_jj_working_polysubml_demo_libs_io_ml.clone();

let num = _home_martin_jj_working_polysubml_demo_libs_num_ml.clone();

let option = _home_martin_jj_working_polysubml_demo_libs_option_ml.clone();

let fibonacci = Value::cell(Value::nothing());
fibonacci.update_cell(Value::func({ let fibonacci = fibonacci.clone(); move|arg| { let n = arg;
 if (Value::bool((n.clone()).as_int() < (Value::int_literal(2)).as_int())).as_bool() { Value::int_literal(1) } else { Value::int(((fibonacci.clone()).apply(Value::int((n.clone()).as_int() - (Value::int_literal(1)).as_int()))).as_int() + ((fibonacci.clone()).apply(Value::int((n.clone()).as_int() - (Value::int_literal(2)).as_int()))).as_int()) } } }));

let n = ((option.clone()).get_field(_Field::unwrap)).apply(((num.clone()).get_field(_Field::s2i)).apply(((io.clone()).get_field(_Field::read_expected_line)).apply(Value::record([]))));

let f = (fibonacci.clone()).apply(n.clone());

println!("{}", f.clone());

}
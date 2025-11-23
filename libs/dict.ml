import "iter.ml";

let type <<iter t>> = any -> [`Some t | `None any];

let empty: (dict@never*never) = __dict_new {};
let length = __dict_length;
let insert = __dict_insert;
let contains = __dict_contains;
let remove = __dict_remove;
let get = __dict_get;
let items = __dict_iter;

let insert_pair = fun (type k v) (d: (dict@k*v), (x:k, y:v)) : (dict@k*v) ->
    insert(d, x, y);

let update = fun (type k v) (d: (dict@k*v), it: <<iter (k*v)>>) : (dict@k*v) ->
    iter.fold(insert_pair[k=k; v=v], d, it);

let collect = fun (type k v) (it: <<iter (k*v)>>) : (dict@k*v) ->
    update(empty, it);

let keys = fun (type k) (d: (dict@k*any)) : (<<iter k>>) ->
    iter.map((fun(k,_)->k), items d);

let vals = fun (type v) (d: (dict@any*v)) : (<<iter v>>) ->
    iter.map((fun(_,v)->v), items d);

let map_keys = fun (type k1 k2 v) (f: k1 -> k2, d: (dict@k1*v)) : (dict@k2*v) ->
    collect iter.map_0(f, (items d));

let map_vals = fun (type k1 k2 v) (f: k1 -> k2, d: (dict@k1*v)) : (dict@k2*v) ->
    collect iter.map_0(f, (items d));

let union = fun (type k v) (d1: (dict@k*v), d2: (dict@k*v)) : (dict@k*v) ->
    iter.fold(insert_pair[k=k; v=v], d1, items d2);

let intersection = fun (type k v) (d1: (dict@k*v), d2: (dict@k*v)) : (dict@k*v) ->
    collect iter.filter_0((fun k -> contains(d1, k)), items d2);

let not = fun x -> if x then false else true;

let difference = fun (type k v) (d1: (dict@k*v), d2: (dict@k*any)) : (dict@k*v) ->
    collect iter.filter_0((fun k -> not contains(d2, k)), items d1);

let xor = fun (type k v) (d1: (dict@k*v), d2: (dict@k*v)) : (dict@k*v) ->
    collect iter.chain(
        iter.filter_0((fun k -> not contains(d2, k)), items d1),
        iter.filter_0((fun k -> not contains(d1, k)), items d2));

let symmetric_difference = xor;

{
    collect; contains; difference; empty; get; insert; insert_pair; intersection; items; keys; length;
    map_keys; map_vals; remove; symmetric_difference; union; vals; xor
}

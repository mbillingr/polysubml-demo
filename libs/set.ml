import "iter.ml";
import "dict.ml";

let type <<iter t>> = any -> [`Some t | `None any];
let type <<set t>> = dict@t*any;

let iterator = iter;

let empty: <<set never>> = __dict_new {};
let length = __dict_length;
let contains = __dict_contains;
let remove = __dict_remove;
let iter = dict.keys;
let map = dict.map_keys;
let union = dict.union;
let intersection = dict.intersection;
let difference = dict.difference;
let symmetric_difference = dict.symmetric_difference;
let xor = dict.xor;

let insert = fun (type a) (d: (<<set a>>), x: a) : (<<set a>>) ->
    dict.insert(d, x, {});

let update = fun (type a) (s: <<set a>>, it: <<iter a>>) : (<<set a>>) ->
    iterator.fold(insert[a=a], s, it);

let collect = fun (type a) (it: <<iter a>>) : (<<set a>>) ->
    update(empty, it);

let rec obj = fun (type a) (xs: <<set a>>) : (rec set_obj = {
    println : any -> any;
    clone: any -> set_obj;
    length: any -> int;
    contains: a -> bool;
    insert: a -> any;
    update: (<<iter a>>) -> any;
    remove: a -> any;
    iter: any -> <<iter a>>;
    pop: any -> [`Some a | `None any]
}) ->
    let state = {mut xs = xs} in
    {
        println  = fun _ -> (print state.xs; {});
        clone = fun _ -> obj state.xs;
        length = fun _ -> length state.xs;
        contains = fun x -> contains(state.xs, x);
        insert = fun x -> (state.xs <- insert(state.xs, x); {});
        update = fun it -> (state.xs <- update(state.xs, it); {});
        remove = fun x -> (state.xs <- remove(state.xs, x); {});
        iter = fun _ -> iter state.xs;
        pop = fun _ ->
            match (iter state.xs) {} with
                | `Some x -> (state.xs <- remove(state.xs, x); `Some x)
                | none -> none
    };

{
    collect; contains; difference; empty; insert; intersection; iter; length; map; obj; remove; symmetric_difference;
    union; update; xor
}

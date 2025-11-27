import "iter.ml";

let iterator = iter;

let type <<iter t>> = any -> [`Some t | `None any];

let empty: (vec@never) = __vec_new {};
let length = __vec_length;
let peek = __vec_get;
let peek_front = __vec_peek_front;
let peek_back = __vec_peek_back;
let push_front = __vec_push_front;
let push_back = __vec_push_back;
let pop_front = __vec_pop_front;
let pop_back = __vec_pop_back;
let split = __vec_split;
let iter = __vec_iter;
let iter_rev = __vec_iter_rev;

let is_empty = fun xs -> (length xs) == 0;

let front = fun (type a) (xs: (vec@a)) : a ->
  match peek_front xs with
    | `None _ -> panic "empty vec"
    | `Some x -> x;

let back = fun (type a) (xs: (vec@a)) : a ->
  match peek_back xs with
    | `None _ -> panic "empty vec"
    | `Some x -> x;

let get = fun (type a) (xs: (vec@a), i: int) : a ->
  match peek(xs, i) with
    | `None _ -> panic "index out of range"
    | `Some x -> x;

let foldl = fun (type a b) (f : (b*a)->b, init : b, lst : vec@a) : b ->
    iterator.fold(f, init, iter(lst));

let foldr = fun (type a b) (f : (a*b)->b, init : b, lst : vec@a) : b ->
    iterator.foldswap(f, init, iter_rev(lst));

let map = fun (type a b) (f : a->b, lst : vec@a) : (vec@b) ->
  foldl ((fun (xs, x) -> push_back(xs, f x)), empty, lst);

let filter = fun (type a) (f : a->bool, lst : vec@a) : (vec@a) ->
  foldl ((fun (xs, x) -> if f x then push_back(xs, x) else xs), empty, lst);

let append = fun (type a) (xs : vec@a, ys : vec@a) : (vec@a) ->
  foldl (push_back[a=a], xs, ys);

let reverse = fun (type a) (xs : vec@a) : (vec@a) ->
  foldl (push_front[a=a], empty, xs);

let merge_sorted = fun (type a) (cmp : ((a*a)->bool), xs : vec@a, ys : vec@a) : (vec@a) ->
    let vars = {mut xs; mut ys; mut acc=empty} in
    loop (
        let xo = peek_back vars.xs;
        let yo = peek_back vars.ys;
        let choice = match xo with
          | `None _ ->
            (match yo with
              | `None _ -> `Break vars.acc
              | `Some y -> `Y y)
          | `Some x ->
            (match yo with
              | `None _ -> `X x
              | `Some y -> (if cmp(y, x) then `X x else `Y y));
        match choice with
          | `X x -> (
            vars.acc <- push_front(vars.acc, x);
            vars.xs <- pop_back vars.xs;
            `Continue {}
          )
          | `Y y -> (
            vars.acc <- push_front(vars.acc, y);
            vars.ys <- pop_back vars.ys;
            `Continue {}
          )
          | other -> other
    );

let rec sort_by = fun (type a) (cmp : ((a*a)->bool), xs : vec@a) : (vec@a) ->
    if length xs < 2 then xs else
    let (lhs, rhs) = split(xs, (__vec_length xs) / 2) in
        merge_sorted(cmp, sort_by(cmp, lhs), sort_by(cmp, rhs));

let sort_int = fun (xs : vec@int) : (vec@int) ->
    sort_by((fun(a,b)->a<b), xs);

let collect_rev = fun (type a) (it: <<iter a>>) : (vec@a) ->
    iterator.fold(push_front[a=a], empty, it);

let collect = fun (type a) (it: <<iter a>>) : (vec@a) ->
    iterator.fold(push_back[a=a], empty, it);

let remove_at = fun (type a) (xs: (vec@a), i: int) : (vec@a) -> begin
  let ixs = iterator.enumerate(iter xs);
  let ixs_ = iterator.filter((fun (j, _) -> j != i), ixs);
  let xs_ = iterator.map((fun (_, x) -> x), ixs_);
  collect(xs_)
end;

{
    append; back; collect; collect_rev; empty; filter; foldl; foldr; front; get; is_empty; iter;
    iter_rev; length; map; merge_sorted; peek; peek_back; peek_front; pop_back; pop_front; push_back;
    push_front; remove_at; reverse; sort_by; sort_int; split
}

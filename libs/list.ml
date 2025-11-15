let type <<list t>> = vec@t;
let type <<iter t>> = any -> [`Some t | `None any];

let nil: (vec@never) = __vec_new {};

let rec is_empty =
          fun lst -> (__vec_length lst) == 0

    and cons = fun (type a) (hd:a, tl:(vec@a)) : (vec@a) ->
      __vec_push_front(tl, hd)

    and head = fun (type a) (lst : (vec@a)) : a ->
      match __vec_peek_front lst with
        | `None _ -> panic "empty list"
        | `Some h -> h

    and tail = fun (type a) (lst : (vec@a)) : (vec@a) ->
      if is_empty lst
      then panic "empty list"
      else __vec_pop_front lst

    and foldl = fun (type a b) (f : (b*a)->b, init : b, lst : <<list a>>) : b ->
      let vars = {mut acc=init; mut xs=lst} in
      loop
        match __vec_peek_front vars.xs with
          | `None _ -> `Break vars.acc
          | `Some x -> begin
              vars.acc <- f(vars.acc, x);
              vars.xs <- __vec_pop_front vars.xs;
              `Continue {}
            end

    and foldr = fun (type a b) (f : (a*b)->b, init : b, lst : <<list a>>) : b ->
      let vars = {mut acc=init; mut xs=lst} in
      loop
        match __vec_peek_back vars.xs with
          | `None _ -> `Break vars.acc
          | `Some x -> begin
              vars.acc <- f(x, vars.acc);
              vars.xs <- __vec_pop_back vars.xs;
              `Continue {}
            end

    and map = fun (type a b) (f : a->b, lst : <<list a>>) : (<<list b>>) ->
      foldr ((fun (h, t) -> cons (f h, t)), nil, lst)

    and filter = fun (type a) (f : a->bool, lst : <<list a>>) : (<<list a>>) ->
      foldr ((fun (h, t) -> if f h then cons (h, t) else t), nil, lst)

    and append = fun (type a) (xs : <<list a>>, ys : <<list a>>) : (<<list a>>) ->
      foldr (cons[t=a], ys, xs)

    and reverse = fun (type a) (xs : <<list a>>) : (<<list a>>) ->
      foldl ((fun (rs, r) -> cons (r, rs)), nil, xs)

    and merge_sorted = fun (type a) (cmp : ((a*a)->bool), xs : <<list a>>, ys : <<list a>>) : (<<list a>>) ->
        let vars = {mut xs; mut ys; mut acc=nil} in
        loop (
            let xo = __vec_peek_back vars.xs;
            let yo = __vec_peek_back vars.ys;
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
                vars.acc <- cons(x, vars.acc);
                vars.xs <- __vec_pop_back vars.xs;
                `Continue {}
              )
              | `Y y -> (
                vars.acc <- cons(y, vars.acc);
                vars.ys <- __vec_pop_back vars.ys;
                `Continue {}
              )
              | other -> other
        )

    and sort = fun (type a) (cmp : ((a*a)->bool), xs : <<list a>>) : (<<list a>>) ->
        if __vec_length xs < 2 then xs else
        let (lhs, rhs) = __vec_split(xs, (__vec_length xs) / 2) in
            merge_sorted(cmp, sort(cmp, lhs), sort(cmp, rhs))

    and collect_rev = fun (type a) (it: <<iter a>>) : (<<list a>>) -> (
      let vars = {mut out = nil};
      loop
        match it {} with
          | `None _ -> `Break vars.out
          | `Some x -> (
                vars.out <- cons (x, vars.out);
                `Continue {}
          ))

    and collect = fun (type a)
      (it: <<iter a>>
      ) : (<<list a>>) ->
      reverse collect_rev it
;

{
  nil; is_empty; cons; head; foldl; foldr; map; filter; append; reverse; sort; merge_sorted; collect_rev; collect
}

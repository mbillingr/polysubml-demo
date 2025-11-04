// The list type is somewhat like
//    type t. rec list = [`Some t * list[t] | `None any]

let nil = `None {} in
let rec is_empty =
          fun lst -> match lst with
            | `None _ -> true
            | `Some _ -> false
          
    and cons = fun (type t u) (hd:t, tl:u) : [`Some t * u] ->
      `Some (hd, tl)
    
    and head = fun (type a) (lst : rec list = [`Some a * list | `None any]) : a ->
      match lst with
        | `None _ -> panic "empty list"
        | `Some (h, _) -> h

    and foldl = fun (type a b)
      (f : (b*a)->b,
       init : b,
       lst : rec list = [`Some a * list | `None any]
      ) : b ->
      let rec inner = fun (acc, lst) ->
        match lst with
          | `None _ -> acc
          | `Some (x, xs) -> inner (f (acc, x), xs)
      in inner (init, lst)

    and foldr = fun (type a b)
      (f : (a*b)->b,
       init : b,
       lst : rec list = [`Some a * list | `None any]
      ) : b ->
      let rec inner = fun lst ->
        match lst with
          | `None _ -> init
          | `Some (x, xs) -> f (x, inner xs)
      in inner lst

    and map = fun (type a b)
      (f : a->b,
       lst : rec list_a = [`Some a * list_a | `None any]
      ) : (rec list_b = [`Some b * list_b | `None any]) ->
      foldr ((fun (h, t) -> cons (f h, t)), nil, lst)

    and filter = fun (type a)
      (f : a->bool,
       lst : rec list = [`Some a * list | `None any]
      ) : (rec list = [`Some a * list | `None any]) ->
      foldr ((fun (h, t) -> if f h then cons (h, t) else t), nil, lst)

    and append = fun (type a)
      (xs : rec list = [`Some a * list | `None any],
       ys : rec list = [`Some a * list | `None any]
      ) : (rec list = [`Some a * list | `None any]) ->
      foldr (cons[t=a], ys, xs)

    and reverse = fun (type a)
      (xs : rec list = [`Some a * list | `None any]
      ) : (rec list = [`Some a * list | `None any]) ->
      foldl ((fun (rs, r) -> cons (r, rs)), nil, xs)

    and collect_rev = fun (type a)
      (it: any -> [`Some a | `None any]
      ) : (rec list = [`Some a * list | `None any]) -> (
      let vars = {mut out = nil};
      loop
        match it {} with
          | `None _ -> `Break vars.out
          | `Some x -> (
                vars.out <- cons (x, vars.out);
                `Continue {}
          ))

    and collect = fun (type a)
      (it: any -> [`Some a | `None any]
      ) : (rec list = [`Some a * list | `None any]) ->
      reverse collect_rev it
in {
  nil; is_empty; cons; head; foldl; foldr; map; filter; append; reverse; collect_rev; collect
}

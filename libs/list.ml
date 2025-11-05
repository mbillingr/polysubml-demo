let type <<list t>> = rec list = [`Some t * list | `None any];
let type <<iter t>> = any -> [`Some t | `None any];

let nil: <<list never>> = `None {};

let rec is_empty =
          fun lst -> match lst with
            | `None _ -> true
            | `Some _ -> false

    and cons = fun (type t u) (hd:t, tl:u) : [`Some t * u] ->
      `Some (hd, tl)

    and head = fun (type a) (lst : <<list a>>) : a ->
      match lst with
        | `None _ -> panic "empty list"
        | `Some (h, _) -> h

    and tail = fun (type a) (lst : <<list a>>) : (<<list a>>) ->
      match lst with
        | `None _ -> panic "empty list"
        | `Some (_, t) -> t

    and foldl = fun (type a b) (f : (b*a)->b, init : b, lst : <<list a>>) : b ->
      let rec inner = fun (acc, lst) ->
        match lst with
          | `None _ -> acc
          | `Some (x, xs) -> inner (f (acc, x), xs)
      in inner (init, lst)

    and foldr = fun (type a b) (f : (a*b)->b, init : b, lst : <<list a>> ) : b ->
      let rec inner = fun lst ->
        match lst with
          | `None _ -> init
          | `Some (x, xs) -> f (x, inner xs)
      in inner lst

    and map = fun (type a b) (f : a->b, lst : <<list a>>) : (<<list b>>) ->
      foldr ((fun (h, t) -> cons (f h, t)), nil, lst)

    and filter = fun (type a) (f : a->bool, lst : <<list a>>) : (<<list a>>) ->
      foldr ((fun (h, t) -> if f h then cons (h, t) else t), nil, lst)

    and append = fun (type a) (xs : <<list a>>, ys : <<list a>>) : (<<list a>>) ->
      foldr (cons[t=a], ys, xs)

    and reverse = fun (type a) (xs : <<list a>>) : (<<list a>>) ->
      foldl ((fun (rs, r) -> cons (r, rs)), nil, xs)

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
  nil; is_empty; cons; head; foldl; foldr; map; filter; append; reverse; collect_rev; collect
}

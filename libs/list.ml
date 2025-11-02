// The list type is somewhat like
//    type t. rec list = [`Some t * list[t] | `None any]

let nil = `None {} in
let rec is_empty =
          fun lst -> match lst with
            | `None _ -> true
            | `Some _ -> false
          
    and cons = fun (type t u) (hd:t, tl:u) : [`Some t * u] ->
      `Some (hd, tl)
    
    and head = fun (type t) (lst:[`Some t * any]) : t ->
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

    and append = fun (type a)
      (xs : rec list = [`Some a * list | `None any],
       ys : rec list = [`Some a * list | `None any]
      ) : (rec list = [`Some a * list | `None any]) ->
      foldr (cons[t=a], ys, xs)
in {
  nil; is_empty; cons; head; foldl; foldr; map; append
}

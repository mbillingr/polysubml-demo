// provide functions for working with
//    option (`None | `Some)
//    result (`Ok | `Err)
//    either (`Left | `Right)
//
// I'm not sure if it is a good idea to combine these types... we'll see :)

let is_positive = fun (val : [`Some any | `None any | `Ok any | `Err any | `Right any | `Left any]): bool ->
      match val with
        | `Some _ -> true
        | `Ok _ -> true
        | `Right _ -> true
        | _ -> false;

let map = fun (type a b c)
      (f : a -> b,
       val : [`Some a | `None c | `Ok a | `Err c | `Right a | `Left c]
         ) : [`Some b | `None c | `Ok b | `Err c | `Right b | `Left c] ->
      match val with
        | `Some x -> `Some f x
        | `Ok x -> `Ok f x
        | `Right x -> `Right f x
        | other -> other;
    
let map_err = fun (type a b c)
  (f : a -> b,
   val : [`Some c | `None a | `Ok c | `Err a | `Right c | `Left a]
     ) : [`Some c | `None b | `Ok c | `Err b | `Right c | `Left b] ->
  match val with
    | `None x -> `None f x
    | `Err x -> `Err f x
    | `Left x -> `Left f x
    | other -> other;

let and_then = fun (type a b c)
  (f : a -> [`Some b | `None c | `Ok b | `Err c | `Right b | `Left c],
   val : [`Some a | `None c | `Ok a | `Err c | `Right a | `Left c]
     ) : [`Some b | `None c | `Ok b | `Err c | `Right b | `Left c] ->
  match val with
    | `Some x -> f x
    | `Ok x -> f x
    | `Right x -> f x
    | other -> other;

let unwrap = fun (type a)
  (val : [`Some a | `None any | `Ok a | `Err any | `Right a | `Left any]
  ): a ->
  match val with
    | `Some x -> x
    | `Ok x -> x
    | `Right x -> x
    | _ -> panic "unwrapped a fault value";

let unwrap_or = fun (type a) (default: a): ([`Some a | `None any | `Ok a | `Err any | `Right a | `Left any] -> a) ->
  fun val ->
    match val with
      | `Some x -> x
      | `Ok x -> x
      | `Right x -> x
      | _ -> default;

{
  and_then; is_positive; map; map_err; unwrap; unwrap_or
}

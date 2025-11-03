(*
    An iterator is a function that ignores its argument and returns the next value on each call.
    Any interesting use will need to keep mutable state in the iterator's closure.
*)
let
    rec iter_stream = fun seq -> (
        let vars = {mut seq};
        fun _ ->
            match (vars.seq) {} with
                | `None _ -> `None {}
                | `Some (x, xs) -> (
                    vars.seq <- xs;
                    `Some x))

    and for_each = fun (f, it) -> (
        loop
            match it {} with
                | `None _ -> `Break {}
                | `Some x -> (
                    f x;
                    `Continue {}))

    and map = fun (f, it) ->
        fun _ ->
            match it {} with
                | `Some x -> `Some f x
                | none -> none

in {
    for_each; map
}
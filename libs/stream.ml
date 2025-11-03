(*
    A stream is like a lazy, possibly infinite list.
    Like an iterator, a stream is a function. Unlike iterators, when called it returns the function yielding the
    subsequent value along with the next value. This is more cumbersome to use but requires no mutable state.
*)
let rec for_each = fun (f, seq) -> (
    let vars = {mut seq};
    loop
        match (vars.seq) {} with
            | `None _ -> `Break {}
            | `Some (x, xs) -> (
                f x;
                vars.seq <- xs;
                `Continue {}))

in {
    for_each
}
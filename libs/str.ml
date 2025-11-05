{
    chars = __chars
    ; split = __split
    ; escape = __escape
    ; unescape = __unescape

    ; join = fun (sep, it) ->
        let vars = {mut out = ""}
        in loop
            match it {} with
                | `None _ -> `Break vars.out
                | `Some s -> `Continue (if vars.out == "" then vars.out <- s else vars.out <- vars.out ^ sep ^ s)
}
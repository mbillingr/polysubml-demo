import "str.ml";

{
    (* Since an iterator is a function that returns some optional value on each call,
       read_line can almost serve as an iterator whose state is implicit in stdin.
       We just have to make sure we return no variant other than Some or None.
    *)
    lines = fun x ->
        match __read_line x with
            | `Err e -> panic e
            | `Eof _ -> `None {}
            | `Ok l -> `Some l

    ; read_line = __read_line
    ; write_str = __write_str
    ; write_line = fun x -> __write_str(x ^ str.unescape("\n"))

    ; read_expected_line = fun x ->
        match __read_line x with
            | `Ok l -> l
            | _ -> panic "could not read line"
}

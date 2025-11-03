
{
    (* Since an iterator is a function that returns some optional value on each call,
       read_line can almost serve as an iterator whose state is implicit in stdin.
       We just have to make sure we return no variant other than Some or None.
    *)
    lines = fun x ->
        match read_line x with
            | `Err e -> panic e
            | `Eof _ -> `None {}
            | `Ok l -> `Some l
}

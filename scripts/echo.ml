loop
    match read_line {} with
      | `Ok line -> (print line; `Continue 0)
      | `Eof _ -> `Break 0
      | `Error e -> panic e
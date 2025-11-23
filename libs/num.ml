{
    i2f = __int_to_float
    ; f2i = __float_to_int
    ; s2i = __str_to_int
    ; s2f = __str_to_float
    ; i2s = __int_to_str
    ; f2s = __float_to_str

    ; isqrt = fun n ->
        if n < 1 then
            0
        else let state = {mut prev2=-1; mut prev1=1}
        in loop (
            let {prev2; prev1} = state;
            let x1 = (prev1 + n / prev1) / 2;
            if x1 == prev1 then
                `Break x1
            else if x1 == prev2 && x1 != prev1 then
                `Break (if x1 < prev1 then x1 else prev1)
            else (
                state.prev2 <- prev1;
                state.prev1 <- x1;
            	`Continue {}
            ))


}
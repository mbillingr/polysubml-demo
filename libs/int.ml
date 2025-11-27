{
    to_float = __int_to_float; 
    from_float = __float_to_int; 
    to_str = __int_to_str; 
    from_str = __str_to_int; 
    
    add = fun(a, b) -> a + b; 
    sub = fun(a, b) -> a - b; 
    mul = fun(a, b) -> a * b; 
    div = fun(a, b) -> a / b; 
    mod = fun(a, b) -> a % b; 
    
    abs = fun n -> if n < 0 then 0 - n else n; 

    sign = fun n -> if n < 0 then -1 else if n > 0 then 1 else 0;

    min = fun (a, b) -> if a < b then a else b;
    max = fun (a, b) -> if a > b then a else b;
    
    sqrt = fun n ->
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
{
    to_int = __float_to_int; 
    from_int = __int_to_float; 
    to_str = __float_to_str; 
    from_str = __str_to_float; 
    
    add = fun(a, b) -> a +. b; 
    sub = fun(a, b) -> a -. b; 
    mul = fun(a, b) -> a *. b; 
    div = fun(a, b) -> a /. b; 
    mod = fun(a, b) -> a %. b; 
    
    abs = fun n -> if n <. 0.0 then 0.0 -. n else n
}
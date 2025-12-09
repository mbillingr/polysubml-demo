import "dict.ml";
import "iter.ml";
import "option.ml";

let add = fun(a, b) -> a + b;
let div = fun(a, b) -> a / b;

let min = fun (a, b) -> if a < b then a else b;
let max = fun (a, b) -> if a > b then a else b;

let sum = fun (it) -> iter.fold(add, 0, it);

let log = fun(base: int, n: int) ->
    let vars = {mut n=1; mut lg=0} in
        loop
            if vars.n > n
                then `Break (vars.lg - 1)
                else `Continue (
                    vars.n <- vars.n * base;
                    vars.lg <- vars.lg + 1
                );

let pow = fun(base: int, n:int) ->
    let vars = {mut out=1; mut n=n} in
        loop
            if vars.n <= 0
                then `Break vars.out
                else `Continue (vars.out <- vars.out * base; vars.n <- vars.n - 1);

// compute number of ways an integer n can be represented as the sum of k integers 
// (0+1 is considered different from 1+0, so sum_permutations{n=1; k=2} == 2)
let sum_permutations = 
    let vars = {mut memo = dict.empty} in
        let rec recursive = fun {n; k} ->
            if k == 1 then
                1
            else match dict.get(vars.memo, {n; k}) with
                | `Some x -> x
                | `None _ -> begin
                        let x = sum iter.map((fun a -> recursive{n=n-a; k=k - 1}), iter.range(0, n+1));
                        vars.memo <- dict.insert(vars.memo, {n; k}, x);
                        x
                    end
        in
        recursive;


{
    to_float = __int_to_float; 
    from_float = __float_to_int; 
    to_str = __int_to_str; 
    from_str = __str_to_int; 
    
    add = add; 
    sub = fun(a, b) -> a - b; 
    mul = fun(a, b) -> a * b; 
    div = div;
    mod = fun(a, b) -> a % b;

    floor_div = div;
    ceil_div = fun (a, b) -> (a + b - 1) / b;
    
    abs = fun n -> if n < 0 then 0 - n else n; 

    sign = fun n -> if n < 0 then -1 else if n > 0 then 1 else 0;

    min;
    max;

    // expects input to be enumerated
    argmax = fun it -> begin
        let fst = (-1, -9999999999999999999999999);
        let (i, _) = iter.fold((fun (mx, (i, x)) -> if x > mx._1 then (i, x) else mx), fst, it);
        i
    end;

    // expects input to be enumerated
    argmin = fun it -> begin
        let fst = (-1, 9999999999999999999999999);
        let (i, _) = iter.fold((fun (mx, (i, x)) -> if x < mx._1 then (i, x) else mx), fst, it);
        i
    end;

    sum;
    
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
            ));

    log = log;
    pow = pow;

    // repeat a number n times
    repeat = fun(d: int, n: int) ->
        let factor = pow(10, log(10, d) + 1) in
        let vars = {mut x=0; mut n=n} in
            loop
                if vars.n <= 0
                    then `Break vars.x
                    else `Continue (vars.x <- vars.x * factor + d; vars.n <- vars.n - 1);

    round_to_multiple = fun n -> fun k -> begin
        let floored = (k / n) * n;
        let remainder = k - floored;
        let rounding = ((remainder * 2) / n) * n;
        floored + rounding
    end;

    gcd = fun(a, b) -> 
        let vars ={mut a; mut b} in
            loop
                if vars.b == 0 then 
                    `Break vars.a
                else begin
                    let t = vars.b;
                    vars.b <- vars.a % vars.b;
                    vars.a <- t;
                    `Continue {}                    
                end;


    sum_permutations
}
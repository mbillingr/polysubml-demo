
let r = 3.99;
let vars = {mut x=0.5; mut n=10000000};

loop
    if (vars.n <- vars.n - 1) <= 0 then
        `Break ()
    else (
        vars.x <- r *. vars.x *. (1.0 -. vars.x);
        `Continue ());

print vars.x

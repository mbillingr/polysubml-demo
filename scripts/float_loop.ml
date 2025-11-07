import "libs/io.ml";
import "libs/iter.ml";
import "libs/num.ml";
import "libs/option.ml";

let r = 3.99;
let vars = {mut x=0.5; mut n=10000000};

loop
    if (vars.n <- vars.n - 1) <= 0 then
        `Break 0
    else (
        vars.x <- r *. vars.x *. (1.0 -. vars.x);
        `Continue 0);

print vars.x
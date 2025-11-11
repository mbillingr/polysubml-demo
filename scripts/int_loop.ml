
let r = 7;
let vars = {mut x=5; mut n=10000000};

loop
    if (vars.n <- vars.n - 1) <= 0 then
        `Break 0
    else (
        vars.x <- (r * vars.x + 9) % 750317;
        `Continue 0);

print vars.x
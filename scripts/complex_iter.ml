import "libs/iter.ml";
import "libs/num.ml";

let add = fun(a, b) -> a + b;

let ints = iter.range(1, 1000001);
let sqrs = iter.map(num.isqrt, ints);
let sum = iter.fold(add, 0, sqrs);
print(sum)
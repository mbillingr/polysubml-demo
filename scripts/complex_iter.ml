import "libs/iter.ml";

let add = fun(a, b) -> a + b;
let sqr = fun x -> x * x;
let odd = fun x -> x % 2 == 1;

let ints1 = iter.range(1, 1000001);
let ints2 = iter.range(8000001, 9000001);
let ints = iter.chain(ints1, ints2);
let squares = iter.map(sqr, ints);
let odd_squares = iter.filter(odd, squares);
let sum = iter.fold(add, 0, odd_squares);
print(sum)

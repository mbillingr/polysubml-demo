import "libs/io.ml";
import "libs/num.ml";
import "libs/option.ml";


let rec fibonacci = fun (n: int): int ->
    if n < 2 then
        1
    else
        fibonacci (n - 1) + fibonacci (n - 2);


let n = option.unwrap num.s2i io.read_expected_line {};
let f = fibonacci(n);
print f

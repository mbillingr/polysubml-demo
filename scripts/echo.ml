import "libs/io.ml";
import "libs/iter.ml";

iter.for_each ((fun x -> (print x; {})), io.lines)

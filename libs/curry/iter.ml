
import "../iter.ml";

let type <<iter t>> = any -> [`Some t | `None any];

{
    enumerate = iter.enumerate;

    filter = fun (type a) (f: a -> bool) :((<<iter a>>) -> (<<iter a>>)) ->
        fun it -> iter.filter(f, it);

    find_idx = iter.find_idx;    

    flatten = iter.flatten;

    fold = fun (type a b) (f: (b*a)->b): (b -> ((<<iter a>>) -> b)) ->
        fun init -> fun it -> iter.fold(f, init, it);

    for_each = fun (type a) (f: a -> any): ((<<iter a>>) -> any) ->
        fun it -> iter.for_each(f, it);

    map = fun (type a b) (f: a -> b) : ((<<iter a>>) -> (<<iter b>>)) ->
        fun it -> iter.map(f, it);

    range = iter.range;
    range_inf = iter.range_inf;
    sliding_pair = iter.sliding_pair;

    take = fun (type a) (n: int): ((<<iter a>>) -> (<<iter a>>)) ->
        fun it -> iter.take(n, it);

    take_while = fun (type a) (f: a -> bool) : ((<<iter a>>) -> (<<iter a>>)) ->
        fun it -> iter.take_while(f, it)
}

import "../iter.ml";

let type <<iter t>> = any -> [`Some t | `None any];

{
    all = fun (type a) (f: a -> bool) : ((<<iter a>>) -> bool) ->
        fun it -> iter.all(f, it);

    any = fun (type a) (f: a -> bool) : ((<<iter a>>) -> bool) ->
        fun it -> iter.any(f, it);

    chain = iter.chain;

    enumerate = iter.enumerate;

    filter = fun (type a) (f: a -> bool) :((<<iter a>>) -> (<<iter a>>)) ->
        fun it -> iter.filter(f, it);

    filter_good = iter.filter_good;

    find_idx = iter.find_idx;    

    flatten = iter.flatten;

    fold = fun (type a b) (f: (b*a)->b): (b -> ((<<iter a>>) -> b)) ->
        fun init -> fun it -> iter.fold(f, init, it);

    fold1 = fun (type a) (f: (a*a)->a): ((<<iter a>>) -> a) ->
        fun it -> iter.fold1(f, it);

    for_each = fun (type a) (f: a -> any): ((<<iter a>>) -> any) ->
        fun it -> iter.for_each(f, it);

    map = fun (type a b) (f: a -> b) : ((<<iter a>>) -> (<<iter b>>)) ->
        fun it -> iter.map(f, it);

    once = iter.once;
    range = iter.range;
    range_inf = iter.range_inf;
    repeat = iter.repeat;

    select_by = fun (type a) (f: (a*a)->bool) : ((<<iter a>>) -> a) ->    
        fun it -> iter.select_by(f, it);

    skip = fun (type a) (n: int) : ((<<iter a>>) -> (<<iter a>>)) ->
        fun it -> iter.skip(n, it);

    sliding_pair = iter.sliding_pair;

    take = fun (type a) (n: int): ((<<iter a>>) -> (<<iter a>>)) ->
        fun it -> iter.take(n, it);

    take_while = fun (type a) (f: a -> bool) : ((<<iter a>>) -> (<<iter a>>)) ->
        fun it -> iter.take_while(f, it);

    zip = iter.zip
}
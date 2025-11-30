(*
    An iterator is a function that ignores its argument and returns the next value on each call.
    Any interesting use will need to keep mutable state in the iterator's closure.
*)

let type <<iter t>> = any -> [`Some t | `None any];

let range = fun (start: int, stop: int): (<<iter int>>) ->
    let state = {mut x = start} in
        fun _ ->
            if state.x >= stop then
                `None {}
            else
                `Some (state.x <- state.x + 1);

let range_inf = fun (start: int): (<<iter int>>) ->
    let state = {mut x = start} in
        fun _ -> `Some (state.x <- state.x + 1);

let fold = fun (type a b) (f: (b*a)->b, init: b, it: <<iter a>>) : b ->
    let vars = {mut acc=init} in
        loop
            match it {} with
                | `None _ -> `Break vars.acc
                | `Some x -> `Continue (vars.acc <- f(vars.acc, x));

// same as fold but function arguments swapped
let foldswap = fun (type a b) (f: (a*b)->b, init: b, it: <<iter a>>) : b ->
    let vars = {mut acc=init} in
        loop
            match it {} with
                | `None _ -> `Break vars.acc
                | `Some x -> `Continue (vars.acc <- f(x, vars.acc));

let for_each = fun (type a) (f: a -> any, it: <<iter a>>): any ->
    loop
        match it {} with
            | `None _ -> `Break {}
            | `Some x -> `Continue (f x);

let count = fun (it: <<iter any>>): int ->
    fold((fun (acc, _) -> acc + 1), 0, it);

let map = fun (type a b) (f: a -> b, it: <<iter a>>): (<<iter b>>) ->
    fun _ ->
        match it {} with
            | `Some x -> `Some f x
            | `None _ -> `None {};

let map_0 = fun (type a b c) (f: a -> b, it: <<iter (a*c)>>): (<<iter (b*c)>>) ->
    fun _ ->
        match it {} with
            | `Some (x, y) -> `Some ((f x), y)
            | `None _ -> `None {};

let map_1 = fun (type a b c) (f: a -> b, it: <<iter (c*a)>>): (<<iter (c*b)>>) ->
    fun _ ->
        match it {} with
            | `Some (x, y) -> `Some (x, (f y))
            | `None _ -> `None {};

let map_inner = fun (type a b) (f: a -> b, it: <<iter <<iter a>>>>): (<<iter <<iter b>>>>) ->
    map((fun inner -> map(f, inner)), it);

let filter = fun (type a) (f: a -> bool, it: <<iter a>>): (<<iter a>>) ->
    fun z ->
        loop
            match it {} with
                | `Some x -> (if f x then
                        `Break `Some x
                    else
                        `Continue z)
                | `None _ -> `Break `None {};

let filter_0 = fun (type a b) (f: a -> bool, it: <<iter (a*b)>>): (<<iter (a*b)>>) ->
    filter((fun(x,_)->f x), it);

let filter_1 = fun (type a b) (f: b -> bool, it: <<iter (a*b)>>): (<<iter (a*b)>>) ->
    filter((fun(_,y)->f y), it);

let any = fun (type a) (f: a -> bool, it: <<iter a>>) : bool ->
    loop
        match it {} with
            | `Some x -> (if f x then `Break true else `Continue {})
            | `None _ -> `Break false;

let all = fun (type a) (f: a -> bool, it: <<iter a>>) : bool ->
    loop
        match it {} with
            | `Some x -> (if f x then `Continue {} else `Break false)
            | `None _ -> `Break true;

let skip = fun (type a) (n: int, it: <<iter a>>) : (<<iter a>>) ->
    let state = {mut action=it; mut k=n} in
    let skipn = fun z ->
        loop
            if state.k > 0 then
                `Continue (state.k <- state.k - 1; it z)
            else (
                state.action <- it;
                `Break (it z))
    in (
        state.action <- skipn;
        fun z -> state.action z
    );

let take = fun (type a) (n: int, it: <<iter a>>) : (<<iter a>>) ->
    let state = {mut k=n}
    in fun z ->
        if state.k > 0 then (
            state.k <- state.k - 1;
            it z)
        else
            `None z;

let zip = fun (type a b) (ia: <<iter a>>, ib: <<iter b>>) : (<<iter (a*b)>>) ->
    fun _ ->
        match ia {} with
            | `Some x -> (match ib {} with
                | `Some y -> `Some (x, y)
                | `None _ -> `None {})
            | `None _ -> `None {};

let chain = fun (type a) (ia: <<iter a>>, ib: <<iter a>>) : (<<iter a>>) ->
    let state = {mut it=ia; mut next=`Some ib}
    in fun _ ->
        match state.it {} with
            | `None _ -> (match state.next with
                | `Some it -> (state.it <- it; it {})
                | `None _ -> `None {})
            | `Some x -> `Some x;

let enumerate = fun (type a) (it: <<iter a>>) : (<<iter (int*a)>>) ->
    zip(range_inf(0), it);

let sliding_pair = fun (type a) (it: <<iter a>>) : (<<iter (a*a)>>) ->
    let state = {mut prev = `None {}} in
    fun _ ->
        match it {} with
            | `None _ -> `None {}
            | `Some x -> (
                match state.prev with
                    | `None _ -> (
                        match it {} with
                            | `None _ -> `None {}
                            | `Some y -> (
                                state.prev <- `Some y;
                                `Some (x, y)))
                    | `Some y -> (state.prev <- `Some x;
                        `Some (y, x))                    
            );

{
    all; any; chain; count; enumerate; filter; filter_0; filter_1; fold; foldswap; for_each; map; map_0; map_1; 
    map_inner; range; range_inf; skip; sliding_pair; take; zip
}
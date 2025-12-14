
import "option.ml";

(*
    An iterator is a function that ignores its argument and returns the next value on each call.
    Any interesting use will need to keep mutable state in the iterator's closure.
*)

let type <<iter t>> = any -> [`Some t | `None any];

let empty = fun _ -> `None {};

let once = fun (type a) (x: a): (<<iter a>>) ->
    let state = {mut x = `Some x} in 
        fun _ -> state.x <- `None {};

let range = fun (start: int, stop: int): (<<iter int>>) ->
    let dx = if stop < start then -1 else 1 in
    let state = {mut x = start} in
        fun _ ->
            if state.x == stop then
                `None {}
            else
                `Some (state.x <- state.x + dx);

let range_inf = fun (start: int): (<<iter int>>) ->
    let state = {mut x = start} in
        fun _ -> `Some (state.x <- state.x + 1);

let repeat = fun (type a) (x: a, n: int): (<<iter a>>) ->
    let state = {mut i = n} in
        fun _ ->
            if state.i <= 0 then
                `None {}
            else
                (state.i <- state.i - 1; `Some x);

let fold = fun (type a b) (f: (b*a)->b, init: b, it: <<iter a>>) : b ->
    let vars = {mut acc=init} in
        loop
            match it {} with
                | `None _ -> `Break vars.acc
                | `Some x -> `Continue (vars.acc <- f(vars.acc, x));

let fold1 = fun (type a) (f: (a*a)->a, it: <<iter a>>) : a ->
    let fst = option.unwrap it {} in
        fold(f, fst, it);

let select_by = fun (type a) (f: (a*a)->bool, it: <<iter a>>) : a ->    
    fold1((fun (x,y) -> if f(y, x) then y else x), it);

let scan = fun (type a b) (f: (b*a)->b, init: b, it: <<iter a>>) : (<<iter b>>) ->
    let vars = {mut acc=init} in
        fun _ ->
            match it {} with
                | `None _ -> `None {}
                | `Some x -> (vars.acc <- f(vars.acc, x); `Some vars.acc);

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

let filter_good = fun (type a) (it: <<iter [`Some a | `None any]>>): (<<iter a>>) ->
    map(option.unwrap[a=a], filter(option.is_positive, it));

let take_while = fun (type a) (f: a -> bool, it: <<iter a>>): (<<iter a>>) ->
    let state = {mut take=true} in
    fun z ->
        if state.take
            then match it {} with
                | `Some x -> (if f x
                    then `Some x
                    else (
                        state.take <- false;
                        `None {}
                    ))
                | `None _ -> `None {}
            else `None {};

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

let zip4 = fun (type a b c d) (ia: <<iter a>>, ib: <<iter b>>, ic: <<iter c>>, id: <<iter d>>) : (<<iter (a*b*c*d)>>) ->
    map((fun ((a, b), (c, d)) -> (a, b, c, d)), zip(zip(ia, ib), zip(ic, id)));

let flatten = fun (type a) (it: <<iter <<iter a>>>>) : (<<iter a>>) ->
    let state = {mut inner=`None {}; mut outer=it}
    in fun _ ->
        loop
            match state.inner with
                | `Some inner_it -> (
                    match inner_it {} with
                        | `Some x -> `Break `Some x
                        | `None _ -> (state.inner <- `None {}; `Continue {}))
                | `None _ -> (
                    match state.outer {} with
                        | `Some new_inner -> (state.inner <- `Some new_inner; `Continue {})
                        | `None _ -> `Break `None {});

let chain = fun (type a) (ia: <<iter a>>, ib: <<iter a>>) : (<<iter a>>) ->
    let state = {mut it=ia; mut next=`Some ib}
    in fun _ ->
        match state.it {} with
            | `None _ -> (match state.next with
                | `Some it -> (state.it <- it; it {})
                | `None _ -> `None {})
            | `Some x -> `Some x;

// alternatingly yield results from two iterators
let splice = fun (type a) (ia: <<iter a>>, ib: <<iter a>>) : (<<iter a>>) ->
    let state = {mut ia=ia; mut ib=ib}
    in fun _ ->
        match state.ia {} with
            | `None _ -> state.ib {}
            | `Some x -> begin 
                let tmp = state.ia;
                state.ia <- state.ib;
                state.ib <- state.ia;
                `Some x 
            end;

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

let find_idx = fun (it: <<iter bool>>): int ->
    count take_while((fun b -> b), it);

{
    all; any; chain; count; enumerate; empty; filter; filter_0; filter_1; filter_good; 
    flatten; find_idx; fold; fold1; foldswap; for_each; map; map_0; map_1; map_inner;
    once; range; range_inf; repeat; scan; select_by; skip; sliding_pair; splice; take; 
    take_while; zip; zip4
}
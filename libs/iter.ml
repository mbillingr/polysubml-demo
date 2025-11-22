(*
    An iterator is a function that ignores its argument and returns the next value on each call.
    Any interesting use will need to keep mutable state in the iterator's closure.
*)

let type <<iter t>> = any -> [`Some t | `None any];

let rec range = fun (start: int, stop: int): (<<iter int>>) ->
        let state = {mut x = start} in
            fun _ ->
                if state.x >= stop then
                    `None {}
                else
                    `Some (state.x <- state.x + 1)

    and fold = fun (type a b) (f: (b*a)->b, init: b, it: <<iter a>>) : b ->
        let vars = {mut acc=init} in
            loop
                match it {} with
                    | `None _ -> `Break vars.acc
                    | `Some x -> `Continue (vars.acc <- f(vars.acc, x))

    // same as fold but function arguments swapped
    and fold2 = fun (type a b) (f: (a*b)->b, init: b, it: <<iter a>>) : b ->
        let vars = {mut acc=init} in
            loop
                match it {} with
                    | `None _ -> `Break vars.acc
                    | `Some x -> `Continue (vars.acc <- f(x, vars.acc))

    and for_each = fun (type a) (f: a -> any, it: <<iter a>>): any ->
        loop
            match it {} with
                | `None _ -> `Break {}
                | `Some x -> `Continue (f x)

    and map = fun (type a b) (f: a -> b, it: <<iter a>>): (<<iter b>>) ->
        fun _ ->
            match it {} with
                | `Some x -> `Some f x
                | none -> none

    and filter = fun (type a) (f: a -> bool, it: <<iter a>>): (<<iter a>>) ->
        fun z ->
            loop
                match it {} with
                    | `Some x -> (if f x then
                            `Break `Some x
                        else
                            `Continue z)
                    | none -> `Break none

    and skip = fun (type a) (n: int, it: <<iter a>>) : (<<iter a>>) ->
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
        )

    and take = fun (type a) (n: int, it: <<iter a>>) : (<<iter a>>) ->
        let state = {mut k=n}
        in fun z ->
            if state.k > 0 then (
                state.k <- state.k - 1;
                it z)
            else
                `None z

    and zip = fun (type a b) (ia: <<iter a>>, ib: <<iter b>>) : (<<iter (a*b)>>) ->
        fun _ ->
            match ia {} with
                | `Some x -> (match ib {} with
                    | `Some y -> `Some (x, y)
                    | none -> none)
                | none -> none

in {
    filter; fold; fold2; for_each; map; range; skip; take; zip
}
import "iter.ml";
import "vec.ml";

let type <<heap t>> = {mut data: vec@{mut x:t}; cmp: (t*t)->bool};

let new = fun (type t) (cmp: (t*t)->bool): (<<heap t>>) -> {
    cmp;
    mut data = #[]
};

let length = fun (type t) (hp: <<heap t>>): int ->
    vec.length hp.data;

let parent_idx = fun i -> (i + 1) / 2 - 1;
let childl_idx = fun i -> i * 2 + 1;
let childr_idx = fun i -> i * 2 + 2;

let cmpswap = fun (type t) (hp: <<heap t>>, parent: int, child:int): any -> begin
    let xp = vec.get(hp.data, parent);
    let xc = vec.get(hp.data, child);
    if hp.cmp(xc.x, xp.x)
    then begin
        let tmp = xp.x;
        xp.x <- xc.x;
        xc.x <- tmp;
        {}
    end else {}
end;

let cmp_child = fun (type t) (hp: <<heap t>>, i: int): int -> begin
    let cl = childl_idx i;
    let cr = childr_idx i;
    if cr >= length hp
    then cl
    else if hp.cmp((vec.get(hp.data, cl)).x, (vec.get(hp.data, cr)).x)
        then cl
        else cr
end;

let bubble_up = fun (type t) (hp: <<heap t>>, i: int): any ->
    let vars = {mut i} in
        loop begin
            let i = vars.i;
            let p = parent_idx i;
            if p < 0
            then `Break {}
            else begin
                cmpswap(hp, p, i);
                `Continue (vars.i <- p)
            end
        end;

let bubble_down = fun (type t) (hp: <<heap t>>, i: int): any ->
    let vars = {mut i=i} in
        loop begin
            let i = vars.i;
            if childl_idx i >= length hp 
            then `Break {}
            else begin
                let c = cmp_child(hp, i);
                cmpswap(hp, i, c);
                `Continue (vars.i <- c)
            end
        end;

let push = fun (type t) (hp: <<heap t>>, x: t): any -> begin
    hp.data <- vec.push_back(hp.data, {mut x});
    bubble_up(hp, length hp - 1)
end;

let peek = fun (type t) (hp: <<heap t>>): [`Some t | `None any] ->
    match vec.peek_front hp.data with
        | `Some x -> `Some x.x
        | `None _ -> `None {};

let pop = fun (type t) (hp: <<heap t>>): [`Some t | `None any] ->
    if length hp == 0
    then `None {}
    else if length hp == 1
    then begin
        let front = vec.front hp.data;
        hp.data <- #[];
        `Some front.x
    end else begin
        let front = vec.front hp.data;
        let back = vec.back hp.data;
        hp.data <- vec.pop_back hp.data;
        let x = front.x;
        front.x <- back.x;
        bubble_down(hp, 0);
        `Some x
    end;
    

let merge = fun (type t) (hp1: <<heap t>>, hp2: <<heap t>>): any -> begin
  iter.for_each((fun {x} -> push(hp1, x)), vec.iter hp2.data);
  hp1
end;




{
    length; merge; new; peek; pop; push
}
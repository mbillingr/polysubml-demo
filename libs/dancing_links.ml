(*
    Dancing Links
    From https://arxiv.org/abs/cs/0011047
*)

import "curry/iter.ml";
import "dict.ml";
import "io.ml";
import "option.ml";
import "set.ml";
import "vec.ml";

let globals = {mut next_id=0};

let new_id = fun _ -> globals.next_id <- globals.next_id + 1;


// low level helper functions

let lt = fun {lt} -> option.unwrap lt;
let rt = fun {rt} -> option.unwrap rt;
let up = fun {up} -> option.unwrap up;
let dn = fun {dn} -> option.unwrap dn;
let col = fun {col} -> option.unwrap col;
let name = fun {name} -> name;

let set_lt = fun (obj, x) -> obj.lt <- `Some x;
let set_rt = fun (obj, x) -> obj.rt <- `Some x;
let set_up = fun (obj, x) -> obj.up <- `Some x;
let set_dn = fun (obj, x) -> obj.dn <- `Some x;
let set_col = fun (obj, x) -> obj.col <- `Some x;

let is_same = fun ({id=a}, {id=b}) -> a == b;

let iter_fld = 
    fun f ->  fun c ->
        let state = {mut i = f c} in
            fun _ -> if is_same(state.i, c) then
                    `None {}
                else
                    `Some (state.i <- f state.i);


let iter_rt = iter_fld rt;
let iter_lt = iter_fld lt;
let iter_up = iter_fld up;
let iter_dn = iter_fld dn;

let iter_rt_inclusive = fun c -> iter.chain(iter.once c, iter_rt c);


// problem setup functions

let new_problem = fun _ -> begin
    let h = {
        id=new_id{}; 
        name="<root>"; mut rows=0;
        mut lt=`None {}; mut rt=`None {};         
        // Unused
        mut dn=`None {}; mut up=`None {}; col=`None {}; row=-1; size=0
    };
    set_lt(h, h);
    set_rt(h, h);
    h
end;

let atmost_once_column = fun(h, name) -> begin
    let c = {
        id = new_id(); name;
        mut col=`None {};
        mut lt=`None {}; mut rt=`None {};
        mut up=`None {}; mut dn=`None {};
        mut size=0;
        // Unused
        row=-1
    };
    set_col(c, c);
    set_up(c, c);
    set_dn(c, c);
    set_lt(c, c);
    set_rt(c, c);
    c
end;

let exactly_once_column = fun(h, name) -> begin
    let c = atmost_once_column(h, name);
    set_lt(c, lt h);
    set_rt(c, h);
    set_rt(lt h, c);
    set_lt(h, c);
    c
end;

let add_row = fun(h, cols) -> begin
    let row = h.rows <- h.rows + 1;

    let xs = vec.map(
        (fun c -> begin 
            c.size <- c.size + 1;
            {
                id=new_id{}; name="";
                mut lt=`None {}; mut rt=`None {};
                mut up=`None {}; mut dn=`None {};
                col=`Some c; row;
                // Unused
                size=-1
            }
        end), 
        cols);
    
    let n = vec.length xs;

    xs
    |> vec.iter
    |> iter.enumerate
    |> (iter.for_each
        (fun (i,x) -> begin
            set_lt(x, vec.get(xs, (i - 1 + n) % n));
            set_rt(x, vec.get(xs, (i + 1) % n));
            set_up(x, up col x);
            set_dn(x, col x);
            set_up(col x, x);
            set_dn(up x, x);
            {}
        end))
end;

let print_matrix = fun h -> begin
    let vars = {mut rows = dict.empty};

    iter_rt h |> (iter.for_each(fun c -> begin
        io.write_str c.name;
        io.write_str " ";
        iter_dn c |> (iter.for_each(fun r -> begin
            let rcols = iter_rt r 
                |> (iter.map(fun x -> (col x).name))
                |> set.collect;
            let rcols = set.insert(rcols, c.name);
            vars.rows <- dict.insert(vars.rows, r.row, rcols)
        end))
    end));
    io.write_line "";

    iter.range(0, h.rows) |> (iter.for_each(fun r ->
        match dict.get(vars.rows, r) with
            | `None _ -> io.write_line("----")
            | `Some rcols -> begin
                    iter_rt h |> (iter.for_each(fun c ->
                    if set.contains(rcols, c.name) then
                        io.write_str "1 "
                    else
                        io.write_str("0 ")));
                    io.write_line ""
                end
    ));

    {}
end;

// problem solving

let select_column = fun h -> 
    //rt h;  // simply take first column. TODO: select column with minimum size
    (iter.select_by(fun (a, b) -> a.size < b.size)) iter_rt h;

let cover_column = fun c -> begin
    set_lt(rt c, lt c);
    set_rt(lt c, rt c);
    iter_dn c |> (iter.for_each(fun i -> begin
        iter_rt i |> (iter.for_each(fun j -> begin
            set_up(dn j, up j);
            set_dn(up j, dn j);
            (col j).size <- (col j).size - 1
        end))
    end))
end;

let uncover_column = fun c -> begin
    iter_up c |> (iter.for_each(fun i -> begin
        iter_lt i |> (iter.for_each(fun j -> begin
            (col j).size <- (col j).size + 1;
            set_up(dn j, j);
            set_dn(up j, j)
        end))
    end));
    set_lt(rt c, c);
    set_rt(lt c, c)
end;


let search_all = fun h ->
    let rec search_rec = fun out -> begin
        if is_same(rt h, h) then
            #[
                (vec.map((fun x -> vec.collect (iter.map(fun y -> name col y)) iter_rt_inclusive x), out))
            ]
        else begin
            let c = select_column h;
            cover_column c;
            let vars = {mut solutions = #[]};
            iter_dn(c) |> (iter.for_each (fun r -> begin
                iter_rt(r) |> (iter.for_each(fun j -> cover_column col j));

                let out_ = vec.push_back(out, r);
                vars.solutions <- vec.append(vars.solutions, search_rec out_);

                iter_lt(r) |> (iter.for_each(fun j -> uncover_column col j))                
            end));
            uncover_column c;
            vars.solutions
        end
    end
    in search_rec #[];


let example = fun _ -> begin
    let h = new_problem {};
    let a = exactly_once_column(h, "A");
    let b = exactly_once_column(h, "B");
    let c = exactly_once_column(h, "C");
    let d = exactly_once_column(h, "D");
    let e = exactly_once_column(h, "E");
    let f = exactly_once_column(h, "F");
    let g = exactly_once_column(h, "G");
    add_row(h, #[c, e, f]);
    add_row(h, #[a, d, g]);
    add_row(h, #[b, c, f]);
    add_row(h, #[a, d]);
    add_row(h, #[b, g]);
    add_row(h, #[d, e, g]);
    h
end;


{ 
    add_row; atmost_once_column; exactly_once_column; example; new_problem; print_matrix; search_all
}
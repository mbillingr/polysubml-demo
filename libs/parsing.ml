import "dict.ml";
import "int.ml";
import "iter.ml";
import "option.ml";
import "set.ml";
import "str.ml";
import "vec.ml";

let type <<optional t>> = [`Some t | `None any];
let type <<result t>> = [`Ok {result:t; rest:vec@str} | `Err str];
let type <<parser r>> = (vec@str) -> <<result r>>;


let parse = fun (type r) (text: str, parser: <<parser r>>): [`Ok r | `Err str] -> begin
	let chs = vec.collect str.chars text;
	match parser(chs) with
        | `Ok {result; rest} -> `Ok result
        | `Err e -> `Err e
end;


let map_result = fun (type r s) (f: r -> s, p: <<parser r>>): (<<parser s>>) ->
    fun(chs) ->
        match p chs with
            | `Err e -> `Err e
            | `Ok {result; rest} -> `Ok {result=f result; rest};

let sequence = fun (type r s) (p1: <<parser r>>, p2: <<parser s>>): (<<parser (r*s)>>) ->
    fun(chs) ->
        match p1 chs with
            | `Err err -> `Err err
            | `Ok {result=r1; rest=chs1} -> (match p2 chs1 with
                | `Err err -> `Err err
                | `Ok {result=r2; rest=chs2} -> `Ok {result=(r1, r2); rest=chs2});

let alternative = fun (type r) (p1: <<parser r>>, p2: <<parser r>>): (<<parser r>>) ->
    fun(chs) ->
        match p1 chs with
            | `Ok ok -> `Ok ok
            | `Err _ -> (match p2 chs with
                | `Ok ok -> `Ok ok
                | `Err e -> `Err e);

let not = fun (type r) (p: <<parser r>>): (<<parser {}>>) ->
    fun chs ->
        match p chs with
            | `Ok _ -> `Err (chs |> vec.peek_front |> (option.unwrap_or ""))
            | `Err _ -> `Ok {result={}; rest=chs};

let opt = fun (type r) (p: <<parser r>>): (<<parser <<optional r>>>>) ->
    fun(chs) ->
        match p chs with
            | `Ok {result; rest} -> `Ok {result=`Some result; rest}
            | `Err _ -> `Ok {result=`None{}; rest=chs};

let prefixed = fun (type r) (left: <<parser any>>, right: <<parser r>>): (<<parser r>>) ->
    map_result(
        (fun (_, x) -> x), 
        sequence(left, right));

let suffixed = fun (type r) (left: <<parser r>>, right: <<parser any>>): (<<parser r>>) ->
    map_result(
        (fun (x, _) -> x), 
        sequence(left, right));

let delimited = fun (type r) (left: <<parser any>>, mid: <<parser r>>, right: <<parser any>>): (<<parser r>>) ->
    prefixed(left, suffixed(mid, right));

let seq = fun (type r) (ps: vec@(<<parser r>>)): (<<parser vec@r>>) ->
    fun chs -> begin
        let ps = vec.iter ps;
        let vars = {mut rest=chs; mut out=#[]};
        loop
            match ps {} with
                | `None _ -> `Break `Ok {result=vars.out; rest=vars.rest}
                | `Some p -> (
                    match p vars.rest with
                        | `Err e -> `Break `Err e
                        | `Ok {result; rest} -> begin
                            vars.out <- vec.push_back(vars.out, result);
                            vars.rest <- rest;
                            `Continue {}
                        end)
    end;

let alt = fun (type r) (ps: vec@(<<parser r>>)): (<<parser r>>) ->
    fun chs -> begin
        let ps = vec.iter ps;
        loop
            match ps {} with
                | `None _ -> `Break `Err (chs |> vec.peek_front |> (option.unwrap_or ""))
                | `Some p -> (
                    match p chs with
                        | `Err e -> `Continue {}
                        | `Ok r -> `Break `Ok r)
    end;

let repeat = fun (type r) (p: <<parser r>>, min: int, max: int): (<<parser vec@r>>) ->
    fun(chs) ->
        let vars = {mut i=0; mut rest=chs; mut out=#[]} in
            loop
                if vars.i >= max
                    then `Break `Ok {result=vars.out; rest=vars.rest}
                else match p vars.rest with
                    | `Err e -> (if vars.i < min
                        then `Break `Err e
                        else `Break `Ok {result=vars.out; rest=vars.rest})
                    | `Ok {result; rest} -> begin
                        vars.out <- vec.push_back(vars.out, result);
                        vars.rest <- rest;
                        vars.i <- vars.i + 1;
                        `Continue {}
                    end;


let repeat_min = fun (type r) (p: <<parser r>>, min: int): (<<parser vec@r>>) ->
    fun(chs) ->
        let vars = {mut i=0; mut rest=chs; mut out=#[]} in
            loop
                match p vars.rest with
                    | `Err e -> (if vars.i < min
                        then `Break `Err e
                        else `Break `Ok {result=vars.out; rest=vars.rest})
                    | `Ok {result; rest} -> begin
                        vars.out <- vec.push_back(vars.out, result);
                        vars.rest <- rest;
                        vars.i <- vars.i + 1;
                        `Continue {}
                    end;


let seplist = fun (type r) (sep: <<parser any>>, p: <<parser r>>, min: int): (<<parser vec@r>>) ->
    fun(chs) ->
        let vars = {mut i=0; mut rest=chs; mut out=#[]} in
            loop begin
                if vars.i > 0 then
                    match sep vars.rest with
                        | `Err e -> (if vars.i < min
                            then `Break `Err e
                            else `Break `Ok {result=vars.out; rest=vars.rest})
                        | `Ok {rest} -> (vars.rest <- rest; {} )
                else {};

                match p vars.rest with
                    | `Err e -> (if vars.i < min
                        then `Break `Err e
                        else `Break `Ok {result=vars.out; rest=vars.rest})
                    | `Ok {result; rest} -> begin
                        vars.out <- vec.push_back(vars.out, result);
                        vars.rest <- rest;
                        vars.i <- vars.i + 1;
                        `Continue {}
                    end
            end;


let epsilon = fun chs -> `Ok {result=""; rest=chs};

let fail = fun chs -> 
    match vec.peek_front chs with
        | `Some c -> `Err c
        | `None _ -> `Err "";


let eof = fun chs ->
    match vec.peek_front chs with
        | `Some c -> `Err c
        | `None _ -> `Ok {result=""; rest=chs};


let any_char = fun chs ->
    match vec.peek_front chs with
        | `Some c -> `Ok {result=c; rest=vec.pop_front chs}
        | `None _ -> `Err "";


let char = fun ch -> fun chs ->
    match any_char chs with
        | `Ok r -> (if r.result == ch then `Ok r else `Err r.result)
        | `Err e -> `Err e;


let ws = fun chs ->
     match any_char chs with
         | `Ok r -> (
             let n = str.ord r.result;
             if n == 32 || n == 13 || n == 10 || n == 9
                then `Ok r
                else `Err r.result)
         | `Err e -> `Err e;


let digit = fun chs ->
    match any_char chs with
        | `Ok r -> (
            let n = str.ord r.result;
            if n >= 48 && n <= 57 then
                `Ok r else
                `Err r.result)
        | `Err e -> `Err e;


let text = fun txt -> 
    iter.fold(
        (fun (ps, p) -> 
            map_result(
                (fun (cs, c) -> cs^c), 
                sequence(ps, p))), 
        epsilon, 
        iter.map(char, str.chars txt));


let one_of = fun chs -> iter.foldswap(alternative[r=str], fail, iter.map(char, str.chars chs));


let num = map_result(
        (fun ds -> str.join("", vec.iter ds) |> int.from_str |> option.unwrap),
        repeat_min(digit, 1));


{
    parse;
    map_result;
    alt; alternative; delimited; not; opt; prefixed; seq; sequence; suffixed; repeat; repeat_min; seplist;
    any_char; char; digit; eof; fail; num; one_of; text; ws;
    epsilon
}
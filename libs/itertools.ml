import "iter.ml";
import "vec.ml";

let type <<iter t>> = any -> [`Some t | `None any];

let chunks = fun (type a) (n: int, it: <<iter a>>) : (<<iter (vec@a)>>) ->
    let state = {mut it=it} in
    fun _ ->
        let vars = {mut out=#[]} in
            loop
                if vec.length vars.out == n then
                    `Break `Some vars.out
                else match state.it {} with
                    | `None _ -> (if vec.is_empty vars.out then `Break `None {} else `Break `Some vars.out)
                    | `Some x -> (
                        vars.out <- vec.push_back(vars.out, x);
                        `Continue {}
                    );

{
  chunks
}
import "iter.ml";


let new = __progress_bar_new;
let step = __progress_bar_step;
let set_len = __progress_bar_setlen;
let finish = __progress_bar_finish;

{
    new;
    step;
    set_len;
    finish;

    range = fun(start:int, stop:int): (any -> [`Some int | `None any]) -> begin
        let prog = new (stop - start);
        let it = iter.range(start, stop);
        fun _ ->
            match it {} with
                | `Some x -> (step(prog, 1); `Some x)
                | `None _ -> (finish prog; `None {})
    end
}
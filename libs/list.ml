import "vec.ml";
let type <<list t>> = vec@t;
let type <<iter t>> = any -> [`Some t | `None any];

let nil: (<<list never>>) = vec.empty;
let is_empty = vec.is_empty;
let foldl = vec.foldl;
let foldr = vec.foldr;
let map = vec.map;
let filter = vec.filter;
let append = vec.append;
let reverse = vec.reverse;
let merge_sorted = vec.merge_sorted;
let sort_by = vec.sort_by;
let collect = vec.collect;
let collect_rev = vec.collect_rev;
let iter = vec.iter;

let cons = fun (type a) (hd:a, tl:(<<list a>>)) : (<<list a>>) ->
      __vec_push_front(tl, hd);

let head = fun (type a) (lst : (<<list a>>)) : a ->
  match __vec_peek_front lst with
    | `None _ -> panic "empty list"
    | `Some h -> h;

let tail = fun (type a) (lst : (<<list a>>)) : (<<list a>>) ->
  if is_empty lst
  then panic "empty list"
  else __vec_pop_front lst;

{
  nil; is_empty; cons; head; foldl; foldr; map; filter; append; reverse; sort_by; merge_sorted; collect_rev; collect; iter
}

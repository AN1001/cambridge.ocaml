type 'a seq = Nil | Cons of 'a * (unit -> 'a seq)
type 'a lazy_tree = Lf | Br of 'a * (unit -> 'a lazy_tree) * (unit -> 'a lazy_tree)

let rec appendq xq yq =
  match xq with
  | Nil -> yq
  | Cons (x, xf) -> Cons(x, fun () -> appendq (xf ()) yq)

let rec lztree_as_lzlist = function
  | Lf -> Nil
  | Br (x,lt,rt) -> Cons (x, fun () -> appendq (lztree_as_lzlist (lt())) (lztree_as_lzlist (rt())))

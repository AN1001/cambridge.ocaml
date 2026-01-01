(* Turns a lazy binary tree into a lazy list
   It does so fairly so every element can
   eventually be reached by using BFS *)
type 'a seq = Nil | Cons of 'a * (unit -> 'a seq)
type 'a ltree = Lf | Br of 'a * (unit -> 'a ltree) * (unit -> 'a ltree)

let rec interleave seq_a seq_b = match seq_a with
  | Nil -> seq_b
  | Cons (x, tail) -> Cons (x, fun () -> interleave seq_b (tail()))

let rec ltree_bfs = function
  | Lf -> Nil
  | Br (x, lt, rt) ->
    let tail = interleave (ltree_bfs (lt())) (ltree_bfs (rt()))
    in Cons (x, fun () -> tail)

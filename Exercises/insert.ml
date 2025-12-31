type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

exception Collision
(* Inserts into dictionary stored as a binary search tree*)
let rec insert cmp new_k new_v = function
  | Lf -> Br((new_k,new_v), Lf, Lf)
  | Br((k, v), lt, rt) ->
      if new_k=k then raise Collision
                 else match cmp new_k k with
                   | false -> Br((k, v), lt, insert cmp new_k new_v rt)
                   | true -> Br((k, v), insert cmp new_k new_v lt, rt)

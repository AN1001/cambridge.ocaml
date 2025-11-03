(* Updates a binary search tree that represents an array *)
exception NoPath
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree
let rec update el k = function
  | Lf -> if k = 1
          then Br (el,Lf,Lf)
          else raise NoPath
  | Br (v,lt,rt) -> 
                    if k = 1 then Br (el, lt, rt)
                    else if k mod 2 = 0
                    then Br (v, update el (k/2) lt,rt)
                    else Br (v, lt, update el (k/2) rt)

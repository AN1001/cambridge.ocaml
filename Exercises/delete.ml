(* Deletes a value from a dictionary stored as a binary search tree *)
exception KeyNotFound

let rec get_leftmost = function
  | Br (v,Lf,rt) -> v
  | Br(v,lt,rt) -> get_leftmost lt
  | Lf -> raise KeyNotFound

let rec remove_leftmost = function
  | Lf -> raise KeyNotFound
  | Br (v, Lf, rt) -> rt
  | Br (v, lt, rt) -> Br(v, remove_leftmost lt, rt)

let rec delete k cmp = function
  | Lf -> raise KeyNotFound
  | Br ((cur_k, _) as v, lt, rt) -> if cur_k = k then Br(get_leftmost rt, lt, remove_leftmost rt)
                                                   else match cmp k cur_k with 
                                                      | true -> Br(v, delete k cmp lt, rt)
                                                      | false -> Br(v, lt, delete k cmp rt)

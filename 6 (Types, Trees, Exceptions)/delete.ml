(*Deletes key,value pair in dictionary tree given key*)
exception KeyNotFound
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

let rec get_leftmost = function
  | Br (v,Lf,rt) -> v
  | Br (v,lt,rt) -> get_leftmost lt
  | Lf -> raise KeyNotFound

let rec replace_leftmost = function
  | Br (v,Lf,rt) -> rt
  | Br (v,lt,rt) -> Br (v,replace_leftmost lt,rt)
  | Lf -> raise KeyNotFound

let rec delete k = function
  | Lf -> raise KeyNotFound
  | Br ((cur_k,v),lt,rt) -> if k > cur_k then Br ((cur_k,v),lt,delete k rt)
                            else if k < cur_k then Br ((cur_k,v),delete k lt,rt)
                            else if rt = Lf 
                                 then lt
                                 else Br (get_leftmost rt, lt, replace_leftmost rt)
;;


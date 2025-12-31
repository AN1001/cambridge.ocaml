(* Removes the first element of a functional array *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree
exception Empty_array
exception Invalid_functional_array

let rec remove_first = function
  | Lf -> raise Empty_array
  | Br (_, Lf, Lf) -> Lf
  | Br (_, lt, rt) -> match lt with
                        | Br (new_v, _, _) -> Br (new_v, rt, remove_first lt)
                        | _ -> raise Invalid_functional_array

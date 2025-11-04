type 'a tree = Lf | Br of 'a * 'a tree * 'a tree
let rec merge l1 l2 =
match l1, l2 with
  | x::xs, y::ys -> x::y::merge xs ys
  | x::xs, [] -> x::xs
  | [], y::ys -> y::ys
  | [],[]->[]

let rec listofarray = function
  | Lf -> []
  | Br (v,lt,rt) -> v::merge (listofarray lt) (listofarray rt)
;;

(*Example Case*)
listofarray (Br (1,Br(2,Lf,Lf),Br(3,Lf,Lf)))

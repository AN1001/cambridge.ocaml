let rec insert el = function
  | [] -> [el]
  | x::xs -> if el < x
             then el::x::xs 
             else x::(insert el xs)
;;

let rec insertion_sort = function
  | [] -> []
  | x::xs -> insert x (insertion_sort xs)
;;

(*Example case*)
insertion_sort [7;6;5;4;2;3;1]

let rec find_minimal cur_min = function
  | x::xs -> if x < cur_min
             then find_minimal x xs
             else find_minimal cur_min xs
  | [] -> cur_min
;;
let rec remove_el el = function
  | [] -> []
  | x::xs -> if x = el then xs else x::(remove_el el xs)
;;
let rec selection_sort list =
  match list with
    | [] -> []
    | x::xs -> let minimum = find_minimal x xs in
               minimum::selection_sort (remove_el minimum (x::xs))
;;
(*Example case*)
selection_sort [7;6;5;4;3;2;1]

let rec smaller_or_equal n = function
  | [] -> []
  | x::xs -> if x <= n then x :: (smaller_or_equal n xs) else smaller_or_equal n xs
;;
let rec greater n = function
  | [] -> []
  | x::xs -> if x > n then x :: (greater n xs) else greater n xs
;;
let rec quicksort = function
  | [] -> []
  | x::xs -> (quicksort (smaller_or_equal x xs)) @ x :: (quicksort (greater x xs))
;;

(*Example case (also worst case)*)
quicksort [7;6;5;4;3;2;1]

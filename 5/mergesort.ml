let rec take n = function
  | [] -> []
  | x::xs -> if n > 0
             then x :: take (n - 1) xs
             else []

let rec drop n = function
  | [] -> []
  | x::xs -> if n > 0
             then drop (n-1) xs
             else x::xs

let rec merge x y =
  match x,y with
    | [],ys -> ys
    | xs,[] -> xs
    | x::xs,y::ys -> if x < y then x::(merge xs (y::ys))
                     else y::(merge (x::xs) ys)
;;

let rec merge_sort = function
  | [] -> []
  | [x] -> [x]
  | xs -> let n = List.length xs / 2 in
          merge (merge_sort (take n xs)) (merge_sort (drop n xs))
;;

(*Example case*)
merge_sort [7;6;5;3;2;1;55]

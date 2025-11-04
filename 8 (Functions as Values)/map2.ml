(* Applies a function to a 2D list *)
let rec map2 f x = 
match x with
  | [] -> []
  | y::ys -> let rec inner_map f = function
    | []->[]
    | b::bs -> f b :: inner_map f bs 
  in inner_map f y :: map2 f ys
;;

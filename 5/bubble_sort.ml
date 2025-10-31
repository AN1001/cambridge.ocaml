let rec bubble_sort = function
  | [] -> []
  | [x] -> [x]
  | x::xs -> let y::ys = bubble_sort xs in
             if x < y then x::y::ys else y::(bubble_sort (x::ys))
             (*This is exhaustive but compiler doesn't realise
             it since xs cannnot be [] as would have been matched earlier*)
;;

(*Example case*)
bubble_sort [7;6;5;4;3;2;1]

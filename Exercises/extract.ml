(* Finds combinations of a list *)
let rec extract n = function
  | _ when n=0 -> [[]]
  | [] -> []
  | x::xs -> List.map (fun el -> x::el) (extract (n-1) xs)
             @ (extract n xs)
;;

extract 3 ["a"; "b"; "c"; "d"]

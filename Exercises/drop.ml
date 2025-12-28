(* Drops the nth element of a list *)
let drop l n = 
  let rec aux acc i = function
    | [] -> acc
    | x::xs -> if i=1 then aux acc n xs
                      else aux (x::acc) (i-1) xs
  in
  List.rev (aux [] n l)
;;

drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3

let duplicate l = 
  let rec aux acc = function
    | x::xs -> aux (x::x::acc) xs
    | [] -> acc
  in
  List.rev (aux [] l)
;;

duplicate ["a"; "b"; "c"; "c"; "d"]

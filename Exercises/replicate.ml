let replicate l n = 
  let rec add count acc x =
    if count=1 then x::acc
               else add (count-1) (x::acc) x
  in
  let rec aux acc = function
    | x::xs -> aux (add n acc x) xs
    | [] -> acc
  in
  List.rev (aux [] l)
;;

replicate ["a"; "b"; "c"; "c"; "d"] 3

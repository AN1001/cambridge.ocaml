(* Splits a list into 2 parts *)
let split l n = 
  let rec aux acc i = function
    | [] -> (List.rev acc, [])
    | x::xs -> if i=1 then (List.rev (x::acc), xs)
                      else aux (x::acc) (i-1) xs
  in
  aux [] n l
;;

split ["a"; "b"; "c"; "d"] 5

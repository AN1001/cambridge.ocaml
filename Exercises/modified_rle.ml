type 'a rle = 
  | One of 'a
  | Many of (int * 'a)

let encode l =
  let make_el count el = if count=1 then One (el)
                                    else Many (count,el)
  in
  let rec aux count acc = function
    | x::x2::xs -> if x=x2 then (aux (count+1) acc (x2::xs))
                           else (aux 0 ((make_el (count+1) x)::acc) (x2::xs))
    | [x] -> (make_el (count+1) x)::acc
    | [] -> acc
  in
  List.rev (aux 0 [] l)
;;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

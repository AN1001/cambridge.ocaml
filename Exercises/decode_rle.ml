type 'a rle = 
  | One of 'a
  | Many of (int * 'a)

let decode l = 
  let rec add c v acc =
    if c=1 then (v::acc) else (add (c-1) v (v::acc))
  in 
  let rec aux acc = function
    | [] -> acc
    | (One v)::xs -> aux (add 1 v acc) xs
    | (Many (c, v))::xs -> aux (add c v acc) xs
  in
  List.rev (aux [] l)
;;

decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]

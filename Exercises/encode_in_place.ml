type 'a rle = 
  | One of 'a
  | Many of int * 'a

let encode l = 
let rec aux count acc = function
  | x::y::xs -> if x=y then aux (count+1) acc (y::xs)
                       else aux 0 ((if count=0 then One(x) 
                                               else Many(count+1, x))::acc) (y::xs)
  | [x] ->  (if count=0 then One(x) else Many(count+1, x))::acc
  | [] -> acc
in List.rev (aux 0 [] l)
;;
encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]

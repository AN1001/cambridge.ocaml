(* Challenge: implement `map (map f)` with only one recusive function *)

let rec map2 f = function
  | [] -> []
  | x::xs ->
    match x with
      | [] -> []::(map2 f xs)
      | y::ys ->
        match map2 f (ys::xs) with
          | z::zs -> ((f y)::z)::zs
          | [] -> [] (* Unreachable, added for
                        compiler exhaustiveness *)
;;

map2 (fun x -> x+x) [[1;2;3;4];[5;6];[7];[8];[9;10;11]]
 

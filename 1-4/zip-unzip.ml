let rec zip xs ys = 
  match xs, ys with
    | x::xs, y::ys -> (x,y)::zip xs ys
    | _ ->  []
;;
let rec unzip = function
  | pair::tail -> let a,b=pair in 
                  let xs,ys = (unzip tail) in 
                    (a::xs,b::ys)
  | [] -> ([],[])
;;

(*Example test*)
zip [1;2;3;4;5] ['1';'2';'3';'4';'5'];;
unzip [(1, '1'); (2, '2'); (3, '3'); (4, '4'); (5, '5')]

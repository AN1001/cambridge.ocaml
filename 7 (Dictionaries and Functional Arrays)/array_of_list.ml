type 'a tree = Lf | Br of 'a * 'a tree * 'a tree
let rec tcons v = function
  | Lf -> Br (v, Lf, Lf)
  | Br (w, t1, t2) -> Br (v, tcons w t2, t1)
let rec arrayoflist = function 
  | [] -> Lf
  | [x] -> tcons x Lf
  | x::xs -> tcons x (arrayoflist xs)
;;
(*Example case*)
arrayoflist [1;2;3]

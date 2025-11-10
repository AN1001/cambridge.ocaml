(*Consumes the first n terms of a sequence*)
exception EndOfSeq
(* Lazy List type def - called seq*)
type 'a seq = Nil | Cons of 'a * (unit -> 'a seq)

let hd = function
  | Nil -> raise EndOfSeq
  | Cons (v,_) -> v
  
let next = function
  | Nil -> raise EndOfSeq
  | Cons (_,tail) -> tail ()
  
let rec get n s = match n,s with
  | 0,_ -> []
  | _,Nil -> []
  | _,xf -> hd xf :: get (n-1) (next xf)
  
let rec from k = Cons (k, (fun () -> from (k+1)))
;;
(*Example case*)
get 5 (from 5)

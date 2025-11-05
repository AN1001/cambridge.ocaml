type 'a seq = Nil | Cons of 'a * (unit -> 'a seq)

let rec filter_seq pred = function
  | Nil -> Nil
  | Cons (x,xf) -> if pred x then Cons( x, ( fun () -> filter_seq pred (xf()) ) )
                   else filter_seq pred (xf())

let rec from k = Cons (k, (fun () -> from (k+1)))

(*Example filtered sequence *)
let from_odds k = filter_seq (fun n -> n mod 2 = 1) (from k)

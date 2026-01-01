type 'a seq = Nil | Cons of 'a * (unit -> 'a seq)

(* Concatenates a sequence of sequences *)
let rec concat_seq = function
  | Nil -> Nil
  | Cons (seq, t) ->
    match seq with
      | Nil -> concat_seq (t())
      | Cons (x, tail) -> 
          Cons ( x, fun () -> concat_seq (Cons(tail(), t)) )

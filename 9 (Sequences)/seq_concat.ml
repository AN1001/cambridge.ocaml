(*Concatenates two sequences together*)
type 'a seq = Nil | Cons of 'a * (unit -> 'a seq)

let rec next = function
  | Nil -> Nil
  | Cons(x,tail) -> tail()

let rec replace_tail new_tail = function
  | Nil -> new_tail
  | Cons(x,tail) -> Cons(x,fun () -> replace_tail new_tail (tail()))

let rec seq_concat = function
  | [] -> Nil
  | seq::seqs -> match seq with
                   | Cons(x,tail) -> Cons(x, fun () -> replace_tail (seq_concat seqs) (tail()))
                   | Nil -> seq_concat seqs

(*Example Case*)
let seq_a = Cons(1,fun () -> Cons(2, fun() -> Nil))
let seq_b = Cons(3,fun () -> Cons(4, fun() -> Nil))
let seq_c = Cons(5,fun () -> Cons(6, fun() -> Nil))
let x = seq_concat [seq_a;seq_b;seq_c];;
next x;;
next (next x);;
next (next (next x));;
next (next (next (next x)));;

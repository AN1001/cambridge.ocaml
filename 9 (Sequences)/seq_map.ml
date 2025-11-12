exception NoHead
type 'a seq = Nil | Cons of 'a * (unit -> 'a seq)

let rec next = function
  | Nil -> Nil
  | Cons(x,tail) -> tail()

let head = function
  | Nil -> raise NoHead
  | Cons (head, _) -> head

let rec seq_map f = function
  | Nil -> Nil
  | Cons (x, tail) -> Cons(f x, fun () -> seq_map f (tail()))

(*Example Case*)
let rec test k = Cons(k, fun () -> test (k+1))
let test_seq = seq_map (fun k -> k*2) (test 2);;
next test_seq

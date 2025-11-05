(*Interleaves two lazy lists/sequences together*)
type 'a seq = Nil | Cons of 'a * (unit -> 'a seq)

let rec interleave s1 s2 = match s1 with
  | Nil -> s2
  | Cons (x,xf) -> Cons(x, fun () -> interleave s2 (xf()))

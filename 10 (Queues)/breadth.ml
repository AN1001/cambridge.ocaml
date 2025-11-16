(*Does BFS on a queue of trees (assumes the queue is well formed)*)
exception EmptyQ
type 'a queue = Q of 'a list * 'a list
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

let normalise = function
  | Q ([], tail) -> Q(List.rev tail, [])
  | q -> q

let q_is_null = function
  | Q ([],[]) -> true
  | q -> false

let enq x = function
  | Q (front, back) -> normalise (Q (front, x::back))

let deq = function
  | Q ([],_) -> raise EmptyQ
  | Q (x::front, back) -> normalise (Q (front, back))

let q_head = function
  | Q ([],_) -> raise EmptyQ
  | Q (x::front, back) -> x

let rec breadth = function
  | Q (Br (x, lt, rt)::front, back) -> x::(breadth (normalise (Q(front, lt::rt::back))))
  | Q (Lf::front, back) -> breadth (normalise (Q(front, back)))
  | q_empty -> []
;;

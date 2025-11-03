type 'a tree = Lf | Br of 'a * 'a tree * 'a tree
let demo_tree = Br (1, Br (2, Br (4, Lf, Lf), Br (5, Lf, Lf)), Br (3, Lf, Lf))
;;
let rec sum_tree = function
  | Lf -> 0
  | Br (v, t1, t2) -> v + sum_tree t1 + sum_tree t2
;;
sum_tree demo_tree;;

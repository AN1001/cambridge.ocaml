(*Strictly infinite series*)
type 'a inf_seq = Seq of 'a * (unit -> 'a inf_seq)

let rec inf_map f = function
  | Seq (x, seq) -> Seq(f x, ( fun () -> inf_map f (seq()) ))

let rec inf_map2 f = function
  | Seq (x, seq) -> Seq(x, (fun () -> inf_map f (inf_map2 f (seq())) ))

let rec interleave l1 l2 = match l1, l2 with
  | Seq (x, seq1), seq2 -> Seq ( x, fun () -> interleave seq2 (seq1()) )

let rec from k = Seq (k, (fun () -> from (k+1) ))

let next = function
  | Seq (x, seq) -> seq ()

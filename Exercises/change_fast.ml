(* Finds all ways of making change using coins in till *)
let change till amt = 
  let rec aux till amt cur acc = match till, amt with
    | _, 0 -> cur::acc
    | [], _ -> acc
    | c::cs, amt when c<= amt -> (* Return all solutions with and without coin *)
                             let without_c = aux cs amt cur acc in 
                             aux till (amt-c) (c::cur) without_c
    | _::cs, amt -> (* Coin too large find solutions without coin *)
                    aux cs amt cur acc
  in aux till amt [] []
;;
(* Till must be sorted descending *)
change [5;2;1] 15

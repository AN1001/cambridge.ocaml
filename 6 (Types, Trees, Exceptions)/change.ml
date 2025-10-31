exception CannotMake

let rec change till amt =
  match till,amt with
    | [],_ -> raise CannotMake
    | _,0 -> []
    | coin::reduced_till,amt -> if coin > amt then change reduced_till amt
                                else try coin::(change till (amt - coin))
                                with CannotMake -> change reduced_till amt
;;
(*Example case*)
change [5;2] 6

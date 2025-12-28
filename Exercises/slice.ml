exception Slice_index_error of string
let slice l i k = 
  let rec get_nth n = function
    | [] -> raise (Slice_index_error "Slice start index out of range")
    | x::xs -> if n=0 then x::xs else get_nth (n-1) xs
  in
  let rec get_n n acc = function
    | [] -> raise (Slice_index_error "Slice end index out of range")
    | x::xs -> if n=0 then (List.rev (x::acc)) else get_n (n-1) (x::acc) xs
  in
  get_n (k-i) [] (get_nth i l)
;;

slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6

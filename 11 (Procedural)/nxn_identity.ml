let nxn_identity n = Array.init n (fun i -> Array.init n (fun j -> if i=j then 1 else 0));;
(*Example Case*)
nxn_identity 3

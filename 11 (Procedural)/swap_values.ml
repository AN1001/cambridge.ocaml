(*Swaps the values of two references*)
let swap_values xr yr = let buffer = !xr in xr := !yr; yr := buffer;

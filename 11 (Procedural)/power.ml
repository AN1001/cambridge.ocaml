(*An implementation of power using while loops instead of recursion*)
let power x n = 
  let power_left = ref n in
  let ans = ref x in
  while !power_left != 1 do
    if !power_left mod 2 = 0 then
      ans := !ans * !ans
    else
      ans := x * !ans * !ans;
    power_left := !power_left / 2;
  done;
  !ans
;;

(* Groups list into disjoint subsets as specified *)
let rec extract_rem n = function
  | unused when n=0 -> [([], unused)]
  | [] -> []
  | x::xs -> List.map (fun (h,t) -> (x::h, t)) (extract_rem (n-1) xs)
             @ List.map (fun (h,t) -> (h, x::t)) (extract_rem n xs)

let rec group els = function
  | [] -> [[]]
  | s::ss -> List.concat_map (fun (el,rem) -> List.map (fun iel -> el::iel) (group rem ss)) (extract_rem s els)

;;
group ["a"; "b"; "c"; "d"] [2; 1]

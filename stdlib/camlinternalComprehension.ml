
let map = List.map

let map_from_to ~f from to_ =
  let rec loop f from to_ res =
    if to_ < from 
    then res 
    else loop f from (to_ - 1) ((f to_)::res)
  in
  loop f from to_ []
;;

let map_from_downto f from to_ =
  let rec loop f from to_ res =
    if to_ > from 
    then res 
    else loop f from (to_ + 1) ((f to_)::res)
  in
  loop f from to_ [] 
;;

let array_map = Array.map

let array_map_from_to f from t =
  Array.init (t - from + 1) (fun i -> f ( i + from))
;;

let array_map_from_downto f from t =
  Array.init (from - t + 1) (fun i -> f ( from - i))
;;

(* Definitions for recursive usage.*)
(*
  TODO: Change 
*)
let concat_map ~f l = 
  List.concat (List.map f l)
;;

let concat_map_from_to ~f from to_ = 
  List.concat (map_from_to f from to_ )
;;

let concat_map_from_downto ~f from to_ = 
  List.concat (map_from_downto f from to_ )
;;

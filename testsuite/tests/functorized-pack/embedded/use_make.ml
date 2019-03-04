module M = Make(struct type t = int let v = 5 end)

module N = M.N(struct type t = int let v = 10 end)

let _ =
  let m1_v = M.M.M1.v in
  Printf.printf "M.M.M1.v: %d\n%!" m1_v;
  let m2_v = M.M.M2.v in
  Printf.printf "M.M.M2.v: %d\n%!" m2_v;
  let n_v = N.v in
  Printf.printf "M.N.v: %d\n%!" n_v;

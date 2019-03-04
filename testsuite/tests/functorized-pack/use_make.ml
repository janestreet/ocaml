module M = Make(struct let v = 5 end)

let _ =
  let m1 = M.M1.v in
  let M.M1.M1 v1 = m1 in
  Format.printf "v1 = %d\n%!" v1;
  let m2 = M.M2.v in
  let m2f = m2.M.M2.v in
  let v2 =
    match m2f with None -> assert false
                 | Some M.M1.M1 v -> v in
  Format.printf "v2 = %d\n%!" v2;
  let M.M1.M1 v3, _ = M.M3.v in
  Format.printf "v3 = %d\n%!" v3

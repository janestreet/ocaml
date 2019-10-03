let some x = Some x;;
let a = some (fun x -> x);;

let r = ref;;
let c = ref (fun x -> x);;

type 'a cell = {get: unit -> 'a; set: 'a -> unit};;
let mkcell x = let r = ref x in {get=(fun() -> !r);set=(:=) r};;

let f (mkc : unit -> _ cell) =
  let id = ignore (mkc ()); fun x -> x in
  id true, id 1;;

let p = f mkcell;;

(* Should fail *)
let p' =
  let id = ignore (mkcell ()); fun x -> x in
  id true, id 1;;

(* Let-reduction fails *)
let f2 (mkc : unit -> _ cell) =
  let (_,id) = (mkc (), fun x -> x) in
  id true, id 1;;

let p2 = f2 mkcell;;

(* Fails: cannot generalize because mkcell is impure  *)
let p2 =
  let (_,id) = (mkcell (), fun x -> x) in
  id true, id 1;;

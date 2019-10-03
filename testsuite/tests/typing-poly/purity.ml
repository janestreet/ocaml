(* TEST
   * expect
*)

let some x = Some x;;
let a = some (fun x -> x);;
[%%expect{|
val some : 'a -> 'a option [@@pure] = <fun>
val a : ('a -> 'a) option [@@pure] = Some <fun>
|}]

let r = ref;;
let c = r (fun x -> x);;
let c' = {contents=fun x -> x};;
[%%expect{|
val r : 'a -> 'a ref = <fun>
val c : ('_weak1 -> '_weak1) ref = {contents = <fun>}
val c' : ('_weak2 -> '_weak2) ref = {contents = <fun>}
|}]

type 'a cell = {get: unit -> 'a; set: 'a -> unit};;
let mkcell x = let r = ref x in {get=(fun() -> !r);set=(:=) r};;
[%%expect{|
type 'a cell = { get : unit -> 'a; set : 'a -> unit; }
val mkcell : 'a -> 'a cell = <fun>
|}]

let f (mkc : unit -> _ cell) =
  let id = ignore (mkc ()); fun x -> x in
  id true, id 1;;

let p = f mkcell;;
[%%expect{|
val f : (unit -> 'a cell) -> bool * int [@@pure] = <fun>
val p : bool * int = (true, 1)
|}]

(* left of ; is ignored *)
let p' =
  let id = ignore (mkcell ()); fun x -> x in
  id true, id 1;;
[%%expect{|
val p' : bool * int [@@pure] = (true, 1)
|}]

(* Let-reduction fails *)
let f2 (mkc : unit -> _ cell) =
  let (_,id) = (mkc (), fun x -> x) in
  id true, id 1;;

let p2 = f2 mkcell;;
[%%expect{|
val f2 : (unit -> 'a cell) -> bool * int [@@pure] = <fun>
val p2 : bool * int = (true, 1)
|}]

(* Fails: cannot generalize because mkcell is impure  *)
let p2' =
  let (_,id) = (mkcell (), fun x -> x) in
  id true, id 1;;
[%%expect{|
Line 3, characters 14-15:
3 |   id true, id 1;;
                  ^
Error: This expression has type int but an expression was expected of type
         bool
|}]


(* Subtle case *)

type mkref = {mkref: 'a. 'a -> 'a ref};;

let f x = let r = x.mkref [] in fun y -> r := [y]; List.hd !r;;
[%%expect{|
type mkref = { mkref : 'a. 'a -> 'a ref; }
val f : mkref -> 'a -> 'a = <fun>
|}]

(* must fail too *)
let f {mkref} = let r = mkref [] in fun y -> r := [y]; List.hd !r;;
[%%expect{|
val f : mkref -> 'a -> 'a = <fun>
|}]

let rec f x = let r = mkref x [] in fun y -> r := [y]; List.hd !r
and mkref : 'a. mkref -> 'a -> 'a ref = fun x -> x.mkref;;
[%%expect{|
val f : mkref -> 'a -> 'a = <fun>
val mkref : mkref -> 'a -> 'a ref = <fun>
|}]

(* Modules *)

module type T = sig
  val some : 'a -> 'a option [@@pure]
end;;

module M : T = struct let some x = Some x end;;
[%%expect{|
module type T = sig val some : 'a -> 'a option [@@pure] end
module M : T
|}]

(* fails *)
module M' : T = struct let some x = let r = ref x in Some !r end;;
[%%expect{|
Line 1, characters 16-64:
1 | module M' : T = struct let some x = let r = ref x in Some !r end;;
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig val some : 'a -> 'a option end
       is not included in
         T
       Values do not match:
         val some : 'a -> 'a option
       is not included in
         val some : 'a -> 'a option [@@pure]
|}]

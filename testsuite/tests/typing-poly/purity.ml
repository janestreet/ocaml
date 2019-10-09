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

(* Failure of subject reduction *)
(* Relaxed value already made it fail, but it was for internal reduction rules.
   See J. Garrigue, Relaxing the value restrion, FLOPS 2004 for an example *)

let id x = x
let k x y = x;;
[%%expect{|
val id : 'a -> 'a [@@pure] = <fun>
val k : 'a -> 'b -> 'a [@@pure] = <fun>
|}]

let p = (fun r -> let id' = k id r in (id' 1, id' true)) ref;;
[%%expect{|
val p : int * bool = (1, true)
|}]
let p' = let id' = k id ref in (id' 1, id' true);;
[%%expect{|
Line 1, characters 43-47:
1 | let p' = let id' = k id ref in (id' 1, id' true);;
                                               ^^^^
Error: This expression has type bool but an expression was expected of type
         int
|}]
let p2 = let r = ref in let id' = k id r in (id' 1, id' true);;
[%%expect{|
Line 1, characters 56-60:
1 | let p2 = let r = ref in let id' = k id r in (id' 1, id' true);;
                                                            ^^^^
Error: This expression has type bool but an expression was expected of type
         int
|}]
let p3 = let id' = ignore ref; id in (id' 1, id' true);;
[%%expect{|
val p3 : int * bool [@@pure] = (1, true)
|}]

(* Must carefuly track polymorphism to keep soundness *)

type mkref = {mkref: 'a. 'a -> 'a ref};;

(* Accessing a polymorphic field is impure *)
let f x = let r = x.mkref [] in fun y -> r := [y]; List.hd !r;;
[%%expect{|
type mkref = { mkref : 'a. 'a -> 'a ref; }
val f : mkref -> 'a -> 'a = <fun>
|}]

(* Shouldn't generalize mkref here either *)
let f {mkref} = let r = mkref [] in fun y -> r := [y]; List.hd !r;;
[%%expect{|
val f : mkref -> 'a -> 'a = <fun>
|}]

(* Beware of polymorphic recursion too *)
let rec f x = let r = mkref x [] in fun y -> r := [y]; List.hd !r
and mkref : 'a. mkref -> 'a -> 'a ref = fun x -> x.mkref;;
[%%expect{|
val f : mkref -> 'a -> 'a = <fun>
val mkref : mkref -> 'a -> 'a ref = <fun>
|}]

(* Limitation on recursion *)
(* recursive call to id impure due to polymorphism *)
let rec id : 'a. 'a -> 'a = fun x -> id x;;
[%%expect{|
val id : 'a -> 'a = <fun>
|}]
(* ok if not called *)
let rec id : 'a. 'a -> 'a = fun x -> x;;
[%%expect{|
val id : 'a -> 'a [@@pure] = <fun>
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


(* Classes *)

let k x y = x;;
class c (x : int) =
  let id x = x in
  object method m = let id = k id x in (id 1, id true) end;;
[%%expect]
class d (x : int) =
  let id x = x in
  object val x = x method m = let id = k id x in (id 1, id true) end;;
[%%expect]

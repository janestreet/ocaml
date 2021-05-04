(* TEST
   * expect
*)

let leak n =
  let[@stack] r = ref n in
  r
[%%expect{|
Line 3, characters 2-3:
3 |   r
      ^
Error: The value r is local, so cannot be used here as it might escape
|}]

external idint : (int[@stack]) -> int = "%identity"
[%%expect{|
external idint : (int [@stack]) -> int = "%identity"
|}]

let noleak n =
  let[@stack] r = ref n in
  idint (r.contents)
[%%expect{|
val noleak : int -> int = <fun>
|}]  


(* This will be typeable eventually, once label mutability affects mode *)
let (!) : ('a ref[@stack]) -> 'a =
  fun r -> r.contents
[%%expect{|
Line 2, characters 11-12:
2 |   fun r -> r.contents
               ^
Error: The value r is local, so cannot be used here as it might escape
|}]



(*
 * Type equalities of function types
 *)

module Equ = struct
  type ('a, 'b) fn = 'a -> 'b
  type ('a, 'b) fn_sarg = ('a[@stack]) -> 'b
  type ('a, 'b) fn_sret = 'a -> ('b[@stackret])
  type ('a, 'b) fn_sargret = ('a[@stack]) -> ('b[@stackret])

  (* When a [@stack] argument appears in a function type with multiple arguments,
     return modes are implicitly [@stackret] until the final argument. *)
  type ('a, 'b, 'c, 'd, 'e) fn5 =
    'a -> ('b[@stack]) -> 'c -> 'd -> 'e

  type ('a,'b,'c,'d,'e) equ_fn5 = unit
    constraint
      ('a, 'b, 'c, 'd, 'e) fn5
      =
      ('a, ('b, ('c, ('d, 'e) fn) fn_sret) fn_sargret) fn
end
[%%expect{|
module Equ :
  sig
    type ('a, 'b) fn = 'a -> 'b
    type ('a, 'b) fn_sarg = ('a [@stack]) -> 'b
    type ('a, 'b) fn_sret = 'a -> ('b [@stackret])
    type ('a, 'b) fn_sargret = ('a [@stack]) -> ('b [@stackret])
    type ('a, 'b, 'c, 'd, 'e) fn5 =
        'a -> ('b [@stack]) -> ('c -> ('d -> 'e [@stackret]) [@stackret])
    type ('a, 'b, 'c, 'd, 'e) equ_fn5 = unit
  end
|}]

type distinct_sarg = unit constraint (int,int) Equ.fn_sarg = int -> int
[%%expect{|
Line 1, characters 37-71:
1 | type distinct_sarg = unit constraint (int,int) Equ.fn_sarg = int -> int
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
Type (int, int) Equ.fn_sarg = (int [@stack]) -> int
is not compatible with type int -> int
|}]
type distinct_sret = unit constraint (int,int) Equ.fn_sret = int -> int
[%%expect{|
Line 1, characters 37-71:
1 | type distinct_sret = unit constraint (int,int) Equ.fn_sret = int -> int
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
Type (int, int) Equ.fn_sret = int -> (int [@stackret])
is not compatible with type int -> int
|}]
type distinct_sarg_sret = unit constraint (int,int) Equ.fn_sarg = (int, int) Equ.fn_sargret
[%%expect{|
Line 1, characters 42-91:
1 | type distinct_sarg_sret = unit constraint (int,int) Equ.fn_sarg = (int, int) Equ.fn_sargret
                                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
Type (int, int) Equ.fn_sarg = (int [@stack]) -> int
is not compatible with type
  (int, int) Equ.fn_sargret = (int [@stack]) -> (int [@stackret])
|}]



(*
 * Curried functions and partial application
 *)

(* f4 results in a local value if it is partially applied to two or
   three arguments, because it closes over the locally-allocated
   second argument. Applications to 1 or 4 arguments are not local. *)
(* FIXME: the printed types, while correct, are unnecessarily ugly *)
let f4 : int -> ('a[@stack]) -> int -> int -> int =
  fun a _ b c -> a + b + c
[%%expect{|
val f4 :
  int -> ('a [@stack]) -> (int -> (int -> int [@stackret]) [@stackret]) =
  <fun>
|}]

let apply1 x = f4 x
[%%expect{|
val apply1 :
  int -> ('a [@stack]) -> (int -> (int -> int [@stackret]) [@stackret]) =
  <fun>
|}]
let apply2 x = f4 x x
[%%expect{|
Line 1, characters 15-21:
1 | let apply2 x = f4 x x
                   ^^^^^^
Error: This locally-allocated return value escapes
|}]
let apply3 x = f4 x x x
[%%expect{|
Line 1, characters 15-23:
1 | let apply3 x = f4 x x x
                   ^^^^^^^^
Error: This locally-allocated return value escapes
|}]
let apply4 x =
  f4 x x x x
[%%expect{|
val apply4 : int -> int = <fun>
|}]

(* Partial applications of two or three arguments are OK if bound locally *)
let apply2_stack x =
  let[@stack] g = f4 x x in
  g x x
let apply3_stack x =
  let[@stack] g = f4 x x x in
  g x
[%%expect{|
val apply2_stack : int -> int = <fun>
val apply3_stack : int -> int = <fun>
|}]

(*
 * Overapplication (functions that return functions)
 *)

let f : ('a[@stack]) -> int -> int = fun _ x -> x
let g : ('a[@stack]) -> int -> _ = fun _ x -> f
[%%expect{|
val f : ('a [@stack]) -> (int -> int [@stackret]) = <fun>
val g :
  ('a [@stack]) ->
  (int -> ('b [@stack]) -> (int -> int [@stackret]) [@stackret]) = <fun>
|}]
let apply1 x = g x
[%%expect{|
Line 1, characters 15-18:
1 | let apply1 x = g x
                   ^^^
Error: This locally-allocated return value escapes
|}]
let apply2 x = g x x
[%%expect{|
val apply2 : int -> ('a [@stack]) -> (int -> int [@stackret]) = <fun>
|}]
let apply3 x = g x x x
[%%expect{|
Line 1, characters 15-22:
1 | let apply3 x = g x x x
                   ^^^^^^^
Error: This locally-allocated return value escapes
|}]
let apply4 x = g x x x x
[%%expect{|
val apply4 : int -> int = <fun>
|}]

(* FIXME: overapplication with reordered labels is probably wrong *)



(*
 * Closures and context locks
 *)

let heap_closure () =
  let[@stack] foo = ref 1 in
  let fn () =
    let[@stack] fn2 () =
      let[@stack] _baz = foo in
      () in
    fn2 () in
  fn ()

[%%expect{|
Line 5, characters 25-28:
5 |       let[@stack] _baz = foo in
                             ^^^
Error: The value foo is local, so cannot be used inside a closure that might escape
|}]

let local_closure () =
  let[@stack] foo = ref 1 in
  let[@stack] fn () =
    let[@stack] fn2 () =
      let[@stack] _baz = foo in
      () in
    fn2 () in
  fn ()

[%%expect{|
val local_closure : unit -> unit = <fun>
|}]

(*
 * Higher order functions, with arguments that promise not to leak
 *)

let use_locally (f : ('a[@stack]) -> 'a) (x : 'a) = f x
(* This version also promises not to leak the closure *)
let use_locally' : (('a [@stack]) -> 'a [@stack]) -> 'a -> 'a = fun f x -> f x
[%%expect{|
val use_locally : (('a [@stack]) -> 'a) -> 'a -> 'a = <fun>
val use_locally' : (('a [@stack]) -> 'a [@stack]) -> ('a -> 'a [@stackret]) =
  <fun>
|}]

let no_leak = use_locally (fun x -> 1) 42
let no_leak' = use_locally' (fun x -> 1) 42
[%%expect{|
val no_leak : int = 1
val no_leak' : int = 1
|}]

let leak_id =
  use_locally (fun x -> x) 42
[%%expect{|
Line 2, characters 24-25:
2 |   use_locally (fun x -> x) 42
                            ^
Error: The value x is local, so cannot be used here as it might escape
|}]

let leak_ref =
  let r = ref None in
  use_locally (fun x -> r.contents <- Some x; x) 42

[%%expect{|
Line 3, characters 43-44:
3 |   use_locally (fun x -> r.contents <- Some x; x) 42
                                               ^
Error: The value x is local, so cannot be used here as it might escape
|}]

let leak_ref_2 =
  let[@stack] r = ref None in
  use_locally (fun x -> let[@stack] _ = r in r.contents <- Some x; x) 42
[%%expect{|
Line 3, characters 40-41:
3 |   use_locally (fun x -> let[@stack] _ = r in r.contents <- Some x; x) 42
                                            ^
Error: The value r is local, so cannot be used inside a closure that might escape
|}]

let leak_ref_3 =
  let[@stack] r = ref None in
  use_locally' (fun x -> let[@stack] _ = r in r.contents <- Some x; x) 42
[%%expect{|
Line 3, characters 65-66:
3 |   use_locally' (fun x -> let[@stack] _ = r in r.contents <- Some x; x) 42
                                                                     ^
Error: The value x is local, so cannot be used here as it might escape
|}]



let no_leak_exn =
  use_locally (fun x -> let[@stack] _exn = Invalid_argument x in "bluh") "blah"
[%%expect{|
val no_leak_exn : string = "bluh"
|}]
let do_leak_exn =
  use_locally (fun x -> let[@stack] _exn = raise (Invalid_argument x) in "bluh") "blah"

[%%expect{|
Line 2, characters 67-68:
2 |   use_locally (fun x -> let[@stack] _exn = raise (Invalid_argument x) in "bluh") "blah"
                                                                       ^
Error: The value x is local, so cannot be used here as it might escape
|}]

(* same, but this time the function is allowed to return its argument *)
let use_locally (f : ('a[@stack]) -> ('a[@stackret])) : ('a[@stack]) -> ('a[@stackret]) = f
[%%expect{|
val use_locally :
  (('a [@stack]) -> ('a [@stackret])) -> ('a [@stack]) -> ('a [@stackret]) =
  <fun>
|}]

let loc = ((fun x -> x) : (int[@stack]) -> (int[@stackret]))

let no_leak_id = let[@stack] _ = use_locally ((fun x -> x) : (int[@stack]) -> (int[@stackret])) 42 in ()

[%%expect{|
Line 1, characters 11-23:
1 | let loc = ((fun x -> x) : (int[@stack]) -> (int[@stackret]))
               ^^^^^^^^^^^^
Error: This locally-allocated return value escapes
|}]

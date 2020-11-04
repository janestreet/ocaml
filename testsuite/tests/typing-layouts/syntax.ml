(* TEST
   * expect
*)

let f (type t : immediate) (x : t) = x
let rec g : ('a : immediate) ('b : immediate8) ('c : value) . unit -> 'a * 'b * 'c = fun () -> g ()
let h : type t : immediate . t -> t = fun x -> x
external rr : ('a : any_layout) . exn -> 'a = "%raise"
module type T = sig type ('a : immediate) s : immediate val id : ('a : immediate) . 'a -> 'a end
type ('a : immediate, 'b : value, 'c : immediate0) ext = ..
type (_,_,_) ext += Ext : ('a : immediate8) . ('a,_,_) ext
type ('a : immediate) g8 : immediate0 = G8 : ('b : immediate8) . 'b g8
[%%expect{|
val f : ('t : immediate). 't -> 't = <fun>
val g : ('b : immediate8) ('a : immediate). unit -> 'a * 'b * 'c = <fun>
val h : ('t : immediate). 't -> 't = <fun>
external rr : ('a : any_layout). exn -> 'a = "%raise"
module type T =
  sig
    type ('a : immediate) s : immediate
    val id : ('a : immediate). 'a -> 'a
  end
type ('a : immediate, 'b, 'c : immediate0) ext = ..
type (_, _, _) ext +=
    Ext : ('c : immediate0) ('a : immediate8). ('a, 'b, 'c) ext
type ('a : immediate) g8 = G8 : ('b : immediate8). 'b g8
|}]

(* same again, with ugly syntax *)

let f = fun[@ocaml.layout: immediate] (type t) (x : t) -> x
let rec g :
  'a 'b 'c . (unit -> ('a * 'b * 'c))
  [@ocaml.layout: immediate -> immediate8 -> value -> _]
  = fun () -> g ()
let h : 't . ('t -> 't)[@ocaml.layout: immediate -> _] =
  fun[@ocaml.layout :immediate] (type t) -> (fun x -> x : t -> t)
external rr : 'a . (exn -> 'a)[@ocaml.layout: any_layout -> _] = "%raise"
module type T =
  sig
    type 'a s[@@ocaml.layout: immediate -> immediate]
    val id : 'a . ('a -> 'a)[@ocaml.layout: immediate -> _]
  end
type ('a, 'b, 'c) ext = ..
  [@@ocaml.layout: immediate -> value -> immediate0 -> _]
type (_, _, _) ext +=
  | Ext: ('a, _, _) ext [@ocaml.layout: a:immediate8 -> _]
type 'a g8 =
  | G8: 'b g8 [@ocaml.layout: b:immediate8 -> _]
  [@@ocaml.layout: immediate -> immediate0]
[%%expect{|
val f : ('t : immediate). 't -> 't = <fun>
val g : ('b : immediate8) ('a : immediate). unit -> 'a * 'b * 'c = <fun>
val h : ('t : immediate). 't -> 't = <fun>
external rr : ('a : any_layout). exn -> 'a = "%raise"
module type T =
  sig
    type ('a : immediate) s : immediate
    val id : ('a : immediate). 'a -> 'a
  end
type ('a : immediate, 'b, 'c : immediate0) ext = ..
type (_, _, _) ext +=
    Ext : ('c : immediate0) ('a : immediate8). ('a, 'b, 'c) ext
type ('a : immediate) g8 = G8 : ('b : immediate8). 'b g8
|}]

(* TEST
   * native
*)

(* FIXME_layout the output of this test assumes 64-bit words. *)



external unbox32 : int32 -> #int32 = "%identity"
external box32 : #int32 -> int32 = "%identity"
external unbox64 : int64 -> #int64 = "%identity"
external box64 : #int64 -> int64 = "%identity"
external unboxfloat : float -> #float = "%identity"
external boxfloat : #float -> float = "%identity"

type t = { mutable a : #int32; mutable b : #float; mutable c : int; mutable d : #int32; mutable e : #int32 }

let pr { a ; b; c; d; e } =
  Printf.printf "a=%ld b=%.1f c=%d d=%ld e=%ld\n"
    (box32 a) (boxfloat b) c (box32 d) (box32 e)

let make a b c d e =
  { a = unbox32 a; b = unboxfloat b; c; d = unbox32 d; e = unbox32 e }

let accum p q =
  p.a <- unbox32 (Int32.add (box32 p.a) (box32 q.a));
  p.b <- unboxfloat ((boxfloat p.b) +. (boxfloat q.b));
  p.c <- p.c + q.c;
  p.d <- unbox32 (Int32.add (box32 p.d) (box32 q.d));
  p.e <- unbox32 (Int32.add (box32 p.e) (box32 q.e))

let () =
  let p = make 2l 3. 42 5l Int32.min_int in
  let q = make 0l 1. 19 0l Int32.min_int in
  pr p;
  pr q;
  pr { p with d = unbox32 17l };
  accum p q;
  pr p;
  pr q;
  Printf.printf "%d %d\n" (Obj.reachable_words (Obj.repr p)) (Obj.reachable_words (Obj.repr q))

(* FIXME_layout: write a test for the >256word Pduprecord case *)


type ('a : immediate0) pack0 : void
external pack0 : 'a -> 'a pack0 = "%identity"
external unpack0 : 'a pack0 -> 'a = "%identity"

type zr = { zr : unit pack0 }
let zr = { zr = pack0 () }

type (_,_) eq = Refl : ('a, 'a) eq

type ('a,'b) eqf = { equ : ('a, 'b) eq pack0; junk : unit pack0; fl : #float }

let f (type a) (x : (a, float) eqf) : float =
  match unpack0 x.equ with
  | Refl -> boxfloat x.fl

let mkeqf x = { equ = pack0 Refl; junk = pack0 (); fl = unboxfloat x }

let () =
  let x = mkeqf 42.42 in
  Printf.printf "%d %.2f\n" (Obj.reachable_words (Obj.repr x)) (f x)

type ('a : immediate8) pack8 : bits8
external pack8 : 'a -> 'a pack8 = "%identity"
external unpack8 : 'a pack8 -> 'a = "%identity"

type c4 = { a : char pack8; b : char pack8; c : char pack8; d : char pack8 }

let () =
  for i = 0 to 255 do
    let a = Char.chr i in
    let b = Char.chr ((i * 7) land 0xff) in
    let c = Char.chr (i lxor 139) in
    let d = Char.chr ((i * 93) land 0xff) in
    let x = { a = pack8 a; b = pack8 b; c = pack8 c; d = pack8 d } in
    assert (Obj.reachable_words (Obj.repr x) = 2);
    assert ((a,b,c,d) = (unpack8 x.a, unpack8 x.b, unpack8 x.c, unpack8 x.d));
  done


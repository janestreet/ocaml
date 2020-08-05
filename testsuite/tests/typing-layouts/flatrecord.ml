(* TEST
   * native
*)

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

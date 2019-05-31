open Types

(* for typeopt.ml *)
type t =
  | Unavailable
  | This of type_expr
  | Only_on_64_bits of type_expr

val get_unboxed_type_representation: Env.t -> type_expr -> t

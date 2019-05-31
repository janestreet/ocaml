(* TEST *)

(* If we move this to the stdlib, we will need to bootstrap the compiler: *)
module Immediate64 : sig
  (** This module allows to define a type [t] with the [immediate64]
      attribute. i.e. a type that is known to be immediate only on 64
      bits architectures.
       @since 4.09.0 *)

   module type Non_immediate = sig
    type t
  end
  module type Immediate = sig
    type t [@@immediate]
  end

   module Make(Immediate : Immediate)(Non_immediate : Non_immediate) : sig
    type t [@@immediate64]
    type 'a repr =
      | Immediate : Immediate.t repr
      | Non_immediate : Non_immediate.t repr
    val repr : t repr
  end
end = struct
  module type Non_immediate = sig
    type t
  end
  module type Immediate = sig
    type t [@@immediate]
  end

   module Make(Immediate : Immediate)(Non_immediate : Non_immediate) = struct
    type t [@@immediate64]
    type 'a repr =
      | Immediate : Immediate.t repr
      | Non_immediate : Non_immediate.t repr
    external magic : _ repr -> t repr = "%identity"
    let repr =
      if Sys.word_size = 64 then
        magic Immediate
      else
        magic Non_immediate
  end
end

module Int = struct
  type t = int

  let zero = 0
  let one = 1

  let add = (+)
end

module M : sig
  type t [@@immediate64]
  val zero : t
  val one : t
  val add : t -> t -> t
end = struct

   include Immediate64.Make(Int)(Int64)

   module type S = sig
    val zero : t
    val one : t
    val add : t -> t -> t
  end

   let impl : (module S) =
    match repr with
    | Immediate ->
        (module Int : S)
    | Non_immediate ->
        (module Int64 : S)

   include (val impl : S)
end

 let () =
  match Sys.word_size with
  | 64 -> assert (Obj.is_int (Obj.repr M.zero))
  | _  -> assert (Obj.is_block (Obj.repr M.zero))

(* TEST
   * expect
*)

module Variant_and_record_intf = struct
  module M (X : sig
    type 'a t
  end) = struct
    module Record = struct
      type 'a t = 'a X.t
    end
  end

  module type S = sig
    type 'a t
    include (module type of M (struct type 'a rep = 'a t type 'a t = 'a rep end))
  end
end

module rec Typerep : sig
  type _ t =
    | Int        : int t
    | Ref        : 'a t -> 'a ref t
    | Function   : ('dom t * 'rng t) -> ('dom -> 'rng) t
    | Tuple      : 'a Typerep.Tuple.t -> 'a t
    | Record     : 'a Typerep.Record.t -> 'a t

  module Tuple : sig
    type _ t = T2 : ('a Typerep.t * 'b Typerep.t) -> ('a * 'b) t
    val arity : _ t -> int
  end

  include Variant_and_record_intf.S with type 'a t := 'a Typerep.t

end = struct
  type _ t =
    | Int : int t
    | Ref : 'a t -> 'a ref t
    | Function : ('dom t * 'rng t) -> ('dom -> 'rng) t
    | Tuple : 'a Typerep.Tuple.t -> 'a t
    | Record : 'a Typerep.Record.t -> 'a t

  module Tuple = struct
    type _ t = T2 : ('a Typerep.t * 'b Typerep.t) -> ('a * 'b) t
    let arity (type a) (T2 _ : a t) = 2
  end

  include Variant_and_record_intf.M (struct type 'a rep = 'a t type 'a t = 'a rep end)
end
[%%expect{|
module Variant_and_record_intf :
  sig
    module M :
      functor (X : sig type 'a t end) ->
        sig module Record : sig type 'a t = 'a X.t end end
    module type S =
      sig type 'a t module Record : sig type 'a t = 'a t end end
  end
module rec Typerep :
  sig
    type _ t =
        Int : int t
      | Ref : 'a t -> 'a ref t
      | Function : ('dom t * 'rng t) -> ('dom -> 'rng) t
      | Tuple : 'a Typerep.Tuple.t -> 'a t
      | Record : 'a Typerep.Record.t -> 'a t
    module Tuple :
      sig
        type _ t = T2 : ('a Typerep.t * 'b Typerep.t) -> ('a * 'b) t
        val arity : 'a t -> int
      end
    module Record : sig type 'a t = 'a Typerep.t end
  end
|}]


module type S_indexed = sig
  type 'a t
  val ignore_m : unit t
end

module type T = S_indexed with type 'a t := 'a
[%%expect{|
module type S_indexed = sig type 'a t val ignore_m : unit t end
module type T = sig val ignore_m : unit end
|}]


module F (X : sig type 'a t end) = struct
  module type S = sig
    type t
    val compute : t X.t
  end
end

module M = struct
  type 'a computation
  include F (struct type 'a t = 'a computation end)
end
[%%expect{|
module F :
  functor (X : sig type 'a t end) ->
    sig module type S = sig type t val compute : t X.t end end
module M :
  sig
    type 'a computation
    module type S = sig type t val compute : t computation end
  end
|}]

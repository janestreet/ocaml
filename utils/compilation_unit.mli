type error =
    Invalid_character of char
  | Bad_compilation_unit_name of string

exception Error of error

module Name : sig
  (** The name of a compilation unit, without any "-for-pack" prefix. *)

  type t

  (** Printing, comparison, sets, maps, etc. *)
  include Identifiable.S with type t := t

  val dummy: t
  (** [dummy] is a placeholder for units that does not have a valid name, as the
     toplevel or the current unit ar the initialization of the compiler. It is
     not a valid identifier, thus cannot be generated through [of_string]. *)

  val of_string : string -> t
  (** [of_string s] checks the given module name is a valid compilation unit
      and generates its representation. *)

  val to_string : t -> string

end

module Prefix : sig

  type component = Pack of Name.t * Name.t list

  type t = component list

  val equal_component : component -> component -> bool

  (** Printing, comparison, sets, maps, etc. *)
  include Identifiable.S with type t := t

  val parse_for_pack : string option -> t
  (** [parse_for_pack p] returns the list of nested packed modules from a
      `-for-pack` argument.*)

  val extract_prefix : string -> t * Name.t
  (** [extract_prefix id] returns the prefix of an identifier and its basename,
      as if it was generated with `-for-pack` *)

  val to_string: t -> string

  val for_address: t -> string
  (** [for_address p] generates a string representation of a prefix stripped of
      the functor(s) parameter(s) *)

  val in_functor: t -> bool

  val in_common_functor: t -> t -> bool

  val in_functor_parameters: Name.t -> t -> bool

end

(** The name of a "compilation unit" along with any "-for-pack" prefix that
    was specified when the unit was compiled.  By "compilation unit" we
    mean the code and data associated with the compilation of a single .ml
    source file: that is to say, file-level entities having OCaml semantics.
    The notion neither includes the special "startup" files nor external
    libraries.

    The [for_pack_prefix] is specified with the oldest pack at the tail of
    the list. *)
type t

(** Create a compilation unit with the given [name] (which is not encoded or
    mangled in any way). *)
val create : ?for_pack_prefix:Prefix.t -> Name.t -> t

(** Create a compilation unit form the given [name] (which is not encoded or
    mangled in any way). The `-for-pack` of prefix is extracted if there is
    any. *)
val of_raw_string : string -> t

(** A distinguished compilation unit for initialisation of mutable state. *)
val none : t

(** The name of the compilation unit, excluding any [for_pack_prefix]. *)
val name : t -> Name.t

(** The [for_pack_prefix] specified to [create] for the given compilation
    unit. *)
val for_pack_prefix : t -> Prefix.t

(** Returns [true] iff the given compilation unit has a non-empty
    [for_pack_prefix]. *)
val is_packed : t -> bool


(** Print only the name of the given compilation unit. *)
val print_name : Format.formatter -> t -> unit

(** Returns the full path of the compilation unit, with the basename of the unit
    as the last component of the prefix. *)
val full_path : t -> Prefix.t

(** Print the full path of the compilation unit, with a dot between each
    pair of components. *)
val print_full_path : Format.formatter -> t -> unit

(** Returns the full path of the compilation unit, as a string. *)
val full_path_as_string : t -> string

val for_address : t -> string

type crcs = (t * Digest.t option) list

(** Printing, comparison, sets, maps, etc. *)
include Identifiable.S with type t := t

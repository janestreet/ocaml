(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Xavier Leroy, projet Gallium, INRIA Rocquencourt                     *)
(*   Gabriel Scherer, projet Parsifal, INRIA Saclay                       *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc

module Consistbl : module type of struct
  include Consistbl.Make (Compilation_unit)
end

type error =
  | Illegal_renaming of
      Compilation_unit.Name.t * Compilation_unit.Name.t * filepath
  | Inconsistent_import of Compilation_unit.Name.t * filepath * filepath
  | Need_recursive_types of Compilation_unit.Name.t
  | Depend_on_unsafe_string_unit of Compilation_unit.Name.t
  | Inconsistent_package_declaration of
      { imported_unit: Compilation_unit.Name.t;
        filename: filepath;
        prefix: Compilation_unit.Prefix.t;
        current_pack: Compilation_unit.Prefix.t }
  | Inconsistent_package_import of filepath * Compilation_unit.Name.t
  | Illegal_import_of_parameter of Compilation_unit.Name.t * filepath

exception Error of error

val report_error: Format.formatter -> error -> unit

(* Remember the current compilation unit. If no prefix is given, it is infered
   from the `-for-pack` CLI argument. Returns "" if outside a compilation unit.
*)
module Current_unit : sig
  (** Get the value of type [Compilation_unit.t] corresponding to the current
      unit being compiled.  An exception will be raised if [set] has not
      previously been called. *)
  val get : unit -> Compilation_unit.t option

  (** Get the value of type [Compilation_unit.t] corresponding to the current
     unit being compiled.  An exception will be raised if [set] has not
     previously been called. *)
  val get_exn : unit -> Compilation_unit.t

  (** Get the value of type [Ident.t] corresponding to the current unit being
     compiled.  An exception will be raised if [set] has not previously been
     called. *)
  val get_id_exn : unit -> Ident.t

  (** [set ~prefix name] Record that the compilation unit being currently
     compiled has name [name] and prefix [prefix]. If no prefix is given, it is
     infered from the `-for-pack` command line. *)
  val set : ?prefix:Compilation_unit.Prefix.t -> Compilation_unit.Name.t -> unit

  (** Record that the given value of type [Compilation_unit.t] is that of the
      current unit being compiled. *)
  val set_unit : Compilation_unit.t -> unit

  (** Check that name given corresponds to the compilation unit being currently
     compiled. *)
  val is : Compilation_unit.Name.t -> bool

  (** Check that the unit given is the one being currently compiled. *)
  val is_unit_exn : Compilation_unit.t -> bool

  (** Check that the name given corresponds to the compilation unit being
      compiled. *)
  val is_name_of : string -> bool

  (** Check that the identifier given corresponds to the compilation unit being
      compiled. *)
  val is_ident_name_of : Ident.t -> bool
end

module Persistent_interface : sig
  type t =
    { filename : string; (** Name of the file containing the signature. *)
      cmi : Cmi_format.cmi_infos }

  (** Function used to load a persistent signature. The default is to look for
      the .cmi file in the load path. This function can be overridden to load
      it from memory, for instance to build a self-contained toplevel. *)
  val load : (unit_name:Compilation_unit.Name.t -> t option) ref
end

type can_load_cmis =
  | Can_load_cmis
  | Cannot_load_cmis of Misc.EnvLazy.log

type 'a t

val empty : unit -> 'a t

val clear : 'a t -> unit
val clear_missing : 'a t -> unit

val fold : 'a t -> (Compilation_unit.Name.t -> 'a -> 'b -> 'b) -> 'b -> 'b

val read : 'a t -> (Persistent_interface.t -> 'a)
  -> Compilation_unit.Name.t -> filepath -> 'a
val find : 'a t -> (Persistent_interface.t -> 'a)
  -> Compilation_unit.Name.t -> 'a

val find_in_cache : 'a t -> Compilation_unit.Name.t -> 'a option

val check : 'a t -> (Persistent_interface.t -> 'a)
  -> loc:Location.t -> Compilation_unit.Name.t -> unit

(* Similar to [read], but does not consider the module as imported *)
val read_as_parameter :
  'a t -> (Persistent_interface.t -> 'a) -> Compilation_unit.Name.t
  -> Persistent_interface.t option

(* [looked_up penv md] checks if one has already tried
   to read the signature for [md] in the environment
   [penv] (it may have failed) *)
val looked_up : 'a t -> Compilation_unit.Name.t -> bool

(* [is_imported penv unit] checks if [unit] has been succesfully
   imported in the environment [penv] *)
val is_imported : 'a t -> Compilation_unit.t -> bool

(* [is_imported_opaque penv md] checks if [md] has been imported
   in [penv] as an opaque module *)
val is_imported_opaque : 'a t -> Compilation_unit.Name.t -> bool

(* [is_imported_parameter penv md] checks if [md] has been imported
   in [penv] as a functor parameter *)
val is_imported_as_parameter : 'a t -> Compilation_unit.Name.t -> bool

(* [is_imported_from_functorized_pack penv md] checks if [md] has been imported
   in [penv] as a unit from the same functorized pack as the current one *)
val is_imported_from_functorized_pack : 'a t -> Compilation_unit.Name.t -> bool

(* [functorized_pack_component_address penv md] returns the local identifier
   generated for [md] as argument for the functor *)
val functorized_pack_component_id : 'a t -> Compilation_unit.Name.t -> Ident.t

val make_cmi : 'a t -> Compilation_unit.Name.t -> Types.compilation_unit -> alerts
  -> Cmi_format.cmi_infos

val save_cmi : 'a t -> Persistent_interface.t -> 'a -> unit

val can_load_cmis : 'a t -> can_load_cmis
val set_can_load_cmis : 'a t -> can_load_cmis -> unit
val without_cmis : 'a t -> ('b -> 'c) -> 'b -> 'c
(* [without_cmis penv f arg] applies [f] to [arg], but does not
    allow [penv] to openi cmis during its execution *)

(* may raise Consistbl.Inconsistency *)
val import_crcs : 'a t -> source:filepath -> Compilation_unit.crcs -> unit

(* Return the set of compilation units imported, with their CRC *)
val imports : 'a t -> Compilation_unit.crcs

(* Return the set of compilation units imported from the same functorized pack
   than the current one, with their local identifier *)
val imports_from_functorized_pack : 'a t -> (Compilation_unit.t * Ident.t) list

(* Return the CRC of the interface of the given compilation unit *)
val crc_of_unit:
  'a t -> (Persistent_interface.t -> 'a) -> Compilation_unit.Name.t -> Digest.t

(* Forward declaration to break mutual recursion with Typecore. *)
val add_delayed_check_forward: ((unit -> unit) -> unit) ref

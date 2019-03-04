(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2010 Institut National de Recherche en Informatique et     *)
(*     en Automatique                                                     *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Compilation-related state that is accumulated prior to being written
    into .cmx files.

    This module forms part of the "middle end" of the compiler.  It is not
    used from [Cmmgen] onwards.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Functions in [Closure_only] are exclusively for use by the original
    "Closure" middle-end. *)
module Closure_only : sig
  (** Given a global identifier that occurs somewhere in the program (without
      knowing the "-for-pack" prefix of its compilation unit), determine
      the approximation for such identifier. *)
  val global_approx : Ident.t -> Clambda.value_approximation

  (** Record the approximation of the unit being compiled. *)
  val set_global_approx : Clambda.value_approximation -> unit

  (** Record the current approximation for the current toplevel phrase.
      This is used by the native toplevel. *)
  val record_global_approx_toplevel : unit -> unit

  (** Record that the given symbol is that of a constant which is to be
      accessible from other units. *)
  val add_exported_constant : Symbol.t -> unit

  type structured_constants

  (** Record the current list of structured constants to be statically
      allocated (c.f. [structured_constants], below). *)
  val snapshot : unit -> structured_constants

  (** Return the list of structured constants to be statically allocated
      to the given earlier state. *)
  val backtrack : structured_constants -> unit

  (** Record a constant to be statically allocated, assigning a symbol for it
      in the process.  If [shared] is [true], the constant can be shared with
      another structurally-equal constant. *)
  val new_structured_constant
     : Clambda.ustructured_constant
    -> shared:bool
    -> Symbol.t

  (** All structured constants to be statically allocated. *)
   val structured_constants : unit -> Clambda.preallocated_constant list

  (** Reset the list of structured constants to be statically allocated. *)
  val clear_structured_constants : unit -> unit

  val structured_constant_of_symbol:
    Symbol.t -> Clambda.ustructured_constant option
end

(** Functions in [Flambda_only] are exclusively for use by the Flambda
    middle-end. *)
module Flambda_only : sig
  (** Record the export information for the unit being compiled. *)
  val set_export_info : Export_info.t -> unit

  (** Retrieve the export information for the given compilation unit,
      if possible.  (It may not be possible e.g. because a .cmx file is
      missing; in such cases, [None] is returned.) *)
  val export_info_for_unit : Compilation_unit.t -> Export_info.t option

  (** Return all the information loaded from external compilation units. *)
  val merged_export_info : unit -> Export_info.t

  (** Table recording sets of closures imported from .cmx files. *)
  val imported_sets_of_closures_table
    : Simple_value_approx.function_declarations option Set_of_closures_id.Tbl.t
end

type compilation_unit_or_predef = private
  | Compilation_unit of Compilation_unit.t
  | Predef

(** Return the [Compilation_unit.t] in which the given [Ident.t] (which must be
    persistent or predef) is defined. *)
val compilation_unit_for_global : Ident.t -> compilation_unit_or_predef

(** Enter the given info in the cache. The info will be returned by
    [symbol_for_global] and [global_approx] without looking at the
    corresponding .cmx file. *)
val cache_unit_info : Cmx_format.Unit_info.t -> unit

(** Note that the given global identifier must be linked. *)
val require_global : Ident.t -> unit

module Snapshot : sig
  type export_info = private
    | Closure of Clambda.value_approximation
    | Flambda of Export_info.t

  type t = private {
    unit : Compilation_unit.t;
    (** Name of unit implemented, along with any -for-pack prefix *)
    defines : Compilation_unit.t list;
    (** (Sub-)units implemented, in initialisation order, with the first to be
        initialised at the head of the list. *)
    imports_cmx : Digest.t option Compilation_unit.Map.t;
    (** Unit information imported *)
    export_info : export_info;
    (** Term language information, e.g. for inlining *)
  }

  (** Take a snapshot of this module's mutable state. *)
  val create : unit -> t
end

(** Clear the mutable state of this module and prepare it for use with
    the given compilation unit.  This function does not set the current
    compilation unit (c.f. [Compilation_unit.set_current]). *)
val reset : Compilation_unit.t -> unit

module Error : sig
  (** Errors raised by the above functions.  Note that errors may also be
      raised from [Cmx_format] when using such functions. *)
  type t = private
    | Illegal_renaming of {
        contains_unit : Compilation_unit.t;
        desired_unit_name : Compilation_unit.Name.t;
        filename : string;
      }
    | Illegal_import_for_pack_prefix of {
        found_unit_name : Compilation_unit.Name.t;
        for_pack_prefix_in_cmx : Compilation_unit.Prefix.t;
        current_for_pack_prefix : Compilation_unit.Prefix.t;
        filename : string;
      }
    | Wrong_for_pack_prefix of {
        expected_prefix : Compilation_unit.Prefix.t;
        found_prefix : Compilation_unit.Prefix.t;
        filename : string;
      }

  (** Print the given error message on the given formatter. *)
  val report_error : Format.formatter -> t -> unit
end

(** Exceptions that hold errors raised from the above functions. *)
exception Error of Error.t

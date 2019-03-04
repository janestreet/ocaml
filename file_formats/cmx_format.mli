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

(** The format of .cmx files. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: update comment *)
(** Each .o file has a matching .cmx file that provides the following info
    on the compilation unit:
      - list of other units imported, with MD5s of their .cmx files
      - for Closure mode, approximation of the structure implemented
        (includes descriptions of known functions: arity and direct entry
         points)
      - for Flambda mode, term language information for inlining etc.
      - list of currying functions and application functions needed
    The .cmx file contains this info (as an externed record) plus a MD5
    of this info. *)
module Unit_info : sig
  type export_info =
    | Closure of Clambda.value_approximation
    | Flambda of Export_info.t

  type t

  (** Create a description of a compilation unit.  The parameters correspond
      to the names of the accessor functions below.

      Information that is only needed for linking is separated out into
      [Unit_info_link_time], below. *)
  val create
     : unit:Compilation_unit.t
    -> defines:Compilation_unit.t list
    (* CR-someday mshinwell: Change [imports_cmi] to use
       [Compilation_unit.Map.t] after insisting that .cmi files are produced
       with a "-for-pack" option (see CR in compilation_state.ml). *)
    -> imports_cmi:Digest.t option Compilation_unit.Map.t
    -> imports_cmx:Digest.t option Compilation_unit.Map.t
    -> functorized_pack_imports:Compilation_unit.t list
    -> export_info:export_info
    -> t

  (** The unit implemented, along with any -for-pack prefix. *)
  val unit : t -> Compilation_unit.t

  (** (Sub-)units implemented, in initialisation order, with the first to be
      initialised at the head of the list. *)
  val defines : t -> Compilation_unit.t list

  (** Interfaces imported. *)
  val imports_cmi : t -> Digest.t option Compilation_unit.Map.t

  (** Info imported. *)
  val imports_cmx : t -> Digest.t option Compilation_unit.Map.t

  (** Units imported from a common functorized pack *)
  val functorized_pack_imports : t -> Compilation_unit.t list

  (** Term language information, e.g. for inlining. *)
  val export_info : t -> export_info

  (** Return a new unit info structure with the given export info. *)
  val with_export_info : t -> export_info -> t
end

module Unit_info_link_time : sig
  type t

  (** Create a description, containing information required at link time
      (rather than compile time), of a compilation unit. *)
  val create
     : curry_fun:Numbers.Int.Set.t
    -> apply_fun:Numbers.Int.Set.t
    -> send_fun:Numbers.Int.Set.t
    -> force_link:bool
    -> t

  (** Currying functions needed. *)
  val curry_fun : t -> Numbers.Int.Set.t

  (** Apply functions needed. *)
  val apply_fun : t -> Numbers.Int.Set.t

  (** Send functions needed. *)
  val send_fun : t -> Numbers.Int.Set.t

  (** Whether the unit must always be linked. *)
  val force_link : t -> bool

  (** Return a new unit info structure with the given force-link property. *)
  val with_force_link : t -> force_link:bool -> t

  (** A description that provides at least as much information as all of the
      given descriptions.  (Used for example when forming a pack.) *)
  val join : t list -> t
end

module Error : sig
  (** Errors raised when using the below functions. *)
  type t = private
    | Not_a_cmx_file of { filename : string; }
    | Corrupted_cmx_file of { filename : string; }

  (** Print the given error message on the given formatter. *)
  val report_error : Format.formatter -> t -> unit
end

(** Exceptions that hold errors raised from the below functions. *)
exception Error of Error.t

(** Load unit information from a .cmx file. *)
val load : filename:string -> Unit_info.t * Unit_info_link_time.t * Digest.t

(** Load unit information, except link-time information, from a .cmx file. *)
val load_compile_time_info : filename:string -> Unit_info.t * Digest.t

(** Save unit information to a .cmx file. *)
val save
   : Unit_info.t
  -> Unit_info_link_time.t
  -> filename:string
  -> unit

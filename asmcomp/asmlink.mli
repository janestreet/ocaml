(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Link a set of .cmx/.o files and produce an executable or a plugin *)

open Misc
open Format

val link: ppf_dump:formatter -> string list -> string -> unit

val link_shared: ppf_dump:formatter -> string list -> string -> unit

val call_linker_shared: string list -> string -> unit

val reset : unit -> unit
val check_consistency: filepath -> Cmx_format.Unit_info.t -> Digest.t -> unit
val extract_crc_interfaces: unit -> Digest.t option Compilation_unit.Map.t
val extract_crc_implementations: unit -> Digest.t option Compilation_unit.Map.t

type error = private
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Missing_implementations of Misc.Stdlib.String.Set.t Compilation_unit.Map.t
  | Inconsistent_interface of Compilation_unit.Name.t * filepath * filepath
  | Inconsistent_implementation of Compilation_unit.Name.t * filepath * filepath
  | Linking_error
  | Multiple_definition of Compilation_unit.Name.t * filepath * filepath
  | Missing_cmx of filepath * Compilation_unit.Name.t

exception Error of error

val report_error: formatter -> error -> unit

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

(** The format of .cmxs files. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** A description of one compilation unit within a dynamically-loaded plugin. *)
module Dynunit_info : sig
  type t

  (** Create a compilation unit description.  The parameter names correspond
      to those of the accessors below. *)
  val create
     : unit:Compilation_unit.t
    -> crc:Digest.t
    -> imports_cmi:Digest.t option Compilation_unit.Map.t
    -> imports_cmx:Digest.t option Compilation_unit.Map.t
    -> defines:Compilation_unit.t list
    -> t

  (** The unit implemented, along with any -for-pack prefix. *)
  val unit : t -> Compilation_unit.t

  (** Checksum of the implementation. *)
  val crc : t -> Digest.t

  (** Interfaces imported. *)
  val imports_cmi : t -> Digest.t option Compilation_unit.Map.t

  (** Unit information imported. *)
  val imports_cmx : t -> Digest.t option Compilation_unit.Map.t

  (** (Sub-)units implemented, in initialisation order, with the first to be
      initialised at the head of the list. *)
  val defines : t -> Compilation_unit.t list
end

(** Each .cmxs dynamically-loaded plugin contains a symbol "caml_plugin_header"
    containing the following info (as an externed record). *)
module Dynheader_info : sig
  type t

  (** Create a plugin header description given the units in the plugin. *)
  val create : units:Dynunit_info.t list -> t

  (** The .cmxs magic number at the time the given plugin was produced. *)
  val magic : t -> string

  (** The list of units in the given plugin. *)
  val units : t -> Dynunit_info.t list
end

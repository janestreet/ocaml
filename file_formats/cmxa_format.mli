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

(** The format of .cmxa files. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Each .a library has a matching .cmxa file that provides the following info
    on the library. *)
module Library_info : sig
  type t

  (** Create a library description.  The parameter names correspond to those
      of the accessors below. *)
  val create
     : units:(Cmx_format.Unit_info.t * Cmx_format.Unit_info_link_time.t
         * Digest.t) list
    -> ccobjs:string list
    -> ccopts:string list
    -> t

  (** List of units' info w/ MD5s. *)
  val units
     : t
    -> (Cmx_format.Unit_info.t
        * Cmx_format.Unit_info_link_time.t
        * Digest.t) list

  (** C object files needed. *)
  val ccobjs : t -> string list

  (** Extra options to pass to the C compiler. *)
  val ccopts : t -> string list
end

module Error : sig
  (** Errors raised when using the below functions. *)
  type t = private
    | Not_a_cmxa_file of { filename : string; }

  (** Print the given error message on the given formatter. *)
  val report_error : Format.formatter -> t -> unit
end

(** Exceptions that hold errors raised from the below functions. *)
exception Error of Error.t

(** Load library information from a .cmxa file. *)
val load : filename:string -> Library_info.t

(** Save library information to a channel .cmxa file. *)
val save : Library_info.t -> filename:string -> unit

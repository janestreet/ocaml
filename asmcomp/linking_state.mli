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

(** Linking- and startup file-related state that is accumulated prior to
    being written into .cmx files.

    This module forms part of the backend of the compiler.  It is not
    used prior to [Cmmgen].
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Reset the mutable state in this module, ready for a new "backend
    compilation unit" to be compiled. *)
val reset : unit -> unit

(** Record the need for a currying function with the given arity. *)
val need_curry_fun : int -> unit

(** Record the need for an application function with the given arity. *)
val need_apply_fun : int -> unit

(** Record the need for a message-sending function with the given arity. *)
val need_send_fun : int -> unit

(** Record that the current unit must always be linked. *)
val force_link : unit -> unit

module Snapshot : sig
  type t = private {
    curry_fun : Numbers.Int.Set.t;
    (** Currying functions needed for the current unit. *)
    apply_fun : Numbers.Int.Set.t;
    (** Apply functions needed for the current unit. *)
    send_fun : Numbers.Int.Set.t;
    (** Send functions needed for the current unit. *)
    force_link : bool;
    (** Whether the current unit must always be linked. *)
  }

  (** Take a snapshot of this module's mutable state. *)
  val create : unit -> t
end

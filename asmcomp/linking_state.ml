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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Int = Numbers.Int

type t = {
  mutable curry_fun : Int.Set.t;
  mutable apply_fun : Int.Set.t;
  mutable send_fun : Int.Set.t;
  mutable force_link : bool;
}

let empty = {
  curry_fun = Int.Set.empty;
  apply_fun = Int.Set.empty;
  send_fun = Int.Set.empty;
  force_link = false;
}

let current_unit = empty

let reset () =
  current_unit.curry_fun <- Int.Set.empty;
  current_unit.apply_fun <- Int.Set.empty;
  current_unit.send_fun <- Int.Set.empty;
  current_unit.force_link <- false

let need_curry_fun n =
  current_unit.curry_fun <- Int.Set.add n current_unit.curry_fun

let need_apply_fun n =
  if n <= 0 then begin
    Misc.fatal_errorf "Bad argument count for [need_apply_fun]: %d" n
  end;
  current_unit.apply_fun <- Int.Set.add n current_unit.apply_fun

let need_send_fun n =
  current_unit.send_fun <- Int.Set.add n current_unit.send_fun

let force_link () =
  current_unit.force_link <- true

module Snapshot = struct
  type t = {
    curry_fun : Int.Set.t;
    apply_fun : Int.Set.t;
    send_fun : Int.Set.t;
    force_link : bool;
  }

  let create () =
    { curry_fun = current_unit.curry_fun;
      apply_fun = current_unit.apply_fun;
      send_fun = current_unit.send_fun;
      force_link = current_unit.force_link;
    }
end

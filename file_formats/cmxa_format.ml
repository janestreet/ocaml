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

type error =
  | Not_a_cmxa_file of { filename : string; }

module Error = struct
  type t = error =
    | Not_a_cmxa_file of { filename : string; }

  let report_error ppf = function
    | Not_a_cmxa_file { filename; } ->
      Format.fprintf ppf "%a@ is not a library description (.cmxa file)."
        Location.print_filename filename
end

exception Error of error

let () =
  Location.register_error_of_exn (function
    | Error err ->
      Some (Location.error_of_printer_file Error.report_error err)
    | _ -> None)

module Library_info = struct
  type t = {
    units : (Cmx_format.Unit_info.t * Cmx_format.Unit_info_link_time.t
      * Digest.t) list;
    ccobjs : string list;
    ccopts : string list;
  }

  let create ~units ~ccobjs ~ccopts =
    { units;
      ccobjs;
      ccopts;
    }

  let units t = t.units
  let ccobjs t = t.ccobjs
  let ccopts t = t.ccopts
end

let load ~filename =
  let ic = open_in_bin filename in
  let buffer =
    really_input_string ic (String.length Config.cmxa_magic_number)
  in
  if buffer <> Config.cmxa_magic_number then begin
    raise (Error (Not_a_cmxa_file { filename; }))
  end;
  let t = (input_value ic : Library_info.t) in
  close_in ic;
  t

let save t ~filename =
  Misc.protect_writing_to_file ~filename ~f:(fun outchan ->
    output_string outchan Config.cmxa_magic_number;
    output_value outchan t)

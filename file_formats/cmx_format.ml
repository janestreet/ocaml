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

type error =
  | Not_a_cmx_file of { filename : string; }
  | Corrupted_cmx_file of { filename : string; }

exception Error of error

module Error = struct
  type t = error =
    | Not_a_cmx_file of { filename : string; }
    | Corrupted_cmx_file of { filename : string; }

  let report_error ppf = function
    | Not_a_cmx_file { filename; } ->
      Format.fprintf ppf "%a@ is not a compilation unit description."
        Location.print_filename filename
    | Corrupted_cmx_file { filename; } ->
      Format.fprintf ppf "Corrupted compilation unit description@ %a"
        Location.print_filename filename

  let () =
    Location.register_error_of_exn
      (function
        | Error err -> Some (Location.error_of_printer_file report_error err)
        | _ -> None
      )
end

module Unit_info = struct
  type export_info =
    | Closure of Clambda.value_approximation
    | Flambda of Export_info.t

  type t = {
    unit : Compilation_unit.t;
    defines : Compilation_unit.t list;
    imports_cmi : Digest.t option Compilation_unit.Map.t;
    imports_cmx : Digest.t option Compilation_unit.Map.t;
    functorized_pack_imports : Compilation_unit.t list;
    export_info : export_info;
  }

  let create
      ~unit ~defines
      ~imports_cmi ~imports_cmx ~functorized_pack_imports
      ~export_info =
    { unit;
      defines;
      imports_cmi;
      imports_cmx;
      functorized_pack_imports;
      export_info;
    }

  let unit t = t.unit
  let defines t = t.defines
  let imports_cmi t = t.imports_cmi
  let imports_cmx t = t.imports_cmx
  let functorized_pack_imports t = t.functorized_pack_imports
  let export_info t = t.export_info

  let with_export_info t export_info =
    { t with
      export_info;
    }
end

module Unit_info_link_time = struct
  type t = {
    curry_fun : Int.Set.t;
    apply_fun : Int.Set.t;
    send_fun : Int.Set.t;
    force_link : bool;
  }

  let create ~curry_fun ~apply_fun ~send_fun ~force_link =
    { curry_fun;
      apply_fun;
      send_fun;
      force_link;
    }

  let curry_fun t = t.curry_fun
  let apply_fun t = t.apply_fun
  let send_fun t = t.send_fun
  let force_link t = t.force_link

  let with_force_link t ~force_link =
    { t with
      force_link;
    }

  let join ts =
    let curry_fun = Int.Set.union_list (List.map curry_fun ts) in
    let apply_fun = Int.Set.union_list (List.map apply_fun ts) in
    let send_fun = Int.Set.union_list (List.map send_fun ts) in
    let force_link = List.exists force_link ts in
    { curry_fun;
      apply_fun;
      send_fun;
      force_link;
    }
end

let load ~filename =
  let ic = open_in_bin filename in
  try
    let buffer =
      really_input_string ic (String.length Config.cmx_magic_number)
    in
    if buffer <> Config.cmx_magic_number then begin
      close_in ic;
      raise (Error (Not_a_cmx_file { filename; }))
    end;
    let unit_info = (input_value ic : Unit_info.t) in
    let unit_info_link_time = (input_value ic : Unit_info_link_time.t) in
    let crc = Digest.input ic in
    close_in ic;
    unit_info, unit_info_link_time, crc
  with End_of_file | Failure _ ->
    close_in ic;
    raise (Error (Corrupted_cmx_file { filename; }))

(* CR-someday mshinwell: Consider rearranging things so we can load only
   the compilation part + CRC. *)
let load_compile_time_info ~filename =
  let unit_info, _unit_info_link_time, crc = load ~filename in
  unit_info, crc

let save unit_info unit_info_link_time ~filename =
  Misc.protect_writing_to_file ~filename ~f:(fun oc ->
    output_string oc Config.cmx_magic_number;
    output_value oc unit_info;
    output_value oc unit_info_link_time;
    flush oc;
    let crc = Digest.file filename in
    Digest.output oc crc)

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

open Misc
open Compenv

type info = {
  source_file : string;
  module_name : Compilation_unit.Name.t;
  output_prefix : string;
  for_pack_prefix : Compilation_unit.Prefix.t;
  env : Env.t;
  ppf_dump : Format.formatter;
  tool_name : string;
  native : bool;
}

let cmx i = i.output_prefix ^ ".cmx"
let obj i = i.output_prefix ^ Config.ext_obj
let cmo i = i.output_prefix ^ ".cmo"
let annot i = i.output_prefix ^ ".annot"

let with_info ~native ~tool_name ~source_file ~output_prefix ~dump_ext k =
  Compmisc.init_path ();
  let module_name =
    Compilation_unit.Name.of_string
      (module_of_filename source_file output_prefix) in
  let for_pack_prefix =
    Compilation_unit.Prefix.parse_for_pack !Clflags.for_package in
  Persistent_env.Current_unit.set ~prefix:for_pack_prefix module_name;
  let env = Compmisc.initial_env() in
  let dump_file = String.concat "." [output_prefix; dump_ext] in
  Compmisc.with_ppf_dump ~file_prefix:dump_file @@ fun ppf_dump ->
  k {
    module_name;
    output_prefix;
    for_pack_prefix;
    env;
    source_file;
    ppf_dump;
    tool_name;
    native;
  }

(** Compile a .mli file *)

let parse_intf i =
  Pparse.parse_interface ~tool_name:i.tool_name i.source_file
  |> print_if i.ppf_dump Clflags.dump_parsetree Printast.interface
  |> print_if i.ppf_dump Clflags.dump_source Pprintast.signature

let typecheck_intf info ast =
  Profile.(record_call typing) @@ fun () ->
  let tintf =
    ast
    |> Typemod.type_interface info.source_file info.env
    |> print_if info.ppf_dump Clflags.dump_typedtree Printtyped.interface
  in
  let uty = tintf.Typedtree.tintf_type in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env ~error:false info.env (fun () ->
        Format.(fprintf std_formatter) "%a@."
          (Printtyp.printed_interface info.source_file)
          uty);
  ignore (Includemod.compunits ~loc:(Location.in_file (info.source_file))
            info.env ~mark:Mark_both uty uty);
  Typecore.force_delayed_checks ();
  Warnings.check_fatal ();
  tintf

let emit_interface info ast tintf =
  let cmi =
    let alerts = Builtin_attributes.alerts_of_sig ast in
    Env.save_interface ~alerts tintf.Typedtree.tintf_type
      info.module_name (info.output_prefix ^ ".cmi")
  in
  Typemod.save_interface info.module_name tintf
    info.output_prefix info.source_file info.env cmi

let interface info =
  Profile.record_call info.source_file @@ fun () ->
  let ast = parse_intf info in
  if Clflags.(should_stop_after Compiler_pass.Parsing) then () else begin
    let tintf = typecheck_intf info ast in
    if not !Clflags.print_types then begin
      emit_interface info ast tintf
    end
  end


(** Frontend for a .ml file *)

let parse_impl i =
  Pparse.parse_implementation ~tool_name:i.tool_name i.source_file
  |> print_if i.ppf_dump Clflags.dump_parsetree Printast.implementation
  |> print_if i.ppf_dump Clflags.dump_source Pprintast.structure

let typecheck_impl i parsetree =
  let always () = Stypes.dump (Some (annot i)) in
  Misc.try_finally ~always (fun () ->
    parsetree
    |> Profile.(record typing)
      (Typemod.type_implementation
         i.source_file i.output_prefix i.module_name i.env)
    |> print_if i.ppf_dump Clflags.dump_typedtree
      Printtyped.implementation_with_coercion
  )

let implementation info ~backend =
  Profile.record_call info.source_file @@ fun () ->
  let exceptionally () =
    let sufs = if info.native then [ cmx; obj ] else [ cmo ] in
    List.iter (fun suf -> remove_file (suf info)) sufs;
  in
  Misc.try_finally ?always:None ~exceptionally (fun () ->
    let parsed = parse_impl info in
    if Clflags.(should_stop_after Compiler_pass.Parsing) then () else begin
      let typed = typecheck_impl info parsed in
      if Clflags.(should_stop_after Compiler_pass.Typing) then () else begin
        backend info typed
      end;
    end;
    Warnings.check_fatal ();
  )

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The batch compiler *)

[@@@ocaml.warning "-40"]

open Misc
open Compile_common

module CU = Compilation_unit
module UI = Cmx_format.Unit_info
module UIL = Cmx_format.Unit_info_link_time

let tool_name = "ocamlopt"

let with_info =
  Compile_common.with_info ~native:true ~tool_name

let interface ~source_file ~output_prefix =
  with_info ~source_file ~output_prefix ~dump_ext:"cmi" @@ fun info ->
  Compile_common.interface info

let (|>>) (x, y) f = (x, f y)

(** Native compilation backend for .ml files. *)

let write_cmx_file filename =
  let imports_cmi =
    (* CR-someday mshinwell: Replace type "modname" everywhere with a new type,
       in its own module, such module to supercede [Compilation_unit.Name]. *)
    List.fold_left (fun imports (unit, digest_opt) ->
        CU.Map.add unit digest_opt imports)
      CU.Map.empty
      (Env.imports ())
  in
  let functorized_pack_imports =
    List.map fst (Env.functorized_pack_imports ()) in
  let unit_info =
    let compilation_state = Compilation_state.Snapshot.create () in
    let unit = compilation_state.unit in
    let defines = compilation_state.defines in
    let imports_cmx = compilation_state.imports_cmx in
    let export_info : UI.export_info =
      match compilation_state.export_info with
      | Closure approx -> Closure approx
      | Flambda export_info -> Flambda export_info
    in
    UI.create ~unit ~defines ~imports_cmi ~imports_cmx
      ~functorized_pack_imports ~export_info
  in
  let unit_info_link_time =
    let linking_state = Linking_state.Snapshot.create () in
    let curry_fun = linking_state.curry_fun in
    let apply_fun = linking_state.apply_fun in
    let send_fun = linking_state.send_fun in
    let force_link = linking_state.force_link in
    UIL.create ~curry_fun ~apply_fun ~send_fun ~force_link
  in
  Cmx_format.save unit_info unit_info_link_time ~filename

let flambda i backend typed =
  if !Clflags.classic_inlining then begin
    Clflags.default_simplify_rounds := 1;
    Clflags.use_inlining_arguments_set Clflags.classic_arguments;
    Clflags.unbox_free_vars_of_closures := false;
    Clflags.unbox_specialised_args := false
  end;
  typed
  |> Profile.(record transl)
    (Translmod.transl_implementation_flambda i.module_name)
  |> Profile.(record generate)
    (fun {Lambda.module_ident; main_module_block_size;
          required_globals; code } ->
    ((module_ident, main_module_block_size), code)
    |>> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.lambda
    |>> Simplif.simplify_lambda
    |>> print_if i.ppf_dump Clflags.dump_lambda Printlambda.lambda
    |> (fun ((module_ident, main_module_block_size), code) ->
      let program : Lambda.program =
        { module_ident;
          main_module_block_size;
          required_globals;
          code;
        }
      in
      Asmgen.compile_implementation
        ~backend
        ~filename:i.source_file
        ~prefixname:i.output_prefix
        ~middle_end:Flambda_middle_end.lambda_to_clambda
        ~ppf_dump:i.ppf_dump
        program);
    write_cmx_file (cmx i))

let closure i backend typed =
  Clflags.use_inlining_arguments_set Clflags.classic_arguments;
  typed
  |> Profile.(record transl)
    (Translmod.transl_store_implementation i.module_name)
  |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.program
  |> Profile.(record generate)
    (fun program ->
       let code = Simplif.simplify_lambda program.Lambda.code in
       { program with Lambda.code }
       |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.program
       |> Asmgen.compile_implementation
            ~backend
            ~filename:i.source_file
            ~prefixname:i.output_prefix
            ~middle_end:Closure_middle_end.lambda_to_clambda
            ~ppf_dump:i.ppf_dump;
       write_cmx_file (cmx i))

(* Emit assembly directly from Linear IR *)
let emit i =
  Linking_state.reset ();
  Asmgen.compile_implementation_linear i.output_prefix ~progname:i.source_file

let implementation ~backend ~source_file ~output_prefix =
  let backend info typed =
    let compilation_unit =
      Compilation_unit.create
        ~for_pack_prefix:info.for_pack_prefix info.module_name in
    Compilation_state.reset compilation_unit;
    Linking_state.reset ();
    (if Config.flambda then flambda else closure) info backend typed
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmx" @@ fun info ->
  if Clflags.(should_start_from Compiler_pass.Emit) then
    emit info
  else
    Compile_common.implementation info ~backend

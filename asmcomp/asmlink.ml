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

(* Link a set of .cmx/.o files and produce an executable *)

[@@@ocaml.warning "-40"]

open Misc
open Config

module BCU = Backend_compilation_unit
module CU = Compilation_unit
module UI = Cmx_format.Unit_info

module String = Misc.Stdlib.String

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Missing_implementations of String.Set.t CU.Map.t
  | Inconsistent_interface of CU.Name.t * filepath * filepath
  | Inconsistent_implementation of CU.Name.t * filepath * filepath
  | Linking_error
  | Multiple_definition of CU.Name.t * filepath * filepath
  | Missing_cmx of filepath * CU.Name.t

exception Error of error

(* Consistency check between interfaces and implementations *)

module Cmi_consistbl = Consistbl.Make (CU)
let crc_interfaces = Cmi_consistbl.create ()
let interfaces = ref CU.Set.empty

module Cmx_consistbl = Consistbl.Make (CU)
let crc_implementations = Cmx_consistbl.create ()
let implementations = ref CU.Set.empty
let implementations_defined = ref CU.Map.empty

let cmx_required = ref CU.Set.empty

let check_consistency file_name unit_info crc =
  begin try
    CU.Map.iter
      (fun imported_unit crco ->
        let name = CU.name imported_unit in
        interfaces := CU.Set.add imported_unit !interfaces;
        match crco with
        | None -> ()
        | Some crc ->
            if CU.Name.equal name (CU.name (UI.unit unit_info))
            then Cmi_consistbl.set crc_interfaces imported_unit crc file_name
            else Cmi_consistbl.check crc_interfaces imported_unit crc file_name)
      (UI.imports_cmi unit_info)
  with Cmi_consistbl.Inconsistency(unit, user, auth) ->
    raise(Error(Inconsistent_interface(CU.name unit, user, auth)))
  end;
  begin try
    CU.Map.iter
      (fun imported_unit crco ->
        let name = CU.name imported_unit in
        implementations := CU.Set.add imported_unit !implementations;
        match crco with
          | None ->
              if CU.Set.mem imported_unit !cmx_required then
                raise(Error(Missing_cmx(file_name, name)))
          | Some crc ->
              Cmx_consistbl.check crc_implementations imported_unit crc file_name)
      (UI.imports_cmx unit_info)
  with Cmx_consistbl.Inconsistency(unit, user, auth) ->
    raise(Error(Inconsistent_implementation(CU.name unit, user, auth)))
  end;
  let comp_unit = UI.unit unit_info in
  begin try
    let source = CU.Map.find comp_unit !implementations_defined in
    raise (Error(Multiple_definition(CU.name comp_unit, file_name, source)))
  with Not_found -> ()
  end;
  implementations := CU.Set.add comp_unit !implementations;
  Cmx_consistbl.set crc_implementations comp_unit crc file_name;
  implementations_defined :=
    CU.Map.add comp_unit file_name !implementations_defined;
  (* CR-someday mshinwell: The following check will be able to be removed
     once .cmi files have to be compiled with "-for-pack". *)
  if CU.is_packed comp_unit then begin
    cmx_required := CU.Set.add comp_unit !cmx_required
  end

let extract_crc_interfaces () =
  Cmi_consistbl.extract_map !interfaces crc_interfaces
let extract_crc_implementations () =
  Cmx_consistbl.extract_map !implementations crc_implementations

(* Add C objects and options and "custom" info from a library descriptor.
   See bytecomp/bytelink.ml for comments on the order of C objects. *)

let lib_ccobjs = ref []
let lib_ccopts = ref []

let add_ccobjs origin l =
  if not !Clflags.no_auto_link then begin
    lib_ccobjs := Cmxa_format.Library_info.ccobjs l @ !lib_ccobjs;
    let replace_origin =
      Misc.replace_substring ~before:"$CAMLORIGIN" ~after:origin
    in
    lib_ccopts :=
      List.map replace_origin (Cmxa_format.Library_info.ccopts l) @ !lib_ccopts
  end

let runtime_lib () =
  let libname = "libasmrun" ^ !Clflags.runtime_variant ^ ext_lib in
  try
    if !Clflags.nopervasives || not !Clflags.with_runtime then []
    else [ Load_path.find libname ]
  with Not_found ->
    raise(Error(File_not_found libname))

let object_file_name name =
  let file_name =
    try
      Load_path.find name
    with Not_found ->
      fatal_errorf "Asmlink.object_file_name: %s not found" name in
  if Filename.check_suffix file_name ".cmx" then
    Filename.chop_suffix file_name ".cmx" ^ ext_obj
  else if Filename.check_suffix file_name ".cmxa" then
    Filename.chop_suffix file_name ".cmxa" ^ ext_lib
  else
    fatal_error "Asmlink.object_file_name: bad ext"

(* First pass: determine which units are needed *)

let missing_globals = (CU.Tbl.create 17 : String.Set.t ref CU.Tbl.t)

let is_required name =
  try ignore (CU.Tbl.find missing_globals name); true
  with Not_found -> false

let add_required by name =
  try
    let rq = CU.Tbl.find missing_globals name in
    rq := String.Set.add by !rq
  with Not_found ->
    CU.Tbl.add missing_globals name (ref (String.Set.singleton by))

let remove_required name =
  CU.Tbl.remove missing_globals name

let extract_missing_globals () =
  let mg = ref CU.Map.empty in
  CU.Tbl.iter (fun md rq -> mg := CU.Map.add md !rq !mg)
    missing_globals;
  !mg

type file =
  | Unit of filepath * UI.t * Cmx_format.Unit_info_link_time.t * Digest.t
  | Library of filepath * Cmxa_format.Library_info.t

let read_file obj_name =
  let filename =
    try
      Load_path.find obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  if Filename.check_suffix filename ".cmx" then begin
    (* This is a .cmx file. It must be linked in any case.
       Read the infos to see which modules it requires. *)
    let info, link_time_info, crc = Cmx_format.load ~filename in
    Unit (filename, info, link_time_info, crc)
  end
  else if Filename.check_suffix filename ".cmxa" then begin
    let infos =
      try Cmxa_format.load ~filename
      with Cmxa_format.Error (Not_a_cmxa_file _) ->
        raise(Error(Not_an_object_file filename))
    in
    Library (filename,infos)
  end
  else raise(Error(Not_an_object_file filename))

let scan_file obj_name tolink =
  match read_file obj_name with
  | Unit (file_name, info, link_time_info, crc) ->
      (* This is a .cmx file. It must be linked in any case. *)
      let comp_unit = UI.unit info in
      remove_required comp_unit;
      let required_by = CU.Name.to_string (CU.name comp_unit) in
      CU.Map.iter (fun imported_unit _crc ->
          add_required required_by imported_unit)
        (UI.imports_cmx info);
      (info, link_time_info, file_name, crc) :: tolink
  | Library (file_name, library_info) ->
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      add_ccobjs (Filename.dirname file_name) library_info;
      List.fold_right
        (fun (info, link_time_info, crc) reqd ->
           let comp_unit = UI.unit info in
           if Cmx_format.Unit_info_link_time.force_link link_time_info
             || !Clflags.link_everything
             || is_required comp_unit
           then begin
             remove_required comp_unit;
             let required_by =
               Printf.sprintf "%s(%s)" file_name
                 (CU.Name.to_string (CU.name (UI.unit info)))
             in
             CU.Map.iter (fun imported_unit _crc ->
                 add_required required_by imported_unit)
               (UI.imports_cmx info);
             (info, link_time_info, file_name, crc) :: reqd
           end else
             reqd)
        (Cmxa_format.Library_info.units library_info)
        tolink

(* Second pass: generate the startup file and link it with everything else *)

let force_linking_of_startup ~ppf_dump =
  Asmgen.compile_phrase ~ppf_dump
    (Cmm.Cdata ([Cmm.Csymbol_address Backend_sym.Names.caml_startup]))

let make_globals_map units_list ~crc_interfaces =
  let crc_interfaces = CU.Tbl.of_map crc_interfaces in
  let defined =
    List.map (fun (unit, _, _, impl_crc) ->
        let compunit = UI.unit unit in
        let name = CU.name compunit in
        let intf_crc = CU.Tbl.find crc_interfaces compunit in
        CU.Tbl.remove crc_interfaces compunit;
        (name, intf_crc, Some impl_crc, UI.defines unit))
      units_list
  in
  CU.Tbl.fold (fun unit intf acc ->
      (CU.name unit, intf, None, []) :: acc)
    crc_interfaces defined

let make_startup_file ~ppf_dump units_list ~crc_interfaces =
  let compile_phrase p = Asmgen.compile_phrase ~ppf_dump p in
  Location.input_name := "caml_startup"; (* set name of "current" input *)
  Linking_state.reset ();
  Emit.begin_assembly BCU.startup;
  let defined_units =
    List.flatten (
      List.map (fun (info, _, _, _) ->
          List.map BCU.compilation_unit (UI.defines info))
        units_list)
  in
  compile_phrase (Cmm_helpers.entry_point defined_units);
  let link_infos = List.map (fun (_, info, _, _) -> info) units_list in
  let joined_link_info = Cmx_format.Unit_info_link_time.join link_infos in
  List.iter compile_phrase (Cmm_helpers.generic_functions false joined_link_info);
  Array.iteri
    (fun i name -> compile_phrase (Cmm_helpers.predef_exception i name))
    Runtimedef.builtin_exceptions;
  compile_phrase (Cmm_helpers.global_table defined_units);
  let globals_map = make_globals_map units_list ~crc_interfaces in
  compile_phrase (Cmm_helpers.globals_map globals_map);
  let defined_units = BCU.startup :: defined_units in
  compile_phrase (Cmm_helpers.data_segment_table defined_units);
  if !Clflags.function_sections then
    compile_phrase
      (Cmm_helpers.code_segment_table (BCU.functions :: defined_units))
  else
    compile_phrase (Cmm_helpers.code_segment_table defined_units);
  let all_units = BCU.runtime_and_external_libs :: defined_units in
  compile_phrase (Cmm_helpers.frame_table all_units);
  if Config.spacetime then begin
    compile_phrase (Cmm_helpers.spacetime_shapes all_units);
  end;
  if !Clflags.output_complete_object then begin
    force_linking_of_startup ~ppf_dump
  end;
  Emit.end_assembly BCU.startup

let make_shared_startup_file ~ppf_dump units_list =
  let compile_phrase p = Asmgen.compile_phrase ~ppf_dump p in
  Location.input_name := "caml_startup";
  Linking_state.reset ();
  Emit.begin_assembly BCU.shared_startup;
  let unit_info_with_crc_list =
    List.map (fun (info, _, _, crc) -> info, crc) units_list
  in
  let unit_info_list = List.map (fun (info, _, _, _) -> info) units_list in
  let link_info_list = List.map (fun (_, info, _, _) -> info) units_list in
  let joined_link_info = Cmx_format.Unit_info_link_time.join link_info_list in
  List.iter compile_phrase (Cmm_helpers.generic_functions true joined_link_info);
  compile_phrase (Cmm_helpers.plugin_header unit_info_with_crc_list);
  let all_units =
    List.map (fun info -> BCU.compilation_unit (UI.unit info))
      unit_info_list
  in
  compile_phrase (Cmm_helpers.global_table all_units);
  if !Clflags.output_complete_object then begin
    (* this is to force a reference to all units, otherwise the linker
       might drop some of them (in case of libraries) *)
    force_linking_of_startup ~ppf_dump
  end;
  Emit.end_assembly BCU.shared_startup

let call_linker_shared file_list output_name =
  if not (Ccomp.call_linker Ccomp.Dll output_name file_list "")
  then raise(Error Linking_error)

let link_shared ~ppf_dump objfiles output_name =
  Profile.record_call output_name (fun () ->
    let units_tolink = List.fold_right scan_file objfiles [] in
    List.iter (fun (info, _link_info, file_name, crc) ->
        check_consistency file_name info crc)
      units_tolink;
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
    let objfiles = List.rev (List.map object_file_name objfiles) @
      (List.rev !Clflags.ccobjs) in
    let startup =
      if !Clflags.keep_startup_file || !Emitaux.binary_backend_available
      then output_name ^ ".startup" ^ ext_asm
      else Filename.temp_file "camlstartup" ext_asm in
    let startup_obj = output_name ^ ".startup" ^ ext_obj in
    let comp_unit = BCU.shared_startup in
    Asmgen.compile_unit ~comp_unit ~output_prefix:output_name
      ~asm_filename:startup ~keep_asm:!Clflags.keep_startup_file
      ~obj_filename:startup_obj
      (fun () -> make_shared_startup_file ~ppf_dump units_tolink);
    call_linker_shared (startup_obj :: objfiles) output_name;
    remove_file startup_obj
  )

let call_linker file_list startup_file output_name =
  let main_dll = !Clflags.output_c_object
                 && Filename.check_suffix output_name Config.ext_dll
  and main_obj_runtime = !Clflags.output_complete_object
  in
  let files = startup_file :: (List.rev file_list) in
  let libunwind =
    if not Config.spacetime then []
    else if not Config.libunwind_available then []
    else String.split_on_char ' ' Config.libunwind_link_flags
  in
  let files, c_lib =
    if (not !Clflags.output_c_object) || main_dll || main_obj_runtime then
      files @ (List.rev !Clflags.ccobjs) @ runtime_lib () @ libunwind,
      (if !Clflags.nopervasives || main_obj_runtime
       then "" else Config.native_c_libraries)
    else
      files, ""
  in
  let mode =
    if main_dll then Ccomp.MainDll
    else if !Clflags.output_c_object then Ccomp.Partial
    else Ccomp.Exe
  in
  if not (Ccomp.call_linker mode output_name files c_lib)
  then raise(Error Linking_error)

(* Main entry point *)

let link ~ppf_dump objfiles output_name =
  Profile.record_call output_name (fun () ->
    let stdlib = "stdlib.cmxa" in
    let stdexit = "std_exit.cmx" in
    let objfiles =
      if !Clflags.nopervasives then objfiles
      else if !Clflags.output_c_object then stdlib :: objfiles
      else stdlib :: (objfiles @ [stdexit]) in
    let units_tolink = List.fold_right scan_file objfiles [] in
    let missing_globals = extract_missing_globals () in
    if not (CU.Map.is_empty missing_globals) then begin
      raise (Error (Missing_implementations missing_globals))
    end;
    List.iter (fun (info, _link_info, file_name, crc) ->
        check_consistency file_name info crc)
      units_tolink;
    let crc_interfaces = extract_crc_interfaces () in
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
                                                 (* put user's opts first *)
    let startup =
      if !Clflags.keep_startup_file || !Emitaux.binary_backend_available
      then output_name ^ ".startup" ^ ext_asm
      else Filename.temp_file "camlstartup" ext_asm in
    let startup_obj = Filename.temp_file "camlstartup" ext_obj in
    let comp_unit = BCU.startup in
    Asmgen.compile_unit ~comp_unit ~output_prefix:output_name
      ~asm_filename:startup ~keep_asm:!Clflags.keep_startup_file
      ~obj_filename:startup_obj
      (fun () -> make_startup_file ~ppf_dump units_tolink ~crc_interfaces);
    Misc.try_finally
      (fun () ->
         call_linker (List.map object_file_name objfiles)
           startup_obj output_name)
      ~always:(fun () -> remove_file startup_obj)
  )

(* Error report *)

open Format

let report_error ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %s" name
  | Not_an_object_file name ->
      fprintf ppf "The file %a is not a compilation unit description"
        Location.print_filename name
  | Missing_implementations l ->
      let print_references ppf = function
        | [] -> ()
        | r1 :: rl ->
            fprintf ppf "%s" r1;
            List.iter (fun r -> fprintf ppf ",@ %s" r) rl
      in
      let print_modules ppf =
        CU.Map.iter
         (fun md rq ->
            fprintf ppf "@ @[<hov 2>%a referenced from %a@]"
              CU.Name.print (CU.name md)
              print_references (String.Set.elements rq))
      in
      fprintf ppf
        "@[<v 2>No implementations provided for the following modules:%a@]"
        print_modules l
  | Inconsistent_interface(intf, file1, file2) ->
      fprintf ppf
        "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
               over interface %a@]"
        Location.print_filename file1
        Location.print_filename file2
        CU.Name.print intf
  | Inconsistent_implementation(impl, file1, file2) ->
      fprintf ppf
        "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
               over implementation %a@]"
        Location.print_filename file1
        Location.print_filename file2
        CU.Name.print impl
  | Linking_error ->
      fprintf ppf "Error during linking"
  | Multiple_definition(modname, file1, file2) ->
      fprintf ppf
        "@[<hov>Files %a@ and %a@ both define a module named %a@]"
        Location.print_filename file1
        Location.print_filename file2
        CU.Name.print modname
  | Missing_cmx(filename, name) ->
      fprintf ppf
        "@[<hov>File %a@ was compiled without access@ \
         to the .cmx file@ for module %a,@ \
         which was produced by `ocamlopt -for-pack'.@ \
         Please recompile %a@ with the correct `-I' option@ \
         so that %a.cmx@ is found.@]"
        Location.print_filename filename
        CU.Name.print name
        Location.print_filename filename
        CU.Name.print name

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let reset () =
  Cmi_consistbl.clear crc_interfaces;
  Cmx_consistbl.clear crc_implementations;
  implementations_defined := CU.Map.empty;
  cmx_required := CU.Set.empty;
  interfaces := CU.Set.empty;
  implementations := CU.Set.empty;
  lib_ccobjs := [];
  lib_ccopts := []

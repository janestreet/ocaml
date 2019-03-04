(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* "Package" a set of .cmx/.o files into one .cmx/.o file having the
   original compilation units as sub-modules. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Misc

module CU = Compilation_unit
module UI = Cmx_format.Unit_info

type error =
  | Illegal_renaming of {
      name_in_cmx : CU.Name.t;
      file : string;
      desired_name : CU.Name.t;
    }
  | Forward_reference of string * CU.Name.t
  | Wrong_for_pack of string * CU.Prefix.t
  | Linking_error
  | File_not_found of string

exception Error of error

(* Read the unit information from a .cmx file. *)

type pack_member_kind =
  | PM_intf
  | PM_impl of UI.t * Cmx_format.Unit_info_link_time.t * Types.compilation_unit

type pack_member =
  { pm_file: filepath;
    pm_name: CU.Name.t;
    pm_kind: pack_member_kind }

let read_member_info current_unit file =
  let name =
    CU.Name.of_string (String.capitalize_ascii (
      Filename.basename (chop_extensions file)))
  in
  let kind =
    if Filename.check_suffix file ".cmi" then
      PM_intf
    else begin
      let info, link_info, crc = Cmx_format.load ~filename:file in
      let name_in_cmx = CU.name (UI.unit info) in
      if not (CU.Name.equal name_in_cmx name) then begin
        raise (Error (Illegal_renaming {
          name_in_cmx;
          file;
          desired_name = name;
        }))
      end;
      let cmx_file_for_pack_prefix = CU.for_pack_prefix (UI.unit info) in
      let full_path_with_params =
        let params =
          List.fold_left (fun acc p ->
              CU.Name.of_string p :: acc) [] !Clflags.functor_parameters in
        CU.for_pack_prefix current_unit @
        [ CU.Prefix.Pack (CU.name current_unit, params) ]
      in
      if not (CU.Prefix.equal
        cmx_file_for_pack_prefix full_path_with_params)
      then begin
        raise
          (Error (Wrong_for_pack (file, CU.full_path current_unit)))
      end;
      Asmlink.check_consistency file info crc;
      Compilation_state.cache_unit_info info;
      PM_impl (info, link_info,
               Env.read_interface name (chop_extensions file ^ ".cmi"))
    end
  in
  { pm_file = file; pm_name = name; pm_kind = kind }

(* Check absence of forward references *)

let check_units members =
  let rec check forbidden = function
    [] -> ()
  | mb :: tl ->
      begin match mb.pm_kind with
      | PM_intf -> ()
      | PM_impl (info, _link_info, _ty) ->
          CU.Map.iter
            (fun unit _crc ->
              let name = CU.name unit in
              if CU.Name.Set.mem name forbidden
              then raise(Error(Forward_reference(mb.pm_file, name))))
            (UI.imports_cmx info)
      end;
      check (CU.Name.Set.remove mb.pm_name forbidden) tl
  in
  let forbidden =
    CU.Name.Set.of_list (List.map (fun mb -> mb.pm_name) members)
  in
  check forbidden members

(* Make the .o file for the package *)

let make_package_object
    ~ppf_dump members targetobj current_unit coercion ~backend =
  Profile.record_call
    (Printf.sprintf "pack(%s)" (CU.Name.to_string (CU.name current_unit)))
    (fun () ->
    let objtemp =
      if !Clflags.keep_asm_file
      then Filename.remove_extension targetobj ^ ".pack" ^ Config.ext_obj
      else
        (* Put the full name of the module in the temporary file name
           to avoid collisions with MSVC's link /lib in case of successive
           packs *)
        let symbol = Symbol.for_module_block current_unit in
        let backend_sym = Backend_sym.of_symbol symbol in
        Filename.temp_file (Backend_sym.to_string backend_sym) Config.ext_obj
    in
    let curr_package_as_prefix =
      let params = List.rev !Clflags.functor_parameters in
      CU.for_pack_prefix current_unit @
      [CU.Prefix.Pack (CU.name current_unit, List.map CU.Name.of_string params)]
    in
    let components =
      List.map
        (fun m ->
           let id =
             Ident.create_persistent
               ~prefix:curr_package_as_prefix
               (CU.Name.to_string m.pm_name) in
           match m.pm_kind with
           | PM_intf ->
               Lambda.PM_intf id
           | PM_impl (ui, _, ty) ->
               let is_functor = match ty with
                   Types.Unit_functor (_, _) -> true
                 | _ -> false in
               let deps = UI.functorized_pack_imports ui in
               Lambda.PM_impl (id, deps, is_functor))
        members
    in
    let unit_names_in_pack =
      CU.Name.Set.of_list (List.map (fun m -> m.pm_name) members)
    in
    let imports_cmi =
      CU.Map.filter (fun cu _crc ->
          not (CU.Name.Set.mem (CU.name cu) unit_names_in_pack))
        (Asmlink.extract_crc_interfaces ())
    in
    let functor_dependencies =
      List.filter_map (fun (unit, _) ->
          let prefix = CU.for_pack_prefix unit in
          if CU.Prefix.(
              in_common_functor curr_package_as_prefix prefix ||
              in_functor_parameters
                (Compilation_unit.name unit) (CU.for_pack_prefix current_unit))
          then
            Some (Ident.create_local (CU.for_address unit))
          else None) (CU.Map.bindings imports_cmi) in
    let targetname = CU.Name.to_string (CU.name current_unit) in
    let module_ident = Ident.create_persistent targetname in
    let prefixname = Filename.remove_extension objtemp in
    let required_globals = Ident.Set.empty in
    let program, middle_end =
      if Config.flambda then
        let main_module_block_size, code =
          Translmod.transl_package_flambda
            current_unit components functor_dependencies coercion
        in
        let code = Simplif.simplify_lambda code in
        let program =
          { Lambda.
            code;
            main_module_block_size;
            module_ident;
            required_globals;
          }
        in
        program, Flambda_middle_end.lambda_to_clambda
      else
        let main_module_block_size, code =
          Translmod.transl_store_package
            current_unit
            components
            functor_dependencies
            coercion
        in
        let code = Simplif.simplify_lambda code in
        let program =
          { Lambda.
            code;
            main_module_block_size;
            module_ident;
            required_globals;
          }
        in
        program, Closure_middle_end.lambda_to_clambda
    in
    Asmgen.compile_implementation ~backend
      ~filename:targetname
      ~prefixname
      ~middle_end
      ~ppf_dump
      program;
    let objfiles =
      List.map
        (fun m -> Filename.remove_extension m.pm_file ^ Config.ext_obj)
        (List.filter (fun m -> m.pm_kind <> PM_intf) members)
    in
    let ok =
      Ccomp.call_linker Ccomp.Partial targetobj (objtemp :: objfiles) ""
    in
    remove_file objtemp;
    if not ok then raise(Error Linking_error)
  )

(* Make the .cmx file for the package *)

let build_package_cmx current_unit members cmxfile =
  let module UI = Cmx_format.Unit_info in
  let unit_names_in_pack =
    CU.Name.Set.of_list (List.map (fun m -> m.pm_name) members)
  in
  let units, unit_link_infos =
    List.split (
      List.fold_right (fun m accu ->
          match m.pm_kind with
          | PM_intf -> accu
          | PM_impl (info, link_info, _) -> (info, link_info) :: accu)
        members [])
  in
  let compilation_state = Compilation_state.Snapshot.create () in
  let current_unit_name = CU.name current_unit in
  let current_unit_crc = Env.crc_of_unit current_unit_name in
  let package_prefix = CU.for_pack_prefix current_unit in
  let functor_parameters =
    List.fold_left (fun acc p ->
        CU.Name.of_string p :: acc) [] !Clflags.functor_parameters in
  let curr_package_as_prefix =
    package_prefix @ [CU.Prefix.Pack (current_unit_name, functor_parameters)]
  in
  let imports_cmi =
    CU.Map.filter (fun cu _crc ->
        not (CU.Name.Set.mem (CU.name cu) unit_names_in_pack) &&
        not (List.exists CU.(Name.equal (name cu)) functor_parameters))
      (Asmlink.extract_crc_interfaces ())
  in
  let functorized_pack_imports =
    List.filter_map (fun (unit, _) ->
        let prefix = CU.for_pack_prefix unit in
        if CU.Prefix.(
            in_common_functor curr_package_as_prefix prefix ||
            in_functor_parameters
              (Compilation_unit.name unit) package_prefix)
        then Some unit
        else None) (CU.Map.bindings imports_cmi) in
  let imports_cmi =
    CU.Map.add current_unit (Some current_unit_crc) imports_cmi
  in
  let imports_cmx =
    CU.Map.filter (fun imported_unit _crc ->
        let name = CU.name imported_unit in
        not (CU.Name.Set.mem name unit_names_in_pack))
      (Asmlink.extract_crc_implementations ())
  in
  let defines =
    (List.flatten (List.map UI.defines units)) @ [current_unit]
  in
  let export_info : UI.export_info =
    match compilation_state.export_info with
    | Closure approx -> Closure approx
    | Flambda export_info ->
      let export_info =
        List.fold_left (fun acc info ->
            match UI.export_info info with
            | Flambda export_info -> Export_info.merge acc export_info
            | Closure _ ->
              Misc.fatal_errorf "%a contains Closure approximations yet \
                  Flambda export info was found"
                CU.print (UI.unit info))
          export_info
          units
      in
      Flambda export_info
  in
  let pkg_infos =
    UI.create ~unit:current_unit ~defines ~imports_cmi ~imports_cmx
      ~functorized_pack_imports ~export_info
  in
  let pkg_link_infos = Cmx_format.Unit_info_link_time.join unit_link_infos in
  Cmx_format.save pkg_infos pkg_link_infos ~filename:cmxfile

(* Make the .cmx and the .o for the package *)

let package_object_files ~ppf_dump files targetcmx
                         targetobj (compunit : CU.t) coercion ~backend =
  let members = map_left_right (read_member_info compunit) files in
  check_units members;
  make_package_object ~ppf_dump members targetobj compunit coercion ~backend;
  build_package_cmx compunit members targetcmx

(* The entry point *)

let package_files ~ppf_dump initial_env files targetcmx ~backend =
  let files =
    List.map
      (fun f ->
        try Load_path.find f
        with Not_found -> raise(Error(File_not_found f)))
      files in
  let prefix = chop_extensions targetcmx in
  let targetcmi = prefix ^ ".cmi" in
  let targetobj = Filename.remove_extension targetcmx ^ Config.ext_obj in
  let targetname =
    CU.Name.of_string (String.capitalize_ascii(Filename.basename prefix)) in
  (* Set the name of the current "input" *)
  Location.input_name := targetcmx;
  (* Set the name of the current compunit *)
  let for_pack_prefix =
      CU.Prefix.parse_for_pack !Clflags.for_package in
  let comp_unit = CU.create ~for_pack_prefix targetname in
  Persistent_env.Current_unit.set_unit comp_unit;
  Compilation_state.reset comp_unit;
  Misc.try_finally (fun () ->
      let coercion =
        Typemod.package_units initial_env files targetcmi targetname in
      package_object_files ~ppf_dump files targetcmx targetobj comp_unit
        coercion ~backend
    )
    ~exceptionally:(fun () -> remove_file targetcmx; remove_file targetobj)

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_renaming { name_in_cmx; file; desired_name; } ->
      fprintf ppf "Wrong file naming: %a@ contains the code for\
                   @ %a when %a was expected"
        Location.print_filename file CU.Name.print name_in_cmx
        CU.Name.print desired_name
  | Forward_reference(file, cu_name) ->
      fprintf ppf "Forward reference to %a in file %a" CU.Name.print cu_name
        Location.print_filename file
  | Wrong_for_pack(file, path) ->
      fprintf ppf "File %a@ was not compiled with the `-for-pack %a' option"
        Location.print_filename file
        Compilation_unit.Prefix.print path
  | File_not_found file ->
      fprintf ppf "File %s not found" file
  | Linking_error ->
      fprintf ppf "Error during partial linking"

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

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

(* "Package" a set of .cmo files into one .cmo file having the
   original compilation units as sub-modules. *)

open Misc
open Instruct
open Cmo_format
module String = Misc.Stdlib.String
module CU = Compilation_unit

type error =
    Forward_reference of string * Ident.t
  | Multiple_definition of string * Ident.t
  | Not_an_object_file of string
  | Illegal_renaming of CU.Name.t * string * CU.Name.t
  | File_not_found of string
  | Wrong_for_pack of string * CU.Prefix.t

exception Error of error

(* References accumulating information on the .cmo files *)

let relocs = ref ([] : (reloc_info * int) list)
let events = ref ([] : debug_event list)
let debug_dirs = ref String.Set.empty
let primitives = ref ([] : string list)
let force_link = ref false

(* Update a relocation offset and check for the link order. *)

let update_offset objfile identifiers defined base (rel, ofs) =
  begin
    match rel with
      Reloc_getglobal id ->
        if List.mem id identifiers && not (List.mem id defined)
        then raise(Error(Forward_reference(objfile, id)))
    | Reloc_setglobal id ->
        if List.mem id identifiers && List.mem id defined
        then raise(Error(Multiple_definition(objfile, id)))
    | _ -> ()
  end;
  relocs := (rel, base + ofs) :: !relocs

(* Record and relocate a debugging event *)

let relocate_debug base subst ev =
  let ev' = { ev with ev_pos = base + ev.ev_pos;
                      ev_typsubst = Subst.compose ev.ev_typsubst subst } in
  events := ev' :: !events

(* Read the unit information from a .cmo file. *)

type pack_member_kind =
    PM_intf
  | PM_impl of compilation_unit * Types.compilation_unit

type pack_member =
  { pm_file: string;
    pm_name: CU.Name.t;
    pm_kind: pack_member_kind;
  }

let read_member_info current_unit file = (
  let name =
    CU.Name.of_string
      (String.capitalize_ascii(Filename.basename(chop_extensions file))) in
  let kind =
    (* PR#7479: make sure it is either a .cmi or a .cmo *)
    if Filename.check_suffix file ".cmi" then
      PM_intf
    else begin
      let ic = open_in_bin file in
      try
        let buffer =
          really_input_string ic (String.length Config.cmo_magic_number)
        in
        if buffer <> Config.cmo_magic_number then
          raise(Error(Not_an_object_file file));
        let compunit_pos = input_binary_int ic in
        seek_in ic compunit_pos;
        let compunit = (input_value ic : compilation_unit) in
        if not (CU.Name.equal compunit.cu_name name)
        then raise(Error(Illegal_renaming(name, file, compunit.cu_name)));
        let params =
          List.fold_left (fun acc p ->
              CU.Name.of_string p :: acc) [] !Clflags.functor_parameters in
        let full_path_with_params =
          Compilation_unit.(
            for_pack_prefix current_unit @
            [ Prefix.Pack (name current_unit, params) ])
        in
        if not (Compilation_unit.Prefix.equal
                  compunit.cu_prefix full_path_with_params) then
          raise (Error (Wrong_for_pack (file, full_path_with_params)));
        close_in ic;
        PM_impl (compunit,
                 Env.read_interface name (chop_extensions file ^ ".cmi"))
      with x ->
        close_in ic;
        raise x
    end in
  { pm_file = file; pm_name = name; pm_kind = kind }
)

(* Read the bytecode from a .cmo file.
   Write bytecode to channel [oc].
   Rename globals as indicated by [mapping] in reloc info.
   Accumulate relocs, debug info, etc.
   Return size of bytecode. *)

let append_bytecode oc mapping defined ofs subst objfile compunit =
  let ic = open_in_bin objfile in
  try
    Bytelink.check_consistency objfile compunit;
    List.iter
      (update_offset objfile mapping defined ofs)
      compunit.cu_reloc;
    primitives := compunit.cu_primitives @ !primitives;
    if compunit.cu_force_link then force_link := true;
    seek_in ic compunit.cu_pos;
    Misc.copy_file_chunk ic oc compunit.cu_codesize;
    if !Clflags.debug && compunit.cu_debug > 0 then begin
      seek_in ic compunit.cu_debug;
      List.iter (relocate_debug ofs subst) (input_value ic);
      debug_dirs := List.fold_left
          (fun s e -> String.Set.add e s)
          !debug_dirs
          (input_value ic);
    end;
    close_in ic;
    compunit.cu_codesize
  with x ->
    close_in ic;
    raise x

(* Same, for a list of .cmo and .cmi files.
   Return total size of bytecode. *)

let rec append_bytecode_list packagename oc identifiers defined ofs subst =
  function
    [] ->
      ofs
  | m :: rem ->
      match m.pm_kind with
      | PM_intf ->
          append_bytecode_list packagename oc identifiers defined ofs
            subst rem
      | PM_impl (compunit, _) ->
          let size =
            append_bytecode oc identifiers defined ofs
              subst m.pm_file compunit in
          (* /!\ TEMP *)
          let prefix = CU.Prefix.parse_for_pack (Some packagename) in
          let id =
            Ident.create_persistent ~prefix (CU.Name.to_string m.pm_name) in
          let root = Path.Pident id in
          append_bytecode_list packagename oc identifiers (id :: defined)
            (ofs + size)
            (Subst.add_module id (Path.Pdot (root, Ident.name id))
               subst)
            rem

(* Generate the code that builds the tuple representing the package module *)

let build_global_target
    ~ppf_dump oc target_name members identifiers dependencies pos coercion =

  let current_prefix =
      Compilation_unit.Prefix.parse_for_pack !Clflags.for_package in
  let current_unit =
    Compilation_unit.create ~for_pack_prefix:current_prefix target_name in
  let components =
    List.map2
      (fun m id ->
         match m.pm_kind with
         | PM_intf -> Lambda.PM_intf id
         | PM_impl (cu, ty) ->
             let is_functor = match ty with
                 Types.Unit_functor (_, _) -> true
               | _ -> false in
             Lambda.PM_impl (id, cu.cu_functor_pack_imports, is_functor))
      members identifiers in
  let lam =
    Translmod.transl_package
      current_unit components dependencies coercion in
  let lam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then
    Format.fprintf ppf_dump "%a@." Printlambda.lambda lam;
  let instrs =
    Bytegen.compile_implementation target_name lam in
  let rel =
    Emitcode.to_packed_file oc instrs in
  relocs := List.map (fun (r, ofs) -> (r, pos + ofs)) rel @ !relocs

(* Build the .cmo file obtained by packaging the given .cmo files. *)

let package_object_files ~ppf_dump files targetfile targetname coercion =
  let for_pack_prefix =
    Compilation_unit.Prefix.parse_for_pack !Clflags.for_package in
  let current_unit =
    Compilation_unit.create ~for_pack_prefix targetname in
  let members =
    map_left_right (read_member_info current_unit) files in
  let required_globals =
    List.fold_right (fun compunit required_globals -> match compunit with
        | { pm_kind = PM_intf } ->
            required_globals
        | { pm_kind = PM_impl ({ cu_required_globals; cu_reloc }, _) } ->
            let remove_required (rel, _pos) required_globals =
              match rel with
                Reloc_setglobal id ->
                  Ident.Set.remove id required_globals
              | _ ->
                  required_globals
            in
            let required_globals =
              List.fold_right remove_required cu_reloc required_globals
            in
            List.fold_right Ident.Set.add cu_required_globals required_globals)
      members Ident.Set.empty
  in
  let curr_package_as_prefix =
    let params = List.fold_left (fun acc p ->
        CU.Name.of_string p :: acc) [] !Clflags.functor_parameters in
    for_pack_prefix @
    [CU.Prefix.Pack (targetname, params)]
  in
  let unit_names =
    List.map (fun m -> m.pm_name) members in
  let identifiers =
    List.map
      (fun name ->
         Ident.create_persistent
           ~prefix:curr_package_as_prefix
           (CU.Name.to_string name))
      unit_names in
  let oc = open_out_bin targetfile in
  try
    output_string oc Config.cmo_magic_number;
    let pos_depl = pos_out oc in
    output_binary_int oc 0;
    let pos_code = pos_out oc in
    let ofs =
      append_bytecode_list
        (CU.Prefix.for_address curr_package_as_prefix) oc identifiers [] 0
        Subst.identity members in
    let imports =
      List.filter
        (fun (unit, _crc) ->
           not (List.mem (CU.name unit) unit_names) &&
           not (CU.Prefix.in_functor_parameters (CU.name unit)
                  curr_package_as_prefix))
        (Bytelink.extract_crc_interfaces()) in
    let functor_dependencies =
      List.filter_map (fun (unit, _) ->
          let prefix = CU.for_pack_prefix unit in
          if CU.Prefix.(
              in_common_functor curr_package_as_prefix prefix ||
              in_functor_parameters
                (CU.name unit) curr_package_as_prefix)
          then
            Some (unit,
                  Ident.create_persistent ~prefix
                    CU.(Name.to_string (name unit)))
          else None) imports in
    let functor_pack_imports, functor_dependencies =
      List.split functor_dependencies in
    build_global_target ~ppf_dump oc targetname
      members identifiers functor_dependencies ofs coercion;
    let pos_debug = pos_out oc in
    if !Clflags.debug && !events <> [] then begin
      output_value oc (List.rev !events);
      output_value oc (String.Set.elements !debug_dirs);
    end;
    let pos_final = pos_out oc in
    let compunit =
      { cu_name = targetname;
        cu_prefix = for_pack_prefix;
        cu_pos = pos_code;
        cu_codesize = pos_debug - pos_code;
        cu_reloc = List.rev !relocs;
        cu_imports =
          (current_unit, Some (Env.crc_of_unit targetname)) :: imports;
        cu_primitives = !primitives;
        cu_required_globals = Ident.Set.elements required_globals;
        cu_functor_pack_imports = functor_pack_imports;
        cu_force_link = !force_link;
        cu_debug = if pos_final > pos_debug then pos_debug else 0;
        cu_debugsize = pos_final - pos_debug } in
    Emitcode.marshal_to_channel_with_possibly_32bit_compat
      ~filename:targetfile ~kind:"bytecode unit"
      oc compunit;
    seek_out oc pos_depl;
    output_binary_int oc pos_final;
    close_out oc
  with x ->
    close_out oc;
    raise x

(* The entry point *)

let package_files ~ppf_dump initial_env files targetfile =
  let files =
    List.map
      (fun f ->
         try Load_path.find f
         with Not_found -> raise(Error(File_not_found f)))
      files in
  let prefix = chop_extensions targetfile in
  let targetcmi = prefix ^ ".cmi" in
  let targetname =
    CU.Name.of_string (String.capitalize_ascii(Filename.basename prefix)) in
  Persistent_env.Current_unit.set targetname;
  Misc.try_finally (fun () ->
      let coercion =
        Typemod.package_units initial_env files targetcmi targetname in
      package_object_files ~ppf_dump files targetfile targetname coercion
      )
      ~exceptionally:(fun () -> remove_file targetfile)

(* Error report *)

open Format

let report_error ppf = function
    Forward_reference(file, ident) ->
      fprintf ppf "Forward reference to %s in file %a" (Ident.name ident)
        Location.print_filename file
  | Multiple_definition(file, ident) ->
      fprintf ppf "File %a redefines %s"
        Location.print_filename file
        (Ident.name ident)
  | Not_an_object_file file ->
      fprintf ppf "%a is not a bytecode object file"
        Location.print_filename file
  | Illegal_renaming(name, file, id) ->
      fprintf ppf "Wrong file naming: %a@ contains the code for\
                   @ %a when %a was expected"
        Location.print_filename file
        CU.Name.print name
        CU.Name.print id
  | File_not_found file ->
      fprintf ppf "File %s not found" file
  | Wrong_for_pack(file, path) ->
      fprintf ppf "File %a@ was not compiled with the `-for-pack %a' option"
        Location.print_filename file
        Compilation_unit.Prefix.print path

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let reset () =
  relocs := [];
  events := [];
  primitives := [];
  force_link := false

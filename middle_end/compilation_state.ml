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

module CU = Compilation_unit
module UI = Cmx_format.Unit_info

type error =
  | Illegal_renaming of {
      contains_unit : CU.t;
      (* CR-someday mshinwell: As per CR below, change the type of this from
         [CU.Name.t] to [CU.t]. *)
      desired_unit_name : CU.Name.t;
      filename : string;
    }
  | Illegal_import_for_pack_prefix of {
      found_unit_name : CU.Name.t;
      for_pack_prefix_in_cmx : CU.Prefix.t;
      current_for_pack_prefix : CU.Prefix.t;
      filename : string;
    }
  | Wrong_for_pack_prefix of {
      expected_prefix : CU.Prefix.t;
      found_prefix : CU.Prefix.t;
      filename : string;
    }

exception Error of error

module Error = struct
  (* CR mshinwell: Try to avoid writing these types out here. *)
  type t = error =
    | Illegal_renaming of {
        contains_unit : CU.t;
        desired_unit_name : CU.Name.t;
        filename : string;
      }
    | Illegal_import_for_pack_prefix of {
        found_unit_name : CU.Name.t;
        for_pack_prefix_in_cmx : CU.Prefix.t;
        current_for_pack_prefix : CU.Prefix.t;
        filename : string;
      }
    | Wrong_for_pack_prefix of {
        expected_prefix : CU.Prefix.t;
        found_prefix : CU.Prefix.t;
        filename : string;
      }

  let report_error ppf error =
    let print_prefix ppf prefix =
      match prefix with
      | [] -> Format.pp_print_string ppf "no `-for-pack' prefix"
      | _ ->
        Format.fprintf ppf "a `-for-pack' prefix of [%a]"
          CU.Prefix.print prefix
    in
    match error with
    | Illegal_renaming { contains_unit; desired_unit_name; filename; } ->
      Format.fprintf ppf "%a contains the description for unit@ \
          [%a] when [%a] was expected"
        Location.print_filename filename
        CU.print_name contains_unit
        CU.Name.print desired_unit_name
    | Wrong_for_pack_prefix { expected_prefix; found_prefix; filename; } ->
      Format.fprintf ppf "%s contains the description for a unit with@ \
          %a;@ but %a was expected"
        filename
        print_prefix found_prefix
        print_prefix expected_prefix
    | Illegal_import_for_pack_prefix { found_unit_name;
        for_pack_prefix_in_cmx; current_for_pack_prefix; filename; } ->
      Format.fprintf ppf "%s contains the description for a unit [%a] with@ \
          %a; this cannot be used because@ "
        filename
        CU.Name.print found_unit_name
        print_prefix for_pack_prefix_in_cmx;
      begin match current_for_pack_prefix with
      | [] ->
        Format.fprintf ppf "the current unit is being compiled \
          without a `-for-pack' prefix"
      | _ ->
        Format.fprintf ppf "the current unit has %a"
          print_prefix current_for_pack_prefix
      end

  let () =
    Location.register_error_of_exn (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None)
end

type export_info =
  | Closure of Clambda.value_approximation
  | Flambda of Export_info.t

type t = {
  mutable defines : CU.t list;
  mutable imports_cmx : Digest.t option CU.Map.t;
  mutable export_info : export_info;
}

let empty_export_info =
  if Config.flambda then Flambda Export_info.empty
  else Closure Value_unknown

let empty = {
  defines = [];
  imports_cmx = CU.Map.empty;
  export_info = empty_export_info;
}

let current_unit = empty

let merged_flambda_export_info = ref Export_info.empty

type find_cmx_result =
  | Current_unit
  | Already_loaded of UI.t
  | Just_loaded of { info : UI.t; filename : string; }
  | No_cmx_file_or_opaque

(* CR-someday mshinwell: The type of [global_infos_table] should be changed
   to [_ CU.Tbl.t] when packing has been improved to provide
   "-for-pack" information on .cmi files.  Then, the checks against [CU.name]
   below will just turn into [CU.equal]. *)
let global_infos_table : find_cmx_result CU.Name.Tbl.t = CU.Name.Tbl.create 17

(* This is the equivalent of the old [Compilenv.get_global_info]. *)
let find_or_load_unit_info_from_cmx ?comp_unit desired_unit
  : find_cmx_result =
  let curr = Persistent_env.Current_unit.get_exn () in
  let current_unit_name = CU.name curr in
  let desired_unit_name = CU.name desired_unit in
  if CU.Name.equal desired_unit_name current_unit_name then
    Current_unit
  else
    try CU.Name.Tbl.find global_infos_table desired_unit_name
    with Not_found ->
      let desired_unit_name_as_string = CU.Name.to_string desired_unit_name in
      let info_and_crc =
        if Env.is_imported_opaque desired_unit_name then None
        else begin
          match Load_path.find_uncap (desired_unit_name_as_string ^ ".cmx") with
          | exception Not_found ->
            let warn = Warnings.No_cmx_file desired_unit_name_as_string in
            Location.prerr_warning Location.none warn;
            None
          | filename ->
            let info, crc = Cmx_format.load_compile_time_info ~filename in
            let contains_unit = UI.unit info in
            if not (CU.Name.equal (CU.name contains_unit) desired_unit_name)
            then begin
              raise (Error (
                Illegal_renaming {
                  contains_unit;
                  desired_unit_name;
                  filename;
                }))
            end;
            Some (info, crc, filename)
        end
      in
      begin match info_and_crc with
      | None ->
          (* If the caller could not specify the [Compilation_unit.t], perhaps
           because they only had an [Ident.t], then we have to assume that the
           .cmx file is not packed. *)
        let comp_unit =
          match comp_unit with
          | None -> desired_unit
          | Some comp_unit -> comp_unit
        in
        current_unit.imports_cmx <-
          CU.Map.add comp_unit None current_unit.imports_cmx;
        CU.Name.Tbl.add global_infos_table desired_unit_name
          No_cmx_file_or_opaque;
        No_cmx_file_or_opaque
      | Some (info, crc, filename) ->
        current_unit.imports_cmx <-
          CU.Map.add (UI.unit info) (Some crc) current_unit.imports_cmx;
        begin match UI.export_info info with
        | Closure _ -> ()
        | Flambda export_info ->
          merged_flambda_export_info :=
            Export_info.merge !merged_flambda_export_info export_info
        end;
        CU.Name.Tbl.add global_infos_table desired_unit_name
          (Already_loaded info);
        Just_loaded { info; filename; }
      end

type compilation_unit_or_predef =
  | Compilation_unit of Compilation_unit.t
  | Predef

(* CR mshinwell: We may be missing the toplevel_approx check that used
   to live in [symbol_for_global]. *)

let compilation_unit_for_global id : compilation_unit_or_predef =
  if not (Ident.persistent id || Ident.is_predef id) then begin
    Misc.fatal_errorf "[compilation_unit_for_global] with identifier %a \
        that is neither persistent nor predefined"
      Ident.print id
  end;
  if Ident.is_predef id then begin
    Predef
  end else begin
    let desired_unit = CU.of_raw_string (Ident.name id) in
    match find_or_load_unit_info_from_cmx desired_unit with
    | Current_unit ->
      Compilation_unit (Persistent_env.Current_unit.get_exn ())
    | No_cmx_file_or_opaque ->
      (* Assume that the compilation unit (called [id]), whose .cmx file
         is missing, is not packed. *)
      Compilation_unit desired_unit
    | Already_loaded info -> Compilation_unit (UI.unit info)
    | Just_loaded { info; filename; } ->
      let for_pack_prefix_in_cmx = CU.for_pack_prefix (UI.unit info) in
      let current_for_pack_prefix =
        CU.for_pack_prefix (Persistent_env.Current_unit.get_exn ())
      in
      let is_valid_prefix =
        Misc.Stdlib.List.is_prefix ~equal:CU.Prefix.equal_component
          for_pack_prefix_in_cmx ~of_:current_for_pack_prefix
      in
      if not is_valid_prefix then begin
        raise (Error (
          Illegal_import_for_pack_prefix {
            found_unit_name = CU.name (UI.unit info);
            for_pack_prefix_in_cmx;
            current_for_pack_prefix;
            filename;
          }))
      end;
      Compilation_unit (UI.unit info)
  end

let cache_unit_info ui =
  CU.Name.Tbl.add global_infos_table (CU.name (UI.unit ui)) (Already_loaded ui);
  match UI.export_info ui with
  | Closure _ -> ()
  | Flambda export_info ->
      merged_flambda_export_info :=
        Export_info.merge !merged_flambda_export_info export_info

let check_is_flambda flambda =
  if Config.flambda <> flambda then begin
    Misc.fatal_errorf "This function can only be used in %s mode"
      (if flambda then "Flambda" else "Closure")
  end

module Closure_only = struct
  module CstMap =
    Map.Make(struct
      type t = Clambda.ustructured_constant
      let compare = Clambda.compare_structured_constants
      (* PR#6442: it is incorrect to use Stdlib.compare on values of type t
         because it compares "0.0" and "-0.0" equal. *)
    end)

  type structured_constants =
    { strcst_shared : Symbol.t CstMap.t;
      strcst_all: (Symbol.t * Clambda.ustructured_constant) list;
    }

  let structured_constants_empty  =
    { strcst_shared = CstMap.empty;
      strcst_all = [];
    }

  let structured_constants = ref structured_constants_empty
  let exported_constants = Symbol.Tbl.create 17

  let reset () =
    structured_constants := structured_constants_empty;
    Symbol.Tbl.clear exported_constants

  let add_exported_constant s =
    check_is_flambda false;
    Symbol.Tbl.replace exported_constants s ()

  let snapshot () =
    check_is_flambda false;
    !structured_constants

  let backtrack s =
    check_is_flambda false;
    structured_constants := s

  let new_structured_constant cst ~shared =
    check_is_flambda false;
    let {strcst_shared; strcst_all} = !structured_constants in
    if shared then
      try
        CstMap.find cst strcst_shared
      with Not_found ->
        let sym = Symbol.for_lifted_anonymous_constant () in
        structured_constants :=
          {
            strcst_shared = CstMap.add cst sym strcst_shared;
            strcst_all = (sym, cst) :: strcst_all;
          };
        sym
    else
      let sym = Symbol.for_lifted_anonymous_constant () in
      structured_constants :=
        {
          strcst_shared;
          strcst_all = (sym, cst) :: strcst_all;
        };
      sym

  let clear_structured_constants () =
    check_is_flambda false;
    structured_constants := structured_constants_empty

  let structured_constant_of_symbol s =
    List.assoc_opt s (!structured_constants).strcst_all

  let structured_constants () =
    check_is_flambda false;
    let provenance : Clambda.usymbol_provenance =
      { original_idents = [];
        module_path =
          Path.Pident (Ident.create_persistent (
              CU.Name.to_string
                (CU.name (Persistent_env.Current_unit.get_exn ()))));
      }
    in
    List.map
      (fun (symbol, definition) ->
         { Clambda.
           symbol;
           exported = Symbol.Tbl.mem exported_constants symbol;
           definition;
           provenance = Some provenance;
         })
      (!structured_constants).strcst_all

  let toplevel_approx :
    Clambda.value_approximation CU.Name.Tbl.t = CU.Name.Tbl.create 16

  let record_global_approx_toplevel () =
    check_is_flambda false;
    let approx =
      match current_unit.export_info with
      | Closure approx -> approx
      | Flambda _ ->
        Misc.fatal_error "Expected Closure approximations but found \
          Flambda export info"
    in
    CU.Name.Tbl.add toplevel_approx
      (CU.name (Persistent_env.Current_unit.get_exn ())) approx

  let global_approx id =
    if not (Ident.persistent id || Ident.is_predef id) then begin
      Misc.fatal_errorf "[global_approx] with identifier %a \
          that is neither persistent nor predefined"
        Ident.print id
    end;
    if Ident.is_predef id then Clambda.Value_unknown
    else
      let name = CU.Name.of_string (Ident.name id) in
      let unit = CU.of_raw_string (Ident.name id) in
      try CU.Name.Tbl.find toplevel_approx name
      with Not_found ->
        match find_or_load_unit_info_from_cmx unit with
        | No_cmx_file_or_opaque -> Clambda.Value_unknown
        | Current_unit ->
          begin match current_unit.export_info with
          | Closure approx -> approx
          | Flambda _ ->
            Misc.fatal_errorf "%a: Expected Closure approximations but found \
                Flambda export info"
              CU.Name.print name
          end
        | Already_loaded info | Just_loaded { info; filename = _; } ->
          begin match UI.export_info info with
          | Closure approx -> approx
          | Flambda _ ->
            Misc.fatal_errorf "%a: Expected Closure approximations but \
                found Flambda export info"
              CU.Name.print name
          end

  let set_global_approx approx =
    check_is_flambda false;
    current_unit.export_info <- Closure approx
end

module Flambda_only = struct
  let imported_sets_of_closures_table = Set_of_closures_id.Tbl.create 10

  let reset () =
    merged_flambda_export_info := Export_info.empty;
    Set_of_closures_id.Tbl.clear imported_sets_of_closures_table

  let set_export_info export_info =
    check_is_flambda true;
    current_unit.export_info <- Flambda export_info

  let export_info_for_unit comp_unit =
    let current_comp_unit = Persistent_env.Current_unit.get_exn () in
    let desired_path = CU.full_path comp_unit in
    let current_path = CU.full_path current_comp_unit in
    let prefix_result =
      Misc.Stdlib.List.find_and_chop_longest_common_prefix
        ~equal:CU.Prefix.equal_component
        ~first:desired_path ~second:current_path
    in
    let for_pack_prefix = prefix_result.longest_common_prefix in
    let desired_path_without_for_pack_prefix = prefix_result.first_without_longest_common_prefix
    in
    let name_of_unit_containing_symbol =
      match desired_path_without_for_pack_prefix with
      | [] -> CU.name current_comp_unit
      | CU.Prefix.Pack (name_of_unit_containing_symbol, _) ::_ ->
          name_of_unit_containing_symbol
    in
    let extract_flambda_export_info info =
      match UI.export_info info with
      | Closure _ ->
        Misc.fatal_errorf "%a: Expected Flambda export info but \
            found Closure approximations"
          CU.print (UI.unit info)
      | Flambda export_info -> Some export_info
    in
    match
      find_or_load_unit_info_from_cmx ~comp_unit
        (CU.create name_of_unit_containing_symbol)
    with
    | No_cmx_file_or_opaque -> None
    | Current_unit ->
      begin match current_unit.export_info with
      | Closure _ ->
        Misc.fatal_errorf "%a (current unit): expected Flambda export info \
            but found Closure approximations"
          CU.print (Persistent_env.Current_unit.get_exn ())
      | Flambda export_info -> Some export_info
      end
    | Already_loaded info -> extract_flambda_export_info info
    | Just_loaded { info; filename; } ->
      let for_pack_prefix_in_cmx = CU.for_pack_prefix (UI.unit info) in
      if not (CU.Prefix.equal for_pack_prefix
                for_pack_prefix_in_cmx)
      then begin
        raise (Error (
          Wrong_for_pack_prefix {
            expected_prefix = for_pack_prefix;
            found_prefix = for_pack_prefix_in_cmx;
            filename;
          }))
      end;
      extract_flambda_export_info info

  let merged_export_info () =
    check_is_flambda true;
    !merged_flambda_export_info
end

let reset comp_unit =
  CU.Name.Tbl.clear global_infos_table;
  current_unit.defines <- [comp_unit];
  current_unit.imports_cmx <- CU.Map.empty;
  current_unit.export_info <- empty_export_info;
  Closure_only.reset ();
  Flambda_only.reset ()

let require_global global_ident =
  if not (Ident.is_predef global_ident) then begin
    ignore (compilation_unit_for_global global_ident
      : compilation_unit_or_predef)
  end

module Snapshot = struct
  type nonrec export_info = export_info =
    | Closure of Clambda.value_approximation
    | Flambda of Export_info.t

  type t = {
    unit : CU.t;
    defines : Compilation_unit.t list;
    imports_cmx : Digest.t option CU.Map.t;
    export_info : export_info;
  }

  let create () =
    { unit = Persistent_env.Current_unit.get_exn ();
      defines = current_unit.defines;
      imports_cmx = current_unit.imports_cmx;
      export_info = current_unit.export_info;
    }
end

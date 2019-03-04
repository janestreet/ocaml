(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*         Mehdi Dogguy, PPS laboratory, University Paris Diderot         *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2010 Mehdi Dogguy                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Dump info on .cmi, .cmo, .cmx, .cma, .cmxa, .cmxs files
   and on bytecode executables. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Printf
open Misc
open Config
open Cmo_format

module CU = Compilation_unit
module LI = Cmxa_format.Library_info
module DH = Cmxs_format.Dynheader_info
module DU = Cmxs_format.Dynunit_info
module UI = Cmx_format.Unit_info
module UIL = Cmx_format.Unit_info_link_time

module Int = Numbers.Int

(* Command line options to prevent printing approximation,
   function code and CRC
 *)
let no_approx = ref false
let no_code = ref false
let no_crc = ref false
let full_path = ref false

let input_stringlist ic len =
  let get_string_list sect len =
    let rec fold s e acc =
      if e != len then
        if sect.[e] = '\000' then
          fold (e+1) (e+1) (String.sub sect s (e-s) :: acc)
        else fold s (e+1) acc
      else acc
    in fold 0 0 []
  in
  let sect = really_input_string ic len in
  get_string_list sect len

let dummy_crc = String.make 32 '-'
let null_crc = String.make 32 '0'

let string_of_crc crc = if !no_crc then null_crc else Digest.to_hex crc

let print_name_crc (unit, crco) =
  let crc =
    match crco with
      None -> dummy_crc
    | Some crc -> string_of_crc crc
  in
  if not !full_path then
    printf "\t%s\t%s\n" crc CU.(Name.to_string (name unit))
  else
    printf "\t%s\t%s\n" crc
      (Format.asprintf "%a" CU.print_full_path unit)

let print_line name =
  printf "\t%s\n" name

let print_required_global id =
  printf "\t%s\n" (Ident.name id)

let print_functor_pack_dependency cu =
  printf "\t%s\n" (Compilation_unit.full_path_as_string cu)

let print_cmo_infos cu =
  printf "Unit name: %s\n" (CU.Name.to_string cu.cu_name);
  print_string "Interfaces imported:\n";
  List.iter print_name_crc cu.cu_imports;
  print_string "Required globals:\n";
  List.iter print_required_global cu.cu_required_globals;
  print_string "Functor pack dependencies:\n";
  List.iter print_functor_pack_dependency cu.cu_functor_pack_imports;
  printf "Uses unsafe features: ";
  (match cu.cu_primitives with
    | [] -> printf "no\n"
    | l  ->
        printf "YES\n";
        printf "Primitives declared in this module:\n";
        List.iter print_line l);
  printf "Force link: %s\n" (if cu.cu_force_link then "YES" else "no")

let print_spaced_string s =
  printf " %s" s

let print_cma_infos (lib : Cmo_format.library) =
  printf "Force custom: %s\n" (if lib.lib_custom then "YES" else "no");
  printf "Extra C object files:";
  (* PR#4949: print in linking order *)
  List.iter print_spaced_string (List.rev lib.lib_ccobjs);
  printf "\nExtra C options:";
  List.iter print_spaced_string lib.lib_ccopts;
  printf "\n";
  print_string "Extra dynamically-loaded libraries:";
  List.iter print_spaced_string lib.lib_dllibs;
  printf "\n";
  List.iter print_cmo_infos lib.lib_units

let print_pers_flags =
  let open Cmi_format in
  function
  | Rectypes -> printf " -rectypes"
  | Alerts _ -> ()
  | Opaque -> printf " -opaque"
  | Unsafe_string -> printf " -unsafe-string"
  | Pack prefix ->
      printf " -for-pack %s" (CU.Prefix.to_string prefix)
  | Parameter_of c ->
      printf " -parameter-of %s" (CU.full_path_as_string c)

let print_cmi_infos name crcs flags =
  printf "Unit name: %s\n" (CU.Name.to_string name);
  printf "Interfaces imported:\n";
  List.iter print_name_crc crcs;
  printf "Compilation flags:\n";
  List.iter print_pers_flags flags


let print_cmt_infos cmt =
  let open Cmt_format in
  printf "Cmt unit name: %s\n" (CU.Name.to_string cmt.cmt_modname);
  print_string "Cmt interfaces imported:\n";
  List.iter print_name_crc cmt.cmt_imports;
  printf "Source file: %s\n"
         (match cmt.cmt_sourcefile with None -> "(none)" | Some f -> f);
  printf "Compilation flags:";
  Array.iter print_spaced_string cmt.cmt_args;
  printf "\nLoad path:";
  List.iter print_spaced_string cmt.cmt_loadpath;
  printf "\n";
  printf "cmt interface digest: %s\n"
    (match cmt.cmt_interface_digest with
     | None -> ""
     | Some crc -> string_of_crc crc)

let print_name f name =
  printf "\t%s\n" (Format.asprintf "%a" f name)

let print_general_infos name crc defines cmi cmx fun_deps =
  printf "Name: %s\n" (Format.asprintf "%a" CU.Name.print (CU.name name));
  printf "CRC of implementation: %s\n" (string_of_crc crc);
  printf "Globals defined:\n";
  let defines =
    List.map (fun comp_unit -> CU.full_path_as_string comp_unit) defines
  in
  List.iter print_line defines;
  printf "Interfaces imported:\n";
  let cmi = CU.Map.bindings cmi in
  List.iter print_name_crc cmi;
  printf "Implementations imported:\n";
  let cmx = CU.Map.bindings cmx in
  List.iter print_name_crc cmx;
  if fun_deps <> [] then begin
    printf "Functorized pack components imported:\n";
    List.iter (print_name CU.print) fun_deps
  end

let print_global_table table =
  printf "Globals defined:\n";
  Symtable.iter_global_map
    (fun id _ -> print_line (Ident.name id))
    table

let print_cmx_infos (ui, ui_link, crc) =
  print_general_infos (UI.unit ui) crc (UI.defines ui)
    (UI.imports_cmi ui) (UI.imports_cmx ui)
    (UI.functorized_pack_imports ui);
  begin match UI.export_info ui with
  | Closure approx ->
    if not !no_approx then begin
      printf "Clambda approximation:\n";
      Format.fprintf Format.std_formatter "  %a@." Printclambda.approx approx
    end else
      Format.printf "Clambda unit@.";
  | Flambda export ->
    if not !no_approx || not !no_code then
      printf "Flambda export information:\n"
    else
      printf "Flambda unit\n";
    if not !no_approx then begin
      let cu = UI.unit ui in
      Persistent_env.Current_unit.set_unit cu;
      let root_symbols =
        List.map (fun comp_unit -> Symbol.for_module_block comp_unit)
          (UI.defines ui)
      in
      Format.printf "approximations@ %a@.@."
        Export_info.print_approx (export, root_symbols)
    end;
    if not !no_code then
      Format.printf "functions@ %a@.@."
        Export_info.print_functions export
  end;
  let pr_funs _ fns =
    List.iter (fun arity -> printf " %d" arity) fns in
  printf "Currying functions:%a\n" pr_funs
    (Int.Set.elements (UIL.curry_fun ui_link));
  printf "Apply functions:%a\n" pr_funs
    (Int.Set.elements (UIL.apply_fun ui_link));
  printf "Send functions:%a\n" pr_funs
    (Int.Set.elements (UIL.send_fun ui_link));
  printf "Force link: %s\n" (if (UIL.force_link ui_link) then "YES" else "no")

let print_cmxa_infos lib =
  printf "Extra C object files:";
  List.iter print_spaced_string (List.rev (LI.ccobjs lib));
  printf "\nExtra C options:";
  List.iter print_spaced_string (LI.ccopts lib);
  printf "\n";
  List.iter print_cmx_infos (LI.units lib)

let print_cmxs_infos header =
  List.iter
    (fun ui ->
       print_general_infos
         (DU.unit ui)
         (DU.crc ui)
         (DU.defines ui)
         (DU.imports_cmi ui)
         (DU.imports_cmx ui)
         ([]))
    (DH.units header)

let p_title title = printf "%s:\n" title

let p_section title = function
  | [] -> ()
  | l ->
      p_title title;
      List.iter print_name_crc l

let p_list title print = function
  | [] -> ()
  | l ->
      p_title title;
      List.iter print l

let dump_byte ic =
  Bytesections.read_toc ic;
  let toc = Bytesections.toc () in
  let toc = List.sort compare toc in
  List.iter
    (fun (section, _) ->
       try
         let len = Bytesections.seek_section ic section in
         if len > 0 then match section with
           | "CRCS" ->
               p_section
                 "Imported units"
                 (input_value ic : Compilation_unit.crcs)
           | "DLLS" ->
               p_list
                 "Used DLLs"
                 print_line
                 (input_stringlist ic len)
           | "DLPT" ->
               p_list
                 "Additional DLL paths"
                 print_line
                 (input_stringlist ic len)
           | "PRIM" ->
               p_list
                 "Primitives used"
                 print_line
                 (input_stringlist ic len)
           | "SYMB" ->
               print_global_table (input_value ic)
           | _ -> ()
       with _ -> ()
    )
    toc

let read_dyn_header filename ic =
  let helper = Filename.concat Config.standard_library "objinfo_helper" in
  let tempfile = Filename.temp_file "objinfo" ".out" in
  try
    try_finally
      ~always:(fun () -> remove_file tempfile)
      (fun () ->
         let rc = Sys.command (sprintf "%s %s > %s"
                                 (Filename.quote helper)
                                 (Filename.quote filename)
                                 tempfile) in
         if rc <> 0 then failwith "cannot read";
         let tc = Scanf.Scanning.from_file tempfile in
         try_finally
           ~always:(fun () -> Scanf.Scanning.close_in tc)
           (fun () ->
              let ofs = Scanf.bscanf tc "%Ld" (fun x -> x) in
              LargeFile.seek_in ic ofs;
              Some(input_value ic : DH.t)))
  with Failure _ | Sys_error _ -> None

let dump_obj filename =
  printf "File %s\n" filename;
  let ic = open_in_bin filename in
  let len_magic_number = String.length cmo_magic_number in
  let magic_number = really_input_string ic len_magic_number in
  if magic_number = cmo_magic_number then begin
    let cu_pos = input_binary_int ic in
    seek_in ic cu_pos;
    let cu = (input_value ic : compilation_unit) in
    close_in ic;
    print_cmo_infos cu
  end else if magic_number = cma_magic_number then begin
    let toc_pos = input_binary_int ic in
    seek_in ic toc_pos;
    let toc = (input_value ic : library) in
    close_in ic;
    print_cma_infos toc
  end else if magic_number = cmi_magic_number ||
              magic_number = cmt_magic_number then begin
    close_in ic;
    let cmi, cmt = Cmt_format.read filename in
    begin match cmi with
     | None -> ()
     | Some cmi ->
         print_cmi_infos cmi.Cmi_format.cmi_name cmi.Cmi_format.cmi_crcs
           cmi.Cmi_format.cmi_flags
    end;
    begin match cmt with
     | None -> ()
     | Some cmt -> print_cmt_infos cmt
    end
  end else if magic_number = cmx_magic_number then begin
    close_in ic;
    let ui, ui_link, crc = Cmx_format.load ~filename in
    print_cmx_infos (ui, ui_link, crc)
  end else if magic_number = cmxa_magic_number then begin
    close_in ic;
    let li = Cmxa_format.load ~filename in
    print_cmxa_infos li
  end else begin
    let pos_trailer = in_channel_length ic - len_magic_number in
    let _ = seek_in ic pos_trailer in
    let magic_number = really_input_string ic len_magic_number in
    if magic_number = Config.exec_magic_number then begin
      dump_byte ic;
      close_in ic
    end else if Filename.check_suffix filename ".cmxs" then begin
      flush stdout;
      match read_dyn_header filename ic with
      | None ->
          printf "Unable to read info on file %s\n" filename;
          exit 2
      | Some header ->
          if DH.magic header = Config.cmxs_magic_number then
            print_cmxs_infos header
          else begin
            printf "Wrong magic number\n"; exit 2
          end;
          close_in ic
    end else begin
      printf "Not an OCaml object file\n"; exit 2
    end
  end

let arg_list = [
  "-full-path", Arg.Set full_path, "";
  "-no-approx", Arg.Set no_approx,
    " Do not print module approximation information";
  "-no-code", Arg.Set no_code,
    " Do not print code from exported flambda functions";
  "-null-crc", Arg.Set no_crc, " Print a null CRC for imported interfaces";
  "-args", Arg.Expand Arg.read_arg,
     "<file> Read additional newline separated command line arguments \n\
     \      from <file>";
  "-args0", Arg.Expand Arg.read_arg0,
     "<file> Read additional NUL separated command line arguments from \n\
     \      <file>";
]
let arg_usage =
   Printf.sprintf "%s [OPTIONS] FILES : give information on files" Sys.argv.(0)

let main() =
  Arg.parse_expand arg_list dump_obj arg_usage;
  exit 0

let _ = main ()

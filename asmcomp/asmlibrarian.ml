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

(* Build libraries of .cmx files *)

open Misc
open Config

type error =
    File_not_found of string
  | Archiver_error of string

exception Error of error

let empty_export_info =
  if Config.flambda then
    Cmx_format.Unit_info.Flambda Export_info.empty
  else
    Cmx_format.Unit_info.Closure Clambda.Value_unknown

let read_info name =
  let module UI = Cmx_format.Unit_info in
  let module UIL = Cmx_format.Unit_info_link_time in
  let filename =
    try
      Load_path.find name
    with Not_found ->
      raise(Error(File_not_found name)) in
  let info, link_info, crc = Cmx_format.load ~filename in
  let link_info =
    UIL.with_force_link link_info
      ~force_link:(UIL.force_link link_info || !Clflags.link_everything)
  in
  (* There is no need to keep the approximation in the .cmxa file,
     since the compiler will go looking directly for .cmx files.
     The linker, which is the only one that reads .cmxa files, does not
     need the approximation. *)
  let info = UI.with_export_info info empty_export_info in
  (Filename.chop_suffix filename ".cmx" ^ ext_obj, (info, link_info, crc))

let create_archive file_list lib_name =
  let archive_name = Filename.remove_extension lib_name ^ ext_lib in
  let (objfile_list, descr_list) =
     List.split (List.map read_info file_list) in
  List.iter2
    (fun file_name (info, _link_info, crc) ->
      Asmlink.check_consistency file_name info crc)
    file_list descr_list;
  let infos =
    Cmxa_format.Library_info.create ~units:descr_list
      ~ccobjs:!Clflags.ccobjs
      ~ccopts:!Clflags.all_ccopts
  in
  Misc.try_finally
    ~exceptionally:(fun () -> remove_file archive_name)
    (fun () ->
      Cmxa_format.save infos ~filename:lib_name;
      if Ccomp.create_archive archive_name objfile_list <> 0 then begin
        raise (Error (Archiver_error archive_name))
      end)

open Format

let report_error ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %s" name
  | Archiver_error name ->
      fprintf ppf "Error while creating the library %s" name

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

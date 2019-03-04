(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module BCU = Backend_compilation_unit

type kind = Text | Data

type t = {
  kind : kind;
  object_file : Object_file.t;
  name : string;
  (* The [name] uniquely determines the symbol within a whole program. *)
}

type backend_sym = t

let to_string { name; _ } = name

include Identifiable.Make (struct
  type nonrec t = t

  (* CR mshinwell: check every field *)

  let compare t1 t2 = String.compare (to_string t1) (to_string t2)

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash t = Hashtbl.hash (to_string t)

  let print ppf { name; _ } = Format.pp_print_string ppf name

  let output chan t = print (Format.formatter_of_out_channel chan) t
end)

let compute_object_file (compilation_unit : BCU.t) =
  match compilation_unit with
  | Compilation_unit compilation_unit ->
    begin match Persistent_env.Current_unit.get () with
    | None -> Object_file.another_compilation_unit
    | Some current_unit ->
      if Compilation_unit.equal compilation_unit current_unit then
        Object_file.current_compilation_unit
      else
        Object_file.another_compilation_unit
    end
  | Startup -> Object_file.startup
  | Shared_startup -> Object_file.shared_startup
  | Runtime_and_external_libs -> Object_file.runtime_and_external_libs
  | Functions -> Object_file.functions

let first_part = "caml"
let separator = "__"

let create ?compilation_unit ~base_name kind =
  let compilation_unit =
    match compilation_unit with
    | None ->
      BCU.compilation_unit (
        Persistent_env.Current_unit.get_exn ())
    | Some compilation_unit -> compilation_unit
  in
  let object_file = compute_object_file compilation_unit in
  let name =
    Printf.sprintf "%s%s%s%s"
      first_part
      (BCU.name_for_backend_sym compilation_unit)
      separator
      base_name
  in
  { kind;
    object_file;
    name;
  }

let create_for_external_call (prim : Primitive.description) =
  { kind = Text;
    object_file = Object_file.runtime_and_external_libs;
    name = Primitive.native_name prim;
  }

let unsafe_create ?compilation_unit name kind =
  let compilation_unit =
    match compilation_unit with
    | None ->
      BCU.compilation_unit (
        Persistent_env.Current_unit.get_exn ())
    | Some compilation_unit -> compilation_unit
  in
  let object_file = compute_object_file compilation_unit in
  { kind;
    object_file;
    name;
  }

let compute_name_for_middle_end_symbol sym =
  let first_part, separator =
    match Symbol.compilation_unit sym with
    | Compilation_unit compilation_unit ->
      let comp_unit =
        BCU.name_for_backend_sym (
          BCU.compilation_unit compilation_unit)
      in
      first_part ^ comp_unit, separator
    | Predef -> first_part ^ "_exn_", ""
  in
  match Symbol.name_for_backend sym with
  | None -> first_part
  | Some name -> first_part ^ separator ^ name

let of_symbol sym =
  let object_file =
    match Symbol.compilation_unit sym with
    | Compilation_unit sym_unit ->
      compute_object_file (BCU.compilation_unit sym_unit)
    | Predef -> Object_file.startup
  in
  let kind : kind =
    match Symbol.kind sym with
    | Text -> Text
    | Data -> Data
  in
  { kind;
    object_file;
    name = compute_name_for_middle_end_symbol sym;
  }

let of_external_name object_file name kind =
  (* This is deliberately not exposed in the .mli.  (Add things to [Names],
     below, instead.) *)
  { kind;
    object_file;
    name;
  }

let kind t = t.kind

let object_file t = t.object_file

let next_lifted_anonymous_constant_stamp = ref 0

(* CR mshinwell: Add [reset] function? *)

let for_lifted_anonymous_constant ?compilation_unit () =
  let stamp = !next_lifted_anonymous_constant_stamp in
  incr next_lifted_anonymous_constant_stamp;
  let compilation_unit =
    match compilation_unit with
    | None ->
      BCU.compilation_unit (
        Persistent_env.Current_unit.get_exn ())
    | Some compilation_unit -> compilation_unit
  in
  let name =
    (* Care: these must not clash with the names produced in
       [Symbol.name_for_backend]. *)
    Printf.sprintf "%s%s%sc%d"
      first_part
      (BCU.name_for_backend_sym compilation_unit)
      separator
      stamp
  in
  { kind = Data;
    object_file = compute_object_file compilation_unit;
    name;
  }

let add_suffix t new_suffix =
  { kind = t.kind;
    object_file = t.object_file;
    name = t.name ^ new_suffix;
  }

let add_suffixes t new_suffixes =
  List.fold_left (fun t new_suffix -> add_suffix t new_suffix) t new_suffixes

let add_int_suffix t new_suffix =
  add_suffix t (string_of_int new_suffix)

let name_for_asm_symbol t = t.name

module Names = struct
  let runtime = Object_file.runtime_and_external_libs
  let startup = Object_file.startup

  let atan =
    of_external_name runtime "atan" Text

  let atan2 =
    of_external_name runtime "atan2" Text

  let cos =
    of_external_name runtime "cos" Text

  let log =
    of_external_name runtime "log" Text

  let log10 =
    of_external_name runtime "log10" Text

  let sin =
    of_external_name runtime "sin" Text

  let sqrt =
    of_external_name runtime "sqrt" Text

  let tan =
    of_external_name runtime "tan" Text

  let __aeabi_idivmod =
    of_external_name runtime "__aeabi_idivmod" Text

  let __aeabi_idiv =
    of_external_name runtime "__aeabi_idiv" Text

  let __aeabi_dadd =
    of_external_name runtime "__aeabi_dadd" Text

  let __aeabi_dsub =
    of_external_name runtime "__aeabi_dsub" Text

  let __aeabi_dmul =
    of_external_name runtime "__aeabi_dmul" Text

  let __aeabi_ddiv =
    of_external_name runtime "__aeabi_ddiv" Text

  let __aeabi_i2d =
    of_external_name runtime "__aeabi_i2d" Text

  let __aeabi_d2iz =
    of_external_name runtime "__aeabi_d2iz" Text

  let __aeabi_dcmpeq =
    of_external_name runtime "__aeabi_dcmpeq" Text

  let __aeabi_dcmplt =
    of_external_name runtime "__aeabi_dcmplt" Text

  let __aeabi_dcmple =
    of_external_name runtime "__aeabi_dcmple" Text

  let __aeabi_dcmpgt =
    of_external_name runtime "__aeabi_dcmpgt" Text

  let __aeabi_dcmpge =
    of_external_name runtime "__aeabi_dcmpge" Text

  let __aeabi_f2d =
    of_external_name runtime "__aeabi_f2d" Text

  let __aeabi_d2f =
    of_external_name runtime "__aeabi_d2f" Text

  let caml_exception_pointer =
    of_external_name runtime "caml_exception_pointer" Data

  let caml_backtrace_pos =
    of_external_name runtime "caml_backtrace_pos" Data

  let caml_exn_Division_by_zero =
    of_external_name startup "caml_exn_Division_by_zero" Data

  let caml_nativeint_ops =
    of_external_name runtime "caml_nativeint_ops" Data

  let caml_int32_ops =
    of_external_name runtime "caml_int32_ops" Data

  let caml_int64_ops =
    of_external_name runtime "caml_int64_ops" Data

  let caml_send n =
    add_int_suffix (of_external_name startup "caml_send" Text) n

  let caml_curry_n n =
    add_int_suffix (of_external_name startup "caml_curry" Text) n

  let caml_curry_m_to_n m n =
    add_suffixes (of_external_name startup "caml_curry" Text)
      [string_of_int m; "_"; string_of_int n]

  let caml_curry_m_to_n_app m n =
    add_suffixes (of_external_name startup "caml_curry" Text)
      [string_of_int m; "_"; string_of_int n; "_app"]

  let caml_tuplify n =
    add_int_suffix (of_external_name startup "caml_tuplify" Text) n

  let caml_apply n =
    add_int_suffix (of_external_name startup "caml_apply" Text) n

  let caml_ba_get n =
    add_int_suffix (of_external_name runtime "caml_ba_get_" Text) n

  let caml_ba_set n =
    add_int_suffix (of_external_name runtime "caml_ba_set_" Text) n

  let caml_call_gc =
    of_external_name runtime "caml_call_gc" Text

  let caml_modify =
    of_external_name runtime "caml_modify" Text

  let caml_initialize =
    of_external_name runtime "caml_initialize" Text

  let caml_get_public_method =
    of_external_name runtime "caml_get_public_method" Text

  let caml_alloc =
    of_external_name runtime "caml_alloc" Text

  let caml_ml_array_bound_error =
    of_external_name runtime "caml_ml_array_bound_error" Text

  let caml_raise_exn =
    of_external_name runtime "caml_raise_exn" Text

  let caml_make_array =
    of_external_name runtime "caml_make_array" Text

  let caml_bswap16_direct =
    of_external_name runtime "caml_bswap16_direct" Text

  type bswap_arg = Int32 | Int64 | Nativeint

  let caml_direct_bswap ty =
    let ty =
      match ty with
      | Int32 -> "int32"
      | Int64 -> "int64"
      | Nativeint -> "nativeint"
    in
    of_external_name runtime (Printf.sprintf "caml_%s_direct_bswap" ty) Text

  let caml_alloc_dummy =
    of_external_name runtime "caml_alloc_dummy" Text

  let caml_alloc_dummy_float =
    of_external_name runtime "caml_alloc_dummy_float" Text

  let caml_alloc_dummy_infix =
    of_external_name runtime "caml_alloc_dummy_infix" Text

  let caml_update_dummy =
    of_external_name runtime "caml_update_dummy" Text

  let caml_program =
    of_external_name startup "caml_program" Text

  let caml_startup =
    of_external_name runtime "caml_startup" Text

  let caml_globals_inited =
    of_external_name runtime "caml_globals_inited" Data

  let caml_globals =
    of_external_name startup "caml_globals" Data

  let caml_plugin_header =
    of_external_name startup "caml_plugin_header" Data

  let caml_globals_map =
    of_external_name startup "caml_globals_map" Data

  let caml_code_segments =
    of_external_name startup "caml_code_segments" Data

  let caml_data_segments =
    of_external_name startup "caml_data_segments" Data

  let caml_frametable =
    of_external_name startup "caml_frametable" Data

  let caml_spacetime_shapes =
    of_external_name startup "caml_spacetime_shapes" Data

  let caml_afl_area_ptr =
    of_external_name runtime "caml_afl_area_ptr" Data

  let caml_afl_prev_loc =
    of_external_name runtime "caml_afl_prev_loc" Data

  let caml_setup_afl =
    of_external_name runtime "caml_setup_afl" Text

  let caml_spacetime_allocate_node =
    of_external_name runtime "caml_spacetime_allocate_node" Text

  let caml_spacetime_indirect_node_hole_ptr =
    of_external_name runtime "caml_spacetime_indirect_node_hole_ptr" Text

  let caml_spacetime_generate_profinfo =
    of_external_name runtime "caml_spacetime_generate_profinfo" Text
end

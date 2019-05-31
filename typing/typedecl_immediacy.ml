(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Gabriel Scherer, projet Parsifal, INRIA Saclay                       *)
(*   Rodolphe Lepigre, projet Deducteam, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Types

type error = Bad_immediate_attribute
exception Error of Location.t * error

let marked_as_immediate decl =
  match
    Builtin_attributes.immediate decl.type_attributes,
    Builtin_attributes.immediate64 decl.type_attributes
  with
  | true, _ -> Always
  | _, true -> Always_on_64bits
  | false, false -> Unknown

let compute_decl env tdecl =
  match (tdecl.type_kind, tdecl.type_manifest) with
  | (Type_variant [{cd_args = Cstr_tuple [arg]; _}], _)
    | (Type_variant [{cd_args = Cstr_record [{ld_type = arg; _}]; _}], _)
    | (Type_record ([{ld_type = arg; _}], _), _)
  when tdecl.type_unboxed.unboxed ->
    begin match Typedecl_unboxed.get_unboxed_type_representation env arg with
    | Unavailable -> Unknown
    | This argrepr -> Ctype.immediacy env argrepr
    | Only_on_64_bits argrepr ->
      match Ctype.immediacy env argrepr with
      | Always -> Always_on_64bits
      | Always_on_64bits | Unknown as x -> x
    end
  | (Type_variant (_ :: _ as cstrs), _) ->
    if not (List.exists (fun c -> c.Types.cd_args <> Types.Cstr_tuple []) cstrs) then
      Always
    else
      Unknown
  | (Type_abstract, Some(typ)) -> Ctype.immediacy env typ
  | (Type_abstract, None) -> marked_as_immediate tdecl
  | _ -> Unknown

let property : (Types.immediacy, unit) Typedecl_properties.property =
  let open Typedecl_properties in
  let eq = (=) in
  let merge ~prop:_ ~new_prop = new_prop in
  let default _decl = Unknown in
  let compute env decl () = compute_decl env decl in
  let update_decl decl immediacy = { decl with type_immediate = immediacy } in
  let check _env _id decl () =
    if more_often_immediate (marked_as_immediate decl) decl.type_immediate then
      raise (Error (decl.type_loc, Bad_immediate_attribute)) in
  {
    eq;
    merge;
    default;
    compute;
    update_decl;
    check;
  }

let update_decls env decls =
  Typedecl_properties.compute_property_noreq property env decls

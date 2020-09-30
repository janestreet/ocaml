(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t = {
  code_id : Code_id.t;
  params_and_body : Function_params_and_body.t Or_deleted.t;
  newer_version_of : Code_id.t option;
  params_arity : Flambda_arity.With_subkinds.t;
  result_arity : Flambda_arity.With_subkinds.t;
  stub : bool;
  inline : Inline_attribute.t;
  is_a_functor : bool;
  recursive : Recursive.t;
}

let code_id { code_id; _ } = code_id

let params_and_body { params_and_body; _ } = params_and_body

let params_and_body_opt { params_and_body; _ } =
  match params_and_body with
  | Deleted -> None
  | Present params_and_body -> Some params_and_body

let params_and_body_must_be_present ~error_context { params_and_body; _ } =
  match params_and_body with
  | Deleted -> Misc.fatal_errorf "%s: params and body are deleted" error_context
  | Present params_and_body -> params_and_body

let newer_version_of { newer_version_of; _ } = newer_version_of

let params_arity { params_arity; _ } = params_arity

let result_arity { result_arity; _ } = result_arity

let stub { stub; _ } = stub

let inline { inline; _ } = inline

let is_a_functor { is_a_functor; _ } = is_a_functor

let recursive { recursive; _ } = recursive

let create
      code_id
      ~params_and_body
      ~newer_version_of
      ~params_arity
      ~result_arity
      ~stub
      ~(inline:Inline_attribute.t)
      ~is_a_functor
      ~recursive =
  begin match stub, inline with
  | true, (Never_inline | Default_inline)
  | false, (Never_inline | Default_inline | Always_inline | Hint_inline
      | Unroll _) -> ()
  | true, (Always_inline | Hint_inline | Unroll _) ->
    Misc.fatal_error "Stubs may not be annotated as [Always_inline], \
      [Hint_inline] or [Unroll]"
  end;
  { code_id;
    params_and_body;
    newer_version_of;
    params_arity;
    result_arity;
    stub;
    inline;
    is_a_functor;
    recursive;
  }

let with_code_id code_id t = { t with code_id }

let with_params_and_body params_and_body t = { t with params_and_body }

let with_newer_version_of newer_version_of t = { t with newer_version_of }

let print_params_and_body_with_cache ~cache ppf params_and_body =
  match params_and_body with
  | Or_deleted.Deleted -> Format.fprintf ppf "Deleted"
  | Or_deleted.Present params_and_body ->
    Function_params_and_body.print_with_cache ~cache ppf
      params_and_body

let print_with_cache ~cache ppf
      { code_id = _; params_and_body; newer_version_of; stub; inline;
        is_a_functor; params_arity; result_arity; recursive; } =
  let module C = Flambda_colours in
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>@<0>%s(newer_version_of@ %a)@<0>%s@]@ \
      @[<hov 1>@<0>%s(stub@ %b)@<0>%s@]@ \
      @[<hov 1>@<0>%s(inline@ %a)@<0>%s@]@ \
      @[<hov 1>@<0>%s(is_a_functor@ %b)@<0>%s@]@ \
      @[<hov 1>@<0>%s(params_arity@ @<0>%s%a@<0>%s)@<0>%s@]@ \
      @[<hov 1>@<0>%s(result_arity@ @<0>%s%a@<0>%s)@<0>%s@]@ \
      @[<hov 1>@<0>%s(recursive@ %a)@<0>%s@]@ \
      %a\
      )@]"
    (if Option.is_none newer_version_of then Flambda_colours.elide ()
     else Flambda_colours.normal ())
    (Misc.Stdlib.Option.print_compact Code_id.print) newer_version_of
    (Flambda_colours.normal ())
    (if not stub then Flambda_colours.elide () else C.normal ())
    stub
    (Flambda_colours.normal ())
    (if Inline_attribute.is_default inline
     then Flambda_colours.elide ()
     else C.normal ())
    Inline_attribute.print inline
    (Flambda_colours.normal ())
    (if not is_a_functor then Flambda_colours.elide () else C.normal ())
    is_a_functor
    (Flambda_colours.normal ())
    (if Flambda_arity.With_subkinds.is_singleton_value params_arity
     then Flambda_colours.elide ()
     else Flambda_colours.normal ())
    (Flambda_colours.normal ())
    Flambda_arity.With_subkinds.print params_arity
    (if Flambda_arity.With_subkinds.is_singleton_value params_arity
     then Flambda_colours.elide ()
     else Flambda_colours.normal ())
    (Flambda_colours.normal ())
    (if Flambda_arity.With_subkinds.is_singleton_value result_arity
     then Flambda_colours.elide ()
     else Flambda_colours.normal ())
    (Flambda_colours.normal ())
    Flambda_arity.With_subkinds.print result_arity
    (if Flambda_arity.With_subkinds.is_singleton_value result_arity
     then Flambda_colours.elide ()
     else Flambda_colours.normal ())
    (Flambda_colours.normal ())
    (match recursive with
     | Non_recursive -> Flambda_colours.elide ()
     | Recursive -> Flambda_colours.normal ())
    Recursive.print recursive
    (Flambda_colours.normal ())
    (print_params_and_body_with_cache ~cache) params_and_body

let print ppf code =
  print_with_cache ~cache:(Printing_cache.create ()) ppf code

let compare { code_id = code_id1; _ } { code_id = code_id2; _ } =
  Code_id.compare code_id1 code_id2

let free_names { code_id = _; params_and_body; newer_version_of;
                 params_arity = _; result_arity = _; stub = _; inline = _;
                 is_a_functor = _; recursive = _; } =
  (* [code_id] is only in [t] for the use of [compare]; it doesn't
     count as a free name. *)
  let from_newer_version_of =
    match newer_version_of with
    | None -> Name_occurrences.empty
    | Some older ->
      Name_occurrences.add_newer_version_of_code_id
        Name_occurrences.empty older Name_mode.normal
  in
  let from_params_and_body =
    match params_and_body with
    | Deleted -> Name_occurrences.empty
    | Present params_and_body ->
      Function_params_and_body.free_names params_and_body
  in
  Name_occurrences.union from_newer_version_of from_params_and_body

let apply_name_permutation
      ({ code_id = _; params_and_body; newer_version_of = _; params_arity = _;
         result_arity = _; stub = _; inline = _; is_a_functor = _;
         recursive = _; } as t)
      perm =
  let params_and_body' : Function_params_and_body.t Or_deleted.t =
    match params_and_body with
    | Deleted -> Deleted
    | Present params_and_body_inner ->
      let params_and_body_inner' =
        Function_params_and_body.apply_name_permutation
          params_and_body_inner perm
      in
      if params_and_body_inner == params_and_body_inner' then
        params_and_body
      else
        Present params_and_body_inner'
  in
  if params_and_body == params_and_body' then t
  else
    { t with params_and_body = params_and_body'; }

let all_ids_for_export { code_id; params_and_body; newer_version_of;
                         params_arity = _; result_arity = _; stub = _;
                         inline = _; is_a_functor = _; recursive = _;} =
  let newer_version_of_ids =
    match newer_version_of with
    | None -> Ids_for_export.empty
    | Some older ->
      Ids_for_export.add_code_id Ids_for_export.empty older
  in
  let params_and_body_ids =
    match params_and_body with
    | Deleted -> Ids_for_export.empty
    | Present params_and_body ->
      Function_params_and_body.all_ids_for_export params_and_body
  in
  Ids_for_export.add_code_id
    (Ids_for_export.union newer_version_of_ids params_and_body_ids)
    code_id

let import import_map
      ({ code_id; params_and_body; newer_version_of; params_arity = _;
         result_arity = _; stub = _; inline = _; is_a_functor = _;
         recursive = _; } as t) =
  let code_id = Ids_for_export.Import_map.code_id import_map code_id in
  let params_and_body : Function_params_and_body.t Or_deleted.t =
    match params_and_body with
    | Deleted -> Deleted
    | Present params_and_body ->
      let params_and_body =
        Function_params_and_body.import import_map params_and_body
      in
      Present params_and_body
  in
  let newer_version_of =
    match newer_version_of with
    | None -> None
    | Some older ->
      let older = Ids_for_export.Import_map.code_id import_map older in
      Some older
  in
  { t with code_id; params_and_body; newer_version_of; }

let make_deleted t =
  { t with params_and_body = Deleted; }

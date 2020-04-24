(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

type simplify_named_result = {
  bindings_outermost_first : (Bindable_let_bound.t * Reachable.t) list;
  dacc : Downwards_acc.t;
}

let bindings_result bindings_outermost_first dacc =
  { bindings_outermost_first; dacc; }

let record_any_symbol_projection dacc (defining_expr : Reachable.t)
      (prim : P.t) args bindable_let_bound ~bound_var named =
  (* Projections from symbols bound to variables are important to remember,
     since if such a variable occurs in a set of closures environment or
     other value that can potentially be lifted, the knowledge that the
     variable is equal to a symbol projection can make the difference between
     being able to lift and not being able to lift.  We try to avoid
     recording symbol projections whose answer is known (in particular the
     answer is a symbol or a constant), since such symbol projection
     knowledge doesn't affect lifting decisions. *)
  let can_record_proj =
    (* We only need to record a projection if the defining expression remains
       as a [Prim].  In particular if the defining expression simplified to
       a variable (via the [Simple] constructor), then in the event that the
       variable is itself a symbol projection, the environment will already
       know this fact.
       We don't need to record a projection if we are currently at toplevel,
       since any variable involved in a constant to be lifted from that
       position will also be at toplevel. *)
    (not (DE.at_unit_toplevel (DA.denv dacc)))
      && match defining_expr with
         | Reachable (Prim _) -> true
         | Reachable (Simple _ | Set_of_closures _ | Static_consts _)
         | Invalid _ -> false
  in
  let proj =
    let module SP = Symbol_projection in
    (* The [args] being queried here are the post-simplification arguments
       of the primitive, so we can directly read off whether they are
       symbols or constants, as needed. *)
    match prim with
    | Unary (Project_var { project_from; var; }, _)
        when can_record_proj ->
      begin match args with
      | [closure] ->
        Simple.pattern_match' closure
          ~const:(fun _ -> None)
          ~symbol:(fun symbol_projected_from ->
            Some (SP.create symbol_projected_from
              (SP.Projection.project_var project_from var)))
          ~var:(fun _ -> None)
      | [] | _::_ ->
        Misc.fatal_errorf "Expected one argument:@ %a@ =@ %a"
          Bindable_let_bound.print bindable_let_bound
          Named.print named
      end
    | Binary (Block_load _, _, _) when can_record_proj ->
      begin match args with
      | [block; index] ->
        Simple.pattern_match index
          ~const:(fun const ->
            match Reg_width_const.descr const with
            | Tagged_immediate imm ->
              Simple.pattern_match' block
                ~const:(fun _ -> None)
                ~symbol:(fun symbol_projected_from ->
                  let index = Target_imm.to_targetint imm in
                  Some (SP.create symbol_projected_from
                    (SP.Projection.block_load ~index)))
                ~var:(fun _ -> None)
            | Naked_immediate _ | Naked_float _
            | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _ ->
              Misc.fatal_errorf "Kind error for [Block_load] index:@ \
                  %a@ =@ %a"
                Bindable_let_bound.print bindable_let_bound
                Named.print named)
          ~name:(fun _ -> None)
      | [] | _::_ ->
        Misc.fatal_errorf "Expected two arguments:@ %a@ =@ %a"
          Bindable_let_bound.print bindable_let_bound
          Named.print named
      end
    | Nullary (Probe_is_enabled _)
    | Unary (
      ( Duplicate_block _
      | Duplicate_array _
      | Is_int
      | Get_tag
      | Array_length _
      | Bigarray_length _
      | String_length _
      | Int_as_pointer
      | Opaque_identity
      | Int_arith _
      | Float_arith _
      | Num_conv _
      | Boolean_not
      | Unbox_number _
      | Box_number _
      | Select_closure _
      | Project_var _ ), _)
    | Binary (
      ( Block_load _
      | Array_load _
      | String_or_bigstring_load _
      | Bigarray_load _
      | Phys_equal _
      | Int_arith _
      | Int_shift _
      | Int_comp _
      | Float_arith _
      | Float_comp _ ), _, _)
    | Ternary (
      ( Block_set _
      | Array_set _
      | Bytes_or_bigstring_set _
      | Bigarray_set _ ), _, _, _)
    | Variadic (
      ( Make_block _
      | Make_array _ ), _) -> None
  in
  match proj with
  | None -> dacc
  | Some proj ->
    let var = Var_in_binding_pos.var bound_var in
    DA.map_denv dacc ~f:(fun denv -> DE.add_symbol_projection denv var proj)

let simplify_named0 dacc (bindable_let_bound : Bindable_let_bound.t)
      (named : Named.t) =
  match named with
  | Simple simple ->
    let bound_var = Bindable_let_bound.must_be_singleton bindable_let_bound in
    let min_name_mode = Var_in_binding_pos.name_mode bound_var in
    begin match S.simplify_simple dacc simple ~min_name_mode with
    | Bottom, ty ->
      let dacc = DA.add_variable dacc bound_var (T.bottom (T.kind ty)) in
      let defining_expr = Reachable.invalid () in
      bindings_result [bindable_let_bound, defining_expr] dacc
    | Ok new_simple, ty ->
      let dacc = DA.add_variable dacc bound_var ty in
      let defining_expr =
        if simple == new_simple then Reachable.reachable named
        else Reachable.reachable (Named.create_simple simple)
      in
      bindings_result [bindable_let_bound, defining_expr] dacc
    end
  | Prim (prim, dbg) ->
    let bound_var = Bindable_let_bound.must_be_singleton bindable_let_bound in
    let term, env_extension, simplified_args, dacc =
      (* [simplified_args] has to be returned from [simplify_primitive] because
         in at least one case (for [Project_var]), the simplifier may return
         something other than a [Prim] as the [term].  However we need the
         simplified arguments of the actual primitive for the symbol
         projection check below. *)
      Simplify_primitive.simplify_primitive dacc ~original_named:named
        prim dbg ~result_var:bound_var
    in
    let kind = P.result_kind' prim in
    let dacc =
      let dacc = DA.add_variable dacc bound_var (T.unknown kind) in
      DA.extend_typing_environment dacc env_extension
    in
    (* CR mshinwell: Add check along the lines of: types are unknown
       whenever [not (P.With_fixed_value.eligible prim)] holds. *)
    (* Primitives with generative effects correspond to allocations.
       Without this check, we could end up lifting definitions that have
       a type that looks like an allocation but that are instead a projection
       from a bigger structure. *)
    let allow_lifting =
      (* CR mshinwell: We probably shouldn't lift if the let binding is going
         to be deleted, as lifting may cause [Dominator]-scoped bindings to
         be inserted, that cannot be deleted.  However this situation probably
         doesn't arise that much, and won't be an issue once we can lift
         [Dominator]-scoped bindings. *)
      P.only_generative_effects prim
        && Name_mode.is_normal (Var_in_binding_pos.name_mode bound_var)
    in
    let defining_expr, dacc, ty =
      Reification.try_to_reify dacc term ~bound_to:bound_var ~allow_lifting
    in
    let defining_expr =
      if T.is_bottom (DA.typing_env dacc) ty then Reachable.invalid ()
      else defining_expr
    in
    let dacc =
      record_any_symbol_projection dacc defining_expr prim simplified_args
        bindable_let_bound ~bound_var named
    in
    bindings_result [bindable_let_bound, defining_expr] dacc
  | Set_of_closures set_of_closures ->
    let bindings, dacc =
      Simplify_set_of_closures.simplify_non_lifted_set_of_closures dacc
        bindable_let_bound set_of_closures
    in
    bindings_result bindings dacc
  | Static_consts static_consts ->
    let { Bindable_let_bound. bound_symbols; scoping_rule = _; } =
      Bindable_let_bound.must_be_symbols bindable_let_bound
    in
    if not (DE.at_unit_toplevel (DA.denv dacc)) then begin
      Misc.fatal_errorf "[Let] binding symbols is only allowed at the toplevel \
          of compilation units (not even at the toplevel of function \
          bodies):@ %a@ =@ %a"
        Bindable_let_bound.print bindable_let_bound
        Named.print named
    end;
    let non_closure_symbols_being_defined =
      Bound_symbols.non_closure_symbols_being_defined bound_symbols
    in
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
        Symbol.Set.fold (fun symbol denv ->
            DE.now_defining_symbol denv symbol)
          non_closure_symbols_being_defined
          denv)
    in
    let bound_symbols, static_consts, dacc =
      try
        Simplify_static_const.simplify_static_consts dacc bound_symbols
          static_consts
      with Misc.Fatal_error -> begin
        if !Clflags.flambda_context_on_error then begin
          Format.eprintf "\n%sContext is:%s simplifying [Let_symbol] binding \
                            of@ %a@ with downwards accumulator:@ %a\n"
            (Flambda_colours.error ())
            (Flambda_colours.normal ())
            Bound_symbols.print bound_symbols
            DA.print dacc
          end;
          raise Misc.Fatal_error
      end
    in
    (* CR mshinwell: Change to be run only when invariants are on, and use
      [Name_occurrences.iter] (to be written).
    Symbol.Set.iter (fun sym ->
        DE.check_symbol_is_bound (DA.denv dacc) sym)
      (Name_occurrences.symbols bound_symbols_free_names);
    Code_id.Set.iter (fun code_id ->
        DE.check_code_id_is_bound (DA.denv dacc) code_id)
      (Name_occurrences.code_ids bound_symbols_free_names);
    *)
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
        Symbol.Set.fold (fun symbol denv ->
            DE.no_longer_defining_symbol denv symbol)
          non_closure_symbols_being_defined
          denv)
    in
    let dacc =
      Static_const.Group.match_against_bound_symbols static_consts bound_symbols
        ~init:dacc
        ~code:(fun dacc _ _ -> dacc)
        ~set_of_closures:(fun dacc ~closure_symbols:_ _ -> dacc)
        ~block_like:(fun dacc symbol static_const ->
          DA.consider_constant_for_sharing dacc symbol static_const)
    in
    let lifted_constants =
      ListLabels.map2
        (Bound_symbols.to_list bound_symbols)
        (Static_const.Group.to_list static_consts)
        ~f:(fun (pat : Bound_symbols.Pattern.t) static_const ->
          match pat with
          | Block_like symbol ->
            let typ =
              TE.find (DA.typing_env dacc) (Name.symbol symbol) (Some K.value)
            in
            (* The [symbol_projections] map is empty because we only introduce
               symbol projections when lifting -- and [static_const] has
               already been lifted. *)
            LC.create_block_like symbol static_const (DA.denv dacc)
              ~symbol_projections:Variable.Map.empty typ
          | Code code_id -> LC.create_code code_id static_const
          | Set_of_closures closure_symbols ->
            let closure_symbols_with_types =
              Closure_id.Lmap.map (fun symbol ->
                  let typ =
                    TE.find (DA.typing_env dacc) (Name.symbol symbol) (Some K.value)
                  in
                  symbol, typ)
                closure_symbols
            in
            LC.create_set_of_closures (DA.denv dacc)
              ~closure_symbols_with_types
              (* Same comment as above re. [symbol_projections]. *)
              ~symbol_projections:Variable.Map.empty
              static_const)
    in
    let dacc = DA.add_lifted_constant dacc (LC.concat lifted_constants) in
    (* We don't need to return any bindings; [Simplify_expr.simplify_let]
       will create the "let symbol" binding when it sees the lifted
       constant. *)
    { bindings_outermost_first = []; dacc; }

let simplify_named dacc bindable_let_bound named =
  try
    simplify_named0 dacc bindable_let_bound named
  with Misc.Fatal_error -> begin
    if !Clflags.flambda_context_on_error then begin
      Format.eprintf "\n%sContext is:%s simplifying [Let] binding@ %a =@ %a@ \
          with downwards accumulator:@ %a\n"
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        Bindable_let_bound.print bindable_let_bound
        Named.print named
        DA.print dacc
    end;
    raise Misc.Fatal_error
  end

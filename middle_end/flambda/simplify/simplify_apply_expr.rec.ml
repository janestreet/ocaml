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

open! Simplify_import

let simplify_direct_tuple_application dacc apply code_id ~down_to_up =
  let dbg = Apply.dbg apply in
  let callee's_code = DE.find_code (DA.denv dacc) code_id in
  let param_arity =
    Code.params_arity callee's_code
  in
  let n = List.length param_arity in
  (* Split the tuple argument from other potential
     over application arguments *)
  let tuple, over_application_args =
    match Apply.args apply with
    | tuple :: others -> tuple, others
    | _ ->
      Misc.fatal_errorf "Empty argument list for direct application"
  in
  (* create the list of variables and projections *)
  let vars_and_fields = List.init n (fun i ->
    let var = Variable.create "tuple_field" in
    let e = Simplify_common.project_tuple ~dbg ~size:n ~field:i tuple in
    var, e)
  in
  (* Change the application to operate on the fields of the tuple *)
  let apply = Apply.with_args apply @@ (
    List.map (fun (v, _) -> Simple.var v) vars_and_fields
    @ over_application_args)
  in
  (* Immediately simplify over_applications to avoid having direct
     applications with the wrong arity. *)
  let apply_expr =
    match over_application_args with
    | [] -> Expr.create_apply apply
    | _ -> Simplify_common.split_direct_over_application apply ~param_arity
  in
  (* Insert the projections and simplify the new expression,
     to allow field projections to be simplified, and
     over-application/full_application optimizations *)
  let expr =
    List.fold_right (fun (v, defining_expr) body ->
        let var_bind = Var_in_binding_pos.create v Name_mode.normal in
        Expr.create_let var_bind defining_expr body)
      vars_and_fields apply_expr
  in
  Simplify_expr.simplify_expr dacc expr ~down_to_up

let rebuild_non_inlined_direct_full_application apply ~use_id ~exn_cont_use_id
      ~result_arity uacc ~after_rebuild =
  let apply =
    Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
      apply
  in
  let expr =
    match use_id with
    | None -> Expr.create_apply apply
    | Some use_id ->
      Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
        result_arity apply
  in
  after_rebuild expr uacc

let simplify_direct_full_application dacc apply function_decl_opt
      ~result_arity ~down_to_up =
  let callee = Apply.callee apply in
  let args = Apply.args apply in
  let inlined =
    match function_decl_opt with
    | None -> None
    | Some (function_decl, function_decl_rec_info) ->
      let apply_inlining_depth = Apply.inlining_depth apply in
      let decision =
        Inlining_decision.make_decision_for_call_site (DA.denv dacc)
          ~function_decl_rec_info
          ~apply_inlining_depth
          (Apply.inline apply)
      in
      match Inlining_decision.Call_site_decision.can_inline decision with
      | Do_not_inline ->
        None
      | Inline { unroll_to; } ->
        let dacc, inlined =
          Inlining_transforms.inline dacc ~callee
            ~args function_decl
            ~apply_return_continuation:(Apply.continuation apply)
            ~apply_exn_continuation:(Apply.exn_continuation apply)
            ~apply_inlining_depth ~unroll_to
            (Apply.dbg apply)
        in
        Some (dacc, inlined)
  in
  match inlined with
  | Some (dacc, inlined) ->
    Simplify_expr.simplify_expr dacc inlined ~down_to_up
  | None ->
    let dacc, use_id =
      match Apply.continuation apply with
      | Never_returns -> dacc, None
      | Return apply_return_continuation ->
        let dacc, use_id =
          DA.record_continuation_use dacc apply_return_continuation Non_inlinable
            ~env_at_use:(DA.denv dacc)
            ~arg_types:(T.unknown_types_from_arity_with_subkinds result_arity)
        in
        dacc, Some use_id
    in
    let dacc, exn_cont_use_id =
      DA.record_continuation_use dacc
        (Exn_continuation.exn_handler (Apply.exn_continuation apply))
        Non_inlinable
        ~env_at_use:(DA.denv dacc)
        ~arg_types:(T.unknown_types_from_arity_with_subkinds (
          Exn_continuation.arity (Apply.exn_continuation apply)))
    in
    down_to_up dacc
      ~rebuild:(rebuild_non_inlined_direct_full_application apply ~use_id
        ~exn_cont_use_id ~result_arity)

let simplify_direct_partial_application dacc apply ~callee's_code_id
      ~callee's_closure_id ~param_arity ~result_arity ~recursive
      ~down_to_up =
  (* For simplicity, we disallow [@inline] attributes on partial
     applications.  The user may always write an explicit wrapper instead
     with such an attribute. *)
  (* CR-someday mshinwell: Pierre noted that we might like a function to be
     inlined when applied to its first set of arguments, e.g. for some kind
     of type class like thing. *)
  let args = Apply.args apply in
  let dbg = Apply.dbg apply in
  let apply_continuation =
    match Apply.continuation apply with
    | Never_returns ->
      Misc.fatal_error "cannot simplify a partial application that never returns"
    | Return continuation -> continuation
  in
  begin match Apply.inline apply with
  | Always_inline | Never_inline ->
    Location.prerr_warning (Debuginfo.to_location dbg)
      (Warnings.Inlining_impossible "[@inlined] attributes may not be used \
        on partial applications")
  | Unroll _ ->
    Location.prerr_warning (Debuginfo.to_location dbg)
      (Warnings.Inlining_impossible "[@unroll] attributes may not be used \
        on partial applications")
  | Default_inline -> ()
  end;
  let arity = List.length param_arity in
  assert (arity > List.length args);
  let applied_args, remaining_param_arity =
    Misc.Stdlib.List.map2_prefix (fun arg kind ->
        if not (K.equal (K.With_subkind.kind kind) K.value) then begin
          Misc.fatal_errorf "Non-[value] kind in partial application: %a"
            Apply.print apply
        end;
        arg)
      args param_arity
  in
  if not (Flambda_arity.With_subkinds.is_singleton_value result_arity)
  then begin
    Misc.fatal_errorf "Partially-applied function with non-[value] \
        return kind: %a"
      Apply.print apply
  end;
  let wrapper_var = Variable.create "partial_app" in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let wrapper_closure_id =
    Closure_id.wrap compilation_unit (Variable.create "partial_app_closure")
  in
  let wrapper_taking_remaining_args, dacc, dummy_code =
    let return_continuation = Continuation.create () in
    let remaining_params =
      List.map (fun kind ->
          let param = Variable.create "param" in
          Kinded_parameter.create param kind)
        remaining_param_arity
    in
    let args = applied_args @ (List.map KP.simple remaining_params) in
    let call_kind =
      Call_kind.direct_function_call callee's_code_id callee's_closure_id
        ~return_arity:result_arity
    in
    let applied_args_with_closure_vars = (* CR mshinwell: rename *)
      List.map (fun applied_arg ->
          Var_within_closure.wrap compilation_unit (Variable.create "arg"),
            applied_arg)
        ((Apply.callee apply) :: applied_args)
    in
    let my_closure = Variable.create "my_closure" in
    let exn_continuation =
      Apply.exn_continuation apply
      |> Exn_continuation.without_extra_args
    in
    let body =
      let full_application =
        Apply.create ~callee:(Apply.callee apply)
          ~continuation:(Return return_continuation)
          exn_continuation
          ~args
          ~call_kind
          dbg
          ~inline:Default_inline
          ~inlining_depth:(Apply.inlining_depth apply)
          ~probe_name:(Apply.probe_name apply)
      in
      List.fold_left (fun expr (closure_var, arg) ->
          match Simple.must_be_var arg with
          | None -> expr
          | Some arg ->
            let arg = VB.create arg Name_mode.normal in
            Expr.create_let arg
              (Named.create_prim
                (Unary (Project_var {
                  project_from = wrapper_closure_id;
                  var = closure_var;
                }, Simple.var my_closure))
                dbg)
              expr)
        (Expr.create_apply full_application)
        (List.rev applied_args_with_closure_vars)
    in
    let params_and_body =
      Function_params_and_body.create ~return_continuation
        exn_continuation
        remaining_params
        ~body
        ~dbg
        ~my_closure
    in
    let code_id =
      Code_id.create
        ~name:(Closure_id.to_string callee's_closure_id ^ "_partial")
        (Compilation_unit.get_current_exn ())
    in
    let code =
      Code.create
        code_id
        ~params_and_body:(Present params_and_body)
        ~newer_version_of:None
        ~params_arity:(KP.List.arity_with_subkinds remaining_params)
        ~result_arity
        ~stub:true
        ~inline:Default_inline
        ~is_a_functor:false
        ~recursive
    in
    let function_decl =
      Function_declaration.create ~code_id ~is_tupled:false ~dbg
    in
    let function_decls =
      Function_declarations.create
        (Closure_id.Lmap.singleton wrapper_closure_id function_decl)
    in
    let closure_elements =
      Var_within_closure.Map.of_list applied_args_with_closure_vars
    in
    let defining_expr =
      Lifted_constant.create_code code_id (Code code)
    in
    let dummy_defining_expr =
      (* We should not add the real piece of code in the lifted constant.
         A new piece of code will always be generated when the [Let] we
         generate below is simplified.  As such we can simply add a lifted
         constant identifying deleted code.  This will ensure, if for some
         reason the constant makes it to Cmm stage, that code size is not
         increased unnecessarily. *)
      Lifted_constant.create_code code_id (Code (Code.make_deleted code))
    in
    let dacc =
      DA.add_lifted_constant dacc dummy_defining_expr
      |> DA.map_denv ~f:(fun denv -> DE.add_lifted_constant denv defining_expr)
    in
    Set_of_closures.create function_decls ~closure_elements,
    dacc,
    dummy_defining_expr
  in
  let apply_cont =
    Apply_cont.create apply_continuation
      ~args:[Simple.var wrapper_var] ~dbg
  in
  let expr =
    let wrapper_var = VB.create wrapper_var Name_mode.normal in
    let closure_vars = [wrapper_var] in
    let pattern = Bindable_let_bound.set_of_closures ~closure_vars in
    Expr.create_pattern_let pattern
      (Named.create_set_of_closures wrapper_taking_remaining_args)
      (Expr.create_apply_cont apply_cont)
  in
  Simplify_expr.simplify_expr dacc expr
    ~down_to_up:(fun dacc ~rebuild ->
      down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
        let uacc = UA.add_outermost_lifted_constant uacc dummy_code in
        rebuild uacc ~after_rebuild))

(* CR mshinwell: Should it be an error to encounter a non-direct application
   of a symbol after [Simplify]? This shouldn't usually happen, but I'm not 100%
   sure it cannot in every case. *)

let simplify_direct_over_application dacc apply ~param_arity ~result_arity:_
      ~down_to_up =
  let expr = Simplify_common.split_direct_over_application apply ~param_arity in
  Simplify_expr.simplify_expr dacc expr ~down_to_up

let simplify_direct_function_call dacc apply ~callee's_code_id_from_type
      ~callee's_code_id_from_call_kind ~callee's_closure_id ~result_arity
      ~recursive ~arg_types:_ ~must_be_detupled function_decl_opt
      ~down_to_up =
  let result_arity_of_application =
    Call_kind.return_arity (Apply.call_kind apply)
  in
  if not (Flambda_arity.With_subkinds.compatible result_arity
    ~when_used_at:result_arity_of_application)
  then begin
    Misc.fatal_errorf "Wrong return arity for direct OCaml function call \
        (expected %a, found %a):@ %a"
      Flambda_arity.With_subkinds.print result_arity
      Flambda_arity.With_subkinds.print result_arity_of_application
      Apply.print apply
  end;
  let callee's_code_id : _ Or_bottom.t =
    match callee's_code_id_from_call_kind with
    | None -> Ok callee's_code_id_from_type
    | Some callee's_code_id_from_call_kind ->
      let code_age_rel = TE.code_age_relation (DA.typing_env dacc) in
      let resolver = TE.code_age_relation_resolver (DA.typing_env dacc) in
      Code_age_relation.meet code_age_rel ~resolver
        callee's_code_id_from_call_kind callee's_code_id_from_type
  in
  match callee's_code_id with
  | Bottom ->
    down_to_up dacc ~rebuild:Simplify_common.rebuild_invalid
  | Ok callee's_code_id ->
    let call_kind =
      Call_kind.direct_function_call callee's_code_id callee's_closure_id
        ~return_arity:result_arity
    in
    let apply = Apply.with_call_kind apply call_kind in
    if must_be_detupled then
      simplify_direct_tuple_application dacc apply callee's_code_id
        ~down_to_up
    else begin
      let args = Apply.args apply in
      let provided_num_args = List.length args in
      let callee's_code = DE.find_code (DA.denv dacc) callee's_code_id in
      (* A function declaration with [is_tupled = true] may effectively have
         an arity that does not match that of the underlying code.
         Since direct calls adopt the calling convention of the code's body
         (whereas indirect_unknown_arity calls use the convention of the
         function_declaration), we here always use the arity from the callee's
         code. *)
      let param_arity = Code.params_arity callee's_code in
      let num_params = List.length param_arity in
      if provided_num_args = num_params then
        simplify_direct_full_application dacc apply function_decl_opt
          ~result_arity ~down_to_up
      else if provided_num_args > num_params then
        simplify_direct_over_application dacc apply ~param_arity ~result_arity
          ~down_to_up
      else if provided_num_args > 0 && provided_num_args < num_params then
        simplify_direct_partial_application dacc apply ~callee's_code_id
          ~callee's_closure_id ~param_arity ~result_arity ~recursive
          ~down_to_up
      else
        Misc.fatal_errorf "Function with %d params when simplifying \
                           direct OCaml function call with %d arguments: %a"
          num_params
          provided_num_args
          Apply.print apply
    end

let rebuild_function_call_where_callee's_type_unavailable apply call_kind
      ~use_id ~exn_cont_use_id uacc ~after_rebuild =
  let apply =
    Apply.with_call_kind apply call_kind
    |> Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
  in
  let expr =
    Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
      (Call_kind.return_arity call_kind) apply
  in
  after_rebuild expr uacc

let simplify_function_call_where_callee's_type_unavailable dacc apply
      (call : Call_kind.Function_call.t) ~args:_ ~arg_types ~down_to_up =
  let cont =
    match Apply.continuation apply with
    | Never_returns ->
      Misc.fatal_error "cannot simplify an application that never returns"
    | Return continuation -> continuation
  in
  let denv = DA.denv dacc in
  let env_at_use = denv in
  let dacc, exn_cont_use_id =
    DA.record_continuation_use dacc
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
      Non_inlinable
      ~env_at_use:(DA.denv dacc)
      ~arg_types:(T.unknown_types_from_arity_with_subkinds (
        Exn_continuation.arity (Apply.exn_continuation apply)))
  in
  let check_return_arity_and_record_return_cont_use ~return_arity =
(*
    let cont_arity = DA.continuation_arity dacc cont in
    if not (Flambda_arity.equal return_arity cont_arity) then begin
      Misc.fatal_errorf "Return arity (%a) on application's continuation@ \
          doesn't match return arity (%a) specified in [Call_kind]:@ %a"
        Flambda_arity.print cont_arity
        Flambda_arity.print return_arity
        Apply.print apply
    end;
*)
    DA.record_continuation_use dacc cont Non_inlinable ~env_at_use
      ~arg_types:(T.unknown_types_from_arity_with_subkinds return_arity)
  in
  let call_kind, use_id, dacc =
    match call with
    | Indirect_unknown_arity ->
      let dacc, use_id =
        DA.record_continuation_use dacc cont Non_inlinable
          ~env_at_use ~arg_types:[T.any_value ()]
      in
      Call_kind.indirect_function_call_unknown_arity (), use_id, dacc
    | Indirect_known_arity { param_arity; return_arity; } ->
      let args_arity =
        T.arity_of_list arg_types
        |> List.map (fun kind -> K.With_subkind.create kind Anything)
      in
      if not (Flambda_arity.With_subkinds.compatible args_arity
        ~when_used_at:param_arity)
      then begin
        Misc.fatal_errorf "Argument arity on indirect-known-arity \
            application doesn't match [Call_kind] (expected %a, \
            found %a):@ %a"
          Flambda_arity.With_subkinds.print param_arity
          Flambda_arity.With_subkinds.print args_arity
          Apply.print apply
      end;
      let dacc, use_id =
        check_return_arity_and_record_return_cont_use ~return_arity
      in
      let call_kind =
        Call_kind.indirect_function_call_known_arity ~param_arity
          ~return_arity
      in
      call_kind, use_id, dacc
    | Direct { return_arity; _ } ->
      let param_arity =
        T.arity_of_list arg_types
        |> List.map (fun kind -> K.With_subkind.create kind Anything)
      in
      (* Some types have regressed in precision.  Since this used to be a
         direct call, however, we know the function's arity even though we
         don't know which function it is. *)
      let dacc, use_id =
        check_return_arity_and_record_return_cont_use ~return_arity
      in
      let call_kind =
        Call_kind.indirect_function_call_known_arity ~param_arity
          ~return_arity
      in
      call_kind, use_id, dacc
  in
  down_to_up dacc
    ~rebuild:(rebuild_function_call_where_callee's_type_unavailable apply
      call_kind ~use_id ~exn_cont_use_id)

(* CR mshinwell: I've seen at least one case where a call of kind
   [Indirect_unknown_arity] has been generated with no warning, despite having
   [@inlined always]. *)

let simplify_function_call dacc apply ~callee_ty
      (call : Call_kind.Function_call.t) ~arg_types ~down_to_up =
  let args = Apply.args apply in
  (* Function declarations and params and body might not have the same
     calling convention. Currently the only case when it happens is for tupled
     functions. For such functions, the function_declaration declares a
     param_arity with a single argument (which is the tuple), whereas the code
     body takes an argument for each field of the tuple (the body is currified).

     When simplifying a function call, it can happen that we need to change
     the calling convention. Currently this only happens when we have a
     generic call (indirect_unknown_arity), which uses the
     generic/function_declaration calling convention, but se simplify it into
     a direct call, which uses the callee's code calling convention. In this
     case, we need to "detuple" the call in order to correctly adopt to the
     change in calling convention.
  *)
  let call_must_be_detupled is_function_decl_tupled =
    match call with
    | Direct _
    | Indirect_known_arity _ ->
      (* In these cases, the calling convention already used in the application
         being simplified is that of the code actually called. Thus we must not
         detuple the function. *)
      false
      (* In the indirect case, the calling convention used currently is the
         generic one. Thus we need to detuple the call iff the function
         declaration is tupled. *)
    | Indirect_unknown_arity -> is_function_decl_tupled
  in
  let type_unavailable () =
    simplify_function_call_where_callee's_type_unavailable dacc apply call
      ~args ~arg_types ~down_to_up
  in
  (* CR mshinwell: Should this be using [meet_shape], like for primitives? *)
  let denv = DA.denv dacc in
  match T.prove_single_closures_entry (DE.typing_env denv) callee_ty with
  | Proved (callee's_closure_id, _closures_entry, func_decl_type) ->
    (* CR mshinwell: We should check that the [set_of_closures] in the
       [closures_entry] structure in the type does indeed contain the
       closure in question. *)
    begin match func_decl_type with
    | Ok (Inlinable inlinable) ->
      let module I = T.Function_declaration_type.Inlinable in
      let callee's_code_id_from_call_kind =
        match call with
        | Direct { code_id; closure_id; _ } ->
          if not (Closure_id.equal closure_id callee's_closure_id) then begin
            Misc.fatal_errorf "Closure ID %a in application doesn't match \
                closure ID %a discovered via typing.@ Application:@ %a"
              Closure_id.print closure_id
              Closure_id.print callee's_closure_id
              Apply.print apply
          end;
          Some code_id
        | Indirect_unknown_arity
        | Indirect_known_arity _ -> None
      in
      (* CR mshinwell: This should go in Typing_env (ditto logic for Rec_info
         in Simplify_simple *)
      let function_decl_rec_info =
        let rec_info = I.rec_info inlinable in
        match Simple.rec_info (Apply.callee apply) with
        | None -> rec_info
        | Some newer -> Rec_info.merge rec_info ~newer
      in
      let callee's_code_id_from_type = I.code_id inlinable in
      let callee's_code = DE.find_code denv callee's_code_id_from_type in
      let must_be_detupled = call_must_be_detupled (I.is_tupled inlinable) in
      simplify_direct_function_call dacc apply ~callee's_code_id_from_type
        ~callee's_code_id_from_call_kind ~callee's_closure_id ~arg_types
        ~result_arity:(Code.result_arity callee's_code)
        ~recursive:(Code.recursive callee's_code)
        ~must_be_detupled
        (Some (inlinable, function_decl_rec_info))
        ~down_to_up
    | Ok (Non_inlinable non_inlinable) ->
      let module N = T.Function_declaration_type.Non_inlinable in
      let callee's_code_id_from_type = N.code_id non_inlinable in
      let callee's_code_id_from_call_kind =
        match call with
        | Direct { code_id; _ } -> Some code_id
        | Indirect_unknown_arity
        | Indirect_known_arity _ -> None
      in
      let must_be_detupled = call_must_be_detupled (N.is_tupled non_inlinable) in
      let callee's_code_from_type =
        DE.find_code denv callee's_code_id_from_type
      in
      simplify_direct_function_call dacc apply ~callee's_code_id_from_type
        ~callee's_code_id_from_call_kind
        ~callee's_closure_id ~arg_types
        ~result_arity:(Code.result_arity callee's_code_from_type)
        ~recursive:(Code.recursive callee's_code_from_type)
        ~must_be_detupled
        None
        ~down_to_up
    | Bottom ->
      down_to_up dacc ~rebuild:Simplify_common.rebuild_invalid
    | Unknown -> type_unavailable ()
    end
  | Unknown -> type_unavailable ()
  | Invalid ->
    down_to_up dacc ~rebuild:Simplify_common.rebuild_invalid

let simplify_apply_shared dacc apply : _ Or_bottom.t =
  let min_name_mode = Name_mode.normal in
  match S.simplify_simple dacc (Apply.callee apply) ~min_name_mode with
  | Bottom, _ty -> Bottom
  | Ok callee, callee_ty ->
    match S.simplify_simples dacc (Apply.args apply) ~min_name_mode with
    | _, Bottom -> Bottom
    | _changed, Ok args_with_types ->
      let args, arg_types = List.split args_with_types in
      let inlining_depth =
        DE.get_inlining_depth_increment (DA.denv dacc)
          + Apply.inlining_depth apply
      in
      let apply =
        Apply.create ~callee
          ~continuation:(Apply.continuation apply)
          (Apply.exn_continuation apply)
          ~args
          ~call_kind:(Apply.call_kind apply)
          (DE.add_inlined_debuginfo' (DA.denv dacc) (Apply.dbg apply))
          ~inline:(Apply.inline apply)
          ~inlining_depth
          ~probe_name:(Apply.probe_name apply)
      in
      Ok (callee_ty, apply, arg_types)

let rebuild_method_call apply ~use_id ~exn_cont_use_id uacc ~after_rebuild =
  let apply =
    Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
      apply
  in
  let expr =
    Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
      (Flambda_arity.With_subkinds.create [K.With_subkind.any_value]) apply
  in
  after_rebuild expr uacc

let simplify_method_call dacc apply ~callee_ty ~kind:_ ~obj ~arg_types
      ~down_to_up =
  let callee_kind = T.kind callee_ty in
  if not (K.is_value callee_kind) then begin
    Misc.fatal_errorf "Method call with callee of wrong kind %a: %a"
      K.print callee_kind
      T.print callee_ty
  end;
  let apply_cont =
    match Apply.continuation apply with
    | Never_returns ->
      Misc.fatal_error "cannot simplify a method call that never returns"
    | Return continuation -> continuation
  in
  let denv = DA.denv dacc in
  DE.check_simple_is_bound denv obj;
  let expected_arity = List.map (fun _ -> K.value) arg_types in
  let args_arity = T.arity_of_list arg_types in
  if not (Flambda_arity.equal expected_arity args_arity) then begin
    Misc.fatal_errorf "All arguments to a method call must be of kind \
        [value]:@ %a"
      Apply.print apply
  end;
  let dacc, use_id =
    DA.record_continuation_use dacc apply_cont Non_inlinable
      ~env_at_use:denv
      ~arg_types:[T.any_value ()]
  in
  let dacc, exn_cont_use_id =
    DA.record_continuation_use dacc
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
      Non_inlinable
      ~env_at_use:(DA.denv dacc)
      ~arg_types:(T.unknown_types_from_arity_with_subkinds (
        Exn_continuation.arity (Apply.exn_continuation apply)))
  in
  (* CR mshinwell: Need to record exception continuation use (check all other
     cases like this too) *)
  down_to_up dacc
    ~rebuild:(rebuild_method_call apply ~use_id ~exn_cont_use_id)

let rebuild_c_call apply ~use_id ~exn_cont_use_id ~return_arity uacc
      ~after_rebuild =
  (* CR mshinwell: Make sure that [resolve_continuation_aliases] has been
     called before building of any term that contains a continuation *)
  let apply =
    Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
      apply
  in
  let expr =
    match use_id with
    | Some use_id ->
      Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
        (Flambda_arity.With_subkinds.of_arity return_arity) apply
    | None ->
      Expr.create_apply apply
  in
  after_rebuild expr uacc

let simplify_c_call dacc apply ~callee_ty ~param_arity ~return_arity
      ~arg_types ~down_to_up =
  let callee_kind = T.kind callee_ty in
  if not (K.is_value callee_kind) then begin
    Misc.fatal_errorf "C callees must be of kind [value], not %a: %a"
      K.print callee_kind
      T.print callee_ty
  end;
  let args_arity = T.arity_of_list arg_types in
  if not (Flambda_arity.equal args_arity param_arity) then begin
    Misc.fatal_errorf "Arity %a of [Apply] arguments doesn't match \
        parameter arity %a of C callee:@ %a"
      Flambda_arity.print args_arity
      Flambda_arity.print param_arity
      Apply.print apply
  end;
(* CR mshinwell: We can't do these checks (here and elsewhere) on [DA]
   any more.  Maybe we can check on [UA] after calling [k] instead.
  let cont = Apply.continuation apply in
  let cont_arity = DA.continuation_arity dacc cont in
  if not (Flambda_arity.equal cont_arity return_arity) then begin
    Misc.fatal_errorf "Arity %a of [Apply] continuation doesn't match \
        return arity %a of C callee:@ %a"
      Flambda_arity.print cont_arity
      Flambda_arity.print return_arity
      Apply.print apply
  end;
*)
  let dacc, use_id =
    match Apply.continuation apply with
    | Return apply_continuation ->
      let dacc, use_id =
        DA.record_continuation_use dacc apply_continuation Non_inlinable
          ~env_at_use:(DA.denv dacc)
          ~arg_types:(T.unknown_types_from_arity return_arity)
      in
      dacc, Some use_id
    | Never_returns ->
      dacc, None
  in
  let dacc, exn_cont_use_id =
    (* CR mshinwell: Try to factor out these stanzas, here and above. *)
    DA.record_continuation_use dacc
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
      Non_inlinable
      ~env_at_use:(DA.denv dacc)
      ~arg_types:(T.unknown_types_from_arity_with_subkinds (
        Exn_continuation.arity (Apply.exn_continuation apply)))
  in
  down_to_up dacc
    ~rebuild:(rebuild_c_call apply ~use_id ~exn_cont_use_id ~return_arity)

let simplify_apply dacc apply ~down_to_up =
  match simplify_apply_shared dacc apply with
  | Bottom -> down_to_up dacc ~rebuild:Simplify_common.rebuild_invalid
  | Ok (callee_ty, apply, arg_types) ->
    match Apply.call_kind apply with
    | Function call ->
      simplify_function_call dacc apply ~callee_ty call ~arg_types ~down_to_up
    | Method { kind; obj; } ->
      simplify_method_call dacc apply ~callee_ty ~kind ~obj ~arg_types
        ~down_to_up
    | C_call { alloc = _; param_arity; return_arity; } ->
      simplify_c_call dacc apply ~callee_ty ~param_arity ~return_arity
        ~arg_types ~down_to_up

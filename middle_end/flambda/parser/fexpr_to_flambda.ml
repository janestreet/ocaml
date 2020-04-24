[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Continuation variables *)
module C = struct
  type t = string
  let compare = String.compare
end
module CM = Map.Make(C)

(* Variables *)
module V = struct
  type t = string
  let compare = String.compare
end
module VM = Map.Make(V)

(* Symbols *)
module S = struct
  type t = string
  let compare = String.compare
end
module SM = Map.Make(S)

(* Code ids *)
module D = struct
  type t = string
  let compare = String.compare
end
module DM = Map.Make(D)

(* Closure ids (globally scoped, so updates are in-place) *)
module U = struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end
module UT = Hashtbl.Make(U)

(* Variables within closures (globally scoped, so updates are in-place) *)
module W = struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end
module WT = Hashtbl.Make(W)

type env = {
  done_continuation : Continuation.t;
  error_continuation : Exn_continuation.t;
  continuations : (Continuation.t * int) CM.t;
  exn_continuations : Exn_continuation.t CM.t;
  variables : Variable.t VM.t;
  symbols : Symbol.t SM.t;
  code_ids : Code_id.t DM.t;
  closure_ids : Closure_id.t UT.t;
  vars_within_closures : Var_within_closure.t WT.t;
}

let init_env done_continuation error_continuation = {
  done_continuation;
  error_continuation;
  continuations = CM.empty;
  exn_continuations = CM.empty;
  variables = VM.empty;
  symbols = SM.empty;
  code_ids = DM.empty;
  closure_ids = UT.create 10;
  vars_within_closures = WT.create 10;
}

let enter_code env = {
  continuations = CM.empty;
  exn_continuations = CM.empty;
  variables = env.variables;
  done_continuation = env.done_continuation;
  error_continuation = env.error_continuation;
  symbols = env.symbols;
  code_ids = env.code_ids;
  closure_ids = env.closure_ids;
  vars_within_closures = env.vars_within_closures;
}

let fresh_cont env ?sort { Fexpr.txt = name; loc = _ } arity =
  let c = Continuation.create ?sort ~name () in
  c,
  { env with
    continuations = CM.add name (c, arity) env.continuations }

let fresh_exn_cont env { Fexpr.txt = name; loc = _ } =
  let c = Continuation.create ~sort:Exn ~name () in
  let e = Exn_continuation.create ~exn_handler:c ~extra_args:[] in
  e,
  { env with
    exn_continuations = CM.add name e env.exn_continuations }

let fresh_var env { Fexpr.txt = name; loc = _ } =
  let v = Variable.create name ~user_visible:() in
  v,
  { env with
    variables = VM.add name v env.variables }

let fresh_code_id env { Fexpr.txt = name; loc = _ } =
  let c = Code_id.create ~name (Compilation_unit.get_current_exn ()) in
  c,
  { env with
    code_ids = DM.add name c env.code_ids }

let fresh_closure_id env { Fexpr.txt = name; loc = _ } =
  let v = Variable.create name in
  let c = Closure_id.wrap (Compilation_unit.get_current_exn ()) v in
  UT.add env.closure_ids name c;
  c

let fresh_or_existing_closure_id env ({ Fexpr.txt = name; loc = _ } as id) =
  match UT.find_opt env.closure_ids name with
  | None -> fresh_closure_id env id
  | Some closure_id -> closure_id

let fresh_var_within_closure env { Fexpr.txt = name; loc = _ } =
  let v = Variable.create name in
  let c = Var_within_closure.wrap (Compilation_unit.get_current_exn ()) v in
  WT.add env.vars_within_closures name c;
  c

let fresh_or_existing_var_within_closure env ({ Fexpr.txt = name; _ } as id) =
  match WT.find_opt env.vars_within_closures name with
  | None -> fresh_var_within_closure env id
  | Some var_within_closure -> var_within_closure

let declare_symbol (env:env) { Fexpr.txt = name; loc } =
  if SM.mem name env.symbols then
    Misc.fatal_errorf "Redefinition of symbol %s: %a"
      name Lambda.print_scoped_location loc
  else
    let symbol =
      Symbol.unsafe_create
        (Compilation_unit.get_current_exn ())
        (Linkage_name.create name)
    in
    symbol,
    { env with
      symbols = SM.add name symbol env.symbols }

let find_with ~descr ~find map { Fexpr.txt = name; loc } =
  match find name map with
  | None ->
    Misc.fatal_errorf "Unbound %s %s: %a"
      descr name Lambda.print_scoped_location loc
  | Some a ->
    a

let get_symbol (env:env) sym =
  find_with ~descr:"symbol" ~find:SM.find_opt env.symbols sym

let find_cont_id env c =
  find_with ~descr:"continuation id" ~find:CM.find_opt env.continuations c

let find_cont env (c : Fexpr.continuation) =
  match c with
  | Special Done -> env.done_continuation, 1
  | Special Error -> Exn_continuation.exn_handler env.error_continuation, 1
  | Named cont_id -> find_cont_id env cont_id

let find_exn_cont_id env c =
  find_with ~descr:"exn_continuation" ~find:CM.find_opt env.exn_continuations c


let find_exn_cont env (c : Fexpr.continuation) =
  match c with
  | Special Done -> Misc.fatal_error "done is not an exception continuation"
  | Special Error -> env.error_continuation
  | Named cont_id -> find_exn_cont_id env cont_id

let find_var env v =
  find_with ~descr:"variable" ~find:VM.find_opt env.variables v

let find_code_id env code_id =
  find_with ~descr:"code id" ~find:DM.find_opt env.code_ids code_id

let targetint (i:Fexpr.targetint) : Targetint.t = Targetint.of_int64 i

let value_kind : Fexpr.kind -> Flambda_kind.With_subkind.t = function
  | Value -> Flambda_kind.With_subkind.any_value
  | Naked_number naked_number_kind ->
    begin
      match naked_number_kind with
      | Naked_immediate -> Flambda_kind.With_subkind.naked_immediate
      | Naked_float -> Flambda_kind.With_subkind.naked_float
      | Naked_int32 -> Flambda_kind.With_subkind.naked_int32
      | Naked_int64 -> Flambda_kind.With_subkind.naked_int64
      | Naked_nativeint -> Flambda_kind.With_subkind.naked_nativeint
    end
  | Fabricated -> Misc.fatal_error "Fabricated should not be used"

let value_kind_without_subkind kind =
  Flambda_kind.With_subkind.kind (value_kind kind)

let value_kind_opt : Fexpr.kind option -> Flambda_kind.With_subkind.t = function
  | Some kind -> value_kind kind
  | None -> Flambda_kind.With_subkind.any_value

let arity a = Flambda_arity.With_subkinds.create (List.map value_kind a)

let arity_without_subkinds a =
  List.map Flambda_kind.With_subkind.kind (arity a)

let const (c:Fexpr.const) : Reg_width_const.t =
  match c with
  | Tagged_immediate i ->
    let i = Targetint.of_string i in
    Reg_width_const.tagged_immediate (Target_imm.int (Targetint.OCaml.of_targetint i))
  | Naked_immediate i ->
    let i = Targetint.of_string i in
    Reg_width_const.naked_immediate (Target_imm.int (Targetint.OCaml.of_targetint i))
  | Naked_float f ->
    Reg_width_const.naked_float (f |> Numbers.Float_by_bit_pattern.create)
  | Naked_int32 i ->
    Reg_width_const.naked_int32 i
  | Naked_int64 i ->
    Reg_width_const.naked_int64 i
  | Naked_nativeint i ->
    Reg_width_const.naked_nativeint (i |> targetint)

let simple env (s:Fexpr.simple) : Simple.t =
  match s with
  | Var { txt = v; loc } -> begin
      match VM.find_opt v env.variables with
      | None ->
        Misc.fatal_errorf "Unbound variable %s : %a" v
          Lambda.print_scoped_location loc
      | Some var ->
        Simple.var var
    end
  | Const c ->
    Simple.const (const c)
  | Symbol sym -> begin
      Simple.symbol (get_symbol env sym)
    end

let name env (s:Fexpr.name) : Simple.t =
  match s with
  | Var { txt = v; loc } -> begin
      match VM.find_opt v env.variables with
      | None ->
        Misc.fatal_errorf "Unbound variable %s : %a" v
          Lambda.print_scoped_location loc
      | Some var ->
        Simple.var var
    end
  | Symbol sym -> begin
      Simple.symbol (get_symbol env sym)
    end

let of_kind_value env (v:Fexpr.of_kind_value)
      : Flambda.Static_const.Field_of_block.t =
  match v with
  | Symbol s ->
    Symbol (get_symbol env s)
  | Tagged_immediate i ->
    let i = Targetint.of_string i in
    Tagged_immediate
      (Target_imm.int (Targetint.OCaml.of_targetint i))
  | Dynamically_computed var ->
    let var = find_var env var in
    Dynamically_computed var

let unop env (unop:Fexpr.unop) : Flambda_primitive.unary_primitive =
  match unop with
  | Get_tag -> Get_tag
  | Is_int -> Is_int
  | Opaque_identity -> Opaque_identity
  | Tag_imm -> Box_number Untagged_immediate
  | Untag_imm -> Unbox_number Untagged_immediate
  | Project_var { project_from; var } ->
    let var = fresh_or_existing_var_within_closure env var in
    let project_from = fresh_or_existing_closure_id env project_from in
    Project_var { project_from; var }
  | Select_closure { move_from; move_to } ->
    let move_from = fresh_or_existing_closure_id env move_from in
    let move_to = fresh_or_existing_closure_id env move_to in
    Select_closure { move_from; move_to }

let infix_binop (binop:Fexpr.infix_binop) : Flambda_primitive.binary_primitive =
  let int_comp c = Flambda_primitive.Int_comp (Tagged_immediate, Signed, Yielding_bool c) in
  match binop with
  | Plus -> Int_arith (Tagged_immediate, Add)
  | Minus -> Int_arith (Tagged_immediate, Sub)
  | Lt -> int_comp Lt
  | Gt -> int_comp Gt
  | Le -> int_comp Le
  | Ge -> int_comp Ge
  | Plusdot
  | Minusdot -> failwith "TODO binop"
  | Eqdot -> Float_comp (Yielding_bool Eq)
  | Neqdot -> Float_comp (Yielding_bool Neq)
  | Ltdot -> Float_comp (Yielding_bool Lt)
  | Gtdot -> Float_comp (Yielding_bool Gt)
  | Ledot -> Float_comp (Yielding_bool Le)
  | Gedot -> Float_comp (Yielding_bool Ge)

let binop (binop:Fexpr.binop) : Flambda_primitive.binary_primitive =
  match binop with
  | Block_load (Values { tag; size; field_kind }, mutability) ->
    let size : Targetint.OCaml.t Or_unknown.t =
      match size with
      | None -> Unknown
      | Some size -> Known (size |> Targetint.OCaml.of_int64)
    in
    Block_load (Values { field_kind;
                         tag = tag |> Tag.Scannable.create_exn;
                         size; }, mutability)
  | Phys_equal (kind, op) ->
    let kind =
      value_kind_without_subkind
        (kind |> Option.value ~default:(Value : Fexpr.kind))
    in
    Phys_equal (kind, op)
  | Infix op ->
    infix_binop op
  | Int_comp (i, s, c) ->
    Int_comp (i, s, Yielding_bool c)

let convert_block_shape ~num_fields =
  List.init num_fields (
    fun _field : Flambda_primitive.Block_of_values_field.t -> Any_value
  )

let varop (varop:Fexpr.varop) n : Flambda_primitive.variadic_primitive =
  match varop with
  | Make_block (tag, mutability) ->
    let shape = convert_block_shape ~num_fields:n in
    let kind : Flambda_primitive.Block_kind.t =
      Values (Tag.Scannable.create_exn tag, shape)
    in
    Make_block (kind, mutability)

let prim env (p:Fexpr.prim) : Flambda_primitive.t =
  match p with
  | Unary (op, arg) ->
    Unary (unop env op, simple env arg)
  | Binary (op, a1, a2) ->
    Binary (binop op, simple env a1, simple env a2)
  | Variadic (op, args) ->
    Variadic (varop op (List.length args), List.map (simple env) args)

let convert_recursive_flag (flag : Fexpr.is_recursive) : Recursive.t =
  match flag with
  | Recursive -> Recursive
  | Nonrecursive -> Non_recursive

let defining_expr env (named:Fexpr.named) : Flambda.Named.t =
  match named with
  | Simple s ->
    Flambda.Named.create_simple (simple env s)
  | Prim p ->
    let p = prim env p in
    Flambda.Named.create_prim p Debuginfo.none
  | _ -> assert false

let set_of_closures env fun_decls closure_elements =
  let fun_decls : Function_declarations.t =
    let translate_fun_decl (fun_decl : Fexpr.fun_decl)
          : (Closure_id.t * Function_declaration.t) =
      let code_id = find_code_id env fun_decl.code_id in
      let closure_id =
        (* By default, pun the code id as the closure id *)
        fun_decl.closure_id |> Option.value ~default:fun_decl.code_id
      in
      let closure_id = fresh_or_existing_closure_id env closure_id in
      let decl =
        Function_declaration.create
          ~code_id
          ~dbg:Debuginfo.none
          ~is_tupled:fun_decl.is_tupled
      in
      closure_id, decl
    in
    List.map translate_fun_decl fun_decls
    |> Closure_id.Lmap.of_list
    |> Function_declarations.create
  in
  let closure_elements = Option.value closure_elements ~default:[] in
  let closure_elements : Simple.t Var_within_closure.Map.t =
    let convert ({ var; value } : Fexpr.closure_element) =
      (fresh_or_existing_var_within_closure env var, simple env value)
    in
    List.map convert closure_elements
    |> Var_within_closure.Map.of_list
  in
  Set_of_closures.create fun_decls ~closure_elements

let apply_cont env ({ cont; args; trap_action } : Fexpr.apply_cont) =
  if Option.is_some trap_action then failwith "TODO trap actions";
  let c, arity = find_cont env cont in
  if List.length args <> arity then
    begin
      let cont_str = match cont with
      | Special Done -> "done"
      | Special Error -> "error"
      | Named { txt = cont_id; _ } -> cont_id
      in
      Misc.fatal_errorf "wrong continuation arity %s" cont_str
    end;
  let args = List.map (simple env) args in
  Flambda.Apply_cont.create c ~args ~dbg:Debuginfo.none

let rec expr env (e : Fexpr.expr) : Flambda.Expr.t =
  match e with
  | Let { bindings = []; _ } ->
    assert false (* should not be possible *)
  | Let { bindings = ({ defining_expr = Closure _; _ } :: _) as bindings;
          closure_elements; body } ->
      let binding_to_var_and_closure_binding : Fexpr.let_binding -> _ = function
        | { var; defining_expr = Closure binding; _ } ->
          (var, binding)
        | { var = { txt = _; loc }; _ } ->
          Misc.fatal_errorf "Cannot use 'and' with non-closure: %a"
            Lambda.print_scoped_location loc
      in
      let vars_and_closure_bindings =
        List.map binding_to_var_and_closure_binding bindings
      in
      let closure_vars, env =
        let convert_binding env (var, _) : Var_in_binding_pos.t * env =
          let var, env = fresh_var env var in
          let var = Var_in_binding_pos.create var Name_mode.normal in
          var, env
        in
        Misc.Stdlib.List.map_accum_left convert_binding env
          vars_and_closure_bindings
      in
      let bound = Bindable_let_bound.set_of_closures ~closure_vars in
      let named =
        let closure_bindings = List.map snd vars_and_closure_bindings in
        set_of_closures env closure_bindings closure_elements
        |> Flambda.Named.create_set_of_closures
      in
      let body = expr env body in
      Flambda.Expr.create_pattern_let bound named body
  | Let { bindings = _ :: _ :: _; _ } ->
    Misc.fatal_errorf
      "Multiple let bindings only allowed when defining closures"
  | Let { closure_elements = Some _; _ } ->
    Misc.fatal_errorf
      "'with' clause only allowed when defining closures"
  | Let { bindings = [{ var; kind = _; defining_expr = d }];
          body;
          closure_elements = None } ->
    let named = defining_expr env d in
    let id, env = fresh_var env var in
    let body = expr env body in
    let var =
      Var_in_binding_pos.create id Name_mode.normal
    in
    Flambda.Expr.create_let var named body
  | Let_cont
      { recursive; body;
        handlers = [handler] } -> begin
      let is_exn_handler = false in
      let name, body_env =
        fresh_cont env handler.name (List.length handler.params)
      in
      let body = expr body_env body in
      let env =
        match recursive with
        | Nonrecursive -> env
        | Recursive -> body_env
      in
      let handler_env, params =
        List.fold_right
          (fun ({ param; kind }:Fexpr.kinded_parameter)
            (env, args) ->
            let var, env = fresh_var env param in
            let param =
              (* CR mshinwell for lmaurer: Allow the subkinds to be specified
                 in the syntax on continuation parameters. *)
              Kinded_parameter.create var (value_kind_opt kind)
            in
            env, param :: args)
          handler.params (env, [])
      in
      let handler =
        expr handler_env handler.handler
      in
      let params_and_handler =
        Flambda.Continuation_params_and_handler.create params ~handler
      in
      let handler =
        Flambda.Continuation_handler.create ~params_and_handler
          ~stub:false
          ~is_exn_handler:is_exn_handler
      in
      match recursive with
      | Nonrecursive ->
        Flambda.Let_cont.create_non_recursive name handler ~body
      | Recursive ->
        let handlers = Continuation.Map.singleton name handler in
        Flambda.Let_cont.create_recursive handlers ~body
    end

  | Let_cont _ ->
      failwith "TODO andwhere"

  | Apply_cont ac ->
    Flambda.Expr.create_apply_cont (apply_cont env ac)

  | Switch { scrutinee; cases } ->
    let arms =
      List.map (fun (case, apply) ->
        Target_imm.int (Targetint.OCaml.of_int case),
        apply_cont env apply
      ) cases
      |> Target_imm.Map.of_list
    in
    Flambda.Expr.create_switch
      ~scrutinee:(simple env scrutinee)
      ~arms

  | Let_symbol { bindings; closure_elements; body } ->
    (* Desugar the abbreviated form for a single set of closures *)
    let found_explicit_set = ref false in
    let closures_in_implicit_set =
      List.filter_map (fun (binding : Fexpr.symbol_binding) ->
        match binding with
        | Closure clo -> Some clo
        | Set_of_closures _ -> found_explicit_set := true; None
        | _ -> None
      ) bindings
    in
    let bindings =
      match closures_in_implicit_set, closure_elements with
      | _ :: _, _ when !found_explicit_set ->
        Misc.fatal_error "Cannot mix implicit and explicit sets of closures"
      | [], Some _ ->
        Misc.fatal_error "Found closure elements but no closures"
      | [], None ->
        bindings
      | _, _ ->
        let not_a_closure (b : Fexpr.symbol_binding) =
          match b with
          | Closure _ -> false
          | _ -> true
        in
        let extra_bindings : Fexpr.symbol_binding list =
          [ Set_of_closures { bindings = closures_in_implicit_set;
                              elements = closure_elements; } ]
        in
        List.filter not_a_closure bindings @ extra_bindings
    in
    let bound_symbols, env =
      let process_binding env (b : Fexpr.symbol_binding)
            : Bound_symbols.Pattern.t * env =
        match b with
        | Code { id; _ } ->
          let code_id, env = fresh_code_id env id in
          Bound_symbols.Pattern.code code_id, env
        | Block_like { symbol; _ } ->
          let symbol, env = declare_symbol env symbol in
          Bound_symbols.Pattern.block_like symbol, env
        | Set_of_closures soc ->
          let closure_binding env ({ symbol;
                                     fun_decl = { closure_id; code_id; _ } }
                                     : Fexpr.static_closure_binding) =
            let symbol, env = declare_symbol env symbol in
            let closure_id = closure_id |> Option.value ~default:code_id in
            let closure_id = fresh_or_existing_closure_id env closure_id in
            (closure_id, symbol), env
          in
          let closure_symbols, env =
            Misc.Stdlib.List.map_accum_left closure_binding env soc.bindings
          in
          Bound_symbols.Pattern.set_of_closures
            (closure_symbols |> Closure_id.Lmap.of_list),
          env
        | Closure _ -> assert false (* should have been filtered out above *)
      in
      Misc.Stdlib.List.map_accum_left process_binding env bindings
    in
    let bound_symbols = bound_symbols |> Bound_symbols.create in
    let static_const env (b : Fexpr.symbol_binding) : Flambda.Static_const.t =
      match b with
      | Block_like { symbol = _; kind = _; defining_expr = def } ->
        begin
          match def with
          | Block { tag; mutability; elements = args } ->
            let tag = Tag.Scannable.create_exn tag in
            Flambda.Static_const.Block
              (tag, mutability,
               List.map (of_kind_value env) args)
        end
      | Set_of_closures { bindings; elements } ->
        let fun_decls =
          List.map (fun (b : Fexpr.static_closure_binding) ->
            b.fun_decl
          ) bindings
        in
        let set = set_of_closures env fun_decls elements in
        Set_of_closures set
      | Closure _ -> assert false (* should have been filtered out above *)
      | Code { id; newer_version_of; param_arity; ret_arity; recursive; inline;
               params_and_body } ->
        let code_id = find_code_id env id in
        let newer_version_of =
          Option.map (find_code_id env) newer_version_of
        in
        let env = enter_code env in
        let params_arity =
          match param_arity with
          | Some ar -> arity ar
          | None ->
            match params_and_body with
            | Deleted ->
              Misc.fatal_errorf
                "Param arity required for deleted code %a"
                  Code_id.print code_id
            | Present { params; _ } ->
              List.map (fun ({ kind; _ } : Fexpr.kinded_parameter) ->
                  value_kind_opt kind)
                params
        in
        let result_arity =
          match ret_arity with
          | None -> [ Flambda_kind.With_subkind.any_value ]
          | Some ar -> arity ar
        in
        let params_and_body : _ Or_deleted.t =
          match params_and_body with
          | Deleted -> Deleted
          | Present { params; closure_var; ret_cont; exn_cont; body } ->
            let params, env =
              Misc.Stdlib.List.map_accum_left
                (fun env ({ param; kind }:Fexpr.kinded_parameter) ->
                  let var, env = fresh_var env param in
                  let param =
                    Kinded_parameter.create var (value_kind_opt kind)
                  in
                  param, env)
                env params
            in
            let my_closure, env = fresh_var env closure_var in
            let return_continuation, env =
              fresh_cont env ret_cont (List.length result_arity)
            in
            let exn_continuation, env =
              fresh_exn_cont env exn_cont
            in
            let body = expr env body in
            let dbg = Debuginfo.none in
            let params_and_body =
              Flambda.Function_params_and_body.create
                ~return_continuation
                exn_continuation params ~body ~my_closure ~dbg
            in
            Present params_and_body
        in
        let recursive = convert_recursive_flag recursive in
        let inline =
          inline |> Option.value ~default:Inline_attribute.Default_inline
        in
        let code =
          Flambda.Code.create
            code_id
            ~params_and_body
            ~newer_version_of
            ~params_arity
            ~result_arity
            ~stub:false
            ~inline
            ~is_a_functor:false
            ~recursive
        in
        Code code
    in
    let static_consts =
      List.map (static_const env) bindings
      |> Flambda.Static_const.Group.create
    in
    let body = expr env body in
    Flambda.Expr.create_let_symbol bound_symbols Syntactic static_consts body

  | Apply {
    func;
    call_kind;
    inline;
    inlining_depth;
    continuation;
    exn_continuation;
    args;
    arities; } ->
    let continuation, _integer_arity = find_cont env continuation in
    let call_kind =
      match call_kind with
      | Function (Direct { code_id; closure_id }) ->
        let closure_id = closure_id |> Option.value ~default:code_id in
        let code_id = find_code_id env code_id in
        let closure_id = fresh_or_existing_closure_id env closure_id in
        let return_arity =
          match arities with
          | None ->
            [ Flambda_kind.With_subkind.any_value ]
          | Some { ret_arity; _ } ->
            arity ret_arity
        in
        Call_kind.direct_function_call code_id closure_id ~return_arity
      | Function Indirect ->
        begin
          match arities with
          | Some { params_arity; ret_arity } ->
            let param_arity = arity params_arity in
            let return_arity = arity ret_arity in
            Call_kind.indirect_function_call_known_arity
              ~param_arity ~return_arity
          | None ->
            Call_kind.indirect_function_call_unknown_arity ()
        end
      | C_call { alloc } ->
        begin
          match arities with
          | Some { params_arity; ret_arity } ->
            let param_arity = arity_without_subkinds params_arity in
            let return_arity = arity_without_subkinds ret_arity in
            Call_kind.c_call ~alloc ~param_arity ~return_arity
          | None ->
            Misc.fatal_errorf "Must specify arities for C call"
        end
    in
    let inline =
      inline |> Option.value ~default:Inline_attribute.Default_inline
    in
    let inlining_depth =
      inlining_depth |> Option.value ~default:0
    in
    let exn_continuation = find_exn_cont env exn_continuation in
    let apply =
      Flambda.Apply.create
        ~callee:(name env func)
        ~continuation:(Return continuation)
        exn_continuation
        ~args:((List.map (simple env)) args)
        ~call_kind
        (Debuginfo.none)
        ~inline
        ~inlining_depth
        ~probe_name:None
    in
    Flambda.Expr.create_apply apply

  | Invalid invalid ->
    Flambda.Expr.create_invalid ~semantics:invalid ()

let conv ~backend ~module_ident (fexpr : Fexpr.flambda_unit) : Flambda_unit.t =
  let module Backend = (val backend : Flambda_backend_intf.S) in
  let module_symbol =
    Backend.symbol_for_global' (
      Ident.create_persistent (Ident.name module_ident))
  in
  let return_continuation = Continuation.create ~name:"done" () in
  let exn_continuation = Continuation.create ~sort:Exn ~name:"error" () in
  let exn_continuation_as_exn_continuation =
    Exn_continuation.create ~exn_handler:exn_continuation ~extra_args:[]
  in
  let env = init_env return_continuation exn_continuation_as_exn_continuation in
  let body = expr env fexpr.body in
  Flambda_unit.create
    ~return_continuation
    ~exn_continuation
    ~body
    ~module_symbol

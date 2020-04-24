[@@@ocaml.warning "+a-4-30-40-41-42"]

module type Convertible_id = sig
  type t
  type fexpr_id

  include Identifiable.S with type t := t

  val desc : string

  val name : t -> string
  val add_tag : string -> int -> string
  val mk_fexpr_id : string -> fexpr_id
end

let default_add_tag name tag = Printf.sprintf "%s_%d" name tag

module Name_map (I : Convertible_id) : sig
  type t

  val empty : t
  val bind : t -> I.t -> I.fexpr_id * t
  val bind_to : t -> I.t -> I.fexpr_id -> t
  val find_exn : t -> I.t -> I.fexpr_id
end = struct
  module String_map = Map.Make(String)

  type t = {
    id_map : I.fexpr_id I.Map.t;
    names : int String_map.t;
  }

  let empty = { id_map = I.Map.empty; names = String_map.empty }
  let bind { id_map; names } id =
    let name = I.name id in
    let rec try_name name names =
      match String_map.find_opt name names with
      | None ->
        let fexpr_id = I.mk_fexpr_id name in
        let names = String_map.add name 1 names in
        fexpr_id, names
      | Some count ->
        let names = String_map.add name (count+1) names in
        let name = I.add_tag name count in
        (* Unlikely but possible that, say, both x and x_1 are used; in this
         * case we'll end up with x_1_1 *)
        try_name name names
    in
    let fexpr_id, names = try_name name names in
    let id_map = I.Map.add id fexpr_id id_map in
    fexpr_id, { id_map; names }

  let bind_to { id_map; names } id fexpr_id =
    let id_map = I.Map.add id fexpr_id id_map in
    { id_map; names }

  let find t id = I.Map.find_opt id t.id_map
  let find_exn t id =
    match find t id with
    | Some fexpr_id -> fexpr_id
    | None -> Misc.fatal_errorf "missing %s %a" I.desc I.print id
end

module Global_name_map (I : Convertible_id) : sig
  type t

  val create : unit -> t
  val translate : t -> I.t -> I.fexpr_id
end = struct
  module String_tbl = Hashtbl.Make(struct
    include String
    let hash = Hashtbl.hash
  end)

  type t = {
    id_tbl : I.fexpr_id I.Tbl.t;
    names : int String_tbl.t;
  }

  let create () = {
    id_tbl = I.Tbl.create 10;
    names = String_tbl.create 10;
  }

  let translate t id =
    match I.Tbl.find_opt t.id_tbl id with
    | Some fexpr_id ->
      fexpr_id
    | None ->
      (* CR-soon lmaurer: Too much duplication with Name_map.bind *)
      let rec try_name name =
        match String_tbl.find_opt t.names name with
        | None ->
          let fexpr_id = I.mk_fexpr_id name in
          String_tbl.add t.names name 1;
          fexpr_id
        | Some count ->
          String_tbl.replace t.names name (count+1);
          let name = Printf.sprintf "%s_%d" name count in
          (* Unlikely but possible that, say, both x and x_1 are used; in this
           * case we'll end up with x_1_1 *)
          try_name name
      in
      let fexpr_id = try_name (I.name id) in
      I.Tbl.add t.id_tbl id fexpr_id;
      fexpr_id
end

let nowhere a = { Fexpr.txt = a; loc = Loc_unknown }

module Env : sig
  type t

  val create : unit -> t
  val bind_var : t -> Variable.t -> Fexpr.variable * t
  val bind_var_in_binding_pos : t -> Var_in_binding_pos.t -> Fexpr.variable * t
  val bind_symbol : t -> Symbol.t -> Fexpr.symbol * t
  val bind_code_id : t -> Code_id.t -> Fexpr.code_id * t
  val bind_named_continuation : t -> Continuation.t -> Fexpr.continuation_id * t
  val bind_special_continuation
    : t -> Continuation.t -> to_:Fexpr.special_continuation -> t
  val find_var_exn : t -> Variable.t -> Fexpr.variable
  val find_symbol_exn : t -> Symbol.t -> Fexpr.symbol
  val find_code_id_exn : t -> Code_id.t -> Fexpr.code_id
  val find_continuation_exn : t -> Continuation.t -> Fexpr.continuation
  val translate_closure_id : t -> Closure_id.t -> Fexpr.closure_id
  val translate_var_within_closure
    : t -> Var_within_closure.t -> Fexpr.var_within_closure
end = struct
  module Variable_name_map = Name_map(struct
    include Variable
    type fexpr_id = Fexpr.variable

    let desc = "variable"
    let name v = raw_name v
    let add_tag = default_add_tag
    let mk_fexpr_id name = name |> nowhere
  end)

  module Symbol_name_map = Name_map(struct
    include Symbol
    type fexpr_id = Fexpr.symbol

    let desc = "symbol"
    let name v = linkage_name v |> Linkage_name.to_string
    let add_tag = default_add_tag
    let mk_fexpr_id name = name |> nowhere
  end)

  module Code_id_name_map = Name_map(struct
    include Code_id
    type fexpr_id = Fexpr.code_id

    let desc = "code id"
    let name v = Code_id.name v
    let add_tag = default_add_tag
    let mk_fexpr_id name = name |> nowhere
  end)

  module Closure_id_name_map = Global_name_map(struct
    include Closure_id
    type fexpr_id = Fexpr.closure_id

    let desc = "closure id"
    let name v = Closure_id.name v
    let add_tag = default_add_tag
    let mk_fexpr_id name = name |> nowhere
  end)

  module Var_within_closure_name_map = Global_name_map(struct
    include Var_within_closure
    type fexpr_id = Fexpr.var_within_closure

    let desc = "var within closure"
    let name v = Variable.raw_name (v |> Var_within_closure.unwrap)
    let add_tag = default_add_tag
    let mk_fexpr_id name = name |> nowhere
  end)

  module Continuation_name_map = Name_map(struct
    include Continuation
    type fexpr_id = Fexpr.continuation

    let desc = "continuation"
    let name c = Continuation.name c
    let add_tag name tag =
      match name with
      | "k" -> Printf.sprintf "k%d" tag
      | _ -> default_add_tag name tag
    let mk_fexpr_id name = Fexpr.Named (name |> nowhere)
  end)

  type t = {
    variables : Variable_name_map.t;
    symbols : Symbol_name_map.t;
    code_ids : Code_id_name_map.t;
    closure_ids : Closure_id_name_map.t;
    vars_within_closures : Var_within_closure_name_map.t;
    continuations : Continuation_name_map.t;
  }

  let create () = {
    variables = Variable_name_map.empty;
    symbols = Symbol_name_map.empty;
    code_ids = Code_id_name_map.empty;
    closure_ids = Closure_id_name_map.create ();
    vars_within_closures = Var_within_closure_name_map.create ();
    continuations = Continuation_name_map.empty;
  }

  let bind_var t v =
    let v, variables = Variable_name_map.bind t.variables v in
    v, { t with variables }

  let bind_var_in_binding_pos t v =
    bind_var t (v |> Var_in_binding_pos.var)

  let bind_symbol t s =
    let s, symbols = Symbol_name_map.bind t.symbols s in
    s, { t with symbols }

  let bind_code_id t c =
    let c, code_ids = Code_id_name_map.bind t.code_ids c in
    c, { t with code_ids }

  let bind_named_continuation t c =
    let c, continuations =
      Continuation_name_map.bind t.continuations c in
    let c_id =
      match c with
      | Named c_id -> c_id
      | Special _ -> assert false
    in
    c_id, { t with continuations }

  let bind_special_continuation t c ~to_:s =
    let continuations =
      Continuation_name_map.bind_to t.continuations c (Special s) in
    { t with continuations }

  let find_var_exn t v = Variable_name_map.find_exn t.variables v

  let find_symbol_exn t s = Symbol_name_map.find_exn t.symbols s

  let find_code_id_exn t c = Code_id_name_map.find_exn t.code_ids c

  let find_continuation_exn t c =
    Continuation_name_map.find_exn t.continuations c

  let translate_closure_id t c = Closure_id_name_map.translate t.closure_ids c

  let translate_var_within_closure t v =
    Var_within_closure_name_map.translate t.vars_within_closures v
end

let name env n =
  Name.pattern_match n
    ~var:(fun v : Fexpr.name -> Var (Env.find_var_exn env v))
    ~symbol:(fun s : Fexpr.name -> Symbol (Env.find_symbol_exn env s))

let const c : Fexpr.const =
  match Reg_width_things.Const.descr c with
  | Naked_immediate imm ->
    Naked_immediate (imm |> Target_imm.to_targetint' |> Targetint.to_string)
  | Tagged_immediate imm ->
    Tagged_immediate (imm |> Target_imm.to_targetint' |> Targetint.to_string)
  | Naked_float f ->
    Naked_float (f |> Numbers.Float_by_bit_pattern.to_float)
  | Naked_int32 i ->
    Naked_int32 i
  | Naked_int64 i ->
    Naked_int64 i
  | Naked_nativeint i ->
    Naked_nativeint (i |> Targetint.to_int64)

let simple env s =
  Simple.pattern_match s
    ~name:(fun n : Fexpr.simple ->
      match name env n with
      | Var v -> Var v
      | Symbol s -> Symbol s
    )
    ~const:(fun c -> Fexpr.Const (const c))

let naked_number_kind (nnk : Flambda_kind.Naked_number_kind.t)
      : Fexpr.Naked_number_kind.t =
  match nnk with
  | Naked_immediate -> Naked_immediate
  | Naked_float -> Naked_float
  | Naked_int32 -> Naked_int32
  | Naked_int64 -> Naked_int64
  | Naked_nativeint -> Naked_nativeint

let kind (k : Flambda_kind.t) : Fexpr.kind =
  match k with
  | Value -> Value
  | Fabricated -> Fabricated
  | Naked_number nnk -> Naked_number (naked_number_kind nnk)

let arity (a : Flambda_arity.With_subkinds.t) : Fexpr.flambda_arity =
  List.map kind (Flambda_arity.With_subkinds.to_arity a)

let arity_without_subkinds a = List.map kind a

let kinded_parameter env (kp : Kinded_parameter.t)
      : Fexpr.kinded_parameter * Env.t =
  let k =
    match kind (Flambda_kind.With_subkind.kind (Kinded_parameter.kind kp)) with
    | Value -> None
    | k -> Some k
  in
  let param, env = Env.bind_var env (Kinded_parameter.var kp) in
  { param; kind = k }, env

let targetint_ocaml (i : Targetint.OCaml.t) : Fexpr.targetint =
  i |> Targetint.OCaml.to_int64

let recursive_flag (r : Recursive.t) : Fexpr.is_recursive =
  match r with
  | Recursive -> Recursive
  | Non_recursive -> Nonrecursive

let unop env (op : Flambda_primitive.unary_primitive) : Fexpr.unop =
  match op with
  | Box_number Untagged_immediate ->
    Tag_imm
  | Get_tag ->
    Get_tag
  | Is_int ->
    Is_int
  | Opaque_identity ->
    Opaque_identity
  | Unbox_number Untagged_immediate ->
    Untag_imm
  | Project_var { project_from; var } ->
    let project_from = Env.translate_closure_id env project_from in
    let var = Env.translate_var_within_closure env var in
    Project_var { project_from; var }
  | Select_closure { move_from; move_to } ->
    let move_from = Env.translate_closure_id env move_from in
    let move_to = Env.translate_closure_id env move_to in
    Select_closure { move_from; move_to }
  | _ ->
    Misc.fatal_errorf "TODO: Unary primitive: %a"
      Flambda_primitive.Without_args.print
      (Flambda_primitive.Without_args.Unary op)

let binop (op : Flambda_primitive.binary_primitive) : Fexpr.binop =
  match op with
  | Block_load (Values { field_kind;
                         size;
                         tag }, mutability) ->
    let size =
      match size with
      | Known size -> Some (size |> targetint_ocaml)
      | Unknown -> None
    in
    Block_load (Values { field_kind;
                         size;
                         tag = tag |> Tag.Scannable.to_int }, mutability)
  | Phys_equal (k, op) ->
    let k =
      match kind k with
      | Value -> None
      | k -> Some k
    in
    Phys_equal (k, op)
  | Int_arith (Tagged_immediate, Add) ->
    Infix Plus
  | Int_arith (Tagged_immediate, Sub) ->
    Infix Minus
  | Int_comp (Tagged_immediate, Signed, Yielding_bool Lt) ->
    Infix Lt
  | Int_comp (Tagged_immediate, Signed, Yielding_bool Gt) ->
    Infix Gt
  | Int_comp (Tagged_immediate, Signed, Yielding_bool Le) ->
    Infix Le
  | Int_comp (Tagged_immediate, Signed, Yielding_bool Ge) ->
    Infix Ge
  | Int_comp (i, s, Yielding_bool c) ->
    Int_comp (i, s, c)
  | Float_comp (Yielding_bool Eq) ->
    Infix Eqdot
  | Float_comp (Yielding_bool Neq) ->
    Infix Neqdot
  | Float_comp (Yielding_bool Lt) ->
    Infix Ltdot
  | Float_comp (Yielding_bool Gt) ->
    Infix Gtdot
  | Float_comp (Yielding_bool Le) ->
    Infix Ledot
  | Float_comp (Yielding_bool Ge) ->
    Infix Gedot
  | _ ->
    Misc.fatal_errorf "TODO: Binary primitive: %a"
      Flambda_primitive.Without_args.print
      (Flambda_primitive.Without_args.Binary op)

let varop (op : Flambda_primitive.variadic_primitive) : Fexpr.varop =
  match op with
  | Make_block (Values (tag, _), mutability) ->
    Make_block (tag |> Tag.Scannable.to_int, mutability)
  | _ ->
    Misc.fatal_errorf "TODO: Variadic primitive: %a"
      Flambda_primitive.Without_args.print
      (Flambda_primitive.Without_args.Variadic op)

let prim env (p : Flambda_primitive.t) : Fexpr.prim =
  match p with
  | Nullary _ -> Misc.fatal_error "Not yet implemented"
  | Unary (op, arg) ->
    Unary (unop env op, simple env arg)
  | Binary (op, arg1, arg2) ->
    Binary (binop op, simple env arg1, simple env arg2)
  | Ternary (op, _, _, _) ->
    Misc.fatal_errorf "TODO: Ternary primitive:"
      Flambda_primitive.Without_args.print
      (Flambda_primitive.Without_args.Ternary op)
  | Variadic (op, args) ->
    Variadic (varop op, List.map (simple env) args)

let closure_elements env map =
  List.map (fun (var, value) ->
    let var = Env.translate_var_within_closure env var in
    let value = simple env value in
    { Fexpr.var; value }
  ) (map |> Var_within_closure.Map.bindings)

let function_declaration env fd closure_id : Fexpr.fun_decl =
  let code_id = Function_declaration.code_id fd in
  let is_tupled = Function_declaration.is_tupled fd in
  let code_id = Env.find_code_id_exn env code_id in
  let closure_id = Env.translate_closure_id env closure_id in
  (* Omit the closure id when possible *)
  let closure_id =
    if String.equal code_id.txt closure_id.txt then None else Some closure_id
  in
  { code_id; closure_id; is_tupled }

let set_of_closures env sc =
  let fun_decls = List.map (fun (closure_id, fun_decl) ->
    function_declaration env fun_decl closure_id
  ) (Set_of_closures.function_decls sc
      |> Function_declarations.funs_in_order
      |> Closure_id.Lmap.bindings)
  in
  let elts = closure_elements env (Set_of_closures.closure_elements sc) in
  let elts =
    match elts with
    | [] -> None
    | _ -> Some elts
  in
  fun_decls, elts

let field_of_block env (field : Flambda.Static_const.Field_of_block.t)
      : Fexpr.of_kind_value =
  match field with
  | Symbol symbol ->
    Symbol (Env.find_symbol_exn env symbol)
  | Tagged_immediate imm ->
    Tagged_immediate (imm |> Target_imm.to_targetint' |> Targetint.to_string)
  | Dynamically_computed var ->
    Dynamically_computed (Env.find_var_exn env var)

let static_const env (sc : Flambda.Static_const.t) : Fexpr.static_part =
  match sc with
  | Block (tag, mutability, fields) ->
    let tag = tag |> Tag.Scannable.to_int in
    let elements = List.map (field_of_block env) fields in
    Block { tag; mutability; elements }
  | Code _ | Set_of_closures _ ->
    assert false
  | _ ->
    Misc.fatal_error "TODO: More static consts"

let rec expr env e =
  match Flambda.Expr.descr e with
  | Let l -> let_expr env l
  | Let_cont lc -> let_cont_expr env lc
  | Apply app -> apply_expr env app
  | Apply_cont app_cont -> apply_cont_expr env app_cont
  | Switch switch -> switch_expr env switch
  | Invalid invalid -> invalid_expr env invalid
and let_expr env le =
  Flambda.Let_expr.pattern_match le ~f:(fun bound ~body : Fexpr.expr ->
    let defining_expr = Flambda.Let_expr.defining_expr le in
    match bound with
    | Singleton var ->
      dynamic_let_expr env [ var ] defining_expr body
    | Set_of_closures { closure_vars; _ } ->
      dynamic_let_expr env closure_vars defining_expr body
    | Symbols { scoping_rule = Dominator; _ } ->
      Misc.fatal_error "TODO: dominator-scoped symbols"
    | Symbols { bound_symbols; scoping_rule = Syntactic } ->
      static_let_expr env bound_symbols defining_expr body
  )
and dynamic_let_expr env vars (defining_expr : Flambda.Named.t) body
      : Fexpr.expr =
  let vars, body_env =
    Misc.Stdlib.List.map_accum_left Env.bind_var_in_binding_pos env vars
  in
  let body = expr body_env body in
  let defining_exprs, closure_elements =
    match defining_expr with
    | Simple s ->
      ([ Simple (simple env s) ] : Fexpr.named list), None
    | Prim (p, _dbg) ->
      ([ Prim (prim env p) ] : Fexpr.named list), None
    | Set_of_closures sc ->
      let fun_decls, closure_elements = set_of_closures env sc in
      let defining_exprs =
        List.map (fun decl : Fexpr.named -> Fexpr.Closure decl) fun_decls
      in
      defining_exprs, closure_elements
    | Static_consts _ ->
      assert false
  in
  if (List.compare_lengths vars defining_exprs <> 0) then
    Misc.fatal_error "Mismatched vars vs. values";
  let bindings =
    List.map2 (fun var defining_expr ->
      { Fexpr.var; kind = None; defining_expr }
    ) vars defining_exprs in
  Let { bindings; closure_elements; body }
and static_let_expr env bound_symbols defining_expr body : Fexpr.expr =
  let static_consts =
    match defining_expr with
    | Flambda.Named.Static_consts static_consts ->
      static_consts |> Flambda.Static_const.Group.to_list
    | _ ->
      assert false
  in
  let bound_symbols = bound_symbols |> Bound_symbols.to_list in
  let env =
    let bind_names env (pat : Bound_symbols.Pattern.t) =
      match pat with
      | Code code_id ->
        let _, env = Env.bind_code_id env code_id in
        env
      | Block_like symbol ->
        let _, env = Env.bind_symbol env symbol in
        env
      | Set_of_closures closure_symbols ->
        Closure_id.Lmap.fold (fun _closure_id symbol env ->
          let _, env = Env.bind_symbol env symbol in
          env
        ) closure_symbols env
    in
    List.fold_left bind_names env bound_symbols
  in
  let translate_const
        (pat : Bound_symbols.Pattern.t)
        (const : Flambda.Static_const.t) : Fexpr.symbol_binding =
    match pat, const with
    | Block_like symbol, _ ->
      (* This is a binding occurrence, but it should have been added
       * already during the first pass *)
      let symbol = Env.find_symbol_exn env symbol in
      let defining_expr = static_const env const in
      Block_like { symbol; kind = None; defining_expr }
    | Set_of_closures closure_symbols, Set_of_closures set ->
      let fun_decls, elements = set_of_closures env set in
      let symbols_by_closure_id =
        closure_symbols
        |> Closure_id.Lmap.bindings
        |> Closure_id.Map.of_list
      in
      let closure_ids =
        Set_of_closures.function_decls set
        |> Function_declarations.funs_in_order
        |> Closure_id.Lmap.keys
      in
      let bindings =
        List.map2 (fun fun_decl closure_id : Fexpr.static_closure_binding ->
          let symbol = Closure_id.Map.find closure_id symbols_by_closure_id in
          let symbol = Env.find_symbol_exn env symbol in
          { symbol; fun_decl }
        ) fun_decls closure_ids
      in
      Set_of_closures { bindings; elements }
    | Code code_id, Code code ->
      (* This is a binding occurrence, but it should have been added
       * already during the first pass *)
      let code_id = Env.find_code_id_exn env code_id in
      let newer_version_of =
        Option.map (Env.find_code_id_exn env)
          (Flambda.Code.newer_version_of code)
      in
      let param_arity =
        match Flambda.Code.params_and_body code with
        | Deleted ->
          Some (arity (Flambda.Code.params_arity code))
        | Present _ ->
          None (* arity will be determined from params *)
      in
      let ret_arity =
        match arity (Flambda.Code.result_arity code) with
        | [ Value ] -> None
        | other -> Some other
      in
      let recursive = recursive_flag (Flambda.Code.recursive code) in
      let inline =
        match Flambda.Code.inline code with
        | Default_inline -> None
        | other -> Some other
      in
      let params_and_body : Fexpr.params_and_body Fexpr.or_deleted =
        match Flambda.Code.params_and_body code with
        | Deleted -> Deleted
        | Present params_and_body ->
          let params_and_body =
            Flambda.Function_params_and_body.pattern_match params_and_body
              ~f:(fun ~return_continuation exn_continuation params ~body
                  ~my_closure : Fexpr.params_and_body ->
                let ret_cont, env =
                  Env.bind_named_continuation env return_continuation
                in
                let exn_cont, env =
                  Env.bind_named_continuation env
                    (Exn_continuation.exn_handler exn_continuation)
                in
                let params, env =
                  Misc.Stdlib.List.map_accum_left kinded_parameter
                    env params
                in
                let closure_var, env = Env.bind_var env my_closure in
                let body = expr env body in
                (* CR-someday lmaurer: Omit exn_cont, closure_var if
                   not used *)
                { params; ret_cont; exn_cont; closure_var; body }
              )
          in
          Present params_and_body
      in
      Code { id = code_id; newer_version_of; param_arity; ret_arity; recursive;
             inline; params_and_body}
    | _, _ ->
      Misc.fatal_errorf "Mismatched pattern and constant: %a vs. %a"
        Bound_symbols.Pattern.print pat
        Flambda.Static_const.print const
  in
  let symbol_bindings = List.map2 translate_const bound_symbols static_consts in
  (* If there's exactly one set of closures, make it implicit *)
  let only_set_of_closures =
    let rec loop only_set (symbol_bindings : Fexpr.symbol_binding list) =
      match symbol_bindings with
      | [] -> only_set
      | Set_of_closures set :: symbol_bindings ->
        begin
          match only_set with
          | None -> loop (Some set) symbol_bindings
          | Some _ -> None
        end
      | _ :: symbol_bindings ->
        loop only_set symbol_bindings
    in
    loop None symbol_bindings
  in
  let body = expr env body in
  match only_set_of_closures with
  | None ->
    Let_symbol { bindings = symbol_bindings; closure_elements = None; body }
  | Some { bindings; elements } ->
    let symbol_bindings =
      List.filter (fun (binding : Fexpr.symbol_binding) ->
        match binding with
        | Set_of_closures _ -> false
        | _ -> true
      ) symbol_bindings
    in
    let extra_symbol_bindings =
      List.map (fun (binding : Fexpr.static_closure_binding)
          : Fexpr.symbol_binding ->
        Closure binding
      ) bindings
    in
    let symbol_bindings = symbol_bindings @ extra_symbol_bindings in
    Let_symbol { bindings = symbol_bindings; closure_elements = elements; body }

and let_cont_expr env (lc : Flambda.Let_cont_expr.t) =
  match lc with
  | Non_recursive { handler; _ } ->
    Flambda.Non_recursive_let_cont_handler.pattern_match handler
      ~f:(fun c ~body ->
        let c, body_env = Env.bind_named_continuation env c in
        let handler =
          cont_handler env c
            (Flambda.Non_recursive_let_cont_handler.handler handler)
        in
        let body = expr body_env body in
        Fexpr.Let_cont { recursive = Nonrecursive;
                         handlers = [ handler ];
                         body }
      )
  | Recursive handlers ->
    Flambda.Recursive_let_cont_handlers.pattern_match handlers
      ~f:(fun ~body handlers ->
        let env =
          Continuation.Set.fold (fun c env ->
            let _, env = Env.bind_named_continuation env c in
            env
          ) (Flambda.Continuation_handlers.domain handlers) env
        in
        let handlers =
          List.map (fun (c, handler) ->
            let c =
              match Env.find_continuation_exn env c with
              | Named c -> c
              | Special _ -> assert false
            in
            cont_handler env c handler
          ) (handlers
              |> Flambda.Continuation_handlers.to_map
              |> Continuation.Map.bindings)
        in
        let body = expr env body in
        Fexpr.Let_cont { recursive = Recursive; handlers; body }
      )
and cont_handler env cont_id h =
  let params_and_handler = Flambda.Continuation_handler.params_and_handler h in
  let stub = Flambda.Continuation_handler.stub h in
  let is_exn_handler = Flambda.Continuation_handler.is_exn_handler h in
  Flambda.Continuation_params_and_handler.pattern_match params_and_handler
    ~f:(fun params ~handler : Fexpr.continuation_handler ->
      let params, env =
        Misc.Stdlib.List.map_accum_left kinded_parameter env params
      in
      let handler = expr env handler in
      { name = cont_id; params; stub; is_exn_handler; handler }
    )
and apply_expr env (app : Apply_expr.t) : Fexpr.expr =
  let func =
    Simple.pattern_match (Apply_expr.callee app)
      ~name:(fun n -> name env n)
      ~const:(fun c ->
        Misc.fatal_errorf "Unexpected const as callee: %a"
          Reg_width_things.Const.print c
      )
  in
  let continuation =
    match Apply_expr.continuation app with
    | Return c -> Env.find_continuation_exn env c
    | Never_returns -> Misc.fatal_error "TODO: Never_returns"
  in
  let exn_continuation =
    let ec = Apply_expr.exn_continuation app in
    let c =
      match Exn_continuation.extra_args ec with
      | [] -> Exn_continuation.exn_handler ec
      | _ -> Misc.fatal_error "TODO: extra args for exn continuation"
    in
    Env.find_continuation_exn env c
  in
  let args = List.map (simple env) (Apply_expr.args app) in
  let call_kind : Fexpr.call_kind =
    match Apply_expr.call_kind app with
    | Function (Direct { code_id; closure_id; return_arity = _ }) ->
      let code_id = Env.find_code_id_exn env code_id in
      let closure_id = Env.translate_closure_id env closure_id in
      let closure_id =
        if String.equal code_id.txt closure_id.txt
          then None
          else Some closure_id
      in
      Function (Direct { code_id; closure_id })
    | Function (Indirect_unknown_arity | Indirect_known_arity _) ->
      Function Indirect
    | C_call { alloc; _ } ->
      C_call { alloc }
    | Method _ ->
      Misc.fatal_error "TODO: Method call kind"
  in
  let arities : Fexpr.function_arities option =
    match Apply_expr.call_kind app with
    | Function (Indirect_known_arity { param_arity; return_arity }) ->
      let params_arity =
        Flambda_arity.With_subkinds.to_arity param_arity
        |> arity_without_subkinds
      in
      let ret_arity =
        Flambda_arity.With_subkinds.to_arity return_arity
        |> arity_without_subkinds
      in
      Some { params_arity; ret_arity; }
    | C_call { param_arity; return_arity; _ } ->
      let params_arity = arity_without_subkinds param_arity in
      let ret_arity = arity_without_subkinds return_arity in
      Some { params_arity; ret_arity }
    | _ ->
      None
  in
  let inline =
    match Apply_expr.inline app with
    | Default_inline -> None
    | other -> Some other
  in
  let inlining_depth =
    match Apply_expr.inlining_depth app with
    | 0 -> None
    | other -> Some other
  in
  Apply { func; continuation; exn_continuation; args; call_kind; inline;
          inlining_depth; arities }
and apply_cont_expr env app_cont : Fexpr.expr =
  Apply_cont (apply_cont env app_cont)
and apply_cont env app_cont : Fexpr.apply_cont =
  let cont =
    Env.find_continuation_exn env (Apply_cont_expr.continuation app_cont)
  in
  let trap_action =
    match Apply_cont_expr.trap_action app_cont with
    | Some _ -> Misc.fatal_error "TODO: trap action"
    | None -> None
  in
  let args = List.map (simple env) (Apply_cont_expr.args app_cont) in
  { cont; trap_action; args }
and switch_expr env switch : Fexpr.expr =
  let scrutinee = simple env (Switch_expr.scrutinee switch) in
  let cases =
    List.map (fun (imm, app_cont) ->
      let tag = imm |> Target_imm.to_targetint' |> Targetint.to_int in
      let app_cont = apply_cont env app_cont in
      tag, app_cont
    ) (Switch_expr.arms switch |> Target_imm.Map.bindings)
  in
  Switch { scrutinee; cases }
and invalid_expr _env invalid : Fexpr.expr =
  Invalid invalid

let conv flambda_unit =
  let done_ = Flambda_unit.return_continuation flambda_unit in
  let error = Flambda_unit.exn_continuation flambda_unit in
  let env = Env.create () in
  let env = Env.bind_special_continuation env done_ ~to_:Done in
  let env = Env.bind_special_continuation env error ~to_:Error in
  let body = expr env (Flambda_unit.body flambda_unit) in
  { Fexpr.body }

[@@@ocaml.warning "-30"]

type location = Lambda.scoped_location
type 'a located = {
  txt : 'a;
  loc : location;
}

type variable = string located
type continuation_id = string located
type code_id = string located
type closure_id = string located
type var_within_closure = string located

type symbol = string located

type immediate = string
type targetint = int64

type const =
  | Naked_immediate of immediate
  | Tagged_immediate of immediate
  | Naked_float of float
  | Naked_int32 of Int32.t
  | Naked_int64 of Int64.t
  | Naked_nativeint of targetint

type of_kind_value =
  | Symbol of symbol
  | Tagged_immediate of immediate
  | Dynamically_computed of variable

type is_recursive =
  | Nonrecursive
  | Recursive

type tag_scannable = int

type mutability = Mutability.t =
  | Mutable
  | Immutable
  | Immutable_unique

type static_part =
  | Block of {
      tag : tag_scannable;
      mutability : mutability;
      elements : of_kind_value list;
    }

module Naked_number_kind = struct
  type t =
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint
end

type kind =
  | Value
  | Naked_number of Naked_number_kind.t
  | Fabricated

type static_structure = {
  symbol : symbol;
  kind : kind option;
  defining_expr : static_part;
}

type invalid_term_semantics = Invalid_term_semantics.t =
  | Treat_as_unreachable
  | Halt_and_catch_fire

type trap_action
type kinded_parameter = {
  param : variable;
  kind : kind option;
}

type name =
  | Var of variable
  | Symbol of symbol

type simple =
  | Var of variable
  | Symbol of symbol
  | Const of const

type unop =
  | Get_tag
  | Is_int
  | Opaque_identity
  | Tag_imm
  | Untag_imm
  | Project_var of {
      project_from : closure_id;
      var : var_within_closure;
    }
  | Select_closure of {
      move_from : closure_id;
      move_to : closure_id;
    }

type generic_array_specialisation =
  | No_specialisation
  | Full_of_naked_floats
  | Full_of_immediates
  | Full_of_arbitrary_values_but_not_floats

type block_access_field_kind = Flambda_primitive.Block_access_field_kind.t =
  | Any_value
  | Immediate

type block_access_kind =
  | Values of {
      tag : tag_scannable;
      size : targetint option;
      field_kind : block_access_field_kind;
    }

type standard_int = Flambda_kind.Standard_int.t =
  | Tagged_immediate
  | Naked_immediate
  | Naked_int32
  | Naked_int64
  | Naked_nativeint

type ordered_comparison = Flambda_primitive.ordered_comparison =
  | Lt | Gt
  | Le | Ge

type equality_comparison = Flambda_primitive.equality_comparison =
  | Eq | Neq

type signed_or_unsigned = Flambda_primitive.signed_or_unsigned =
  | Signed | Unsigned

type infix_binop =
  | Plus | Plusdot
  | Minus | Minusdot
  | Eqdot
  | Neqdot
  | Lt | Ltdot
  | Gt | Gtdot
  | Le | Ledot
  | Ge | Gedot

type binop =
  | Block_load of block_access_kind * mutability
  | Phys_equal of kind option * equality_comparison
  | Int_comp of standard_int * signed_or_unsigned * ordered_comparison
  | Infix of infix_binop

type varop =
  | Make_block of tag_scannable * mutability

type prim =
  | Unary of unop * simple
  | Binary of binop * simple * simple
  | Variadic of varop * simple list

type is_fabricated =
  | Value | Fabricated

type flambda_arity = kind list

type function_call =
  | Direct of {
      code_id : code_id;
      closure_id : closure_id option;
    }
  | Indirect (* Will translate to indirect_known_arity or
                indirect_unknown_arity depending on whether the apply record's
                arities field has a value *)

type method_kind = Self | Public | Cached

type call_kind =
  | Function of function_call
  (* | Method of { kind : method_kind; obj : simple; } *)
  | C_call of {
      alloc : bool;
    }

type special_continuation =
  | Done (* top-level normal continuation *)
  | Error (* top-level exception continuation *)

type continuation =
  | Named of continuation_id
  | Special of special_continuation

type function_arities = {
  params_arity : flambda_arity;
  ret_arity : flambda_arity;
}

type inline_attribute = Inline_attribute.t =
  | Always_inline
  | Never_inline
  | Unroll of int
  | Default_inline
  | Hint_inline

type apply = {
    func : name;
    continuation : continuation;
    exn_continuation : continuation;
    args : simple list;
    call_kind : call_kind;
    arities : function_arities option;
    inline : inline_attribute option;
    inlining_depth : int option;
  }

type size = int

type apply_cont = {
  cont : continuation;
  trap_action : trap_action option;
  args : simple list;
}

type expr =
  | Let of let_
  | Let_cont of let_cont
  | Let_symbol of let_symbol
  | Apply of apply
  | Apply_cont of apply_cont
  | Switch of {
      scrutinee : simple;
      cases : (int * apply_cont) list;
    }
  | Invalid of invalid_term_semantics

and closure_elements = closure_element list

and closure_element = {
  var : var_within_closure;
  value : simple;
}

and let_ = {
  bindings : let_binding list;
  closure_elements : closure_elements option;
  body : expr;
}

and let_binding = {
    var : variable;
    kind : kind option;
    defining_expr : named;
  }

and named =
  | Simple of simple
  | Prim of prim
  | Closure of fun_decl

and fun_decl = {
  code_id : code_id;
  closure_id : closure_id option; (* defaults to same name as code id *)
  is_tupled : bool;
}

and let_cont = {
  recursive : is_recursive;
  body : expr;
  handlers : let_cont_handlers;
}

and let_cont_handlers = continuation_handler list

and continuation_handler = {
  name : continuation_id;
  params : kinded_parameter list;
  stub : bool;
  is_exn_handler : bool;
  handler : expr;
}

and let_symbol = {
  bindings : symbol_binding list;
  (* Only used if there's no [Set_of_closures] in the list *)
  closure_elements : closure_elements option;
  body : expr;
}

and symbol_binding =
  | Block_like of static_structure
  | Code of code
  | Closure of static_closure_binding
  | Set_of_closures of static_set_of_closures

and static_set_of_closures = {
  bindings : static_closure_binding list;
  elements : closure_elements option;
}

and code = {
  id : code_id;
  newer_version_of : code_id option;
  param_arity : flambda_arity option;
  ret_arity : flambda_arity option;
  recursive : is_recursive;
  inline : inline_attribute option;
  params_and_body : params_and_body or_deleted;
}

and params_and_body = {
  params : kinded_parameter list;
  closure_var : variable;
  ret_cont : continuation_id;
  exn_cont : continuation_id;
  body : expr;
}

and 'a or_deleted =
  | Present of 'a
  | Deleted

and static_closure_binding = {
  symbol : symbol;
  fun_decl : fun_decl;
}

type flambda_unit = {
  body : expr;
}

type expect_test_spec = {
  before : flambda_unit;
  after : flambda_unit;
}

type markdown_node =
  | Text of string
  | Expect of expect_test_spec

type markdown_doc = markdown_node list

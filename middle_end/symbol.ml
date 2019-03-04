(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Module_block of {
      compilation_unit : Compilation_unit.t;
    }
  | Lifted_variable of {
      compilation_unit : Compilation_unit.t;
      name : string;
      stamp : int;
    }
  | Lifted_closure of {
      compilation_unit : Compilation_unit.t;
      name : string;
      stamp : int;
    }
  | Lifted_anonymous_constant of {
      compilation_unit : Compilation_unit.t;
      stamp : int;
    }
  | Code_pointer of {
      compilation_unit : Compilation_unit.t;
      name : string;
      stamp : int;
    }
  | Predefined_exn of string

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    match t1, t2 with
    | Module_block { compilation_unit = compilation_unit1; },
        Module_block { compilation_unit = compilation_unit2; } ->
      Compilation_unit.compare compilation_unit1 compilation_unit2
    | Module_block _, _ -> -1
    | Lifted_variable {
        compilation_unit = compilation_unit1;
        name = name1;
        stamp = stamp1;
      },
        Lifted_variable {
          compilation_unit = compilation_unit2;
          name = name2;
          stamp = stamp2;
        } ->
      let c = Compilation_unit.compare compilation_unit1 compilation_unit2 in
      if c <> 0 then c
      else
        let c = String.compare name1 name2 in
        if c <> 0 then c
        else Stdlib.compare stamp1 stamp2
    | Lifted_variable _, Module_block _ -> 1
    | Lifted_variable _, _ -> -1
    | Lifted_closure {
        compilation_unit = compilation_unit1;
        name = name1;
        stamp = stamp1;
      },
        Lifted_closure {
          compilation_unit = compilation_unit2;
          name = name2;
          stamp = stamp2;
        } ->
      let c = Compilation_unit.compare compilation_unit1 compilation_unit2 in
      if c <> 0 then c
      else
        let c = String.compare name1 name2 in
        if c <> 0 then c
        else Stdlib.compare stamp1 stamp2
    | Lifted_closure _, (Module_block _ | Lifted_variable _) -> 1
    | Lifted_closure _, _ -> -1
    | Lifted_anonymous_constant {
        compilation_unit = compilation_unit1;
        stamp = stamp1;
      },
        Lifted_anonymous_constant {
          compilation_unit = compilation_unit2;
          stamp = stamp2;
        } ->
      let c = Compilation_unit.compare compilation_unit1 compilation_unit2 in
      if c <> 0 then c
      else Stdlib.compare stamp1 stamp2
    | Lifted_anonymous_constant _,
        (Module_block _ | Lifted_variable _ | Lifted_closure _) -> 1
    | Lifted_anonymous_constant _, _ -> -1
    | Code_pointer {
        compilation_unit = compilation_unit1;
        name = name1;
        stamp = stamp1;
      },
        Code_pointer {
          compilation_unit = compilation_unit2;
          name = name2;
          stamp = stamp2;
        } ->
      let c = Compilation_unit.compare compilation_unit1 compilation_unit2 in
      if c <> 0 then c
      else
        let c = String.compare name1 name2 in
        if c <> 0 then c
        else Stdlib.compare stamp1 stamp2
    | Code_pointer _,
        (Module_block _ | Lifted_variable _ | Lifted_closure _
          | Lifted_anonymous_constant _) -> 1
    | Code_pointer _, _ -> -1
    | Predefined_exn name1, Predefined_exn name2 ->
      String.compare name1 name2
    | Predefined_exn _,
        (Module_block _ | Lifted_variable _ | Lifted_closure _
          | Lifted_anonymous_constant _ | Code_pointer _) -> 1

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash t =
    match t with
    | Module_block { compilation_unit; } ->
      Hashtbl.hash (0, Compilation_unit.hash compilation_unit)
    | Lifted_variable { compilation_unit; name; stamp; } ->
      Hashtbl.hash (1, (
        Compilation_unit.hash compilation_unit, Hashtbl.hash name,
          Hashtbl.hash stamp))
    | Lifted_closure { compilation_unit; name; stamp; } ->
      Hashtbl.hash (2, (
        Compilation_unit.hash compilation_unit, Hashtbl.hash name,
          Hashtbl.hash stamp))
    | Lifted_anonymous_constant { compilation_unit; stamp; } ->
      Hashtbl.hash (3, (
        Compilation_unit.hash compilation_unit, Hashtbl.hash stamp))
    | Code_pointer { compilation_unit; name; stamp; } ->
      Hashtbl.hash (4, (
        Compilation_unit.hash compilation_unit, Hashtbl.hash name,
          Hashtbl.hash stamp))
    | Predefined_exn name ->
      Hashtbl.hash (5, Hashtbl.hash name)

  let print ppf t =
    match t with
    | Module_block { compilation_unit; } ->
      Format.fprintf ppf "@[<hov 2>(Module_block@ \
          @[<hov 2>(compilation_unit@ %a)@])@]"
        Compilation_unit.print compilation_unit
    | Lifted_variable { compilation_unit; name; stamp; } ->
      Format.fprintf ppf "@[<hov 2>(Lifted_variable@ \
          @[<hov 2>(compilation_unit@ %a)@]@ \
          @[<hov 2>(name@ %s)@]@ \
          @[<hov 2>(stamp@ %d)@])@]"
        Compilation_unit.print compilation_unit
        name
        stamp
    | Lifted_closure { compilation_unit; name; stamp; } ->
      Format.fprintf ppf "@[<hov 2>(Lifted_closure@ \
          @[<hov 2>(compilation_unit@ %a)@]@ \
          @[<hov 2>(name@ %s)@]@ \
          @[<hov 2>(stamp@ %d)@])@]"
        Compilation_unit.print compilation_unit
        name
        stamp
    | Lifted_anonymous_constant { compilation_unit; stamp; } ->
      Format.fprintf ppf "@[<hov 2>(Lifted_anonymous_constant@ \
          @[<hov 2>(compilation_unit@ %a)@]@ \
          @[<hov 2>(stamp@ %d)@])@]"
        Compilation_unit.print compilation_unit
        stamp
    | Code_pointer { compilation_unit; name; stamp; } ->
      Format.fprintf ppf "@[<hov 2>(Code_pointer@ \
          @[<hov 2>(compilation_unit@ %a)@]@ \
          @[<hov 2>(name@ %s)@]@ \
          @[<hov 2>(stamp@ %d)@])@]"
        Compilation_unit.print compilation_unit
        name
        stamp
    | Predefined_exn name ->
      Format.fprintf ppf "@[(Predefined_exn %s)@]" name

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

let for_module_block compilation_unit =
  Module_block {
    compilation_unit;
  }

let for_lifted_variable variable =
  let compilation_unit = Variable.get_compilation_unit variable in
  let name = Variable.name variable in
  let stamp = Variable.stamp variable in
  Lifted_variable {
    compilation_unit;
    name;
    stamp;
  }

let for_lifted_closure closure_id =
  let compilation_unit = Closure_id.get_compilation_unit closure_id in
  let name = Closure_id.name closure_id in
  let stamp = Closure_id.stamp closure_id in
  Lifted_closure {
    compilation_unit;
    name;
    stamp;
  }

let for_function closure_id =
  let compilation_unit = Closure_id.get_compilation_unit closure_id in
  let name = Closure_id.name closure_id in
  let stamp = Closure_id.stamp closure_id in
  Code_pointer {
    compilation_unit;
    name;
    stamp;
  }

let next_anonymous_constant_stamp = ref 0

(* CR mshinwell: Add [reset] function? *)

let for_lifted_anonymous_constant ?compilation_unit () =
  let stamp = !next_anonymous_constant_stamp in
  incr next_anonymous_constant_stamp;
  let compilation_unit =
    match compilation_unit with
    | None -> Persistent_env.Current_unit.get_exn ()
    | Some compilation_unit -> compilation_unit
  in
  Lifted_anonymous_constant {
    compilation_unit;
    stamp;
  }

let for_predefined_exn id =
  if not (Ident.is_predef id) then begin
    Misc.fatal_errorf "[Symbol.for_predefined_exn] with non-predefined \
        identifier %a"
      Ident.print id
  end;
  Predefined_exn (Ident.name id)

let lifted_closure_symbol_from_code_pointer_symbol t =
  match t with
  | Code_pointer {
      compilation_unit;
      name;
      stamp;
    } ->
    Lifted_closure {
      compilation_unit;
      name;
      stamp;
    }
  | _ -> Misc.fatal_errorf "Non-closure symbol %a" print t

type compilation_unit_or_predef =
  | Compilation_unit of Compilation_unit.t
  | Predef

let compilation_unit t =
  match t with
  | Module_block { compilation_unit; }
  | Lifted_variable { compilation_unit; _ }
  | Lifted_closure { compilation_unit; _ }
  | Lifted_anonymous_constant { compilation_unit; _ }
  | Code_pointer { compilation_unit; _ } ->
    Compilation_unit compilation_unit
  | Predefined_exn _ -> Predef

type kind = Text | Data

let kind t : kind =
  match t with
  | Code_pointer _ -> Text
  | Module_block _
  | Lifted_variable _
  | Lifted_closure _
  | Lifted_anonymous_constant _
  | Predefined_exn _ -> Data

let name_for_backend t =
  (* Care: these must not clash with the names produced in
     [Backend_sym.for_lifted_anonymous_constant]. *)
  match t with
  | Module_block _ -> None
  | Lifted_variable { name; stamp; _ } ->
    Some (Printf.sprintf "%s_%d" name stamp)
  | Lifted_closure { name; stamp; _ } ->
    Some (Printf.sprintf "%s_closure_%d" name stamp)
  | Lifted_anonymous_constant { stamp; _ } ->
    Some (string_of_int stamp)
  | Code_pointer { name; stamp; _ } ->
    Some (Printf.sprintf "%s_%d" name stamp)
  | Predefined_exn name -> Some name

let in_compilation_unit t comp_unit =
  match compilation_unit t with
  | Compilation_unit comp_unit' -> Compilation_unit.equal comp_unit comp_unit'
  | Predef -> false

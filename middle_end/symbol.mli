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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Middle-end symbols: names of statically-allocated constants.  These
    symbols are used inside Closure and Flambda.  They always point at
    well-formed OCaml values in a data section, never at code, with the
    exception of those symbols created with [for_function].

    Middle-end symbols should in general not be created later than the middle
    end. The infrastructure for these symbols relies on a current compilation
    unit being set, which might not always be the case later on, e.g. when
    compiling a startup file.
*)

(** The type of middle-end symbols. *)
type t

(** Printing, comparison, sets, maps, etc. *)
include Identifiable.S with type t := t

(** Create a symbol to reference the module block for the given compilation
    unit. *)
val for_module_block : Compilation_unit.t -> t

(** Create a symbol to correspond to an Flambda variable whose definition
    has been lifted. *)
val for_lifted_variable : Variable.t -> t

(** Create a symbol to correspond to an Flambda closure that has been lifted. *)
val for_lifted_closure : Closure_id.t -> t

(** Create a symbol to correspond to the code pointer for direct, full
    application of the code associated with the given closure ID. *)
val for_function : Closure_id.t -> t

(** Create a symbol to correspond to a lifted constant, in the current
    compilation unit (unless specified otherwise), that does not have a
    source-level name associated with it. *)
val for_lifted_anonymous_constant
   : ?compilation_unit:Compilation_unit.t
  -> unit
  -> t

(** Create a symbol to correspond to the given predefined exception. *)
val for_predefined_exn : Ident.t -> t

(** Transform a symbol constructed using [for_function], representing the
    code pointer of a function, to the symbol to be used for any
    corresponding lifted constant closure containing such function.  This is
    only intended for the use of [Cmmgen]. *)
val lifted_closure_symbol_from_code_pointer_symbol : t -> t

type compilation_unit_or_predef = private
  | Compilation_unit of Compilation_unit.t
  | Predef

(** The compilation unit in which the given symbol's definition lies. *)
val compilation_unit : t -> compilation_unit_or_predef

type kind = private Text | Data

(** Whether a symbol points at executable code ("text") or data. *)
val kind : t -> kind

(** The name of the symbol as to be used for construction of [Backend_sym.t]
    values, without any mangling or compilation unit prefix.  Symbols
    corresponding to compilation units will return [None] here. *)
val name_for_backend : t -> string option

(** Test whether the given symbol is defined in the given compilation unit. *)
val in_compilation_unit : t -> Compilation_unit.t -> bool

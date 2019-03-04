[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare

module String = Misc.Stdlib.String

type error =
    Invalid_character of char
  | Bad_compilation_unit_name of string

exception Error of error

module Name = struct

  type t = string

  include Identifiable.Make (struct
      type nonrec t = t

      let compare = String.compare
      let equal = String.equal
      let hash = Hashtbl.hash
      let print = String.print
      let output chan t = print (Format.formatter_of_out_channel chan) t
    end)

  let isupper chr =
    Stdlib.(=) (Char.uppercase_ascii chr) chr

  let of_string str =
    if String.length str < 1 || not (isupper (String.get str 0)) then begin
      raise (Error (Bad_compilation_unit_name str))
    end;
    str

  let dummy = ""

  let to_string t = t

end

module Prefix = struct

  type component = Pack of Name.t * Name.t list

  type t = component list

  let equal_component (Pack (m, args)) (Pack (m', args')) =
    Name.equal m m' && Misc.Stdlib.List.equal Name.equal args args'

  let print_gen pp_functor pp_arg fmt p =
    let open Format in
    pp_print_list
      ~pp_sep:(fun ppf () -> pp_print_string ppf ".")
      (pp_functor pp_arg)
      fmt
      p

  include Identifiable.Make (struct
      type nonrec t = t
      let equal = Misc.Stdlib.List.equal equal_component

      let compare (p1 : t) (p2 : t) =
        let compare_functors (Pack (m1, args1)) (Pack (m2, args2)) =
          let c = String.compare m1 m2 in
          if c = 0 then Misc.Stdlib.List.compare String.compare args1 args2
          else c
        in
        Misc.Stdlib.List.compare compare_functors p1 p2

      let hash = Hashtbl.hash

      let print fmt p =
        let open Format in
        let pp_arg fmt arg = fprintf fmt "(%a)" Name.print arg in
        let pp_functor pp_arg fmt (Pack (m, args)) =
          fprintf fmt "%a%a"
            Name.print m
            (pp_print_list pp_arg) args
        in
        print_gen pp_functor pp_arg fmt p

      let output chan t = print (Format.formatter_of_out_channel chan) t
    end)

  let is_valid_character first_char c =
    let code = Char.code c in
    if first_char then
      code >= 65 && code <= 90 (* [A-Z] *)
    else
      Char.equal c '_'
      || code >= 48 && 57 <= 90 (* [0-9] *)
      || code >= 65 && code <= 90 (* [A-Z] *)
      || code >= 97 && code <= 122 (* [a-z] *)

  let parse_functorized_pack p =
    let rec extract acc i =
      if i >= String.length p then acc
      else if not (Char.equal p.[i] '(') then raise (Error (Invalid_character p.[i]))
      else
        match String.index_from_opt p i ')' with
          None -> raise (Error (Invalid_character ')'))
        | Some stop ->
            extract (String.sub p (i+1) (stop-i-1) :: acc) (stop+1)
    in
    match String.index_opt p '(' with
      None -> Pack (p, [])
    | Some i ->
        let rev_args = extract [] i in
        Pack (String.sub p 0 i, List.rev rev_args)

  let check_module_name name =
    String.iteri (fun i c ->
        if not (is_valid_character (i=0) c) then
          raise (Error (Invalid_character c)))
      name

  let parse pack =
    let prefix =
      String.split_on_char '.' pack |> List.map parse_functorized_pack in
    List.iter (function Pack (module_name, args) ->
        check_module_name module_name;
        List.iter check_module_name args)
      prefix;
    prefix

  let parse_for_pack = function
      None -> []
    | Some pack -> parse pack

  let extract_prefix name =
    match String.rindex_opt name '.' with
    | None -> [], name
    | Some pos ->
        parse (String.sub name 0 pos),
        String.sub name (pos+1) (String.length name - pos - 1)

  let to_string p =
    Format.asprintf "%a" print p

  let for_address p =
    let open Format in
    let pp_functor _ fmt (Pack (m, _)) =
      fprintf fmt "%s" m
    in
    Format.asprintf "%a"
      (print_gen pp_functor (fun _ _ -> ())) p

  let in_functor prefix =
    List.exists (function Pack (_, args) ->
        match args with [] -> false | _ -> true) prefix

  let in_common_functor curr dep =
    let common =
      Misc.Stdlib.List.find_and_chop_longest_common_prefix
        ~equal:(fun (Pack (m1, _)) (Pack (m2, _)) -> Name.equal m1 m2)
        ~first:curr
        ~second:dep
    in
    in_functor common.Misc.Stdlib.List.longest_common_prefix

  let in_functor_parameters unit_name prefix =
    List.exists (function Pack (_, args) ->
        List.exists (Name.equal unit_name) args)
      prefix

end

type t = {
  basename : Name.t;
  for_pack_prefix : Prefix.t;
  hash : int;
}

let create ?(for_pack_prefix = []) basename =
  { basename;
    for_pack_prefix;
    hash = Hashtbl.hash (basename, for_pack_prefix)
  }

let of_raw_string str =
  let for_pack_prefix, name = Prefix.extract_prefix str in
  create ~for_pack_prefix name

let none = create (Name.of_string "*none*")

let name unit = unit.basename

let for_pack_prefix unit = unit.for_pack_prefix

let is_packed t =
  match t.for_pack_prefix with
  | [] -> false
  | _::_ -> true

let full_path unit =
  unit.for_pack_prefix @ [ Prefix.Pack (unit.basename, []) ]

let for_address unit =
  match unit.for_pack_prefix with
    [] -> unit.basename
  | (_ :: _) as prefix ->
      Format.asprintf "%s.%a" (Prefix.for_address prefix) Name.print unit.basename

type crcs = (t * Digest.t option) list

include Identifiable.Make (struct
  type nonrec t = t

  let compare
        ({ basename = basename1; for_pack_prefix = for_pack_prefix1; hash = hash1; _} as t1)
        ({ basename = basename2; for_pack_prefix = for_pack_prefix2; hash = hash2; _} as t2)
        =
    if t1 == t2 then 0
    else
      let c = Stdlib.compare hash1 hash2 in
      if c <> 0 then c
      else
        let c = String.compare basename1 basename2 in
        if c <> 0 then c
        else Prefix.compare for_pack_prefix1 for_pack_prefix2

  let equal x y =
    if x == y then true
    else compare x y = 0

  let print ppf { for_pack_prefix; hash = _; basename } =
    match for_pack_prefix with
    | [] ->
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(id@ %s)@])@]"
        basename
    | for_pack_prefix ->
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(for_pack_prefix@ %a)@]@;\
          @[<hov 1>(basename@ %s)@]"
        Prefix.print for_pack_prefix
        basename

  let output oc t =
    print (Format.formatter_of_out_channel oc) t

  let hash t = t.hash
end)

(** Pretty printing *)

let print_name ppf t =
  Format.pp_print_string ppf t.basename

let print_full_path fmt unit =
  match unit.for_pack_prefix with
    [] -> Format.fprintf fmt "%a" Name.print unit.basename
  | _ :: _ ->
      Format.fprintf fmt "%a.%a"
        Prefix.print unit.for_pack_prefix
        Name.print unit.basename

let full_path_as_string unit =
  Format.asprintf "%a" print_full_path unit

(* TEST
files="inside_pack.ml use_pack.ml"
* setup-ocamlopt.byte-build-env
** ocamlopt.byte
flags = "-for-pack Pack"
module = "inside_pack.ml"
*** ocamlopt.byte
module = ""
program = "pack.cmx"
flags = "-pack"
all_modules = "inside_pack.cmx"
**** ocamlopt.byte
program = "use_pack.cmx"
flags = ""
all_modules = "pack.cmx use_pack.ml"
**** ocamlopt.byte
program = "use_pack.cmx"
flags = "-for-pack Another_pack"
all_modules = "pack.cmx use_pack.ml"
*)


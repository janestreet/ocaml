(* TEST
files="inside_pack.ml use_pack.ml"
* setup-ocamlopt.byte-build-env
** ocamlopt.byte
flags = "-for-pack Pack.Sub_pack"
module = "inside_pack.ml"
*** ocamlopt.byte
module = ""
program = "sub_pack.cmx"
flags = "-pack -for-pack Pack"
all_modules = "inside_pack.cmx"
**** ocamlopt.byte
module = ""
program = "pack.cmx"
flags = "-pack"
all_modules = "sub_pack.cmx"
***** ocamlopt.byte
program = "bad.cmx"
flags = ""
all_modules = "pack.cmx use_pack.ml"
ocamlopt_byte_exit_status = "2"
***** ocamlopt.byte
program = "bad.cmx"
flags = "-for-pack Pack.Another_sub_pack"
all_modules = "pack.cmx use_pack.ml"
compiler_reference = "${test_source_directory}/use_pack.reference"
ocamlopt_byte_exit_status = "2"
****** check-ocamlopt.byte-output
*)


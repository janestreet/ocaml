(* TEST

files="param.mli m1.ml m2.ml m3.ml use_make.ml"

* setup-ocamlc.byte-build-env
program = "${test_build_directory}/use_make.byte"
** ocamlc.byte
flags = "-parameter-of Make"
module = "param.mli"
*** ocamlc.byte
flags = "-for-pack Make(Param)"
module = "m1.ml"
**** ocamlc.byte
flags = "-for-pack Make(Param)"
module = "m2.ml"
***** ocamlc.byte
flags = "-for-pack Make(Param)"
module = "m3.ml"
****** ocamlc.byte
flags = "-parameter Param -pack"
module = ""
program = "make.cmo"
all_modules = "m1.cmo m2.cmo m3.cmo"
******* ocamlc.byte
module = "use_make.ml"
flags = ""
******** ocamlc.byte
module = ""
program = "${test_build_directory}/use_make.byte"
flags = ""
all_modules = "make.cmo use_make.cmo"
********* run
********** check-program-output
reference = "${test_source_directory}/use_make.reference"

* setup-ocamlopt.byte-build-env
program = "${test_build_directory}/use_make.asm"
** ocamlopt.byte
flags = "-parameter-of Make"
module = "param.mli"
*** ocamlopt.byte
flags = "-for-pack Make(Param)"
module = "m1.ml"
**** ocamlopt.byte
flags = "-for-pack Make(Param)"
module = "m2.ml"
***** ocamlopt.byte
flags = "-for-pack Make(Param)"
module = "m3.ml"
****** ocamlopt.byte
flags = "-parameter Param -pack"
module = ""
program = "make.cmx"
all_modules = "m1.cmx m2.cmx m3.cmx"
******* ocamlopt.byte
module = "use_make.ml"
flags = ""
******** ocamlopt.byte
module = ""
program = "${test_build_directory}/use_make.asm"
flags = ""
all_modules = "make.cmx use_make.cmx"
********* run
********** check-program-output
reference = "${test_source_directory}/use_make.reference"

*)

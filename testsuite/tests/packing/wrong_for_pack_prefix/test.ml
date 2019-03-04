(* TEST

* flambda
** native
files="baz.ml baz.mli bar.ml foo.ml"
*** setup-ocamlopt.byte-build-env
**** script
script = "mkdir -p ${test_build_directory}/baz_correct"
***** script
script = "cp -a baz.ml ${test_build_directory}/baz_correct"
****** script
script = "mkdir -p ${test_build_directory}/baz_incorrect"
******* script
script = "cp -a baz.ml ${test_build_directory}/baz_incorrect"
******** ocamlopt.byte
flags = ""
module = "baz.mli"
********* ocamlopt.byte
flags = "-for-pack Pack"
module = "baz_correct/baz.ml"
********** ocamlopt.byte
flags = "-for-pack Another_pack"
module = "baz_incorrect/baz.ml"
*********** ocamlopt.byte
flags = "-for-pack Pack -I ./baz_correct"
module = "bar.ml"
************ ocamlopt.byte
flags = "-for-pack Pack.Subpack -I ./baz_incorrect"
module = "foo.ml"
ocamlopt_byte_exit_status = "2"
compiler_reference = "${test_source_directory}/foo.reference"
************* check-ocamlopt.byte-output
*)


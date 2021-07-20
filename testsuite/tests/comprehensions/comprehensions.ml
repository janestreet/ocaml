(* TEST
   * expect
*)


[i for i = 0 to 10];;
[%%expect{|
- : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
|}];;


[i for i = 10 downto 0];;
[%%expect{|
- : int list = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0]
|}];;


true::[i for i = 10 downto 0];;
[%%expect{|
Line 1, characters 7-8:
1 | true::[i for i = 10 downto 0];;
           ^
Error: This expression has type int but an expression was expected of type
         bool
|}];;


module M = struct type t = A | B end;;
let x : M.t list  = [A for i = 1 to 1];;
[%%expect{|
module M : sig type t = A | B end
val x : M.t list = [M.A]
|}];;

[A for i = 1 to 1];;
[%%expect{|
Line 1, characters 1-2:
1 | [A for i = 1 to 1];;
     ^
Error: Unbound constructor A
|}];;

M.B::[A for i = 1 to 1];;
[%%expect{|
- : M.t list = [M.B; M.A]
|}, Principal{|
Line 1, characters 6-7:
1 | M.B::[A for i = 1 to 1];;
          ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.
- : M.t list = [M.B; M.A]
|}];;


let y = 10;;
[i for i in y];;
[%%expect{|
val y : int = 10
Line 2, characters 12-13:
2 | [i for i in y];;
                ^
Error: This expression has type int but an expression was expected of type
         'a list
       because it is in the iteration argument of a comprehension
|}];;


let y = [1;2;3];;
[i for i in y];;
[%%expect{|
val y : int list = [1; 2; 3]
- : int list = [1; 2; 3]
|}];;


let y = [1;2;3];;
true::[i for i in y];;
[%%expect{|
val y : int list = [1; 2; 3]
Line 2, characters 7-8:
2 | true::[i for i in y];;
           ^
Error: This expression has type int but an expression was expected of type
         bool
|}];;

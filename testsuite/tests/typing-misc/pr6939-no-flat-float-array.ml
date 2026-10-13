(* TEST
 no-flat-float-array;
 expect;
*)

let rec x = [| x |]; 1.;;
[%%expect{|
Line 1, characters 12-19:
1 | let rec x = [| x |]; 1.;;
                ^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.

Line 1, characters 12-23:
1 | let rec x = [| x |]; 1.;;
                ^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec".
       It refers to "x", but only functions ("fun" or "function")
       may refer to the names bound by a "let rec".
|}];;

let rec x = let u = [|y|] in 10. and y = 1.;;
[%%expect{|
Line 1, characters 12-32:
1 | let rec x = let u = [|y|] in 10. and y = 1.;;
                ^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec".
       It refers to "y", but only functions ("fun" or "function")
       may refer to the names bound by a "let rec".
|}];;

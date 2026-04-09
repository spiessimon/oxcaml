(* TEST
 expect;
*)

(* #13185 *)

module type S1 = sig end
module type S2 = functor (X : S1) -> sig module M = X end
[%%expect{|
module type S1 = sig end
module type S2 = functor (X : S1) -> sig module M = X end
|}]

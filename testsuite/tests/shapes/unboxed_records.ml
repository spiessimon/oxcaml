(* TEST
 flags = "-dshape";
 expect;
*)

type t = #{ a : int; b : string }
[%%expect{|
{
 "t"[type] ->
   Record_unboxed_product { a: Predef int ()
   ; b: Predef string ()
    };
 }
type t = #{ a : int; b : string; }
|}]

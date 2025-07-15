(* TEST
 flags = "-dshape";
 expect;
*)

type t = #{ a : int; b : string }
[%%expect{|
{
 "t"[type] ->
   <.3> = Tds_record_unboxed_product { a: Ts_predef int (); b: Ts_predef string () };
 }
type t = #{ a : int; b : string; }
|}]

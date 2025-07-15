(* TEST
 flags = "-dshape";
 expect;
*)

(* We depart slightly from the example in the PLDI'19 paper, which actually
   doesn't type... *)

module type Stringable = sig
  type t
  val to_string : t -> string
end
[%%expect{|
{
 "Stringable"[module type] -> <.2>;
 }
module type Stringable = sig type t val to_string : t -> string end
|}]

module Pair (X : Stringable) (Y : Stringable) = struct
  type t = X.t * Y.t
  let to_string (x, y) =
    X.to_string x ^ " " ^ Y.to_string y
end
[%%expect{|
{
 "Pair"[module] ->
   Abs<.9>
      (X, Y,
       {
        "t"[type] ->
          <.10> = Tds_alias Ts_tuple (Ts_constr shape=X<.3> . "t"[type]
           (), Ts_constr shape=Y<.4> . "t"[type]
           ());
        "to_string"[value] -> <.6>;
        });
 }
module Pair :
  functor (X : Stringable) (Y : Stringable) ->
    sig type t = X.t * Y.t val to_string : X.t * Y.t -> string end
|}]

module Int = struct
  type t = int
  let to_string i = string_of_int i
end
[%%expect{|
{
 "Int"[module] ->
   {<.14>
    "t"[type] -> <.15> = Tds_alias Ts_predef int ();
    "to_string"[value] -> <.12>;
    };
 }
module Int : sig type t = int val to_string : int -> string end
|}]

module String = struct
  type t = string
  let to_string s = s
end
[%%expect{|
{
 "String"[module] ->
   {<.19>
    "t"[type] -> <.20> = Tds_alias Ts_predef string ();
    "to_string"[value] -> <.17>;
    };
 }
module String : sig type t = string val to_string : 'a -> 'a end
|}]

module P = Pair(Int)(Pair(String)(Int))
[%%expect{|
{
 "P"[module] ->
   {<.21>
    "t"[type] ->
      <.22> = Tds_alias Ts_tuple (Ts_constr shape=<.15> = Tds_alias Ts_predef int ()
       (), Ts_constr shape=<.23> = Tds_alias Ts_tuple (Ts_constr shape=
                           <.20> = Tds_alias Ts_predef string ()
                            (), Ts_constr shape=<.15> = Tds_alias Ts_predef int ()
                            ())
       ());
    "to_string"[value] -> <.6>;
    };
 }
module P :
  sig
    type t = Int.t * Pair(String)(Int).t
    val to_string : Int.t * Pair(String)(Int).t -> string
  end
|}];;

P.to_string (0, ("!=", 1))
[%%expect{|
{}
- : string = "0 != 1"
|}]

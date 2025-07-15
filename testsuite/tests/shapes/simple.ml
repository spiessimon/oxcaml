(* TEST
 flags = "-dshape";
 expect;
*)

let x = ()
[%%expect{|
{
 "x"[value] -> <.0>;
 }
val x : unit = ()
|}]

external y : int -> int = "%identity"
[%%expect{|
{
 "y"[value] -> <.2>;
 }
external y : int -> int = "%identity"
|}]

type t = A of foo
and foo = Bar
[%%expect{|
{
 "foo"[type] -> Variant Bar;
 "t"[type] -> Variant A of <> ;
 }
type t = A of foo
and foo = Bar
|}]

module type S = sig
  type t
end
[%%expect{|
{
 "S"[module type] -> <.9>;
 }
module type S = sig type t end
|}]

exception E
[%%expect{|
{
 "E"[extension constructor] -> {<.10>};
 }
exception E
|}]

type ext = ..
[%%expect{|
{
 "ext"[type] -> <>;
 }
type ext = ..
|}]

type ext += A | B
[%%expect{|
{
 "A"[extension constructor] -> {<.12>};
 "B"[extension constructor] -> {<.13>};
 }
type ext += A | B
|}]

module M = struct
  type ext += C
end
[%%expect{|
{
 "M"[module] -> {<.15>
                 "C"[extension constructor] -> {<.14>};
                 };
 }
module M : sig type ext += C end
|}]

module _ = struct
  type t = Should_not_appear_in_shape
end
[%%expect{|
{}
|}]

module rec M1 : sig
  type t = C of M2.t
end = struct
  type t = C of M2.t
end

and M2 : sig
  type t
  val x : t
end = struct
  type t = T
  let x = T
end
[%%expect{|
{
 "M1"[module] -> {
                  "t"[type] -> Variant C of <> ;
                  };
 "M2"[module] -> {
                  "t"[type] -> Variant T;
                  "x"[value] -> <.36>;
                  };
 }
module rec M1 : sig type t = C of M2.t end
and M2 : sig type t val x : t end
|}]

class c = object end
[%%expect{|
{
 "c"[type] -> <.38>;
 "c"[class] -> <.38>;
 "c"[class type] -> <.38>;
 }
class c : object  end
|}]

class type c = object end
[%%expect{|
{
 "c"[type] -> <.41>;
 "c"[class type] -> <.41>;
 }
class type c = object  end
|}]

type u = t
[%%expect{|
{
 "u"[type] -> <>;
 }
type u = t
|}]

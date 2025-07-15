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
 "foo"[type] ->
   <.4>Tds_variant simple_constructors=Bar complex_constructors=;
 "t"[type] ->
   <.3>Tds_variant simple_constructors= complex_constructors=(A of Ts_shape (
   Tds_variant simple_constructors=Bar complex_constructors=
   ));
 }
type t = A of foo
and foo = Bar
|}]

module type S = sig
  type t
end
[%%expect{|
{
 "S"[module type] -> <.13>;
 }
module type S = sig type t end
|}]

exception E
[%%expect{|
{
 "E"[extension constructor] -> {<.14>};
 }
exception E
|}]

type ext = ..
[%%expect{|
{
 "ext"[type] -> <.15>Tds_other;
 }
type ext = ..
|}]

type ext += A | B
[%%expect{|
{
 "A"[extension constructor] -> {<.17>};
 "B"[extension constructor] -> {<.18>};
 }
type ext += A | B
|}]

module M = struct
  type ext += C
end
[%%expect{|
{
 "M"[module] -> {<.20>
                 "C"[extension constructor] -> {<.19>};
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
 "M1"[module] ->
   {
    "t"[type] ->
      <.41>Tds_variant simple_constructors= complex_constructors=(C of Ts_shape (
      M2<.26> . "t"[type]
      ));
    };
 "M2"[module] ->
   {
    "t"[type] ->
      <.45>Tds_variant simple_constructors=T complex_constructors=;
    "x"[value] -> <.48>;
    };
 }
module rec M1 : sig type t = C of M2.t end
and M2 : sig type t val x : t end
|}]

class c = object end
[%%expect{|
{
 "c"[type] -> <.50>;
 "c"[class] -> <.50>;
 "c"[class type] -> <.50>;
 }
class c : object  end
|}]

class type c = object end
[%%expect{|
{
 "c"[type] -> <.53>;
 "c"[class type] -> <.53>;
 }
class type c = object  end
|}]

type u = t
[%%expect{|
{
 "u"[type] ->
   <.54>Tds_alias Ts_shape (<.3>Tds_variant simple_constructors= complex_constructors=(A of Ts_shape (
                            Tds_variant simple_constructors=Bar complex_constructors=
                            ))
   );
 }
type u = t
|}]

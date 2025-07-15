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
 "y"[value] -> <.1>;
 }
external y : int -> int = "%identity"
|}]

type t = A of foo
and foo = Bar
[%%expect{|
{
 "foo"[type] ->
   <.6> = Tds_variant simple_constructors=Bar complex_constructors=;
 "t"[type] ->
   <.7> = Tds_variant simple_constructors= complex_constructors=(A of Ts_constr shape=
   <.3>
    ());
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
 "ext"[type] -> <.12> = Tds_other;
 }
type ext = ..
|}]

type ext += A | B
[%%expect{|
{
 "A"[extension constructor] -> {<.13>};
 "B"[extension constructor] -> {<.14>};
 }
type ext += A | B
|}]

module M = struct
  type ext += C
end
[%%expect{|
{
 "M"[module] -> {<.16>
                 "C"[extension constructor] -> {<.15>};
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
      <.35> = Tds_variant simple_constructors= complex_constructors=(C of Ts_constr shape=
      M2<.21> . "t"[type]
       ());
    };
 "M2"[module] ->
   {
    "t"[type] ->
      <.36> = Tds_variant simple_constructors=T complex_constructors=;
    "x"[value] -> <.34>;
    };
 }
module rec M1 : sig type t = C of M2.t end
and M2 : sig type t val x : t end
|}]

class c = object end
[%%expect{|
{
 "c"[type] -> <.37>;
 "c"[class] -> <.37>;
 "c"[class type] -> <.37>;
 }
class c : object  end
|}]

class type c = object end
[%%expect{|
{
 "c"[type] -> <.40>;
 "c"[class type] -> <.40>;
 }
class type c = object  end
|}]

type u = t
[%%expect{|
{
 "u"[type] -> <.42> = Tds_alias Ts_constr shape=<.2>  ();
 }
type u = t
|}]

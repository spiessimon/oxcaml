(* TEST
 flags = "-dshape";
 expect;
*)

module type S = sig
  type t
  val x : t
end
[%%expect{|
{
 "S"[module type] -> <.3>;
 }
module type S = sig type t val x : t end
|}]

module Falias (X : S) = X
[%%expect{|
{
 "Falias"[module] -> Abs<.5>(X, X<.4>);
 }
module Falias : functor (X : S) -> sig type t = X.t val x : t end
|}]

module Finclude (X : S) = struct
  include X
end
[%%expect{|
{
 "Finclude"[module] ->
   Abs<.7>
      (X, {
           "t"[type] -> X<.6> . "t"[type];
           "x"[value] -> X<.6> . "x"[value];
           });
 }
module Finclude : functor (X : S) -> sig type t = X.t val x : t end
|}]

module Fredef (X : S) = struct
  type t = X.t
  let x = X.x
end
[%%expect{|
{
 "Fredef"[module] ->
   Abs<.13>
      (X,
       {
        "t"[type] -> <.9>Tds_alias Ts_shape (X<.8> . "t"[type] );
        "x"[value] -> <.12>;
        });
 }
module Fredef : functor (X : S) -> sig type t = X.t val x : X.t end
|}]

module Fignore (_ : S) = struct
  type t = Fresh
  let x = Fresh
end
[%%expect{|
{
 "Fignore"[module] ->
   Abs<.19>
      ((),
       {
        "t"[type] ->
          <.15>Tds_variant simple_constructors=Fresh complex_constructors=;
        "x"[value] -> <.18>;
        });
 }
module Fignore : S -> sig type t = Fresh val x : t end
|}]

module Arg : S = struct
  type t = T
  let x = T
end
[%%expect{|
{
 "Arg"[module] ->
   {<.25>
    "t"[type] ->
      <.21>Tds_variant simple_constructors=T complex_constructors=;
    "x"[value] -> <.24>;
    };
 }
module Arg : S
|}]

include Falias(Arg)
[%%expect{|
{
 "t"[type] -> <.21>Tds_variant simple_constructors=T complex_constructors=;
 "x"[value] -> <.24>;
 }
type t = Arg.t
val x : t = <abstr>
|}]

include Finclude(Arg)
[%%expect{|
{
 "t"[type] -> <.21>Tds_variant simple_constructors=T complex_constructors=;
 "x"[value] -> <.24>;
 }
type t = Arg.t
val x : t = <abstr>
|}]

include Fredef(Arg)
[%%expect{|
{
 "t"[type] ->
   <.9>Tds_alias Ts_shape (<.21>Tds_variant simple_constructors=T complex_constructors=
   );
 "x"[value] -> <.12>;
 }
type t = Arg.t
val x : Arg.t = <abstr>
|}]

include Fignore(Arg)
[%%expect{|
{
 "t"[type] ->
   <.15>Tds_variant simple_constructors=Fresh complex_constructors=;
 "x"[value] -> <.18>;
 }
type t = Fignore(Arg).t = Fresh
val x : t = Fresh
|}]

include Falias(struct type t = int let x = 0 end)
[%%expect{|
{
 "t"[type] -> <.27>Tds_alias Ts_predef int ();
 "x"[value] -> <.30>;
 }
type t = int
val x : t = 0
|}]

include Finclude(struct type t = int let x = 0 end)
[%%expect{|
{
 "t"[type] -> <.32>Tds_alias Ts_predef int ();
 "x"[value] -> <.35>;
 }
type t = int
val x : t = 0
|}]

include Fredef(struct type t = int let x = 0 end)
[%%expect{|
{
 "t"[type] -> <.9>Tds_alias Ts_shape (<.37>Tds_alias Ts_predef int () );
 "x"[value] -> <.12>;
 }
type t = int
val x : int = 0
|}]

include Fignore(struct type t = int let x = 0 end)
[%%expect{|
{
 "t"[type] ->
   <.15>Tds_variant simple_constructors=Fresh complex_constructors=;
 "x"[value] -> <.18>;
 }
type t = Fresh
val x : t = Fresh
|}]

module Fgen () = struct
  type t = Fresher
  let x = Fresher
end
[%%expect{|
{
 "Fgen"[module] ->
   Abs<.51>
      ((),
       {
        "t"[type] ->
          <.47>Tds_variant simple_constructors=Fresher complex_constructors=;
        "x"[value] -> <.50>;
        });
 }
module Fgen : functor () -> sig type t = Fresher val x : t end
|}]

include Fgen ()
[%%expect{|
{
 "t"[type] ->
   <.47>Tds_variant simple_constructors=Fresher complex_constructors=;
 "x"[value] -> <.50>;
 }
type t = Fresher
val x : t = Fresher
|}]

(***************************************************************************)
(* Make sure we restrict shapes even when constraints imply [Tcoerce_none] *)
(***************************************************************************)

module type Small = sig
  type t
end
[%%expect{|
{
 "Small"[module type] -> <.55>;
 }
module type Small = sig type t end
|}]

module type Big = sig
  type t
  type u
end
[%%expect{|
{
 "Big"[module type] -> <.60>;
 }
module type Big = sig type t type u end
|}]

module type B2S = functor (X : Big) -> Small with type t = X.t
[%%expect{|
{
 "B2S"[module type] -> <.63>;
 }
module type B2S = functor (X : Big) -> sig type t = X.t end
|}]

module Big_to_small1 : B2S = functor (X : Big) -> X
[%%expect{|
{
 "Big_to_small1"[module] ->
   Abs<.65>(X, {<.64>
                "t"[type] -> X<.64> . "t"[type];
                });
 }
module Big_to_small1 : B2S
|}]

module Big_to_small2 : B2S = functor (X : Big) -> struct include X end
[%%expect{|
{
 "Big_to_small2"[module] -> Abs<.67>(X, {
                                         "t"[type] -> X<.66> . "t"[type];
                                         });
 }
module Big_to_small2 : B2S
|}]

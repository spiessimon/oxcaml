(* TEST
 flags = "-dshape";
 expect;
*)
module M = struct end (* uid 0 *)
module F(X : sig end) = M
module App = F(List)
[%%expect{|
{
 "M"[module] -> {<.0>};
 }
module M : sig end
{
 "F"[module] -> Abs<.2>(X, {<.0>});
 }
module F : functor (X : sig end) -> sig end
{
 "App"[module] -> {<.3>};
 }
module App : sig end
|}]


module M = struct end (* uid 4 *)
module F(X : sig end) = struct include M type t end
module App = F(List)
[%%expect{|
{
 "M"[module] -> {<.4>};
 }
module M : sig end
{
 "F"[module] -> Abs<.8>(X, {
                            "t"[type] -> <.6>Tds_other;
                            });
 }
module F : functor (X : sig end) -> sig type t end
{
 "App"[module] -> {<.9>
                   "t"[type] -> <.6>Tds_other;
                   };
 }
module App : sig type t = F(List).t end
|}]

module M = struct end (* uid 9 *)
module F(X : sig end) = X
module App = F(M)
[%%expect{|
{
 "M"[module] -> {<.10>};
 }
module M : sig end
{
 "F"[module] -> Abs<.12>(X, X<.11>);
 }
module F : functor (X : sig end) -> sig end
{
 "App"[module] -> {<.13>};
 }
module App : sig end
|}]

module Id(X : sig end) = X
module Struct = struct
  module L = List
end
[%%expect{|
{
 "Id"[module] -> Abs<.15>(X, X<.14>);
 }
module Id : functor (X : sig end) -> sig end
{
 "Struct"[module] ->
   {<.17>
    "L"[module] -> Alias(<.16>
                         CU Stdlib . "List"[module]);
    };
 }
module Struct : sig module L = List end
|}]

module App = Id(List) (* this should have the App uid *)
module Proj = Struct.L
  (* this should have the Proj uid and be an alias to Struct.L *)
[%%expect{|
{
 "App"[module] -> (CU Stdlib . "List"[module])<.18>;
 }
module App : sig end
{
 "Proj"[module] -> Alias(<.19>
                         Alias(<.16>
                               CU Stdlib . "List"[module]));
 }
module Proj = Struct.L
|}]

module F (X :sig end ) = struct module M = X end
module N = F(struct end)
module O = N.M
[%%expect{|
{
 "F"[module] -> Abs<.22>(X, {
                             "M"[module] -> X<.20>;
                             });
 }
module F : functor (X : sig end) -> sig module M : sig end end
{
 "N"[module] -> {<.23>
                 "M"[module] -> {<.20>};
                 };
 }
module N : sig module M : sig end end
{
 "O"[module] -> Alias(<.24>
                      {<.20>});
 }
module O = N.M
|}]

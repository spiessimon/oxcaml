(* TEST
 flags = "-dshape";
 expect;
*)
module A = struct type t end
module B = A
[%%expect{|
{
 "A"[module] -> {<.2>
                 "t"[type] -> <.0>Tds_other;
                 };
 }
module A : sig type t end
{
 "B"[module] -> Alias(<.3>
                      {<.2>
                       "t"[type] -> <.0>Tds_other;
                       });
 }
module B = A
|}]

type u = B.t

[%%expect{|
{
 "u"[type] -> <.4>Tds_alias Ts_shape (<.0>Tds_other );
 }
type u = B.t
|}]

module F (X : sig type t end) = X
module F' = F
[%%expect{|
{
 "F"[module] -> Abs<.10>(X, X<.9>);
 }
module F : functor (X : sig type t end) -> sig type t = X.t end
{
 "F'"[module] -> Alias(<.11>
                       Abs<.10>(X, X<.9>));
 }
module F' = F
|}]

module C = F'(A)
[%%expect{|
{
 "C"[module] -> {<.12>
                 "t"[type] -> <.0>Tds_other;
                 };
 }
module C : sig type t = A.t end
|}]


module C = F(B)

[%%expect{|
{
 "C"[module] -> Alias(<.13>
                      {<.2>
                       "t"[type] -> <.0>Tds_other;
                       });
 }
module C : sig type t = B.t end
|}]

module D = C

[%%expect{|
{
 "D"[module] -> Alias(<.14>
                      Alias(<.13>
                            {<.2>
                             "t"[type] -> <.0>Tds_other;
                             }));
 }
module D = C
|}]

module G (X : sig type t end) = struct include X end
[%%expect{|
{
 "G"[module] -> Abs<.18>(X, {
                             "t"[type] -> X<.17> . "t"[type];
                             });
 }
module G : functor (X : sig type t end) -> sig type t = X.t end
|}]

module E = G(B)
[%%expect{|
{
 "E"[module] -> {<.19>
                 "t"[type] -> <.0>Tds_other;
                 };
 }
module E : sig type t = B.t end
|}]

module M = struct type t let x = 1 end
module N : sig type t end = M
module O = N
[%%expect{|
{
 "M"[module] -> {<.23>
                 "t"[type] -> <.20>Tds_other;
                 "x"[value] -> <.22>;
                 };
 }
module M : sig type t val x : int end
{
 "N"[module] -> {<.27>
                 "t"[type] -> <.20>Tds_other;
                 };
 }
module N : sig type t end
{
 "O"[module] -> Alias(<.28>
                      {<.27>
                       "t"[type] -> <.20>Tds_other;
                       });
 }
module O = N
|}]

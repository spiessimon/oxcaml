(* TEST
    flags = "-extension layouts_alpha";
    expect;
*)

module F (M : sig type t end) = struct
  type t : immutable_data with M.t
end

module Int = struct
  type t = int
end
type t : immutable_data = F(Int).t
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module T = struct
  type t
end
type t : immutable_data = F(T).t
[%%expect {|
module T : sig type t end
Line 4, characters 26-30:
4 | type t : immutable_data = F(T).t
                              ^^^^
Error: Unbound module "F"
|}]

module F (M : sig type 'a t end) = struct
  type 'a t : immutable_data with 'a M.t
end

module Ref = struct
  type 'a t = 'a ref
end
type 'a t : mutable_data with 'a = 'a F(Ref).t
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module Ref = struct
  type 'a t = 'a ref
end
type 'a t : immutable_data with 'a = 'a F(Ref).t
[%%expect {|
module Ref : sig type 'a t = 'a ref end
Line 4, characters 40-46:
4 | type 'a t : immutable_data with 'a = 'a F(Ref).t
                                            ^^^^^^
Error: Unbound module "F"
|}]

module Ref = struct
  type 'a t = 'a ref
end
type 'a t : mutable_data = 'a F(Ref).t
[%%expect {|
module Ref : sig type 'a t = 'a ref end
Line 4, characters 30-36:
4 | type 'a t : mutable_data = 'a F(Ref).t
                                  ^^^^^^
Error: Unbound module "F"
|}]

module F (M : sig
  type t
  type u
end) = struct
  type t : immediate with M.u with M.t
end

module Int_int = struct
  type t = int
  type u = int
end
type t : immutable_data = F(Int_int).t
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module Int_abstract = struct
  type t = int
  type u : value mod global
end
type t : value mod global = F(Int_abstract).t
type t : value mod portable with Int_abstract.u = F(Int_abstract).t
[%%expect {|
module Int_abstract : sig type t = int type u : value mod global end
Line 5, characters 28-43:
5 | type t : value mod global = F(Int_abstract).t
                                ^^^^^^^^^^^^^^^
Error: Unbound module "F"
|}]

type t : value mod portable = F(Int_abstract).t
[%%expect {|
Line 1, characters 30-45:
1 | type t : value mod portable = F(Int_abstract).t
                                  ^^^^^^^^^^^^^^^
Error: Unbound module "F"
|}]

(* TEST
    flags = "-extension layouts_alpha";
    expect;
*)

(* Test that not-best kinds are respected *)

module M : sig
  type 'a t : immutable_data with 'a
end = struct
  type 'a t
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

(* This appears sound to accept. But it isn't. See the following test. *)
module M : sig
  type a
  type t : value mod portable with a
end = struct
  type a
  type t
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

(* This test demonstrates why the above shouldn't be accepted. We can learn more about
   a via gadt refinement *)
type ('a, 'b) eq = Eq : ('a, 'a) eq

module M : sig
  type a
  type t : value mod portable with a
  val a_is_int : (a, int) eq
end = struct
  type a = int
  type t : value mod portable with a
  let a_is_int : (a, int) eq = Eq
end

let f (x : M.t @ nonportable) : M.t @ portable =
  match M.a_is_int with
  | Eq -> x
[%%expect {|
type ('a, 'b) eq = Eq : ('a, 'a) eq
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

type a
type b = a
type u
type t : value mod global with a = u
[%%expect {|
type a
type b = a
type u
Uncaught exception: Typedecl.Error(_, _)

|}]

module F (M : sig type t end) = struct
  module type S = sig
    type t : value mod global with M.t
  end
  type t
  module type T = S with type t = t
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

module type S = sig
  type t
end

type a
module M : S with type t = a = struct
  type t = a
end

module _ : sig
  type t : value mod portable contended with M.t
end = struct
  type t : value mod portable
end
[%%expect {|
module type S = sig type t end
type a
module M : sig type t = a end
Uncaught exception: Typemod.Error(_, _, _)

|}]

module type S = sig
  type t
  type u = t
end

type a
module M : S with type t := a = struct
  type t = a
  type u = a
end

module M : sig
  type t : value mod portable contended with M.u
end = struct
  type t : value mod portable
end
[%%expect {|
module type S = sig type t type u = t end
type a
module M : sig type u = a end
Uncaught exception: Typemod.Error(_, _, _)

|}]

module M : sig
  type a = [`a of string | `b]
  type t : value mod global with a
end = struct
  type a = [`a of string | `b]
  type t
end
(* CR layouts v2.8: this is fine to accept. Internal ticket 4294. *)
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type a = [`a of string | `b]
6 |   type t
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a = [ `a of string | `b ] type t end
       is not included in
         sig
           type a = [ `a of string | `b ]
           type t : value mod forkable unyielding
         end
       Type declarations do not match:
         type t
       is not included in
         type t : value mod forkable unyielding
       The kind of the first is value
         because of the definition of t at line 6, characters 2-8.
       But the kind of the first must be a subkind of
           value mod forkable unyielding
         because of the definition of t at line 3, characters 2-34.
|}]

module M : sig
  type 'a u = [< `a of string | `b] as 'a
  type 'a t : value mod global with 'a u
end = struct
  type 'a u = [< `a of string | `b] as 'a
  type 'a t constraint 'a = [< `a of string | `b]
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

module M : sig
  type 'a u = [< `a of (int -> int) | `b] as 'a
  type 'a t : value mod portable with 'a u
end = struct
  type 'a u = [< `a of (int -> int) | `b] as 'a
  type 'a t constraint 'a = [< `a of (int -> int) | `b]
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

module M : sig
  type 'a u = [> `a of string | `b] as 'a
  type 'a t : value mod portable with 'a u
end = struct
  type 'a u = [> `a of string | `b] as 'a
  type 'a t constraint 'a = [> `a of string | `b]
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

module M : sig
  type a = < value : string >
  type t : value mod global with a
end = struct
  type a = < value : string >
  type t
end
(* CR layouts v2.8: this is fine to accept. Internal ticket 5125. *)
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

type gadt = Foo : int -> gadt
module M : sig
  type t : value mod portable with gadt
end = struct
  type t : value mod portable
end
[%%expect {|
type gadt = Foo : int -> gadt
module M : sig type t : value mod portable end
|}]

type gadt = Foo : int -> gadt
module M : sig
  type t : value mod global with gadt
end = struct
  type t
end
[%%expect {|
type gadt = Foo : int -> gadt
Lines 4-6, characters 6-3:
4 | ......struct
5 |   type t
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t end
       is not included in
         sig type t : value mod forkable unyielding end
       Type declarations do not match:
         type t
       is not included in
         type t : value mod forkable unyielding
       The kind of the first is value
         because of the definition of t at line 5, characters 2-8.
       But the kind of the first must be a subkind of
           value mod forkable unyielding
         because of the definition of t at line 3, characters 2-37.
|}]

type gadt = Foo : int -> gadt [@@unboxed]
module M : sig
  type t : value mod portable with gadt
end = struct
  type t
end
[%%expect {|
type gadt = Foo : int -> gadt [@@unboxed]
Lines 4-6, characters 6-3:
4 | ......struct
5 |   type t
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t end
       is not included in
         sig type t : value mod portable end
       Type declarations do not match:
         type t
       is not included in
         type t : value mod portable
       The kind of the first is value
         because of the definition of t at line 5, characters 2-8.
       But the kind of the first must be a subkind of value mod portable
         because of the definition of t at line 3, characters 2-39.
|}]

module M : sig
  type a = int ref * int
  type t : value mod contended with a
end = struct
  type a = int ref * int
  type t
end
[%%expect {|
module M : sig type a = int ref * int type t end
|}]

module M : sig
  type a = #(int ref * int)
  type t : value mod contended with a
end = struct
  type a = #(int ref * int)
  type t
end
[%%expect {|
module M : sig type a = #(int ref * int) type t end
|}]

module M : sig
  type a = int -> int
  type t : value mod portable with a
end = struct
  type a = int -> int
  type t
end
[%%expect {|
module M : sig type a = int -> int type t end
|}]

module M : sig
  type a = { foo : 'a. 'a ref } [@@unboxed]
  type t : value mod contended with a
end = struct
  type a = { foo : 'a. 'a ref } [@@unboxed]
  type t
end
[%%expect {|
module M : sig type a = { foo : 'a. 'a ref; } [@@unboxed] type t end
|}]

module M : sig
  type a = { foo : ('a : value). 'a }
  type t : value mod contended with a
end = struct
  type a = { foo : ('a : value). 'a }
  type t
end
(* CR layouts v2.8: If we ever give univars min mod-bounds, this should get
   rejected. Internal ticket 5746. *)
[%%expect {|
module M : sig type a = { foo : 'a. 'a; } type t end
|}]

module M : sig
  type a = { foo : ('a : value). 'a } [@@unboxed]
  type t : value mod contended with a
end = struct
  type a = { foo : ('a : value). 'a } [@@unboxed]
  type t
end
(* CR layouts v2.8: If we ever give univars min mod-bounds, this should get
   rejected. Internal ticket 5746. *)
[%%expect {|
module M : sig type a = { foo : 'a. 'a; } [@@unboxed] type t end
|}]

module type S = sig
  val nonportable_f : int -> int
end
type s = (module S)
module M : sig
  type t : value mod portable with s
end = struct
  type t
end
(* CR layouts v2.8: this should be accepted because module types should be best
   (once we start giving them proper kinds). Internal ticket 5126 *)
[%%expect {|
module type S = sig val nonportable_f : int -> int end
type s = (module S)
Uncaught exception: Typemod.Error(_, _, _)

|}]

type a : value = private int
module M : sig
  type t : value mod portable with a
end = struct
  type t
end
[%%expect {|
type a = private int
Lines 4-6, characters 6-3:
4 | ......struct
5 |   type t
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t end
       is not included in
         sig type t : value mod portable end
       Type declarations do not match:
         type t
       is not included in
         type t : value mod portable
       The kind of the first is value
         because of the definition of t at line 5, characters 2-8.
       But the kind of the first must be a subkind of value mod portable
         because of the definition of t at line 3, characters 2-36.
|}]

type a : value mod many = private string
module M : sig
  type t : value mod many portable with a
end = struct
  type t : value mod many
end
[%%expect {|
type a = private string
Lines 4-6, characters 6-3:
4 | ......struct
5 |   type t : value mod many
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : value mod many end
       is not included in
         sig type t : value mod many portable end
       Type declarations do not match:
         type t : value mod many
       is not included in
         type t : value mod many portable
       The kind of the first is value mod many
         because of the definition of t at line 5, characters 2-25.
       But the kind of the first must be a subkind of value mod many portable
         because of the definition of t at line 3, characters 2-41.
|}]

type a = { foo : int -> int }
module M : sig
  type t : value mod portable with a
end = struct
  type t
end
[%%expect {|
type a = { foo : int -> int; }
module M : sig type t end
|}]

type a = Foo of (int -> int)
module M : sig
  type t : value mod portable with a
end = struct
  type t
end
[%%expect {|
type a = Foo of (int -> int)
module M : sig type t end
|}]

type a = ..
module M : sig
  type t : value mod portable with a
end = struct
  type t
end
[%%expect {|
type a = ..
module M : sig type t end
|}]

(* TEST
    flags = "-extension layouts_alpha";
    expect;
*)

(* Test printing errors *)

type t : immediate = A of int
[%%expect {|
Line 1, characters 0-29:
1 | type t : immediate = A of int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immediate
         because of the annotation on the declaration of the type t.
|}]

type 'a t : immutable_data = A of 'a
[%%expect {|
Uncaught exception: Typedecl.Error(_, _)

|}]

type ('a, 'b) t : immutable_data with 'a = { a : 'a; b : 'b }
[%%expect {|
Uncaught exception: Typedecl.Error(_, _)

|}]

type 'a t : immutable_data = Foo of 'a @@ portable
[%%expect {|
Uncaught exception: Typedecl.Error(_, _)

|}]

module M : sig
  type t : immutable_data
end = struct
  type 'a t = Foo of 'a
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t = Foo of 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = Foo of 'a end
       is not included in
         sig type t : immutable_data end
       Type declarations do not match:
         type 'a t = Foo of 'a
       is not included in
         type t : immutable_data
       They have different arities.
|}]

module M : sig
  type 'a t : immutable_data
end = struct
  type 'a t = Foo of 'a @@ contended many
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

module M : sig
  type a
  type t : immutable_data with a @@ portable
end = struct
    type a
    type t = Foo of a | Bar of a @@ contended
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

type a
type ('a : immutable_data) t
let f () : a t = failwith ""
[%%expect {|
type a
type ('a : immutable_data) t
Line 3, characters 11-12:
3 | let f () : a t = failwith ""
               ^
Error: This type "a" should be an instance of type "'a"
       The kind of "a" is value
         because of the definition of a at line 1, characters 0-6.
       But the kind of "a" must be a subkind of immutable_data
         because of the definition of t at line 2, characters 0-28.
|}]

type a = int ref
type ('a : immutable_data) t
let f () : a t = failwith ""
[%%expect {|
type a = int ref
type ('a : immutable_data) t
Line 3, characters 11-12:
3 | let f () : a t = failwith ""
               ^
Error: This type "a" = "int ref" should be an instance of type "'a"
       The kind of "a" is mutable_data.
       But the kind of "a" must be a subkind of immutable_data
         because of the definition of t at line 2, characters 0-28.
|}, Principal{|
type a = int ref
type ('a : immutable_data) t
Uncaught exception: Typetexp.Error(_, _, _)

|}]

type 'a u = Foo of 'a @@ portable
type ('a : immutable_data) t
let f () : (int -> int) u t = failwith ""
[%%expect {|
type 'a u = Foo of 'a @@ portable
type ('a : immutable_data) t
Line 3, characters 11-25:
3 | let f () : (int -> int) u t = failwith ""
               ^^^^^^^^^^^^^^
Error: This type "(int -> int) u" should be an instance of type "'a"
       The kind of "(int -> int) u" is value mod portable immutable non_float
         because of the definition of u at line 1, characters 0-33.
       But the kind of "(int -> int) u" must be a subkind of immutable_data
         because of the definition of t at line 2, characters 0-28.
|}, Principal{|
type 'a u = Foo of 'a @@ portable
type ('a : immutable_data) t
Uncaught exception: Typetexp.Error(_, _, _)

|}]

module M : sig
  type t : value mod portable
end = struct
  type t : value mod contended
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : value mod contended
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : value mod contended end
       is not included in
         sig type t : value mod portable end
       Type declarations do not match:
         type t : value mod contended
       is not included in
         type t : value mod portable
       The kind of the first is value mod contended
         because of the definition of t at line 4, characters 2-30.
       But the kind of the first must be a subkind of value mod portable
         because of the definition of t at line 2, characters 2-29.
|}]

module M : sig
  type 'a t : value mod portable with 'a
end = struct
  type 'a t : value mod contended with 'a
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

module M : sig
  type 'a t : value mod portable contended with 'a
end = struct
  type 'a t : value mod contended with 'a
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

module M : sig
  type 'a t : value mod portable with 'a @@ portable
end = struct
  type 'a t : value mod portable with 'a
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

module M : sig
  type 'a t : immutable_data with 'a ref
end = struct
  type 'a t : mutable_data with 'a
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

module M : sig
  type 'a t : value mod portable with 'a @@ portable
end = struct
  type 'a t : value mod portable with 'a @@ portable with 'a
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

type 'a t_mutable : mutable_data with 'a
module M : sig
  type 'a t : immutable_data with 'a list with 'a option
end = struct
  type 'a t : immutable_data with 'a t_mutable
end
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type 'a t : immutable_data with 'a list with 'a option
end = struct
  type 'a t : immutable_data with 'a ref
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

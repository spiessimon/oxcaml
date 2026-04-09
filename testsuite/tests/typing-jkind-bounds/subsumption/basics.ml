(* TEST
    flags = "-extension layouts_alpha";
    expect;
*)

module M : sig
  type t
end = struct
  type t : value with int
end
[%%expect {|
module M : sig type t end
|}]

module M : sig
  type _ t
end = struct
  type 'a t : value with 'a
end
[%%expect {|
module M : sig type _ t end
|}]

module M : sig
  type t : immediate
end = struct
  type t : immediate with int
end
[%%expect {|
module M : sig type t : immediate end
|}]

module M : sig
  type t : immediate
end = struct
  type t : immediate with string
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : immediate with string
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : immutable_data end
       is not included in
         sig type t : immediate end
       Type declarations do not match:
         type t : immutable_data
       is not included in
         type t : immediate
       The kind of the first is immutable_data
         because of the definition of t at line 4, characters 2-32.
       But the kind of the first must be a subkind of immediate
         because of the definition of t at line 2, characters 2-20.
|}]

module M : sig
  type t : float64
end = struct
  type t : float64 with int
end
[%%expect {|
module M : sig type t : float64 end
|}]

type u : immutable_data
type t : immutable_data with int = u
[%%expect {|
type u : immutable_data
type t = u
|}]

type ('a, 'b) t : immutable_data with 'b with 'a

module type S = sig
  type ('a, 'b) t : immutable_data with 'a with 'b
end

module type T = S with type ('a, 'b) t = ('a, 'b) t
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type ('a, 'b) t : immutable_data with 'a with 'b
end = struct
  type ('a, 'b) t : immutable_data with 'b
end
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type ('a, 'b) t : immutable_data with 'a
end = struct
  type ('a, 'b) t : immutable_data with 'b with 'a
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

type ('a, 'b) u : immutable_data with 'b with 'a
type ('a, 'b) t : immutable_data with 'a = ('a, 'b) u
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

type ('a, 'b) t : immutable_data with 'a with 'b

module type S = sig
  type ('a, 'b) t : immutable_data with 'a
end

module type T = S with type ('a, 'b) t = ('a, 'b) t
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type 'a t : mutable_data with 'a
end = struct
  type 'a t : immutable_data with 'a ref
end
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type 'a t : immutable_data with 'a ref
end = struct
  type 'a t : mutable_data with 'a
end
(* This isn't accepted because ['a ref] is always [many] *)
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

module M : sig
  type 'a t : immutable_data with 'a ref
end = struct
  type 'a t : mutable_data with 'a @@ many unyielding forkable
end
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

(* CR layouts v2.8: 'a u's kind should get normalized to just immutable_data.
   Internal ticket 4770. *)
module M = struct
  type ('a : immutable_data) u : immutable_data with 'a
  type 'a t : immutable_data = 'a u
end
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type t : mutable_data
end = struct
  type a : immediate
  type b : immutable_data with a with int with string with a ref
  type c : mutable_data with b with a with b with a
  type d : immediate with a
  type t : immutable_data mod aliased with d with d with b with d with c with a
end
[%%expect {|
module M : sig type t : mutable_data end
|}]

type u
type t : value mod portable with u
type q : value mod portable with t = { x : t }
[%%expect {|
type u
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

type u
type t : value mod portable with u
type v : value mod portable with t
type q : value mod portable with t = { x : v }
[%%expect {|
type u
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

type u
type t = private u
type v : immutable_data with u = { value : t }
[%%expect {|
type u
type t = private u
Uncaught exception: Typedecl.Error(_, _)

|}]

type t : immediate with t = [`foo | `bar]
[%%expect {|
type t = [ `bar | `foo ]
|}]

type t
type u : immutable_data with t = [`foo of t]
[%%expect {|
type t
type u = [ `foo of t ]
|}]

type (_, _) eq = Eq : ('a, 'a) eq

type t1
type t2

module M : sig
  type a : immutable_data with t2
  type b : immutable_data with t1
  val eq : (a, b) eq
end = struct
  type a = int
  type b = int
  let eq = Eq
end

let _ =
  match M.eq with
  | Eq ->
    (* M.a = M.b *)
    let module _ : sig
      type t : immutable_data with t1
    end = struct
      type t : immutable_data with M.a
    end in
    ()
(* CR layouts v2.8: Ideally this would be accepted *)
[%%expect {|
type (_, _) eq = Eq : ('a, 'a) eq
type t1
type t2
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type 'a t : immutable_data with 'a
end = struct
  type 'a t = 'a
end
(* CR layouts v2.8: This should get accepted. But we should wait until we have kind_of *)
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

module Foo : sig
  type ('a, 'b) u : value mod portable with 'b

  type ('a
       , 'b
       , 'c)
         t :
         value mod portable with 'a with 'b with ('b, 'c) u with ('a * 'b, 'c) u
end = struct
  type ('a, 'b) u : value mod portable with 'b

  type ('a
       , 'b
       , 'c)
         t :
         value mod portable with 'a with 'b with ('b, 'c) u with ('a * 'b, 'c) u
end

[%%expect{|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

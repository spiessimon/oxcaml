(* TEST
    flags = "-extension layouts_alpha";
    expect;
*)

module M : sig
  type ('a, 'b) t : immutable_data with 'a
end = struct
  type ('a, 'b) t : immutable_data with 'a @@ portable
end
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type ('a, 'b) t : immutable_data with 'a @@ portable
end = struct
  type ('a, 'b) t : immutable_data with 'a
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

module M : sig
  type 'a t : immutable_data with 'a
end = struct
  type 'a t : immutable_data with 'a @@ portable
end
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type 'a t : value mod portable
end = struct
  type 'a t : immutable_data with 'a @@ portable
end
[%%expect {|
module M : sig type 'a t : value mod portable end
|}]

module M : sig
  type 'a t : mutable_data with 'a @@ portable
end = struct
  type 'a t : mutable_data with 'a @@ portable
end
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

type 'a u : immutable_data with 'a @@ contended
type 'a t : value mod portable = 'a u
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type 'a t : value mod global
end = struct
  type 'a t : immediate with 'a @@ global
end
[%%expect {|
module M : sig type 'a t : value mod global end
|}]

module M : sig
  type 'a t : value mod contended
end = struct
  type 'a t : immutable_data with 'a @@ contended
end
[%%expect {|
module M : sig type 'a t : value mod contended end
|}]

type 'a u : immutable_data with 'a @@ many
type 'a t : value mod many = 'a u
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type 'a t : value mod aliased
end = struct
  type 'a t : immediate with 'a @@ aliased
end
[%%expect {|
module M : sig type 'a t : value mod aliased end
|}]

module M : sig
  type 'a t : value mod global aliased many portable contended
end = struct
  type 'a t : immediate with 'a @@ aliased many contended global portable
end
[%%expect {|
module M : sig type 'a t : value mod global many portable contended end
|}]

module M : sig
  type ('a, 'b) t : value mod portable
end = struct
  type ('a, 'b) t : value mod portable with 'a @@ portable with 'b @@ portable
end
[%%expect {|
module M : sig type ('a, 'b) t : value mod portable end
|}]

type ('a, 'b) t : value mod portable with 'a @@ portable with 'b @@ contended

module type S = sig
  type ('a, 'b) t : value mod portable
end

module type T = S with type ('a, 'b) t = ('a, 'b) t
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type ('a, 'b) t : value mod portable with 'b
end = struct
  type ('a, 'b) t : value mod portable with 'a @@ portable with 'b @@ contended
end
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type ('a, 'b) t : value mod portable with 'a @@ portable with 'b @@ contended
end = struct
  type ('a, 'b) t : value mod portable with 'b
end
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type 'a t : immutable_data with 'a @@ portable
end = struct
  type 'a t : immutable_data with 'a @@ portable with 'a
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

module M : sig
  type 'a t : immutable_data with 'a @@ portable
end = struct
  type 'a t : immutable_data with 'a with 'a @@ portable
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

module M : sig
  type 'a t : immutable_data with 'a
end = struct
  type 'a t : immutable_data with 'a @@ portable with 'a
end
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

type 'a u : value mod contended with 'a @@ global
type 'a t : value mod global = 'a u
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type 'a t : immutable_data with 'a @@ contended portable
end = struct
  type 'a t : immutable_data with 'a @@ contended with 'a @@ portable
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

module M : sig
  type 'a t : immutable_data with 'a @@ contended with 'a @@ portable
end = struct
  type 'a t : immutable_data with 'a @@ contended portable
end
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type 'a t : immutable_data with 'a @@ portable
end = struct
  type 'a t : immutable_data with 'a @@ contended portable with 'a @@ portable many
end
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

type ('a, 'b) u : immutable_data with 'a @@ portable with 'b @@ contended
type ('a, 'b) t : immutable_data with 'a @@ portable with 'b @@ contended = ('a, 'b) u
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module M : sig
  type ('a, 'b) t : immutable_data with 'a @@ portable with 'b @@ contended
end = struct
  type ('a, 'b) t : immutable_data with 'a @@ contended with 'b @@ portable
end
[%%expect {|
Uncaught exception: Typemod.Error(_, _, _)

|}]

module M : sig
  type 'a t : immutable_data with 'a @@ portable
end = struct
  type 'a t : immutable_data with 'a @@ portable contended portable
end
[%%expect {|
Line 4, characters 40-48:
4 |   type 'a t : immutable_data with 'a @@ portable contended portable
                                            ^^^^^^^^
Warning 213: This portability is overridden by portable later.
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

type t : immutable_data with int ref @@ immutable

module type S = sig
  type t : immutable_data
end

module type T = S with type t = t
[%%expect {|
type t : immutable_data
module type S = sig type t : immutable_data end
module type T = sig type t = t/2 end
|}]

(* Test case for bug where type abbreviations incorrectly satisfy modal kinds.
   Before the fix, this was incorrectly accepted even though refs cannot be mod contended. *)
module type X = sig
  type t : value mod contended with t
end

module Xm : X = struct
  type t = int ref
end
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

type q : value mod contended = Xm.t
[%%expect {|
Line 1, characters 31-33:
1 | type q : value mod contended = Xm.t
                                   ^^
Error: Unbound module "Xm"
|}]


module type X = sig
  type t : value mod contended portable with t

  val create : int -> t
  val set : t -> int -> unit
  val get : t -> int
end
[%%expect {|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

module Xm : X = struct
  type t = int ref

  let create n = ref n
  let set r n = r := n
  let get r = !r
end
[%%expect {|
Line 1, characters 12-13:
1 | module Xm : X = struct
                ^
Error: Unbound module type "X"
|}]


let fork (f : (unit -> unit) @ portable) = failwith "not implemented";;
[%%expect {|
val fork : (unit -> unit) @ portable -> 'a = <fun>
|}]

(* Data race previously allowed by the compiler! *)
let r = Xm.create 0;;
fork (fun () -> Xm.set r 1);;
Xm.get r;;
[%%expect {|
Line 1, characters 8-10:
1 | let r = Xm.create 0;;
            ^^
Error: Unbound module "Xm"
|}]


(* Also data race, but this was already a type error: r' is contended *)
let r' = ref 0;;
fork (fun () -> r' := 1);;
!r'
[%%expect {|
val r' : int ref = {contents = 0}
Line 2, characters 16-18:
2 | fork (fun () -> r' := 1);;
                    ^^
Error: This value is "contended"
         because it is used inside the function at line 2, characters 5-24
         which is expected to be "portable".
       However, the highlighted expression is expected to be "uncontended".
|}]

(* TEST
    expect;
*)

let use_global : 'a @ global -> unit = fun _ -> ()
let use_unique : 'a @ unique -> unit = fun _ -> ()
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let use_many : 'a @ many -> unit = fun _ -> ()
[%%expect{|
val use_global : 'a -> unit = <fun>
val use_unique : 'a @ unique -> unit = <fun>
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_many : 'a -> unit = <fun>
|}]

(* Simple cases : closed polymorphic variants with fields *)

type 'a t : immutable_data with 'a = [ `A of 'a ]
[%%expect{|
type 'a t = [ `A of 'a ]
|}]

type 'a u : mutable_data with 'a = [ `B of 'a ref ]
[%%expect{|
type 'a u = [ `B of 'a ref ]
|}]

type 'a v : value mod contended with 'a = [ `C of 'a | `D of 'a -> 'a | `E of 'a option ]
[%%expect{|
type 'a v = [ `C of 'a | `D of 'a -> 'a | `E of 'a option ]
|}]

type 'a w : value mod contended with 'a = [ 'a t | 'a v ]
[%%expect{|
type 'a w = [ `A of 'a | `C of 'a | `D of 'a -> 'a | `E of 'a option ]
|}]

let cross_contention (x : int t @ contended) = use_uncontended x
let cross_portability (x : int t @ nonportable) = use_portable x
let cross_linearity (x : int t @ once) = use_many x
[%%expect{|
val cross_contention : int t @ contended -> unit = <fun>
val cross_portability : int t -> unit = <fun>
val cross_linearity : int t @ once -> unit = <fun>
|}]

let don't_cross_unique (x : int t @ aliased) = use_unique x
[%%expect{|
Line 1, characters 58-59:
1 | let don't_cross_unique (x : int t @ aliased) = use_unique x
                                                              ^
Error: This value is "aliased" but is expected to be "unique".
|}]

let don't_cross_locality (x : int t @ local) = use_global x [@nontail]
[%%expect{|
Line 1, characters 58-59:
1 | let don't_cross_locality (x : int t @ local) = use_global x [@nontail]
                                                              ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]


let cross_contention (x : int w @ contended) = use_uncontended x
[%%expect{|
val cross_contention : int w @ contended -> unit = <fun>
|}]

let don't_cross_portability (x : int w @ nonportable) = use_portable x
[%%expect{|
Line 1, characters 69-70:
1 | let don't_cross_portability (x : int w @ nonportable) = use_portable x
                                                                         ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* Quality *)

module type S = sig
  type 'a polyvar = [ `A of 'a ]
  type 'a abstract : immutable_data with 'a polyvar
end
[%%expect{|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

(* Since the jkind of [ `A of 'a ] has best quality, we can substitute with another type *)
type 'a simple : immutable_data with 'a
module type S2 = S with type 'a abstract = 'a simple
[%%expect{|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

(* Contrast: jkinds of abstract types always have non-best quality. *)
type 'a test : immutable_data with 'a

module type S = sig
  type 'a abstract : immutable_data with 'a test
end

module type S2 = S with type 'a abstract = 'a simple
[%%expect{|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

(* When we give open polymorphic variants more precise kinds, we should make sure to give them not-best quality *)
(* CR reisenberg: This test output should probably mention [polyvar] still *)
module type S = sig
  type 'a polyvar = private [< `A of 'a | `B of int ref | `C ]
  type 'a abstract : immutable_data with 'a polyvar
end
[%%expect{|
Uncaught exception: File "typing/jkind.ml", line 1056, characters 42-48: Assertion failed

|}]

(* CR reisenberg: In the output, [abstract] should have kind [immutable_data] *)
module type S2 = S with type 'a polyvar = [ `C ]
[%%expect{|
Line 1, characters 17-18:
1 | module type S2 = S with type 'a polyvar = [ `C ]
                     ^
Error: Unbound module type "S"
|}]

(* CR layouts v2.8: ['a abstract] should get the kind [immutable_data with 'a polyvar]
   and this should fail: (Internal ticket 4294) *)
module type S3 = S with type 'a record = 'a simple
[%%expect{|
Line 1, characters 17-18:
1 | module type S3 = S with type 'a record = 'a simple
                     ^
Error: Unbound module type "S"
|}]


(* Harder cases: row variables *)

(* CR layouts v2.8: These are both correct, but we could probably infer a more precise kind for both. Internal ticket 4294 *)
type ('a, 'b) t : immutable_data with 'a = [< `X | `Y of 'a] as 'b
[%%expect{|
Uncaught exception: Typedecl.Error(_, _)

|}]
type ('a, 'b) u : immutable_data with 'a = [> `X | `Y of 'a] as 'b
[%%expect{|
Uncaught exception: Typedecl.Error(_, _)

|}]

(* less-than rows *)

let f (x : [< `A of int | `B of string] @ contended) =
  use_uncontended x
(* CR layouts v2.8: This should be accepted. Internal ticket 4294  *)
[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended x
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* CR layouts v2.8: This should also be accepted, but not with a best quality. Internal ticket 4294 *)
module M : sig
  type 'a t : immutable_data with 'a = private [< `A of 'a | `B of ('a * 'a) | `C ]
end = struct
  type 'a t = [ `C ]
end
[%%expect{|
Uncaught exception: Typedecl.Error(_, _)

|}]

(* Tunivar-ified row variables *)

(* With [> `Foo of int] as 'a, this should not be accepted
   -- we can't restrict the row variable to be [value mod portable]. *)

type t1 = { f : ('a : value mod portable). ([> `Foo of int] as 'a) -> unit }
[%%expect{|
Line 1, characters 64-65:
1 | type t1 = { f : ('a : value mod portable). ([> `Foo of int] as 'a) -> unit }
                                                                    ^
Error: This alias is bound to type "[> `Foo of int ]"
       but is used as an instance of type "'a"
       The kind of "[> `Foo of int ]" is value mod non_float
         because it's a polymorphic variant type.
       But the kind of "[> `Foo of int ]" must be a subkind of
           value mod portable
         because of the annotation on the universal variable 'a.
|}]

type t2 = { f : ('a : value mod portable). ([< `Foo of int] as 'a) -> unit }
(* CR layouts v2.8: This should be accepted. Internal ticket 4294 *)
[%%expect{|
Line 1, characters 64-65:
1 | type t2 = { f : ('a : value mod portable). ([< `Foo of int] as 'a) -> unit }
                                                                    ^
Error: This alias is bound to type "[< `Foo of int ]"
       but is used as an instance of type "'a"
       The kind of "[< `Foo of int ]" is value mod non_float
         because it's a polymorphic variant type.
       But the kind of "[< `Foo of int ]" must be a subkind of
           value mod portable
         because of the annotation on the universal variable 'a.
|}]

(* Recursive polymorphic variants. *)
type trec1 : immutable_data = [ `A of string | `B of 'a ] as 'a
[%%expect{|
type trec1 = [ `A of string | `B of 'a ] as 'a
|}]

type trec2 : immutable_data = [ `A | `B of 'a list ] as 'a
[%%expect{|
type trec2 = [ `A | `B of 'a list ] as 'a
|}]

type trec_fails : immutable_data = [ `C | `D of 'a * unit -> 'a ] as 'a

[%%expect{|
Line 1, characters 0-71:
1 | type trec_fails : immutable_data = [ `C | `D of 'a * unit -> 'a ] as 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "[ `C | `D of 'a * unit -> 'a ] as 'a" is
           value mod immutable non_float
         because it's a polymorphic variant type.
       But the kind of type "[ `C | `D of 'a * unit -> 'a ] as 'a" must be a subkind of
         immutable_data
         because of the definition of trec_fails at line 1, characters 0-71.
|}]

type trec_succeeds : value mod immutable = [ `C | `D of 'a * unit -> 'a ] as 'a

[%%expect{|
type trec_succeeds = [ `C | `D of 'a * unit -> 'a ] as 'a
|}]

type trec_rec_fails : immutable_data =
  [ `X of 'b | `Y of [ `Z of ('a -> 'b) | `W of 'a | `Loop of 'b ] as 'b ] as 'a

[%%expect{|
Lines 1-2, characters 0-80:
1 | type trec_rec_fails : immutable_data =
2 |   [ `X of 'b | `Y of [ `Z of ('a -> 'b) | `W of 'a | `Loop of 'b ] as 'b ] as 'a
Error: The kind of type "[ `X of
                            [ `Loop of 'b | `W of 'a | `Z of 'a -> 'b ] as 'b
                        | `Y of 'b ] as 'a" is value mod immutable non_float
         because it's a polymorphic variant type.
       But the kind of type "[ `X of
                                [ `Loop of 'b | `W of 'a | `Z of 'a -> 'b ]
                                as 'b
                            | `Y of 'b ] as 'a" must be a subkind of
           immutable_data
         because of the definition of trec_rec_fails at lines 1-2, characters 0-80.
|}]

type trec_rec_succeeds : value mod immutable =
  [ `X of 'b | `Y of [ `Z of ('a -> 'b) | `W of 'a | `Loop of 'b ] as 'b ] as 'a

[%%expect{|
type trec_rec_succeeds =
    [ `X of [ `Loop of 'b | `W of 'a | `Z of 'a -> 'b ] as 'b | `Y of 'b ]
    as 'a
|}]

(* Future tests for when we start adding row variables to with-bounds. *)

type 'a t1 = [< `A of string | `B of int ] as 'a
type 'a t2 : immediate with 'a t1 = C of string  (* should be rejected, at least until we sort out closed-but-not-static bestness *)
[%%expect{|
type 'a t1 = 'a constraint 'a = [< `A of string | `B of int ]
Uncaught exception: Typedecl.Error(_, _)

|}]
type t3 : immediate with [ `A of string] t1 = C of string  (* should be accepted *)
[%%expect{|
type t3 = C of string
|}]

type 'a t1 = [> `A of string | `B of int ] as 'a
type 'a t2 : immediate with 'a t1 = C of string  (* should be rejected *)
[%%expect{|
type 'a t1 = 'a constraint 'a = [> `A of string | `B of int ]
Uncaught exception: Typedecl.Error(_, _)

|}]
type t3 : immediate with [ `A of string | `B of int | `C ] t1 = C of string  (* should be accepted *)
[%%expect{|
type t3 = C of string
|}]

module type S = sig
  type t = private [< `A of string | `B of int ]
end
module M1 : S = struct
  type t = [ `A of string ]
end
type t2 : immediate with M1.t = C of string  (* should be rejected, at least until we sort out closed-but-not-static bestness *)
[%%expect{|
module type S = sig type t = private [< `A of string | `B of int ] end
module M1 : S
Uncaught exception: Typedecl.Error(_, _)

|}]

module M2 : S with type t = [ `A of string ] = struct
  type t = [ `A of string ]
end
type t3 : immediate with M2.t = C of string (* should be accepted *)
[%%expect{|
module M2 : sig type t = [ `A of string ] end
type t3 = C of string
|}]

type (_, _) eq = Refl : ('a, 'a) eq

(* I'm not sure whether module substitution over a non-static private row type preserves the Tvariant structure; so I made this harder case, too *)
let sneaky (x : (M1.t, [ `A of string ]) eq) = match x with
  | Refl -> let open struct
    type t4 : immediate with M1.t = C of string  (* not sure what will happen, but we should eventually accept *)
  end in ()
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
Uncaught exception: Typedecl.Error(_, _)

|}]

module type S = sig
  type t = private [> `A of string | `B of int ]
end
module M1 : S = struct
  type t = [ `A of string | `B of int | `C of (int -> int) ref ]
end
type t2 : immediate with M1.t = C of string  (* should be rejected *)
[%%expect{|
module type S = sig type t = private [> `A of string | `B of int ] end
module M1 : S
Uncaught exception: Typedecl.Error(_, _)

|}]

module M2 : S with type t = [ `A of string | `B of int ] = struct
  type t = [ `A of string | `B of int ]
end
type t3 : immediate with M2.t = C of string (* should be accepted *)
[%%expect{|
module M2 : sig type t = [ `A of string | `B of int ] end
type t3 = C of string
|}]

let sneaky (x : (M1.t, [ `A of string | `B of int ]) eq) = match x with
  | Refl -> let open struct
    type t4 : immediate with M1.t = C of string  (* not sure what will happen, but we should eventually accept *)
  end in ()
[%%expect{|
Uncaught exception: Typedecl.Error(_, _)

|}]

type json : immutable_data =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of string
  | `Object of (string * json) list
  | `Array of json list
  ]
[%%expect {|
type json =
    [ `Array of json list
    | `False
    | `Null
    | `Number of string
    | `Object of (string * json) list
    | `String of string
    | `True ]
|}]

type json =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of string
  | `Object of (string * json) list
  | `Array of json list
  ]
let f (x : json @ nonportable) = use_portable x
[%%expect {|
type json =
    [ `Array of json list
    | `False
    | `Null
    | `Number of string
    | `Object of (string * json) list
    | `String of string
    | `True ]
val f : json -> unit = <fun>
|}]

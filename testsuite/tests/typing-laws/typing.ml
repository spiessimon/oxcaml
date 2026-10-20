(* TEST
 flags = "-extension laws";
 expect;
*)

(* A law in a structure is type checked and becomes an item of its
   signature. *)

module M = struct
  let length = List.length
  law? length_nonneg (xs : 'a list) : length xs >= 0
  law? trivial : 1 = 1
end
[%%expect {|
module M :
  sig
    val length : 'a list -> int
    law? length_nonneg (xs : 'a list) : (length xs) >= (0)
    law? trivial : (1) = (1)
  end
|}]

(* A law in a signature sees the preceding items. *)

module type S = sig
  type t
  val create : int -> t
  val to_int : t -> int
  law? roundtrip (n : int) : n >= 0 ===> to_int (create n) = n
end
[%%expect {|
module type S =
  sig
    type t
    val create : int -> t
    val to_int : t -> int
    law? roundtrip (n : int) : n >= (0) ===> (to_int (create n)) = n
  end
|}]

(* A law may have several assumptions. *)

law? two_assumptions (n : int) (m : int) : n >= 0 ===> m >= 0 ===> n + m >= 0
[%%expect {|
law? two_assumptions (n : int) (m : int) : n >= (0) ===> m >= (0) ===>
  (n + m) >= (0)
|}]

(* The conclusion must have type [bool]. *)

law? not_bool (xs : int list) : List.length xs
[%%expect {|
Line 1, characters 32-46:
1 | law? not_bool (xs : int list) : List.length xs
                                    ^^^^^^^^^^^^^^
Error: This expression has type "int" but an expression was expected of type
         "bool"
|}]

(* So must every assumption. *)

law? assumption_not_bool (xs : int list) : List.length xs ===> 1 = 1
[%%expect {|
Line 1, characters 43-57:
1 | law? assumption_not_bool (xs : int list) : List.length xs ===> 1 = 1
                                               ^^^^^^^^^^^^^^
Error: This expression has type "int" but an expression was expected of type
         "bool"
|}]

(* A clause may only refer to the parameters and to the items in scope. *)

law? unbound (xs : int list) : List.length ys = 0
[%%expect {|
Line 1, characters 43-45:
1 | law? unbound (xs : int list) : List.length ys = 0
                                               ^^
Error: Unbound value "ys"
|}]

(* A parameter of a type that is not representable is rejected. *)

type any : any
law? not_representable (x : any) : 1 = 1
[%%expect {|
type any : any
Line 2, characters 28-31:
2 | law? not_representable (x : any) : 1 = 1
                                ^^^
Error: The parameters of a law must be representable.
       The layout of any is any
         because of the definition of any at line 1, characters 0-14.
       But the layout of any must be representable
         because it's the type of a law parameter.
|}]

(* The type of a parameter without annotation is inferred from the
   clauses, as for the parameter of a function. *)

law? foo_bar x y z : x + y = z
[%%expect {|
law? foo_bar (x : int) (y : int) (z : int) : (x + y) = z
|}]

(* A parameter whose type is not constrained by the clauses stays
   polymorphic, as does one that is never used. *)

law? refl x : x = x
law? unused x : 1 = 1
[%%expect {|
law? refl (x : 'a) : x = x
law? unused (x : 'a) : (1) = (1)
|}]

(* An unannotated parameter may be inferred at an unboxed type. *)

module type Unboxed = sig
  val f : float# -> float
  law? unboxed x : f x = f x
end
[%%expect {|
module type Unboxed =
  sig val f : float# -> float law? unboxed (x : float#) : (f x) = (f x) end
|}]

(* The inferred type must be representable, as the type of the argument
   of a function. *)

module type Any = sig
  val g : any -> bool
  law? at_any x : g x
end
[%%expect {|
Line 3, characters 20-21:
3 |   law? at_any x : g x
                        ^
Error: Function arguments and returns must be representable.
       The layout of any is any
         because of the definition of any at line 1, characters 0-14.
       But the layout of any must be representable
         because we must know concretely how to pass a function argument.
|}]

(* Two parameters of a law cannot have the same name. *)

law? dup (x : int) (x : int) : x = x
[%%expect {|
Line 1, characters 20-21:
1 | law? dup (x : int) (x : int) : x = x
                        ^
Error: The law parameter "x" is bound several times.
|}]

(* A law cannot refer to a value that a later item of the structure
   shadows: the signature would have no way to refer to it. *)

module Shadowed = struct
  let f x = x
  law? l (x : int) : f x = x
  let f x = x + 1
end
[%%expect {|
Line 4, characters 6-7:
4 |   let f x = x + 1
          ^
Error: Illegal shadowing of the value "f" used by a law.
Line 3, characters 2-28:
3 |   law? l (x : int) : f x = x
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
  The law "l" refers to the value "f", so "f" must be accessible
  outside of this module.
|}]

(* Nor to a value introduced by [open struct], for the same reason. *)

module Hidden = struct
  open struct let helper x = x end
  law? l (x : int) : helper x = x
end
[%%expect {|
Line 2, characters 2-34:
2 |   open struct let helper x = x end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "helper" introduced by this open is used by a law.
Line 3, characters 2-33:
3 |   law? l (x : int) : helper x = x
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The law "l" refers to the value "helper", so "helper" must be accessible
  outside of this module.
|}]

(* The same holds for a law in a submodule, which the error locates. *)

module Shadowed_in_submodule = struct
  let f x = x
  module Inner = struct
    law? l (x : int) : f x = x
  end
  let f x = x + 1
end
[%%expect {|
Line 6, characters 6-7:
6 |   let f x = x + 1
          ^
Error: Illegal shadowing of the value "f" used by a law.
Line 4, characters 4-30:
4 |     law? l (x : int) : f x = x
        ^^^^^^^^^^^^^^^^^^^^^^^^^^
  The law "l" refers to the value "f", so "f" must be accessible
  outside of this module.
|}]

(* And for a law in a module type. *)

module Shadowed_in_module_type = struct
  let f x = x
  module type S = sig
    law? l (x : int) : f x = x
  end
  let f x = x + 1
end
[%%expect {|
Line 6, characters 6-7:
6 |   let f x = x + 1
          ^
Error: Illegal shadowing of the value "f" used by a law.
Line 4, characters 4-30:
4 |     law? l (x : int) : f x = x
        ^^^^^^^^^^^^^^^^^^^^^^^^^^
  The law "l" refers to the value "f", so "f" must be accessible
  outside of this module.
|}]

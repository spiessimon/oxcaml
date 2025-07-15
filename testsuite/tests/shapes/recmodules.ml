(* TEST
 flags = "-dshape";
 expect;
*)

(**********)
(* Simple *)
(**********)

module rec A : sig
   type t = Leaf of B.t
 end = struct
   type t = Leaf of B.t
 end
 and B
   : sig type t = int end
   = struct type t = int end
[%%expect{|
{
 "A"[module] ->
   {
    "t"[type] ->
      <.11> = Tds_variant simple_constructors= complex_constructors=(Leaf of Ts_constr shape=
      B<.1> . "t"[type]
       ());
    };
 "B"[module] -> {
                 "t"[type] -> <.12> = Tds_alias Ts_predef int ();
                 };
 }
module rec A : sig type t = Leaf of B.t end
and B : sig type t = int end
|}]

(*****************)
(* Intf only ... *)
(*****************)

(* reduce is going to die on this. *)

module rec A : sig
   type t = Leaf of B.t
 end = A

and B : sig
  type t = int
end = B
[%%expect{|
{
 "A"[module] -> A<.13>;
 "B"[module] -> B<.14>;
 }
module rec A : sig type t = Leaf of B.t end
and B : sig type t = int end
|}]

(***************************)
(* Example from the manual *)
(***************************)

 module rec A : sig
   type t = Leaf of string | Node of ASet.t
   val compare: t -> t -> int
 end = struct
   type t = Leaf of string | Node of ASet.t
   let compare t1 t2 =
     match (t1, t2) with
     | (Leaf s1, Leaf s2) -> Stdlib.compare s1 s2
     | (Leaf _, Node _) -> 1
     | (Node _, Leaf _) -> -1
     | (Node n1, Node n2) -> ASet.compare n1 n2
 end

(* we restrict the sig to limit the bloat in the expected output. *)
and ASet : sig
  type t
  type elt = A.t
  val compare : t -> t -> int
end = Set.Make(A)
[%%expect{|
{
 "A"[module] ->
   {
    "compare"[value] -> <.40>;
    "t"[type] ->
      <.47> = Tds_variant simple_constructors= complex_constructors=(Leaf of Ts_predef string ()) | (Node of Ts_constr shape=
      ASet<.22> . "t"[type]
       ());
    };
 "ASet"[module] ->
   {
    "compare"[value] ->
      CU Stdlib . "Set"[module] . "Make"[module](A<.21>) . "compare"[value];
    "elt"[type] ->
      CU Stdlib . "Set"[module] . "Make"[module](A<.21>) . "elt"[type];
    "t"[type] ->
      CU Stdlib . "Set"[module] . "Make"[module](A<.21>) . "t"[type];
    };
 }
module rec A :
  sig
    type t = Leaf of string | Node of ASet.t
    val compare : t -> t -> int
  end
and ASet : sig type t type elt = A.t val compare : t -> t -> int end
|}]

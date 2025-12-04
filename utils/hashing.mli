(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Simon Spies, Jane Street Europe                       *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   Permission is hereby granted, free of charge, to any person          *)
(*   obtaining a copy of this software and associated documentation       *)
(*   files (the "Software"), to deal in the Software without              *)
(*   restriction, including without limitation the rights to use, copy,   *)
(*   modify, merge, publish, distribute, sublicense, and/or sell copies   *)
(*   of the Software, and to permit persons to whom the Software is       *)
(*   furnished to do so, subject to the following conditions:             *)
(*                                                                        *)
(*   The above copyright notice and this permission notice shall be       *)
(*   included in all copies or substantial portions of the Software.      *)
(*                                                                        *)
(*   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,      *)
(*   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF   *)
(*   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                *)
(*   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS  *)
(*   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN   *)
(*   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN    *)
(*   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE     *)
(*   SOFTWARE.                                                            *)
(*                                                                        *)
(**************************************************************************)

(** [mix h v] combines a hash accumulator [h] with a new value [v]. *)
val mix : int -> int -> int

val mix2 : int -> int -> int

val mix3 : int -> int -> int -> int

val mix4 : int -> int -> int -> int -> int

val mix5 : int -> int -> int -> int -> int -> int

val mix6 : int -> int -> int -> int -> int -> int -> int

val mix7 : int -> int -> int -> int -> int -> int -> int -> int

val mix_list : ('a -> int) -> 'a list -> int

val mix_array : ('a -> int) -> 'a array -> int

val mix_map :
  (('k -> 'v -> int -> int) -> 'm -> int -> int) ->
  ('k -> int) ->
  ('v -> int) ->
  'm ->
  int

(** [mix_option hash_elem opt] hashes an optional value. Returns [0] for [None],
    which allows treating a (key, value) map as a function [key -> value option]
    in terms of the hash: an unbound key hashes to zero. *)
val mix_option : ('a -> int) -> 'a option -> int

val mix_string : string -> int

(** [Dedup] is a generative functor: each application [(Dedup(H)())] produces
    a module with a fresh, incompatible [t] type. This prevents accidentally
    mixing values from different deduplication domains that happen to share
    the same underlying representation. *)
module type Dedup = sig
  (** A deduplicated element wrapping a value with its precomputed hash. *)
  type t

  type value

  (** [create tbl v] returns a canonical element for [v]. If an element
      structurally equal to [v] already exists in [tbl], that element is
      returned. Otherwise, a new element is created, added to [tbl], and
      returned. *)
  val create : value -> t

  (** [equal e1 e2] is [true] iff [e1] and [e2] are physically equal.
      This is O(1) due to deduplication. *)
  val equal : t -> t -> bool

  (** [hash e] returns the precomputed hash of [e]. This is O(1). *)
  val hash : t -> int

  (** [value e] returns the underlying value of [e]. *)
  val value : t -> value
end

val deduplicate :
  initial_size:int ->
  (module Hashtbl.HashedType with type t = 'a) ->
  (module Dedup with type value = 'a)

(** A polymorphic map module that also provides a hash function for keys. *)
module type Hashable_map = sig
  type 'a t

  type key

  val empty : 'a t

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val find : key -> 'a t -> 'a

  val hash_key : key -> int
end

(** A functor that wraps a map with an incrementally-computed hash and size.
    The hash is updated in O(1) time on each [add] operation using XOR, which
    is commutative and thus independent of insertion order. The value hash
    function is passed at runtime to [add], allowing the functor to be
    applied before the value type is defined. *)
module Incrementally_hashed_map (Arg : Hashable_map) : sig
  type 'a t

  val empty : 'a t

  val add : ('a -> int) -> Arg.key -> 'a -> 'a t -> 'a t

  val find : Arg.key -> 'a t -> 'a

  val hash : 'a t -> int

  val cardinal : 'a t -> int

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

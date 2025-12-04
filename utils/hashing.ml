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

(** [mix h v] combines a hash accumulator [h] with a new value [v].
    Based on FxHash from the Rust compiler (see
    https://github.com/rust-lang/rustc-hash). The constant is truncated
    to 62 bits to fit in OCaml's int type. *)
let[@inline] mix acc value =
  let acc = (acc lsl 5) lor (acc lsr 58) in
  let x = acc lxor value in
  x * 0x117cc1b727220a95

let[@inline] mix2 a b = mix a b

let[@inline] mix3 a b c = mix (mix2 a b) c

let[@inline] mix4 a b c d = mix (mix3 a b c) d

let[@inline] mix5 a b c d e = mix (mix4 a b c d) e

let[@inline] mix6 a b c d e f = mix (mix5 a b c d e) f

let[@inline] mix7 a b c d e f g = mix (mix6 a b c d e f) g

let[@inline] mix_list hash_elem list =
  List.fold_left (fun acc x -> mix acc (hash_elem x)) 0 list

let[@inline] mix_array hash_elem arr =
  Array.fold_left (fun acc x -> mix acc (hash_elem x)) 0 arr

let[@inline] mix_map fold hash_key hash_value m =
  fold (fun k v acc -> mix acc (mix2 (hash_key k) (hash_value v))) m 0

(** [mix_option hash_elem opt] hashes an optional value. Returns [0] for
    [None], which allows treating a map [key -> value] as a function
    [key -> value option] in terms of the hash: an unbound key hashes
    identically to a key bound to [None]. *)
let[@inline] mix_option hash_elem = function
  | None -> 0
  | Some x -> mix2 0x27d4eb2d (hash_elem x)

let mix_string = Hashtbl.hash

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

type 'a dedup =
  { hash : int;
    value : 'a
  }

let deduplicate (type a) ~initial_size
    (module M : Hashtbl.HashedType with type t = a) =
  let module Table = Hashtbl.Make (struct
    type nonrec t = a dedup

    let equal e1 e2 = M.equal e1.value e2.value && Int.equal e1.hash e2.hash

    let hash e = e.hash
  end) in
  let table = Table.create initial_size in
  let module D = struct
    type t = a dedup

    type value = M.t

    let create value =
      let hash = M.hash value in
      let elem = { hash; value } in
      match Table.find_opt table elem with
      | Some existing -> existing
      | None ->
        Table.add table elem elem;
        elem

    let equal e1 e2 = e1 == e2

    let hash e = e.hash

    let value e = e.value
  end in
  (module D : Dedup with type value = a)

module type Hashable_map = sig
  type 'a t

  type key

  val empty : 'a t

  val add : key -> 'a -> 'a t -> 'a t

  val find : key -> 'a t -> 'a

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val hash_key : key -> int
end

module Incrementally_hashed_map (Arg : Hashable_map) = struct
  type 'a t =
    { map : 'a Arg.t;
      hash : int
    }

  let empty = { map = Arg.empty; hash = 0 }

  let hash_binding hash_value key value =
    mix2 (Arg.hash_key key) (hash_value value)

  let add hash_value key value t =
    let old_hash =
      match Arg.find key t.map with
      | exception Not_found -> 0
      | old_value -> hash_binding hash_value key old_value
    in
    let new_hash = hash_binding hash_value key value in
    let hash = t.hash lxor old_hash lxor new_hash in
    let map = Arg.add key value t.map in
    { map; hash }

  let find key t = Arg.find key t.map

  let hash t = t.hash

  let equal eq_value t1 t2 = Arg.equal eq_value t1.map t2.map
end

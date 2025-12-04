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

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val find : key -> 'a t -> 'a

  val hash_key : key -> int
end

module Incrementally_hashed_map (Arg : Hashable_map) = struct
  type 'a t =
    { map : 'a Arg.t;
      hash : int;
      cardinal : int
    }

  let empty = { map = Arg.empty; hash = 0; cardinal = 0 }

  let hash_binding hash_value key value =
    mix2 (Arg.hash_key key) (hash_value value)

  let add hash_value key value t =
    let old_hash = ref None in
    let new_cardinal = ref None in
    let map = Arg.update key (function
      | None ->
          (* Key not present: old binding contributes 0 to the XOR hash
             (absence is represented as zero in the incremental hash). *)
          old_hash := Some 0;
          new_cardinal := Some (t.cardinal + 1);
          Some value
      | Some old_value ->
          (* Key present: XOR out the old binding's hash contribution. *)
          old_hash := Some (hash_binding hash_value key old_value);
          new_cardinal := Some t.cardinal;
          Some value
    ) t.map in
    let new_hash = hash_binding hash_value key value in
    let hash = t.hash lxor Option.get !old_hash lxor new_hash in
    let cardinal = Option.get !new_cardinal in
    { map; hash; cardinal }

  let hash t = t.hash

  let cardinal t = t.cardinal

  let iter f t = Arg.iter f t.map

  let find k t = Arg.find k t.map

  let equal eq_value t1 t2 =
    t1 == t2 ||
    (Int.equal t1.hash t2.hash &&
     Misc.map_equal_iter_find ~iter ~cardinal ~find eq_value t1 t2)
end

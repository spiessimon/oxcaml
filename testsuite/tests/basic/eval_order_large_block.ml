(* TEST
 flambda2;
 {
   native;
 }{
   flags = "-Oclassic";
   native;
 }{
   flags = "-O3";
   native;
 }{
   bytecode;
 }
*)

(* Field initialisers of heap blocks larger than [Max_young_wosize] (256 words)
   must run in the same order as for smaller blocks: [b] is bound before [a],
   so [b = 1] and [a = 2]. The unboxed products are only padding, flattened
   into ordinary int fields. *)

type w2 = #(int * int)
type w4 = #(w2 * w2)
type w8 = #(w4 * w4)
type w16 = #(w8 * w8)
type w32 = #(w16 * w16)
type w64 = #(w32 * w32)
type w128 = #(w64 * w64)
type w256 = #(w128 * w128)

(* 2 + 256 = 258 words *)
type big = { a : int; b : int; pad256 : w256 }

(* 2 + 254 = 256 words *)
type small =
  { a : int; b : int; pad254 : #(w128 * w64 * w32 * w16 * w8 * w4 * w2) }

let counter = ref 0

let[@inline never] next () =
  incr counter;
  !counter

(* Global arguments, so that the records are heap-allocated. *)
let[@inline never] print_big : big -> unit =
 fun { a; b; _ } -> Printf.printf "big: a = %d, b = %d\n" a b

let[@inline never] print_small : small -> unit =
 fun { a; b; _ } -> Printf.printf "small: a = %d, b = %d\n" a b

let[@inline never] test_big () =
  let w2 = #(0, 0) in
  let w4 = #(w2, w2) in
  let w8 = #(w4, w4) in
  let w16 = #(w8, w8) in
  let w32 = #(w16, w16) in
  let w64 = #(w32, w32) in
  let w128 = #(w64, w64) in
  let w256 = #(w128, w128) in
  counter := 0;
  let b = next () in
  let a = next () in
  print_big { a; b; pad256 = w256 }

let[@inline never] test_small () =
  let w2 = #(0, 0) in
  let w4 = #(w2, w2) in
  let w8 = #(w4, w4) in
  let w16 = #(w8, w8) in
  let w32 = #(w16, w16) in
  let w64 = #(w32, w32) in
  let w128 = #(w64, w64) in
  counter := 0;
  let b = next () in
  let a = next () in
  print_small { a; b; pad254 = #(w128, w64, w32, w16, w8, w4, w2) }

let () =
  test_big ();
  test_small ()

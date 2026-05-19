(* Companion to e2e_inline.ml. [@inline always] helpers shaped to
   mimic real-world inlinable code: stdlib HOF call-sites that
   require an anonymous closure, chains where one [@inline always]
   callee consumes the result of another, and a wrapper around a
   recursive helper whose [rec] body must survive inlining. *)

let[@inline always] square x = x * x

let[@inline always] add a b = a + b

let[@inline always] compose f g x = f (g x)

let[@inline always] mk_adder x = fun[@inline never] y -> x + y

let[@inline always] apply_thrice f x = f (f (f x))

let[@inline always] iterate_n f init n =
  let rec loop i acc =
    if i >= n then acc else loop (i + 1) (f acc)
  in
  loop 0 init

module Inner = struct
  let[@inline always] scale n x = n * x

  (* Map with an anonymous closure marked [@inline never] so the
     closure body must live as its own symbol after [triple]
     inlines away. *)
  let[@inline always] triple xs =
    List.map (fun[@inline never] x -> x * 3) xs

  (* Depends on [triple] and adds a second HOF call site
     ([List.fold_left] with another non-inlinable closure). *)
  let[@inline always] add_pair xs =
    List.fold_left (fun[@inline never] acc x -> acc + x) 0 (triple xs)

  module Deeper = struct
    (* Three-level dependency: [square] composed under
       [List.map], then funnelled into [add_pair] (which itself
       uses [triple] + fold). *)
    let[@inline always] sum_squares xs =
      add_pair (List.map (fun[@inline never] x -> square x) xs)
  end
end

(* [f <- g <- h <- closure]: a curried function that returns nested
   closures all the way down. Each layer is [@inline never] so all
   three closure bodies are forced to keep their own symbols. *)
let[@inline always] chain4 a =
  fun[@inline never] b ->
    fun[@inline never] c ->
      fun[@inline never] d -> a + b + c + d

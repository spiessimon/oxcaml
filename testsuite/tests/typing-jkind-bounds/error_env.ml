(* TEST
 expect;
*)

let require_portable (_ : (_ : value mod portable)) = ()
[%%expect {|
val require_portable : ('a : value mod portable). 'a -> unit = <fun>
|}]

let f a b = require_portable (a, b)
[%%expect {|
Uncaught exception: Typecore.Error(_, _, _)

|}]

let f a b = require_portable (a, b, c, d, e)
[%%expect {|
Uncaught exception: Typecore.Error(_, _, _)

|}]

let f a b = require_portable ((a, b), (c, d))
[%%expect {|
Uncaught exception: Typecore.Error(_, _, _)

|}]

type ('a, 'b) t = { a : 'a; b : 'b }
let f a b = require_portable (a, b)
[%%expect {|
type ('a, 'b) t = { a : 'a; b : 'b; }
Uncaught exception: Typecore.Error(_, _, _)

|}]

type ('a, 'b) t = Foo of 'a * 'b
let f a b = require_portable (a, b)
[%%expect {|
type ('a, 'b) t = Foo of 'a * 'b
Uncaught exception: Typecore.Error(_, _, _)

|}]

let f (a : _ list) (b : _ option) = require_portable (a, b)
[%%expect {|
Uncaught exception: Typecore.Error(_, _, _)

|}]

type 'a t_no_bound = unit
type 'a t_with_bound = 'a option
let id x = x
let f (a : _ t_no_bound) (b : _ t_with_bound) =
  require_portable (id (a, b))
(* CR layouts: in the non-principal case, the jkind should be [with 'b] rather
   than [with 'a]. Internal ticket 6133. *)
[%%expect {|
type 'a t_no_bound = unit
type 'a t_with_bound = 'a option
val id : 'a -> 'a = <fun>
Uncaught exception: Typecore.Error(_, _, _)

|}]

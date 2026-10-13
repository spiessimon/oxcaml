(* TEST *)

(* A [let rec] group mixes recursive functions with bindings that do not
   refer to the group. The latter are evaluated first, in order, then the
   functions are defined. *)

let rec f () =
  print_endline "f called";
  x
and x = (print_endline "x evaluated"; 42)
and g () = f () + y
and y = (print_endline "y evaluated"; 1)

let () = Printf.printf "%d\n" (g ())

let () =
  let rec local_f () = local_x
  and local_x = (print_endline "local_x evaluated"; "local")
  and local_g () = local_f () ^ "!" in
  print_endline (local_g ())

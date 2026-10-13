(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Vincent Laviron, OCamlPro                      *)
(*                                                                        *)
(*   Copyright 2023 OCamlPro, SAS                                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Types related to the compilation of recursive value bindings *)

(** The kind of a binding in a [let rec] group, as computed by
    [Value_rec_check.classify_recursive_binding]. *)
type recursive_binding_kind =
| Recursive_function
  (** A syntactic function. It may refer to the other bindings of the group
      and is compiled as part of a [Lletrec]. *)
| Not_recursive
  (** A binding that does not refer to any binding of the group. It is
      compiled as a regular let-binding, evaluated before the functions. *)

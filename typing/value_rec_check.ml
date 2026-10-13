(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Jeremy Yallop, University of Cambridge                   *)
(*               Gabriel Scherer, Project Parsifal, INRIA Saclay          *)
(*               Alban Reynaud, ENS Lyon                                  *)
(*                                                                        *)
(*   Copyright 2017 Jeremy Yallop                                         *)
(*   Copyright 2018 Alban Reynaud                                         *)
(*   Copyright 2018 INRIA                                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Static checking of recursive declarations.

    A [let rec] only defines functions. In a group
    {[
      let rec x1 = e1 and ... and xn = en
    ]}
    each right-hand side [ei] must either be a syntactic function ([fun] or
    [function]), which may refer freely to [x1 ... xn] because those
    references are only evaluated when the function is called, or not
    mention any of [x1 ... xn] at all, in which case it is an ordinary
    definition that merely shares the [let rec] with the functions.

    Recursive non-function values such as [let rec xs = 0 :: xs] or
    [let rec r = { f = fun () -> r }] are rejected: no cyclic data can be
    created without mutation, and every [let rec] is compiled as ordinary
    let-bindings followed by a single recursive definition of closures (see
    [Translcore.transl_let]).

    Recursive class definitions follow the same principle: the let-bindings
    in the spine of a class expression are evaluated when the class is
    defined, so they may not refer to the classes of the group. Method
    bodies and the other parts of the class body are only evaluated later
    and are unrestricted. *)

open Typedtree

exception Found of Ident.t

(* Identifiers in the typed tree are unique, so a plain traversal suffices
   to find references: there is no shadowing to account for. *)
let check_path ids path =
  match path with
  | Path.Pident id when List.exists (Ident.same id) ids -> raise (Found id)
  | Path.Pident _ | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ -> ()

(* Values can only be referred to through [Texp_ident] and the operators of
   [Texp_letop]. *)
let value_reference_iterator ids =
  let open Tast_iterator in
  let expr sub e =
    begin match e.exp_desc with
    | Texp_ident { path; _ } -> check_path ids path
    | _ -> ()
    end;
    default_iterator.expr sub e
  in
  let binding_op sub bop =
    check_path ids bop.bop_op_path;
    default_iterator.binding_op sub bop
  in
  { default_iterator with expr; binding_op }

(* Classes can only be referred to through [Texp_new] and [Tcl_ident]. *)
let class_reference_iterator ids =
  let open Tast_iterator in
  let expr sub e =
    begin match e.exp_desc with
    | Texp_new (path, _, _, _) -> check_path ids path
    | _ -> ()
    end;
    default_iterator.expr sub e
  in
  let class_expr sub ce =
    begin match ce.cl_desc with
    | Tcl_ident (path, _, _) -> check_path ids path
    | _ -> ()
    end;
    default_iterator.class_expr sub ce
  in
  { default_iterator with expr; class_expr }

let find_reference iterator traverse =
  match traverse iterator with
  | () -> None
  | exception Found id -> Some id

let classify_recursive_binding ids expr =
  match expr.exp_desc with
  | Texp_function _ -> Ok Value_rec_types.Recursive_function
  | _ ->
    let iterator = value_reference_iterator ids in
    let traverse (it : Tast_iterator.iterator) = it.expr it expr in
    match find_reference iterator traverse with
    | None -> Ok Value_rec_types.Not_recursive
    | Some id -> Error id

let check_class_expr ids ce =
  let iterator = class_reference_iterator ids in
  let rec spine ce =
    match ce.cl_desc with
    | Tcl_let (_, bindings, _, ce) ->
      let traverse (it : Tast_iterator.iterator) =
        List.iter (it.value_binding it) bindings
      in
      begin match find_reference iterator traverse with
      | Some id -> Error id
      | None -> spine ce
      end
    | Tcl_constraint (ce, _, _, _, _) | Tcl_open (_, ce) -> spine ce
    | Tcl_ident _ | Tcl_structure _ | Tcl_fun _ | Tcl_apply _ -> Ok ()
  in
  spine ce

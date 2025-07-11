module Uid = Shape.Uid
module Layout = Jkind_types.Sort.Const

type base_layout = Jkind_types.Sort.base

type path_lookup = Path.t -> args:Shape.t list -> Shape.t option

module Type_shape : sig
  val of_type_expr :
    Types.type_expr -> path_lookup -> Shape.without_layout Shape.ts
end

module Type_decl_shape : sig
  val of_type_declarations :
    (Ident.t * Types.type_declaration) list -> path_lookup -> Shape.t list
end

type shape_with_layout =
  { type_shape : Shape.without_layout Shape.ts;
    type_layout : Layout.t;
    type_name : string
  }
(* CR sspies: There are two options here: We can fold the layout into the shape,
    or we can keep it on the outside. Currently, we keep it on the outside to
    make it easier to connect type shapes and shapes (which are agnostic about
   layouts) in subsequent PRs. *)

val all_type_decls : Shape.t Uid.Tbl.t

val all_type_shapes : shape_with_layout Uid.Tbl.t

(* Passing [Path.t -> Uid.t] instead of [Env.t] to avoid a dependency cycle. *)
val add_to_type_decls :
  (Ident.t * Types.type_declaration) list -> path_lookup -> unit

val add_to_type_shapes :
  Uid.t ->
  Types.type_expr ->
  Jkind_types.Sort.Const.t ->
  name:string ->
  path_lookup ->
  unit

val find_in_type_decls : Uid.t -> Shape.t option

val print_table_all_type_decls : Format.formatter -> unit

val print_table_all_type_shapes : Format.formatter -> unit

module Uid = Shape.Uid
module Layout = Jkind_types.Sort.Const

type base_layout = Jkind_types.Sort.base

type path_lookup = Path.t -> args:Shape.t list -> Shape.t option

let extended_env f path ~args =
  let open Shape in
  match Shape.Predef.of_string (Path.name path) with
  | Some predef ->
    let predef_shape = Ts_predef predef in
    Some (Shape.app_list (Shape.smart_type_ predef_shape) args)
  | None -> f path ~args

let smart_type_shape (sh : Shape.t) =
  match sh.desc with
  | Shape.Type ts -> ts
  | _ -> Shape.Ts_shape (sh, Shape.Layout_to_be_determined)

module Dynamic_binder : sig
  type dynamic_binder

  val mk_dynamic_binder : unit -> dynamic_binder

  val use_dynamic_binder : dynamic_binder -> Shape.t

  val bind_dynamic_binder : dynamic_binder -> Shape.t -> Shape.t

  val bind_dynamic_binder_type_shape :
    dynamic_binder ->
    Shape.without_layout Shape.ts ->
    Shape.without_layout Shape.ts
end = struct
  type dynamic_binder =
    { uid : Uid.t;
      mutable used : bool
    }

  let mk_dynamic_binder () = { uid = Uid.mk ~current_unit:None; used = false }

  let use_dynamic_binder db =
    db.used <- true;
    Shape.leaf db.uid

  let bind_dynamic_binder db sh =
    if not db.used then sh else Shape.mu None db.uid sh

  let bind_dynamic_binder_type_shape db ts =
    if not db.used
    then ts
    else
      Shape.Ts_shape
        (Shape.mu None db.uid (Shape.type_ ts), Shape.Layout_to_be_determined)
end

module Type_shape = struct
  (* Similarly to [value_kind], we track a set of visited types to avoid cycles
     in the lookup and we, additionally, carry a maximal depth for the recursion.
     We allow a deeper bound than [value_kind]. *)
  (* CR sspies: Consider additionally adding a max size for the set of visited types.
     Also consider reverting to the original value kind depth limit (although 2
     seems low). *)
  let rec of_type_expr_go ~visited ~depth (expr : Types.type_expr)
      (subst : (Types.type_expr * Shape.t) list) shape_of_path :
      Shape.without_layout Shape.ts =
    let open Shape in
    let[@inline] cannot_proceed () =
      Numbers.Int.Map.mem (Types.get_id expr) visited || depth >= 10
    in
    if cannot_proceed ()
    then
      match Numbers.Int.Map.find_opt (Types.get_id expr) visited with
      | Some db ->
        let sh = Dynamic_binder.use_dynamic_binder db in
        Ts_shape (sh, Layout_to_be_determined)
      | None -> Ts_other Layout_to_be_determined
    else
      match
        List.find_opt (fun (p, _) -> Types.get_id p == Types.get_id expr) subst
      with
      (* CR sspies: Physical equality is also how printing in [printtyp.ml] works. It
         seems to be the way to substitute type parameters (after type inference has
         already made them more precise). *)
      | Some (_, s) -> smart_type_shape s
      | None ->
        let rec_binder = Dynamic_binder.mk_dynamic_binder () in
        let visited =
          Numbers.Int.Map.add (Types.get_id expr) rec_binder visited
        in
        let depth = depth + 1 in
        let desc = Types.get_desc expr in
        let map_expr_list (exprs : Types.type_expr list) =
          List.map
            (fun expr ->
              of_type_expr_go ~depth ~visited expr subst shape_of_path)
            exprs
        in
        let type_shape =
          match desc with
          | Tconstr (path, constrs, _) ->
            let args =
              List.map (fun ts -> Shape.smart_type_ ts) (map_expr_list constrs)
            in
            let shape = shape_of_path path ~args in
            let shape = Option.map smart_type_shape shape in
            Option.value shape ~default:(Ts_other Layout_to_be_determined)
          | Ttuple exprs -> Ts_tuple (map_expr_list (List.map snd exprs))
          | Tvar _ -> Ts_other Layout_to_be_determined
          | Tpoly (type_expr, _type_vars) ->
            (* CR sspies: At the moment, we simply ignore the polymorphic variables.
               This code used to only work for [_type_vars = []]. *)
            of_type_expr_go ~depth ~visited type_expr subst shape_of_path
          | Tunboxed_tuple exprs ->
            Ts_unboxed_tuple (map_expr_list (List.map snd exprs))
          | Tobject _ | Tnil | Tfield _ ->
            Ts_other Layout_to_be_determined
            (* Objects are currently not supported in the debugger. *)
          | Tlink _ | Tsubst _ ->
            Misc.fatal_error
              "linking and substitution should not reach this stage."
          | Tvariant rd ->
            let row_fields = Types.row_fields rd in
            let row_fields =
              List.concat_map
                (fun (name, desc) ->
                  match Types.row_field_repr desc with
                  | Types.Rpresent (Some ty) ->
                    [ { pv_constr_name = name;
                        pv_constr_args =
                          [ of_type_expr_go ~depth ~visited ty subst
                              shape_of_path ]
                      } ]
                  | Types.Rpresent None ->
                    [{ pv_constr_name = name; pv_constr_args = [] }]
                  | Types.Rabsent -> [] (* we filter out absent constructors *)
                  | Types.Reither (_, args, _) ->
                    [ { pv_constr_name = name;
                        pv_constr_args = map_expr_list args
                      } ])
                row_fields
            in
            Ts_variant row_fields
          | Tarrow (_, arg, ret, _) ->
            Ts_arrow
              ( of_type_expr_go ~depth ~visited arg subst shape_of_path,
                of_type_expr_go ~depth ~visited ret subst shape_of_path )
          | Tunivar _ -> Ts_other Layout_to_be_determined
          | Tof_kind _ -> Ts_other Layout_to_be_determined
          | Tpackage _ -> Ts_other Layout_to_be_determined
          (* CR sspies: Support first-class modules. *)
        in
        Dynamic_binder.bind_dynamic_binder_type_shape rec_binder type_shape

  let of_type_expr (expr : Types.type_expr) shape_of_path =
    of_type_expr_go ~visited:Numbers.Int.Map.empty ~depth:(-1) expr []
      (extended_env shape_of_path)

  let of_type_expr_with_type_subst (expr : Types.type_expr) shape_of_path subst
      =
    of_type_expr_go ~visited:Numbers.Int.Map.empty ~depth:(-1) expr subst
      (extended_env shape_of_path)
end

module Type_decl_shape = struct
  let mixed_block_shape_to_base_layout = function
    | Types.Value -> Jkind_types.Sort.Value
    | Types.Float_boxed ->
      Jkind_types.Sort.Float64
      (* [Float_boxed] records are unboxed inside the variant at runtime,
         contrary to the name.*)
    | Types.Float64 -> Jkind_types.Sort.Float64
    | Types.Float32 -> Jkind_types.Sort.Float32
    | Types.Bits32 -> Jkind_types.Sort.Bits32
    | Types.Bits64 -> Jkind_types.Sort.Bits64
    | Types.Vec128 -> Jkind_types.Sort.Vec128
    | Types.Vec256 -> Jkind_types.Sort.Vec256
    | Types.Vec512 -> Jkind_types.Sort.Vec512
    | Types.Word -> Jkind_types.Sort.Word
    | Types.Product _ -> Misc.fatal_error "unimplemented"

  let of_variant_constructor_with_args type_subst name
      (cstr_args : Types.constructor_declaration)
      ((constructor_repr, _) : Types.constructor_representation * _)
      shape_of_path =
    let open Shape in
    let args =
      match cstr_args.cd_args with
      | Cstr_tuple list ->
        List.map
          (fun ({ ca_type = type_expr; ca_sort = type_layout; _ } :
                 Types.constructor_argument) ->
            { Shape.field_name = None;
              field_value =
                ( Type_shape.of_type_expr_with_type_subst type_expr
                    shape_of_path type_subst,
                  type_layout )
            })
          list
      | Cstr_record list ->
        List.map
          (fun (lbl : Types.label_declaration) ->
            { Shape.field_name = Some (Ident.name lbl.ld_id);
              field_value =
                ( Type_shape.of_type_expr_with_type_subst lbl.ld_type
                    shape_of_path type_subst,
                  lbl.ld_sort )
            })
          list
    in
    let constructor_repr =
      match constructor_repr with
      | Constructor_mixed shapes ->
        let shapes_and_fields = List.combine (Array.to_list shapes) args in
        List.iter
          (fun (mix_shape, { field_name = _; field_value = _, ly }) ->
            let ly2 =
              Layout.Base (mixed_block_shape_to_base_layout mix_shape)
            in
            if not (Layout.equal ly ly2)
            then
              Misc.fatal_errorf
                "Type_shape: variant constructor with mismatched layout, has \
                 %a but expected %a"
                Layout.format ly Layout.format ly2)
          shapes_and_fields;
        Constructor_mixed (Array.map mixed_block_shape_to_base_layout shapes)
      | Constructor_uniform_value ->
        List.iter
          (fun { field_name = _; field_value = _, ly } ->
            if not (Layout.equal ly (Layout.Base Value))
            then
              Misc.fatal_errorf
                "Type_shape: variant constructor with mismatched layout, has \
                 %a but expected value"
                Layout.format ly)
          args;
        Constructor_uniform_value
    in
    { name; kind = constructor_repr; args }

  let is_empty_constructor_list (cstr_args : Types.constructor_declaration) =
    let length =
      match cstr_args.cd_args with
      | Cstr_tuple list -> List.length list
      | Cstr_record list -> List.length list
    in
    length = 0

  let record_of_labels ~shape_of_path ~type_subst kind labels =
    Shape.Tds_record
      { fields =
          List.map
            (fun (lbl : Types.label_declaration) ->
              ( Ident.name lbl.ld_id,
                Type_shape.of_type_expr_with_type_subst lbl.ld_type
                  shape_of_path type_subst,
                lbl.ld_sort ))
            labels;
        kind
      }

  let type_var_count = ref 0

  let of_type_declaration_go (rec_binder : Dynamic_binder.dynamic_binder)
      (type_declaration : Types.type_declaration) type_param_shapes
      shape_of_path =
    let module Types_predef = Predef in
    let open Shape in
    let type_params = type_declaration.type_params in
    let type_subst = List.combine type_params type_param_shapes in
    (* Duplicates are fine, the constraint system makes sure they are instantiated
       with the same type expression. *)
    let definition =
      match type_declaration.type_manifest with
      | Some type_expr ->
        Tds_alias
          (Type_shape.of_type_expr_with_type_subst type_expr shape_of_path
             type_subst)
      | None -> (
        match type_declaration.type_kind with
        | Type_variant (cstr_list, Variant_boxed layouts, _unsafe_mode_crossing)
          ->
          let cstrs_with_layouts =
            List.combine cstr_list (Array.to_list layouts)
          in
          let simple_constructors, complex_constructors =
            List.partition_map
              (fun ((cstr, arg_layouts) : Types.constructor_declaration * _) ->
                let name = Ident.name cstr.cd_id in
                match is_empty_constructor_list cstr with
                | true -> Left name
                | false ->
                  Right
                    (of_variant_constructor_with_args type_subst name cstr
                       arg_layouts shape_of_path))
              cstrs_with_layouts
          in
          Tds_variant { simple_constructors; complex_constructors }
        | Type_variant ([cstr], Variant_unboxed, _unsafe_mode_crossing)
          when not (is_empty_constructor_list cstr) ->
          let name = Ident.name cstr.cd_id in
          let field_name, type_expr, layout =
            match cstr.cd_args with
            | Cstr_tuple [ca] -> None, ca.ca_type, ca.ca_sort
            | Cstr_record [ld] ->
              Some (Ident.name ld.ld_id), ld.ld_type, ld.ld_sort
            | Cstr_tuple _ | Cstr_record _ ->
              Misc.fatal_error "Unboxed variant must have exactly one argument."
          in
          Tds_variant_unboxed
            { name;
              arg_name = field_name;
              arg_layout = layout;
              arg_shape =
                Type_shape.of_type_expr_with_type_subst type_expr shape_of_path
                  type_subst
            }
        | Type_variant ([_], Variant_unboxed, _unsafe_mode_crossing) ->
          Misc.fatal_error "Unboxed variant must have constructor arguments."
        | Type_variant (([] | _ :: _ :: _), Variant_unboxed, _) ->
          Misc.fatal_error "Unboxed variant must have exactly one constructor."
        | Type_variant
            (_, (Variant_extensible | Variant_with_null), _unsafe_mode_crossing)
          ->
          Tds_other (* CR sspies: These variants are not yet supported. *)
        | Type_record (lbl_list, record_repr, _unsafe_mode_crossing) -> (
          match record_repr with
          (* CR sspies: Why is there another copy of the layouts of the fields
             here? Which one should we use? Shouldn't they both be just values? *)
          | Record_boxed _ ->
            record_of_labels ~shape_of_path ~type_subst Record_boxed lbl_list
          | Record_mixed fields ->
            record_of_labels ~shape_of_path ~type_subst
              (Record_mixed (Array.map mixed_block_shape_to_base_layout fields))
              lbl_list
          | Record_unboxed ->
            record_of_labels ~shape_of_path ~type_subst Record_unboxed lbl_list
          | Record_float | Record_ufloat ->
            let lbl_list =
              List.map
                (fun (lbl : Types.label_declaration) ->
                  { lbl with
                    ld_sort = Base Float64;
                    ld_type = Types_predef.type_unboxed_float
                  })
                  (* CR sspies: We are changing the type and the layout here. Consider
                     adding a name for the types of the fields instead of replacing
                     it with [float#]. *)
                lbl_list
            in
            record_of_labels ~shape_of_path ~type_subst Record_floats lbl_list
          | Record_inlined _ ->
            Misc.fatal_error "inlined records not allowed here"
            (* Inline records of this form should not occur as part of type delcarations.
               They do not exist for top-level declarations, but they do exist tempoarily
               such as inside of a match (e.g., [t] is an inline record in
               [match e with Foo t -> ...]). *))
        | Type_abstract _ -> Tds_other
        | Type_open -> Tds_other
        | Type_record_unboxed_product (lbl_list, _, _) ->
          record_of_labels ~shape_of_path ~type_subst Record_unboxed_product
            lbl_list)
    in
    Dynamic_binder.bind_dynamic_binder rec_binder
      (Shape.type_decl None definition)

  let rec shape_of_path_with_declarations decl_lookup_map shape_of_path path
      ~args =
    match shape_of_path path ~args with
    | Some s -> Some s
    | None -> (
      match path with
      | Path.Pident id -> (
        match Ident.Map.find_opt id decl_lookup_map with
        | Some decl ->
          let rec_binder = Dynamic_binder.mk_dynamic_binder () in
          let guarded_shape_of_path path ~args:inner_args =
            match path with
            | Path.Pident id'
              when Ident.equal id id' && List.equal Shape.equal args inner_args
              ->
              Some (Dynamic_binder.use_dynamic_binder rec_binder)
            | Path.Pident id' when Ident.equal id id' ->
              Misc.fatal_errorf "different args, original %a new %a"
                (Format.pp_print_list Shape.print)
                args
                (Format.pp_print_list Shape.print)
                inner_args
            | _ -> shape_of_path path ~args
          in
          let shape_of_path =
            shape_of_path_with_declarations decl_lookup_map
              guarded_shape_of_path
          in
          Some (of_type_declaration_go rec_binder decl args shape_of_path)
        | None -> None)
      | _ -> None)

  let of_type_declaration_with_variables (id : Ident.t)
      (type_declaration : Types.type_declaration) shape_of_path =
    let type_param_idents =
      List.map
        (fun _ ->
          let name = Format.asprintf "a/%d" !type_var_count in
          type_var_count := !type_var_count + 1;
          Ident.create_local name)
        type_declaration.type_params
    in
    let type_param_shapes =
      List.map (fun id -> Shape.var' None id) type_param_idents
    in
    match shape_of_path (Path.Pident id) ~args:type_param_shapes with
    (* This works, because we add the declarations to the environment below *)
    | None -> assert false
    | Some definition -> Shape.abs_list definition type_param_idents

  let of_type_declarations
      (type_declarations : (Ident.t * Types.type_declaration) list)
      shape_of_path =
    let decl_lookup_map = Ident.Map.of_list type_declarations in
    (* We unbind all declarations, to avoid accidental recursive cycles. *)
    let shape_of_path path ~args =
      match path with
      | Path.Pident id when Ident.Map.mem id decl_lookup_map -> None
      | _ -> shape_of_path path ~args
    in
    let shape_of_path = extended_env shape_of_path in
    let shape_of_path =
      shape_of_path_with_declarations decl_lookup_map shape_of_path
    in
    List.map
      (fun (id, decl) ->
        of_type_declaration_with_variables id decl shape_of_path)
      type_declarations
end

type type_shape_with_name =
  { type_shape : Shape.without_layout Shape.ts;
    type_layout : Layout.t;
    type_name : string
  }

let (all_type_decls : Shape.t Uid.Tbl.t) = Uid.Tbl.create 16

let (all_type_shapes : type_shape_with_name Uid.Tbl.t) = Uid.Tbl.create 16

let add_to_type_decls (decls : (Ident.t * Types.type_declaration) list)
    shape_of_path =
  let type_decl_shapes =
    Type_decl_shape.of_type_declarations decls shape_of_path
  in
  List.iter
    (fun ((_, decl), sh) -> Uid.Tbl.add all_type_decls decl.Types.type_uid sh)
    (List.combine decls type_decl_shapes)

let add_to_type_shapes var_uid type_expr type_layout ~name shape_of_path =
  let type_shape = Type_shape.of_type_expr type_expr shape_of_path in
  Uid.Tbl.add all_type_shapes var_uid
    { type_shape; type_layout; type_name = name }

let find_in_type_decls (type_uid : Uid.t) =
  Uid.Tbl.find_opt all_type_decls type_uid

let print_table ppf (columns : (string * string list) list) =
  if List.length columns = 0 then Misc.fatal_errorf "print_table: empty table";
  let column_widths =
    List.map
      (fun (name, entries) ->
        List.fold_left max (String.length name) (List.map String.length entries))
      columns
  in
  let table_depth = List.hd columns |> snd |> List.length in
  let table_width =
    List.fold_left ( + ) 0 column_widths
    + 4 (* boundary characters *)
    + ((List.length column_widths - 1) * 3 (* inter column boundaries *))
  in
  let columns = List.combine column_widths columns in
  let columns =
    List.map
      (fun (w, (name, entries)) -> w, name, Array.of_list entries)
      columns
  in
  Format.fprintf ppf "%s\n" (String.make table_width '-');
  let headers =
    List.map
      (fun (w, name, _) ->
        Format.asprintf "%s%s" name (String.make (w - String.length name) ' '))
      columns
  in
  Format.fprintf ppf "| %s |\n" (String.concat " | " headers);
  Format.fprintf ppf "%s\n" (String.make table_width '-');
  let print_row ppf i =
    let row_strings =
      List.map
        (fun (w, _, entries) ->
          Format.asprintf "%s%s" entries.(i)
            (String.make (w - String.length entries.(i)) ' '))
        columns
    in
    Format.fprintf ppf "| %s |\n" (String.concat " | " row_strings)
  in
  for i = 0 to table_depth - 1 do
    print_row ppf i
  done;
  Format.fprintf ppf "%s\n" (String.make table_width '-')

let print_table_all_type_decls ppf =
  let entries = Uid.Tbl.to_list all_type_decls in
  let entries = List.sort (fun (a, _) (b, _) -> Uid.compare a b) entries in
  let entries =
    List.map
      (fun (k, v) ->
        Format.asprintf "%a" Uid.print k, Format.asprintf "%a" Shape.print v)
      entries
  in
  let uids, decls = List.split entries in
  print_table ppf ["UID", uids; "Type Declaration", decls]

let print_table_all_type_shapes ppf =
  let entries = Uid.Tbl.to_list all_type_shapes in
  let entries = List.sort (fun (a, _) (b, _) -> Uid.compare a b) entries in
  let entries =
    List.map
      (fun (k, { type_shape; type_layout; type_name }) ->
        ( Format.asprintf "%a" Uid.print k,
          ( Format.asprintf "%a" Shape.print_type_shape type_shape,
            ( type_name,
              Format.asprintf "%a" Jkind_types.Sort.Const.format type_layout )
          ) ))
      entries
  in
  let uids, rest = List.split entries in
  let type_shapes, rest = List.split rest in
  let names, sorts = List.split rest in
  print_table ppf
    ["UID", uids; "Type", names; "Sort", sorts; "Shape", type_shapes]

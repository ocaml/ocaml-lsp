open! Import
open! Ts_types

(* TypeScript to OCaml conversion pipeline. The goal of this pipeline is to do
   the conversion in logical stages. Unfortunately, this doesn't quite work *)

(* These declarations are all excluded because we don't support them or their
   definitions are hand written *)
let skipped_ts_decls =
  [ "InitializedParams"
  ; "NotificationMessage"
  ; "RequestMessage"
  ; "ResponseError"
  ; "DocumentUri"
  ; "ResponseMessage"
  ; "Message"
  ; "ErrorCodes"
  ; "MarkedString"
  ; "ProgressToken"
  ; "ProgressParams"
  ]

(* Super classes to remove because we handle their concerns differently (or not
   at all) *)
let removed_super_classes = []

(* The preprocessing stage maps over the typescript AST. It should only do very
   simple clean ups *)
let preprocess =
  let union_fields l1 l2 ~f =
    let of_map =
      String.Map.of_list_map_exn ~f:(fun (x : _ Named.t) -> (x.name, x))
    in
    String.Map.union (of_map l1) (of_map l2) ~f |> String.Map.values
  in
  let open Unresolved in
  let traverse =
    object (self)
      inherit Unresolved.map as super

      val mutable current_name = None

      method name =
        match current_name with
        | None -> assert false
        | Some n -> n

      method! sum vars =
        match
          List.filter_map vars ~f:(function
            | Record [] -> None
            | _ as t -> Some (self#typ t))
        with
        | [] -> assert false
        | [ t ] -> t
        | ts -> Sum ts

      method! field x =
        if x.name = "documentChanges" then
          (* This gross hack is needed for the documentChanges field. We can
             ignore the first constructor since it's completely representable
             with the second one. *)
          match x.data with
          | Single
              { typ = Sum [ List (Ident "TextDocumentEdit"); (List _ as typ) ]
              ; optional
              } ->
            let data = Single { typ; optional } in
            super#field { x with data }
          | _ -> super#field x
        else if x.name = "change" then
          match x.data with
          | Single { typ = Ident "number"; optional = true } ->
            let typ = Ident "TextDocumentSyncKind" in
            let data = Single { typ; optional = true } in
            super#field { x with data }
          | _ -> super#field x
        else
          super#field x

      method! typ x =
        (* XXX what does this to? I don't see any sums of records in
           TextDocumentContentChangeEvent *)
        match x with
        | Sum [ Record f1; Record f2 ]
          when self#name = "TextDocumentContentChangeEvent" ->
          let t =
            Record
              (union_fields f1 f2 ~f:(fun k t1 t2 ->
                   assert (k = "text");
                   assert (t1 = t2);
                   Some t1))
          in
          super#typ t
        | t -> super#typ t

      (* all toplevel declarations that we decide to skip *)
      method! t x =
        current_name <- Some x.name;
        super#t x

      method! interface i =
        let extends =
          List.filter i.extends ~f:(fun name ->
              not (List.mem removed_super_classes name ~equal:String.equal))
        in
        let fields =
          (* We ignore this field until we switch to our own code gen. *)
          if self#name = "FormattingOptions" then
            List.filter i.fields ~f:(fun (f : field) ->
                match f.data with
                | Pattern _ when f.name = "key" -> false
                | _ -> true)
          else
            i.fields
        in
        super#interface { i with extends; fields }
    end
  in
  fun i -> traverse#t i

module Expanded = struct
  (** The expanded form is still working with typescript types. However, all
      "anonymous" records and sums have been hoisted to the toplevel. So there
      is a 1-1 correspondence to the OCaml typse we are going to generate *)

  [@@@ocaml.warning "-37"]

  type binding =
    | Record of Resolved.field list
    | Interface of Resolved.interface
    | Poly_enum of Resolved.typ list
    | Alias of Resolved.typ

  type t = binding Ml.Module.t

  (** Every anonymous record *)
  let new_binding_of_typ (x : Resolved.typ) : binding option =
    let record = function
      | [ { Named.name = _; data = Resolved.Pattern _ } ] -> None
      | f -> Some (Record f)
    in
    match x with
    | Record d -> record d
    | Sum [ _; Record d ] -> record d
    | _ -> None

  class discovered_types =
    object
      inherit [binding Named.t list] Resolved.fold as super

      (** Every record valued field introduces a new type

          TODO handle the case where two fields share a type *)
      method! field f ~init =
        let init =
          match f.data with
          | Pattern _ -> init
          | Single { optional = _; typ } -> (
            match new_binding_of_typ typ with
            | None -> init
            | Some data -> { f with data } :: init)
        in
        super#field f ~init
    end

  let bindings (r : Resolved.t) =
    let t : binding Named.t =
      let data =
        match r.data with
        | Enum_anon _ -> assert false
        | Interface i -> Interface i
        | Type typ -> (
          match new_binding_of_typ typ with
          | Some data -> data
          | None -> Alias typ)
      in
      { data; name = "t" }
    in
    let init = [ t ] in
    match r.data with
    | Enum_anon _ -> assert false
    | Type typ -> (new discovered_types)#typ typ ~init
    | Interface intf -> (new discovered_types)#typ (Record intf.fields) ~init

  let of_ts (r : Resolved.t) : t =
    let name = Ml.Module.Name.of_string (String.capitalize_ascii r.name) in
    { Ml.Module.name; bindings = bindings r }
end

module Json = Json_gen

module Module : sig
  open Ml

  type t = (Module.sig_ Module.t, Module.impl Module.t) Kind.pair

  val add_private_values : t -> Expr.toplevel Named.t list -> t

  val type_decls : Module.Name.t -> Type.decl Named.t list Kind.Map.t -> t

  (** Use Json.Nullable_option or Json.Assoc.t where appropriate *)
  val use_json_conv_types : t -> t

  (** Rename fields that are also OCaml keywords *)
  val rename_invalid_fields : Ml.Kind.t -> Type.decl -> Type.decl

  val pp : t -> unit Pp.t Kind.Map.t
end = struct
  module Module = Ml.Module

  type t = (Module.sig_ Module.t, Module.impl Module.t) Ml.Kind.pair

  let type_decls name (type_decls : Ml.Type.decl Named.t list Ml.Kind.Map.t) : t
      =
    let module_ bindings = { Ml.Module.name; bindings } in
    let intf : Module.sig_ Module.t =
      List.map type_decls.intf ~f:(fun (td : Ml.Type.decl Named.t) ->
          { td with
            Named.data = (Ml.Module.Type_decl td.data : Ml.Module.sig_)
          })
      |> module_
    in
    let impl =
      List.map type_decls.impl ~f:(fun (td : Ml.Type.decl Named.t) ->
          { td with Named.data = Ml.Module.Type_decl td.data })
      |> module_
    in
    { Ml.Kind.intf; impl }

  let add_private_values (t : t) bindings : t =
    let bindings =
      List.map bindings ~f:(fun (v : _ Named.t) ->
          { v with Named.data = Ml.Module.Value v.data })
    in
    let impl = { t.impl with bindings = t.impl.bindings @ bindings } in
    { t with impl }

  let json_assoc_t = Ml.Path.Dot (Dot (Ident "Json", "Assoc"), "t")

  let rename_invalid_fields =
    let map (kind : Ml.Kind.t) =
      let open Ml.Type in
      object (self)
        inherit [unit, unit] Ml.Type.mapreduce as super

        method empty = ()

        method plus () () = ()

        method! field x f =
          let f =
            if Ml.is_kw f.name then
              let attrs =
                match kind with
                | Impl -> ("key", [ sprintf "%S" f.name ]) :: f.attrs
                | Intf -> f.attrs
              in
              { f with name = f.name ^ "_"; attrs }
            else
              f
          in
          super#field x f

        method! assoc x k v = self#t x (App (Path json_assoc_t, [ k; v ]))
      end
    in
    fun kind t -> (map kind)#decl () t |> fst

  let use_json_conv_types =
    let map =
      let open Ml.Type in
      object (self)
        inherit [unit, unit] Ml.Type.mapreduce as super

        method empty = ()

        method plus () () = ()

        method! optional x t =
          if t = Json_gen.json_t then
            super#optional x t
          else
            let opt =
              Ml.Path.Dot (Dot (Ident "Json", "Nullable_option"), "t")
            in
            self#t x (App (Path opt, [ t ]))

        method! field x f =
          let f =
            match f.typ with
            | Optional t ->
              if t = Json_gen.json_t then
                { f with attrs = ("yojson.option", []) :: f.attrs }
              else
                { f with
                  attrs =
                    ("default", [ "None" ])
                    :: ("yojson_drop_default", [ "( = )" ]) :: f.attrs
                }
            | _ -> f
          in
          super#field x f

        method! assoc x k v = self#t x (App (Path json_assoc_t, [ k; v ]))
      end
    in
    fun (t : t) ->
      let impl =
        let bindings =
          List.map t.impl.bindings ~f:(fun (x : _ Named.t) ->
              let data =
                match x.data with
                | Ml.Module.Type_decl decl ->
                  Ml.Module.Type_decl (map#decl () decl |> fst)
                | x -> x
              in
              { x with data })
        in
        { t.impl with bindings }
      in
      { t with impl }

  let pp (t : t) ~kind =
    match (kind : Ml.Kind.t) with
    | Intf -> Ml.Module.pp_sig t.intf
    | Impl -> Ml.Module.pp_impl t.impl

  let pp t = { Ml.Kind.intf = pp t ~kind:Intf; impl = pp t ~kind:Impl }
end

let pp_file pp ch =
  let fmt = Format.formatter_of_out_channel ch in
  Pp.to_fmt fmt pp;
  Format.pp_print_flush fmt ()

module Create : sig
  (* Generate create functions with optional/labeled arguments *)
  val intf_of_type : Ml.Type.decl Named.t -> Ml.Module.sig_ Named.t list

  val impl_of_type : Ml.Type.decl Named.t -> Ml.Module.impl Named.t list
end = struct
  let f_name name =
    if name = "t" then
      "create"
    else
      sprintf "create_%s" name

  let need_unit =
    List.exists ~f:(fun (f : Ml.Type.field) ->
        match f.typ with
        | Ml.Type.Optional _ -> true
        | _ -> false)

  let intf { Named.name; data = fields } =
    let type_ =
      let need_unit = need_unit fields in
      let fields : Ml.Type.t Ml.Arg.t list =
        List.map fields ~f:(fun (field : Ml.Type.field) ->
            match field.typ with
            | Optional t -> Ml.Arg.Optional (field.name, t)
            | t -> Labeled (field.name, t))
      in
      let args : Ml.Type.t Ml.Arg.t list =
        if need_unit then
          (* Gross hack because I was too lazy to allow patterns in toplevel
             exprs *)
          fields @ [ Ml.Arg.Unnamed Ml.Type.unit ]
        else
          fields
      in
      Ml.Type.fun_ args (Ml.Type.name name)
    in
    let f_name = f_name name in
    { Named.name = f_name; data = type_ }

  let impl { Named.name; data = fields } =
    let make =
      let fields =
        List.map fields ~f:(fun (field : Ml.Type.field) ->
            let open Ml.Expr in
            (field.name, Create (Ident field.name)))
      in
      Ml.Expr.Create (Record fields)
    in
    let pat =
      let need_unit = need_unit fields in
      let fields =
        List.map fields ~f:(fun (field : Ml.Type.field) ->
            match field.typ with
            | Optional t -> (Ml.Arg.Optional (field.name, field.name), t)
            | t -> (Ml.Arg.Labeled (field.name, field.name), t))
      in
      if need_unit then
        (* Gross hack because I was too lazy to allow patterns in toplevel
           exprs *)
        fields @ [ (Unnamed "()", Ml.Type.unit) ]
      else
        fields
    in
    let body = { Ml.Expr.pat; type_ = Ml.Type.name name; body = make } in
    let f_name = f_name name in
    { Named.name = f_name; data = body }

  let impl_of_type (t : Ml.Type.decl Named.t) =
    match (t.data : Ml.Type.decl) with
    | Record fields ->
      let create = impl { t with data = fields } in
      [ { create with data = Ml.Module.Value create.data } ]
    | _ -> []

  let intf_of_type (t : Ml.Type.decl Named.t) : Ml.Module.sig_ Named.t list =
    match (t.data : Ml.Type.decl) with
    | Record fields ->
      let create = intf { t with data = fields } in
      [ { create with data = Ml.Module.Value create.data } ]
    | _ -> []
end

let enum_module ~allow_other ({ Named.name; data = constrs } as t) =
  let json_bindings =
    Json_gen.Enum.conv ~allow_other ~poly:false { t with name = "t" }
  in
  let t =
    let data =
      let constrs =
        List.map constrs ~f:(fun (name, _) -> Ml.Type.constr ~name [])
      in
      let constrs =
        if allow_other then
          (* [String] is a hack. It could be a differnt type, but it isn't in
             practice *)
          constrs @ [ Ml.Type.constr ~name:"Other" [ Ml.Type.Prim String ] ]
        else
          constrs
      in
      Ml.Type.Variant constrs
    in
    { Named.name = "t"; data }
  in
  let type_decls = Ml.Kind.Map.make_both [ t ] in
  let module_ = Module.type_decls (Ml.Module.Name.of_string name) type_decls in
  Module.add_private_values module_ json_bindings

module Entities = struct
  type t = (Ident.t * Resolved.t) list

  let find db e : _ Named.t =
    match List.assoc db e with
    | Some s -> s
    | None ->
      Code_error.raise "Entities.find: unable to find" [ ("e", Ident.to_dyn e) ]

  let of_map map ts =
    List.map ts ~f:(fun (r : Resolved.t) -> (String.Map.find_exn map r.name, r))

  let rev_find (db : t) (resolved : Resolved.t) : Ident.t =
    match
      List.filter_map db ~f:(fun (id, r) ->
          if r.name = resolved.name then
            Some id
          else
            None)
    with
    | [] -> Code_error.raise "rev_find: resolved not found" []
    | [ x ] -> x
    | _ :: _ -> Code_error.raise "re_vind: duplicate entries" []
end

module Mapper : sig
  (* Convert typescript types to OCaml types *)

  val make_typ : Entities.t -> Resolved.typ Named.t -> Ml.Type.t

  type literal_field =
    { field_name : string
    ; literal_value : string
    }

  (** Map a TS record into an OCaml record. Literal valued fields such as kind:
      'foo' are extracted into a separate list *)
  val record_ :
       Entities.t
    -> Resolved.field list Named.t
    -> Ml.Type.decl Named.t * literal_field list

  (** Extract all untagged unions in field position. These will be turned into
      polymorphic variants using a naming scheme for the tags. *)
  val extract_poly_vars :
    Ml.Type.decl -> Ml.Type.decl * Ml.Type.constr list Named.t list
end = struct
  type literal_field =
    { field_name : string
    ; literal_value : string
    }

  module Type = Ml.Type

  let is_same_as_json =
    let constrs =
      [ Prim.Null; String; Bool; Number; Object; List ]
      |> List.map ~f:(fun s -> Resolved.Ident s)
    in
    fun set ->
      List.for_all constrs ~f:(fun e -> List.mem set e ~equal:Poly.equal)

  let id = Type.name "Jsonrpc.Id.t"

  let is_same_as_id =
    let sort = List.sort ~compare:Poly.compare in
    let constrs =
      [ Prim.String; Number ] |> List.map ~f:(fun s -> Resolved.Ident s) |> sort
    in
    fun cs -> List.equal ( = ) constrs (sort cs)

  (* Any type that includes null needs to be extracted to be converted to an
     option *)
  let remove_null cs =
    let is_null x =
      match x with
      | Resolved.Ident Prim.Null -> Left x
      | _ -> Right x
    in
    let nulls, non_nulls = List.partition_map ~f:is_null cs in
    match nulls with
    | [] -> `No_null_present
    | _ :: _ :: _ -> assert false
    | [ _ ] -> `Null_removed non_nulls

  let make_typ db { Named.name; data = t } =
    let rec type_ topmost_field_name (t : Resolved.typ) =
      match t with
      | Ident Uinteger ->
        Type.int (* XXX shall we use a dedicated uinteger eventually? *)
      | Ident Number -> Type.int
      | Ident String -> Type.string
      | Ident Bool -> Type.bool
      | Ident Any
      | Ident Object ->
        Type.json
      | Ident Self -> Type.t (* XXX wrong *)
      | Ident Null -> assert false
      | Ident List -> Type.list Type.json
      | Ident (Resolved r) -> (
        match r.kind with
        | Type_variable -> Type.unit
        | Name -> Type.module_t (Entities.find db r).name)
      | List t -> Type.list (type_ topmost_field_name t)
      | Tuple ts -> Type.Tuple (List.map ~f:(type_ topmost_field_name) ts)
      | Sum s -> sum topmost_field_name s
      | App _
      | Literal _ ->
        Type.void
      | Record r -> record r
    and sum topmost_field_name s =
      if is_same_as_json s then
        Type.json
      else
        match remove_null s with
        | `No_null_present ->
          if is_same_as_id s then
            id
          else
            poly topmost_field_name s
        | `Null_removed [ s ] -> Type.Optional (type_ topmost_field_name s)
        | `Null_removed [] -> assert false
        | `Null_removed cs -> Type.Optional (sum topmost_field_name cs)
    and simplify_record (fields : Resolved.field list) =
      (* A record with only a pattern field is simplified to an association
         list *)
      match fields with
      | [ { Named.name; data = Pattern { pat; typ } } ] ->
        let topmost_field_name = Some name in
        let key = type_ topmost_field_name pat in
        let data = type_ topmost_field_name typ in
        Some (Type.assoc_list ~key ~data)
      | _ -> None
    and record fields =
      match simplify_record fields with
      | None -> Type.name name
      | Some a -> a
    and poly topmost_field_name s : Ml.Type.t =
      let type_ = type_ topmost_field_name in
      try
        Type.Poly_variant
          (List.map s ~f:(fun t ->
               let name, constrs =
                 match (t : Resolved.typ) with
                 | Ident Self
                 | Ident Null ->
                   assert false
                 | Ident String -> ("String", [ type_ t ])
                 | Ident Number -> ("Int", [ type_ t ])
                 | Ident Any
                 | Ident Object ->
                   ("Assoc", [ type_ t ])
                 | Ident Bool -> ("Bool", [ type_ t ])
                 | List _
                 | Ident List ->
                   ("List", [ type_ t ])
                 | Ident (Resolved r) -> ((Entities.find db r).name, [ type_ t ])
                 | Tuple [ Ident Uinteger; Ident Uinteger ] ->
                   ("Offset", [ type_ t ])
                 | Literal (String x) -> (x, [])
                 | Record _ ->
                   let topmost_field_name =
                     Option.value_exn topmost_field_name
                   in
                   (topmost_field_name, [ type_ t ])
                 | _ -> raise Exit
               in
               Type.constr ~name constrs))
      with
      | Exit -> Type.unit
    in
    type_ (Some name) t

  let make_field db (field : Resolved.field) =
    match field.data with
    | Pattern { pat; typ } ->
      let key = make_typ db { Named.name = field.name; data = pat } in
      let data = make_typ db { Named.name = field.name; data = typ } in
      let typ = Type.assoc_list ~key ~data in
      Left (Ml.Type.field typ ~name:field.name)
    | Resolved.Single { typ = Literal s; optional = false } ->
      let literal_value =
        match s with
        | String s -> s
        | _ -> assert false
      in
      Right { literal_value; field_name = field.name }
    | Resolved.Single { typ; optional } ->
      let typ = make_typ db { Named.name = field.name; data = typ } in
      let typ =
        if optional then
          Type.Optional typ
        else
          typ
      in
      Left (Ml.Type.field typ ~name:field.name)

  let record_ db { Named.name; data = (fields : Resolved.field list) } =
    let data, literals =
      match fields with
      | [ { Named.name; data = Pattern { pat; typ } } ] ->
        let key = make_typ db { Named.name; data = pat } in
        let data = make_typ db { Named.name; data = typ } in
        (Type.Alias (Type.assoc_list ~key ~data), [])
      | _ ->
        let fields, literals = List.partition_map fields ~f:(make_field db) in
        (Type.Record fields, literals)
    in
    ({ Named.name; data }, literals)

  let extract_poly_vars s =
    let extract =
      object (self)
        inherit
          [string option, Ml.Type.constr list Named.t list] Ml.Type.mapreduce as super

        method empty = []

        (* TODO grossly slow *)
        method plus x y = x @ y

        method! field _ (f : Ml.Type.field) =
          let env = Some f.name in
          super#field env f

        method! poly_variant env constrs =
          match env with
          | None -> super#poly_variant env constrs
          | Some name ->
            (* This hack is needed to avoid collision with user visible types
               that we might introduce *)
            let name = name ^ "_pvar" in
            let replacement = Ml.Type.name name in
            let constrs, m = self#fold_left_map ~f:(self#constr env) constrs in
            (replacement, self#plus m [ { Named.name; data = constrs } ])
      end
    in
    extract#decl None s
end

module Gen : sig
  val module_ : Entities.t -> Expanded.binding Ml.Module.t -> Module.t
end = struct
  module Type = Ml.Type

  let type_ db ({ Named.name; data = _ } as t) =
    let main_type =
      let typ = Mapper.make_typ db t in
      { Named.name; data = Type.Alias typ }
    in
    [ main_type ]

  let record db ({ Named.name = _; data = fields } as t) =
    let main_type, literals = Mapper.record_ db t in
    (* why do we need this check at all? *)
    match fields with
    | [] -> None
    | _ :: _ -> Some (main_type, literals)

  let poly_enum { Named.name; data = _ } : Type.decl Named.t list =
    [ { Named.name; data = Type.Alias Type.unit } ]

  let poly_enum_conv (t : _ Named.t) =
    if List.for_all t.data ~f:(fun (c : Ml.Type.constr) -> List.is_empty c.args)
    then
      (* This is equivalent to an enum *)
      List.map t.data ~f:(fun (c : Ml.Type.constr) ->
          (c.name, Literal.String c.name))
      |> Named.set_data t
      |> Json_gen.Enum.conv ~allow_other:false ~poly:true
    else
      [ Json_gen.Poly_variant.of_json t; Json_gen.Poly_variant.to_json t ]

  (* This is the more complex case *)

  let module_ db { Ml.Module.name; bindings } : Module.t =
    let type_decls =
      let add_record = function
        | None -> []
        | Some (decl, literals) -> [ `Record (decl, literals) ]
      in
      let add_else = List.map ~f:(fun x -> `Type x) in
      List.concat_map bindings ~f:(fun (r : Expanded.binding Named.t) ->
          match r.data with
          | Record data -> record db { r with data } |> add_record
          | Interface data ->
            record db { r with data = data.fields } |> add_record
          | Poly_enum data -> poly_enum { r with data } |> add_else
          | Alias data -> type_ db { r with data } |> add_else)
    in
    let intf : Ml.Module.sig_ Named.t list =
      List.map type_decls ~f:(function
        | `Record (t, _) -> t
        | `Type t -> t)
      |> List.concat_map ~f:(fun (td : Ml.Type.decl Named.t) ->
             let td =
               { td with data = Module.rename_invalid_fields Intf td.data }
             in
             [ { td with
                 Named.data = (Ml.Module.Type_decl td.data : Ml.Module.sig_)
               }
             ]
             @ Create.intf_of_type td)
    in
    let impl : Ml.Module.impl Named.t list =
      (* TODO we should make sure to handle duplicate variants extracted *)
      List.concat_map type_decls ~f:(fun d ->
          let d, literal_wrapper =
            match d with
            | `Record (l, [ lw ]) -> (l, Some lw)
            | `Record (l, []) -> (l, None)
            | `Record (_, _ :: _) ->
              assert false
              (* we don't support multiple literals in a single record for
                 now *)
            | `Type l -> (l, None)
          in
          let typ_, poly_vars = Mapper.extract_poly_vars (Named.data d) in
          let poly_vars_and_convs =
            List.concat_map poly_vars ~f:(fun pv ->
                let decl =
                  Named.map pv ~f:(fun decl ->
                      Ml.Module.Type_decl (Alias (Poly_variant decl)))
                in
                let json_conv =
                  poly_enum_conv pv
                  |> List.map ~f:(Named.map ~f:(fun v -> Ml.Module.Value v))
                in
                decl :: json_conv)
          in
          let typ_ = { d with data = typ_ } in
          let literal_wrapper =
            match literal_wrapper with
            | None -> []
            | Some { field_name; literal_value } ->
              Json_gen.make_literal_wrapper_conv ~field_name ~literal_value
                ~type_name:typ_.name
          in
          let typ_ =
            { typ_ with data = Module.rename_invalid_fields Impl typ_.data }
          in
          let json_convs_for_t =
            match d.data with
            | Alias (Poly_variant data) ->
              poly_enum_conv { d with Named.data }
              |> List.map ~f:(Named.map ~f:(fun v -> Ml.Module.Value v))
            | _ -> []
          in
          poly_vars_and_convs
          @ [ { typ_ with data = Ml.Module.Type_decl typ_.data } ]
          @ json_convs_for_t @ Create.impl_of_type typ_ @ literal_wrapper)
    in
    let module_ bindings = { Ml.Module.name; bindings } in
    { Ml.Kind.intf = module_ intf; impl = module_ impl }
end

let expand_super_classes db ts =
  let interface_fields (i : Resolved.interface) =
    let rec interface init (i : Resolved.interface) =
      let init =
        List.fold_left i.extends ~init ~f:(fun init a ->
            match a with
            | Prim.Resolved r -> type_ init (Entities.find db r).data
            | _ -> assert false)
      in
      init @ i.fields
    and type_ init (i : Resolved.decl) : Resolved.field list =
      match i with
      | Enum_anon _ -> assert false
      | Type (Record fields) -> List.rev_append fields init
      | Type _ -> assert false
      | Interface i -> interface init i
    in
    interface [] i
  in
  List.map ts ~f:(fun (r : Resolved.t) ->
      let data =
        match r.data with
        | Interface i ->
          let fields = interface_fields i in
          Resolved.Interface { i with fields; extends = [] }
        | r -> r
      in
      { r with data })

(* extract all resovled identifiers *)
class name_idents =
  object
    inherit [Ident.t list] Resolved.fold

    method! ident i ~init =
      match i with
      | Resolved r -> (
        match r.kind with
        | Name -> r :: init
        | Type_variable -> init)
      | _ -> init
  end

let resolve_and_pp_typescript (ts : Unresolved.t list) =
  let ts = List.map ts ~f:preprocess in
  let ts, db = Typescript.resolve_all ts in
  let db = Entities.of_map db ts in
  match
    let idents = new name_idents in
    Ident.Top_closure.top_closure ts
      ~key:(fun x -> Entities.rev_find db x)
      ~deps:(fun x -> idents#t x ~init:[] |> List.map ~f:(Entities.find db))
  with
  | Error cycle ->
    let cycle = List.map cycle ~f:(fun (x : Resolved.t) -> x.name) in
    Code_error.raise "Unexpected cycle"
      [ ("cycle", Dyn.Encoder.(list string) cycle) ]
  | Ok ts -> (db, ts)

let of_resolved_typescript db (ts : Resolved.t list) =
  let ts = expand_super_classes db ts in
  let simple_enums, everything_else =
    List.filter_partition_map ts ~f:(fun (t : Resolved.t) ->
        if List.mem skipped_ts_decls t.name ~equal:String.equal then
          Skip
        else
          match t.data with
          | Enum_anon data -> Left { t with data }
          | Interface _
          | Type _ ->
            Right t)
  in
  let simple_enums =
    List.map simple_enums ~f:(fun (t : _ Named.t) ->
        (* "open" enums need an `Other constructor *)
        let allow_other = t.name = "CodeActionKind" in
        let data =
          List.filter_map t.data ~f:(fun (constr, v) ->
              match (v : Ts_types.Enum.case) with
              | Literal l -> Some (constr, l)
              | Alias _ ->
                (* TODO we don't handle these for now *)
                None)
        in
        enum_module ~allow_other { t with data })
  in
  let everything_else =
    List.map everything_else ~f:(fun (t : _ Named.t) ->
        let mod_ = Expanded.of_ts t in
        Gen.module_ db mod_)
  in
  simple_enums @ everything_else
  |> List.map ~f:(fun (decl : _ Ml.Kind.pair) ->
         let decl =
           let intf = Json_gen.add_json_conv_for_t decl.intf in
           { decl with intf }
         in
         Module.use_json_conv_types decl)

let of_typescript ts =
  let db, ts = resolve_and_pp_typescript ts in
  of_resolved_typescript db ts

let output modules ~kind out =
  let open Ml.Kind in
  let intf, impl =
    List.map modules ~f:(fun m ->
        let { intf; impl } = Module.pp m in
        (intf, impl))
    |> List.unzip
  in
  let def = { intf; impl } in
  let def = Ml.Kind.Map.map def ~f:(Pp.concat ~sep:Pp.newline) in
  let pp = Ml.Kind.Map.get def kind in
  pp_file pp out

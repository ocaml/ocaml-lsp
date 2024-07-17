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
  ; "TextDocumentFilter"
  ; "PrepareRenameResult"
  ; "LSPAny"
  ; "LSPObject"
  ; "LSPArray"
  ; "LSPErrorCodes"
  ; "NotebookDocumentSyncOptions"
  ; "NotebookDocumentFilter"
  ; "NotebookDocumentSyncRegistrationOptions"
  ; "URI"
  ]
;;

(* XXX this is temporary until we support the [supportsCustomValues] field *)
let with_custom_values =
  [ "FoldingRangeKind"; "CodeActionKind"; "PositionEncodingKind"; "WatchKind" ]
;;

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
    | List (Record d) | Record d -> record d
    | Sum [ _; Record d ] -> record d
    | _ -> None
  ;;

  class discovered_types =
    object
      inherit [binding Named.t list] Resolved.fold as super

      (** Every record valued field introduces a new type

          TODO handle the case where two fields share a type *)
      method! field f ~init =
        let init =
          match f.data with
          | Pattern _ -> init
          | Single { optional = _; typ } ->
            (match new_binding_of_typ typ with
             | None -> init
             | Some data ->
               let new_record = { f with data } in
               if List.mem ~equal:Poly.equal init new_record
               then init
               else new_record :: init)
        in
        super#field f ~init
    end

  let bindings (r : Resolved.t) =
    let t : binding Named.t =
      let data =
        match r.data with
        | Enum_anon _ -> assert false
        | Interface i -> Interface i
        | Type typ ->
          (match new_binding_of_typ typ with
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
  ;;

  let of_ts (r : Resolved.t) : t =
    let name = Ml.Module.Name.of_string (String.capitalize_ascii r.name) in
    { Ml.Module.name; bindings = bindings r }
  ;;
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

  let type_decls name (type_decls : Ml.Type.decl Named.t list Ml.Kind.Map.t) : t =
    let module_ bindings = { Ml.Module.name; bindings } in
    let intf : Module.sig_ Module.t =
      List.map type_decls.intf ~f:(fun (td : Ml.Type.decl Named.t) ->
        { td with Named.data = (Ml.Module.Type_decl td.data : Ml.Module.sig_) })
      |> module_
    in
    let impl =
      List.map type_decls.impl ~f:(fun (td : Ml.Type.decl Named.t) ->
        { td with Named.data = Ml.Module.Type_decl td.data })
      |> module_
    in
    { Ml.Kind.intf; impl }
  ;;

  let add_private_values (t : t) bindings : t =
    let bindings =
      List.map bindings ~f:(fun (v : _ Named.t) ->
        { v with Named.data = Ml.Module.Value v.data })
    in
    let impl = { t.impl with bindings = t.impl.bindings @ bindings } in
    { t with impl }
  ;;

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
            if Ml.is_kw f.name
            then (
              let attrs =
                match kind with
                | Impl -> ("key", [ sprintf "%S" f.name ]) :: f.attrs
                | Intf -> f.attrs
              in
              { f with name = f.name ^ "_"; attrs })
            else f
          in
          super#field x f

        method! assoc x k v = self#t x (App (Path json_assoc_t, [ k; v ]))
      end
    in
    fun kind t -> (map kind)#decl () t |> fst
  ;;

  let use_json_conv_types =
    let map =
      let open Ml.Type in
      object (self)
        inherit [unit, unit] Ml.Type.mapreduce as super
        method empty = ()
        method plus () () = ()

        method! optional x t =
          if t = Json_gen.json_t
          then super#optional x t
          else (
            let opt = Ml.Path.Dot (Dot (Ident "Json", "Nullable_option"), "t") in
            self#t x (App (Path opt, [ t ])))

        method! field x f =
          let f =
            match f.typ with
            | Optional t ->
              if t = Json_gen.json_t
              then { f with attrs = ("yojson.option", []) :: f.attrs }
              else
                { f with
                  attrs =
                    ("default", [ "None" ])
                    :: ("yojson_drop_default", [ "( = )" ])
                    :: f.attrs
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
              match (x.data : Module.impl) with
              | Type_decl decl -> Ml.Module.Type_decl (map#decl () decl |> fst)
              | x -> x
            in
            { x with data })
        in
        { t.impl with bindings }
      in
      { t with impl }
  ;;

  let pp (t : t) ~kind =
    match (kind : Ml.Kind.t) with
    | Intf -> Ml.Module.pp_sig t.intf
    | Impl -> Ml.Module.pp_impl t.impl
  ;;

  let pp t = { Ml.Kind.intf = pp t ~kind:Intf; impl = pp t ~kind:Impl }
end

let enum_module ~allow_other ({ Named.name; data = constrs } as t) =
  let json_bindings = Json_gen.Enum.conv ~allow_other ~poly:false { t with name = "t" } in
  let t =
    let data =
      let constrs = List.map constrs ~f:(fun (name, _) -> Ml.Type.constr ~name []) in
      let constrs =
        if allow_other
        then
          (* [String] is a hack. It could be a differnt type, but it isn't in
             practice *)
          constrs @ [ Ml.Type.constr ~name:"Other" [ Ml.Type.Prim String ] ]
        else constrs
      in
      Ml.Type.Variant constrs
    in
    { Named.name = "t"; data }
  in
  let type_decls = Ml.Kind.Map.make_both [ t ] in
  let module_ = Module.type_decls (Ml.Module.Name.of_string name) type_decls in
  Module.add_private_values module_ json_bindings
;;

module Entities = struct
  type t = (Ident.t * Resolved.t) list

  let find db e : _ Named.t =
    match List.assoc db e with
    | Some s -> s
    | None -> Code_error.raise "Entities.find: unable to find" [ "e", Ident.to_dyn e ]
  ;;

  let of_map map ts =
    List.map ts ~f:(fun (r : Resolved.t) -> String.Map.find_exn map r.name, r)
  ;;

  let rev_find (db : t) (resolved : Resolved.t) : Ident.t =
    match
      List.filter_map db ~f:(fun (id, r) ->
        if r.name = resolved.name then Some id else None)
    with
    | [] -> Code_error.raise "rev_find: resolved not found" []
    | [ x ] -> x
    | _ :: _ -> Code_error.raise "re_vind: duplicate entries" []
  ;;
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
  val record_
    :  Entities.t
    -> Resolved.field list Named.t
    -> Ml.Type.decl Named.t * literal_field list

  (** Extract all untagged unions in field position. These will be turned into
      polymorphic variants using a naming scheme for the tags. *)
  val extract_poly_vars : Ml.Type.decl -> Ml.Type.decl * Ml.Type.constr list Named.t list
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
    fun set -> List.for_all constrs ~f:(fun e -> List.mem set e ~equal:Poly.equal)
  ;;

  let id = Type.name "Jsonrpc.Id.t"

  let is_same_as_id =
    let sort = List.sort ~compare:Poly.compare in
    let constrs =
      [ Prim.String; Number ] |> List.map ~f:(fun s -> Resolved.Ident s) |> sort
    in
    fun cs -> List.equal ( = ) constrs (sort cs)
  ;;

  (* Any type that includes null needs to be extracted to be converted to an
     option *)
  let remove_null cs =
    let is_null x =
      match x with
      | Resolved.Ident Prim.Null -> Either.Left x
      | _ -> Right x
    in
    let nulls, non_nulls = List.partition_map ~f:is_null cs in
    match nulls with
    | [] -> `No_null_present
    | _ :: _ :: _ -> assert false
    | [ _ ] -> `Null_removed non_nulls
  ;;

  let make_typ db { Named.name; data = t } =
    let rec type_ topmost_field_name (t : Resolved.typ) =
      match t with
      | Ident Uinteger -> Type.int (* XXX shall we use a dedicated uinteger eventually? *)
      | Ident Number -> Type.int
      | Ident String -> Type.string
      | Ident Bool -> Type.bool
      | Ident Object -> Type.json_object
      | Ident Self -> Type.t (* XXX wrong *)
      | Ident Any -> Type.json
      | Ident Null -> assert false
      | Ident List -> Type.list Type.json
      | Ident Uri | Ident (Resolved { id = _; name = "URI" }) ->
        Type.module_t "DocumentUri"
      | Ident (Resolved { id = _; name = "LSPAny" }) -> Type.json
      | Ident (Resolved { id = _; name = "LSPObject" }) -> Type.json_object
      | Ident (Resolved r) ->
        let entity = Entities.find db r in
        Type.module_t entity.name
      | List (Ident (Uinteger | Number)) when topmost_field_name = Some "data" ->
        Type.array Type.int
      | List t -> Type.list (type_ topmost_field_name t)
      | Tuple ts -> Type.Tuple (List.map ~f:(type_ topmost_field_name) ts)
      | Sum s -> sum topmost_field_name s
      | App _ | Literal _ -> Type.void
      | Record r -> record r
    and sum topmost_field_name s =
      if is_same_as_json s
      then Type.json
      else (
        match remove_null s with
        | `No_null_present -> if is_same_as_id s then id else poly topmost_field_name s
        | `Null_removed [ s ] -> Type.Optional (type_ topmost_field_name s)
        | `Null_removed [] -> assert false
        | `Null_removed cs -> Type.Optional (sum topmost_field_name cs))
    and simplify_record (fields : Resolved.field list) =
      (* A record with only a pattern field is simplified to an association
         list *)
      match fields with
      | [ { Named.name; data = Pattern { pat; typ } } ] ->
        let topmost_field_name = Some name in
        let key = type_ topmost_field_name pat in
        let data = type_ topmost_field_name typ in
        Some (Type.assoc_list ~key ~data)
      | [] -> Some Type.json_object
      | _ -> None
    and record fields =
      match simplify_record fields with
      | None -> Type.name name
      | Some a -> a
    and poly topmost_field_name s : Ml.Type.t =
      let type_ = type_ topmost_field_name in
      try
        Poly_variant
          (List.map s ~f:(fun t ->
             let name, constrs =
               match (t : Resolved.typ) with
               | Ident Self | Ident Null -> assert false
               | Ident String -> "String", [ type_ t ]
               | Ident Number -> "Int", [ type_ t ]
               | Ident Object -> "Assoc", [ type_ t ]
               | Ident Bool -> "Bool", [ type_ t ]
               | List _ | Ident List -> "List", [ type_ t ]
               | Ident (Resolved r) -> (Entities.find db r).name, [ type_ t ]
               | Tuple [ Ident Uinteger; Ident Uinteger ] -> "Offset", [ type_ t ]
               | Literal (String x) -> x, []
               | Record _ ->
                 let topmost_field_name = Option.value_exn topmost_field_name in
                 topmost_field_name, [ type_ t ]
               | _ -> raise Exit
             in
             Type.constr ~name constrs))
      with
      | Exit -> Type.unit
    in
    type_ (Some name) t
  ;;

  let make_field db (field : Resolved.field) =
    match field.data with
    | Pattern { pat; typ } ->
      let key = make_typ db { Named.name = field.name; data = pat } in
      let data = make_typ db { Named.name = field.name; data = typ } in
      let typ = Type.assoc_list ~key ~data in
      Either.Left (Ml.Type.field typ ~name:field.name)
    | Single { typ = Literal s; optional = false } ->
      let literal_value =
        match s with
        | String s -> s
        | _ -> assert false
      in
      Right { literal_value; field_name = field.name }
    | Single { typ; optional } ->
      let typ = make_typ db { Named.name = field.name; data = typ } in
      let typ = if optional then Type.Optional typ else typ in
      Left (Ml.Type.field typ ~name:field.name)
  ;;

  let record_ db { Named.name; data = (fields : Resolved.field list) } =
    let data, literals =
      match fields with
      | [ { Named.name; data = Pattern { pat; typ } } ] ->
        let key = make_typ db { Named.name; data = pat } in
        let data = make_typ db { Named.name; data = typ } in
        Type.Alias (Type.assoc_list ~key ~data), []
      | [] -> Type.Alias Type.json_object, []
      | _ ->
        let fields, literals = List.partition_map fields ~f:(make_field db) in
        Type.Record fields, literals
    in
    { Named.name; data }, literals
  ;;

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
            replacement, self#plus m [ { Named.name; data = constrs } ]
      end
    in
    extract#decl None s
  ;;
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
  ;;

  let record db ({ Named.name = _; data = _ } as t) =
    let main_type, literals = Mapper.record_ db t in
    Some (main_type, literals)
  ;;

  let poly_enum { Named.name; data = _ } : Type.decl Named.t list =
    [ { Named.name; data = Type.Alias Type.unit } ]
  ;;

  let poly_enum_conv (t : _ Named.t) =
    if List.for_all t.data ~f:(fun (c : Ml.Type.constr) -> List.is_empty c.args)
    then
      (* This is equivalent to an enum *)
      List.map t.data ~f:(fun (c : Ml.Type.constr) -> c.name, Literal.String c.name)
      |> Named.set_data t
      |> Json_gen.Enum.conv ~allow_other:false ~poly:true
    else [ Json_gen.Poly_variant.of_json t; Json_gen.Poly_variant.to_json t ]
  ;;

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
        | Interface data -> record db { r with data = data.fields } |> add_record
        | Poly_enum data -> poly_enum { r with data } |> add_else
        | Alias data -> type_ db { r with data } |> add_else)
    in
    let intf : Ml.Module.sig_ Named.t list =
      List.map type_decls ~f:(function
        | `Record (t, _) -> t
        | `Type t -> t)
      |> List.concat_map ~f:(fun (td : Ml.Type.decl Named.t) ->
        let td = { td with data = Module.rename_invalid_fields Intf td.data } in
        [ { td with Named.data = (Ml.Module.Type_decl td.data : Ml.Module.sig_) } ]
        @ Ml_create.intf_of_type td)
    in
    let impl : Ml.Module.impl Named.t list =
      (* TODO we should make sure to handle duplicate variants extracted *)
      List.concat_map type_decls ~f:(fun d ->
        let d, literal_wrapper =
          match d with
          | `Record (l, [ lw ]) -> l, Some lw
          | `Record (l, []) -> l, None
          | `Record (_, _ :: _) ->
            assert false
            (* we don't support multiple literals in a single record for
               now *)
          | `Type l -> l, None
        in
        let typ_, poly_vars = Mapper.extract_poly_vars (Named.data d) in
        let poly_vars_and_convs =
          List.concat_map poly_vars ~f:(fun pv ->
            let decl =
              Named.map pv ~f:(fun decl ->
                Ml.Module.Type_decl (Alias (Poly_variant decl)))
            in
            let json_conv =
              poly_enum_conv pv |> List.map ~f:(Named.map ~f:(fun v -> Ml.Module.Value v))
            in
            decl :: json_conv)
        in
        let typ_ = { d with data = typ_ } in
        let literal_wrapper =
          match literal_wrapper with
          | None -> []
          | Some { field_name; literal_value } ->
            Json_gen.make_literal_wrapper_conv
              ~field_name
              ~literal_value
              ~type_name:typ_.name
        in
        let typ_ = { typ_ with data = Module.rename_invalid_fields Impl typ_.data } in
        let json_convs_for_t =
          match d.data with
          | Alias (Poly_variant data) ->
            poly_enum_conv { d with Named.data }
            |> List.map ~f:(Named.map ~f:(fun v -> Ml.Module.Value v))
          | _ -> []
        in
        poly_vars_and_convs
        @ [ { typ_ with data = Ml.Module.Type_decl typ_.data } ]
        @ json_convs_for_t
        @ Ml_create.impl_of_type typ_
        @ literal_wrapper)
    in
    let module_ bindings = { Ml.Module.name; bindings } in
    { Ml.Kind.intf = module_ intf; impl = module_ impl }
  ;;
end

(* extract all resovled identifiers *)
class name_idents =
  object
    inherit [Ident.t list] Resolved.fold

    method! ident i ~init =
      match i with
      | Resolved r -> r :: init
      | _ -> init
  end

let resolve_typescript (ts : Unresolved.t list) =
  let ts, db = Typescript.resolve_all ts in
  let db = Entities.of_map db ts in
  match
    let idents = new name_idents in
    Ident.Top_closure.top_closure
      ts
      ~key:(fun x -> Entities.rev_find db x)
      ~deps:(fun x -> idents#t x ~init:[] |> List.map ~f:(Entities.find db))
  with
  | Error cycle ->
    let cycle = List.map cycle ~f:(fun (x : Resolved.t) -> x.name) in
    Code_error.raise "Unexpected cycle" [ "cycle", Dyn.(list string) cycle ]
  | Ok ts -> db, ts
;;

let of_resolved_typescript db (ts : Resolved.t list) =
  let simple_enums, everything_else =
    List.filter_partition_map ts ~f:(fun (t : Resolved.t) ->
      if List.mem skipped_ts_decls t.name ~equal:String.equal
      then Skip
      else (
        match t.data with
        | Enum_anon data -> Left { t with data }
        | Interface _ | Type _ -> Right t))
  in
  let simple_enums =
    List.map simple_enums ~f:(fun (t : _ Named.t) ->
      (* "open" enums need an `Other constructor *)
      let allow_other = List.mem ~equal:String.equal with_custom_values t.name in
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
;;

let of_typescript ts =
  let db, ts = resolve_typescript ts in
  of_resolved_typescript db ts
;;

open! Import
open! Ts_types

let preprocess =
  let union_fields l1 l2 ~f =
    let of_map =
      String.Map.of_list_map_exn ~f:(fun (x : _ Named.t) -> (x.name, x))
    in
    String.Map.union (of_map l1) (of_map l2) ~f |> String.Map.values
  in
  let module M = struct
    exception Skip
  end in
  let open M in
  let traverse =
    object (self)
      inherit Resolved.map as super

      val mutable current_name = None

      method name =
        match current_name with
        | None -> assert false
        | Some n -> n

      method! field x =
        if x.name <> "documentChanges" then
          super#field x
        else
          (* This gross hack is needed for the documentChanges field. We can
             ignore the first constructor since it's completely representable
             with the second one. *)
          match x.data with
          | Single
              { typ =
                  Sum
                    [ Resolved.List
                        (Ident
                          (Resolved { Named.name = "TextDocumentEdit"; _ }))
                    ; (List _ as typ)
                    ]
              ; optional
              } ->
            let data = Resolved.Single { typ; optional } in
            super#field { x with data }
          | _ -> super#field x

      method! typ x =
        match x with
        | Sum [ Resolved.Record f1; Record f2 ]
          when self#name = "TextDocumentContentChangeEvent" ->
          let t =
            Resolved.Record
              (union_fields f1 f2 ~f:(fun k t1 t2 ->
                   assert (k = "text");
                   assert (t1 = t2);
                   Some t1))
          in
          super#typ t
        | t -> super#typ t

      method! t x =
        match x.name with
        | "InitializedParams"
        | "NotificationMessage"
        | "RequestMessage"
        | "ResponseError"
        | "ResponseMessage"
        | "Message"
        | "MarkedString" ->
          raise_notrace Skip
        | name ->
          current_name <- Some name;
          super#t x

      method! interface i =
        let extends =
          List.filter i.extends ~f:(fun x ->
              match x with
              | Prim.Resolved r -> (
                match r.name with
                | "WorkDoneProgressParams"
                | "PartialResultParams" ->
                  false
                | _ -> true )
              | _ -> true)
        in
        let fields =
          (* We ignore this field until we switch to our own code gen. *)
          if self#name = "FormattingOptions" then
            List.filter i.fields ~f:(fun (f : Resolved.field) ->
                match f.data with
                | Pattern _ when f.name = "key" -> false
                | _ -> true)
          else
            i.fields
        in
        super#interface { i with extends; fields }
    end
  in
  fun i -> try Some (traverse#t i) with Skip -> None

module Expanded = struct
  [@@@ocaml.warning "-37"]

  type binding =
    | Record of Resolved.field list
    | Interface of Resolved.interface
    | Poly_enum of Resolved.typ list
    | Alias of Resolved.typ

  type t = binding Ml.Module.t

  let new_binding_of_typ (x : Resolved.typ) : binding option =
    match x with
    | Record [ { Named.name = _; data = Pattern _ } ] -> None
    | Record d -> Some (Record d)
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
            | Some data -> { f with data } :: init )
        in
        super#field f ~init
    end

  let bindings (r : Resolved.t) =
    let t = { r with name = "t" } in
    let t : binding Named.t =
      match r.data with
      | Enum_anon _ -> assert false
      | Interface i -> { t with data = Interface i }
      | Type typ -> (
        match new_binding_of_typ typ with
        | Some data -> { t with data }
        | None -> { t with data = Alias typ } )
    in
    let init = [ t ] in
    match r.data with
    | Enum_anon _ -> assert false
    | Type typ -> (new discovered_types)#typ typ ~init
    | Interface intf -> (new discovered_types)#typ (Record intf.fields) ~init

  let of_ts (r : Resolved.t) : t =
    { Ml.Module.name = r.name; bindings = bindings r }
end

module Json = struct
  let pat_of_literal (t : Literal.t) : Ml.Expr.pat =
    let open Ml.Expr in
    let tag, args =
      match t with
      | Literal.String s -> ("String", Pat (Ml.Expr.String s))
      | Int i -> ("Int", Pat (Ml.Expr.Int i))
      | Float _ -> assert false
    in
    Pat (Constr { poly = true; tag; args = [ args ] })

  let constr_of_literal (t : Literal.t) : Ml.Expr.t =
    let open Ml.Expr in
    let tag, args =
      match t with
      | Literal.String s -> ("String", Create (Ml.Expr.String s))
      | Int i -> ("Int", Create (Ml.Expr.Int i))
      | Float _ -> assert false
    in
    Create (Constr { poly = true; tag; args = [ args ] })

  let json_error_pat name =
    let open Ml.Expr in
    ( Wildcard
    , App
        ( Create (Ident "Json.error")
        , [ Unnamed (Create (String name)); Unnamed (Create (Ident "json")) ] )
    )

  let is_json_constr (constr : Ml.Type.constr) =
    List.mem constr.name ~set:[ "String"; "Int"; "Bool" ]

  module Name = struct
    let of_ = sprintf "%s_of_yojson"

    let to_ = sprintf "yojson_of_%s"

    let conv = function
      | `To -> to_
      | `Of -> of_
  end

  let of_json ~name expr =
    let open Ml.Expr in
    let pat = [ (Unnamed "json", Ml.Type.json) ] in
    let data = { Ml.Expr.pat; type_ = Ml.Type.name name; body = expr } in
    let name = Name.of_ name in
    { Named.name; data }

  let to_json ~name expr =
    let open Ml.Expr in
    let pat = [ (Unnamed name, Ml.Type.name name) ] in
    let data = { Ml.Expr.pat; type_ = Ml.Type.json; body = expr } in
    let name = Name.to_ name in
    { Named.name; data }
end

module Module = struct
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

  let add_json_conv_for_t (t : t) =
    let conv_t = { Named.name = "t"; data = Module.Json_conv_sig } in
    let intf = { t.intf with bindings = t.intf.bindings @ [ conv_t ] } in
    { t with intf }

  let pp (t : t) ~kind =
    match (kind : Ml.Kind.t) with
    | Intf -> Ml.Module.pp_sig t.intf
    | Impl -> Ml.Module.pp_impl t.impl

  let pp t = { Ml.Kind.intf = pp t ~kind:Intf; impl = pp t ~kind:Impl }
end

let pp_file pp ch =
  let fmt = Format.formatter_of_out_channel ch in
  Pp.render_ignore_tags fmt pp;
  Format.pp_print_flush fmt ()

module Enum = struct
  let of_json ~poly { Named.name; data = constrs } =
    let open Ml.Expr in
    let body =
      let clauses =
        List.map constrs ~f:(fun (constr, literal) ->
            let pat = Json.pat_of_literal literal in
            let tag = constr in
            (pat, Create (Constr { tag; poly; args = [] })))
      in
      Match (Create (Ident "json"), clauses @ [ Json.json_error_pat name ])
    in
    Json.of_json ~name body

  let to_json ~poly { Named.name; data = constrs } =
    let open Ml.Expr in
    let body =
      let clauses =
        List.map constrs ~f:(fun (constr, literal) ->
            let pat = Pat (Constr { tag = constr; poly; args = [] }) in
            (pat, Json.constr_of_literal literal))
      in
      Match (Create (Ident name), clauses)
    in
    Json.to_json ~name body

  let conv ~poly t =
    let to_json = to_json ~poly t in
    let of_json = of_json ~poly t in
    [ to_json; of_json ]

  let module_ ({ Named.name; data = constrs } as t) =
    let json_bindings = conv ~poly:false { t with name = "t" } in
    let t =
      let data =
        Ml.Type.Variant
          (List.map constrs ~f:(fun (name, _) -> Ml.Type.constr ~name []))
      in
      { Named.name = "t"; data }
    in
    let type_decls = Ml.Kind.Map.make_both [ t ] in
    let module_ = Module.type_decls name type_decls in
    Module.add_private_values module_ json_bindings
    |> Module.add_json_conv_for_t
end

module Poly_variant = struct
  type constrs =
    { json_constrs : Ml.Type.constr list
    ; untagged_constrs : Ml.Type.constr list
    }

  let split_clauses constrs =
    let json_constrs, untagged_constrs =
      List.partition_map constrs ~f:(fun x ->
          if Json.is_json_constr x then
            Left x
          else
            Right x)
    in
    { json_constrs; untagged_constrs }

  let conv_of_constr target (utc : Ml.Type.constr) =
    let conv (name : string) =
      let conv name = Json.Name.conv target name in
      match String.rsplit2 ~on:'.' name with
      | None -> conv name
      | Some (module_, name) -> sprintf "%s.%s" module_ (conv name)
    in
    let open Ml.Expr in
    let json_mod n =
      match target with
      | `To -> Ident ("Json.To." ^ n)
      | `Of -> Ident ("Json.Of." ^ n)
    in
    let conv t = Create (Ident (conv t)) in
    match (utc.args : Ml.Type.t list) with
    | [ Named t ] -> conv t
    | [ List (Named t) ] -> App (Create (json_mod "list"), [ Unnamed (conv t) ])
    | [ Tuple [ Prim Int; Prim Int ] ] -> Create (json_mod "int_pair")
    | [] -> assert false
    | _ ->
      Code_error.raise "untagged" [ ("utc.name", Dyn.Encoder.string utc.name) ]

  let json_clauses json_constrs =
    List.map json_constrs ~f:(fun (c : Ml.Type.constr) ->
        let open Ml.Expr in
        let constr arg = Constr { tag = c.name; poly = true; args = [ arg ] } in
        let pat = Pat (constr (Pat (Ident "j"))) in
        let expr : t = Create (constr (Create (Ident "j"))) in
        (pat, expr))

  let to_json { Named.name; data = constrs } =
    let { json_constrs; untagged_constrs } = split_clauses constrs in
    let open Ml.Expr in
    let json_clauses = json_clauses json_constrs in
    let untagged_clauses =
      List.map untagged_constrs ~f:(fun (utc : Ml.Type.constr) ->
          let constr arg =
            Constr { tag = utc.name; poly = true; args = [ arg ] }
          in
          let pat = Pat (constr (Pat (Ident "s"))) in
          let expr =
            App (conv_of_constr `To utc, [ Unnamed (Create (Ident "s")) ])
          in
          (pat, expr))
    in
    let expr = Match (Create (Ident name), json_clauses @ untagged_clauses) in
    Json.to_json ~name expr

  let of_json { Named.name; data = constrs } =
    let { json_constrs; untagged_constrs } = split_clauses constrs in
    let open Ml.Expr in
    let clauses = json_clauses json_constrs in
    let untagged =
      let args =
        let constrs =
          List.map untagged_constrs ~f:(fun (utc : Ml.Type.constr) ->
              let create =
                let of_json =
                  App
                    (conv_of_constr `Of utc, [ Unnamed (Create (Ident "json")) ])
                in
                Create
                  (Constr { tag = utc.name; poly = true; args = [ of_json ] })
              in
              Fun ([ Unnamed (Pat (Ident "json")) ], create))
        in
        Create (List constrs)
      in
      App
        ( Create (Ident "Json.Of.untagged_union")
        , [ Unnamed (Create (String name))
          ; Unnamed args
          ; Unnamed (Create (Ident "json"))
          ] )
    in
    let expr =
      match (json_constrs, untagged_constrs) with
      | [], [] -> assert false
      | [], _ -> untagged
      | _, [] ->
        Match (Create (Ident "json"), clauses @ [ Json.json_error_pat name ])
      | _ :: _, _ :: _ ->
        Match (Create (Ident "json"), clauses @ [ (Wildcard, untagged) ])
    in
    Json.of_json ~name expr
end

module Mapper = struct
  module Type = Ml.Type

  let is_same_as_json =
    let constrs =
      [ Prim.Null; String; Bool; Number; Object; List ]
      |> List.map ~f:(fun s -> Resolved.Ident s)
    in
    fun set -> List.for_all constrs ~f:(List.mem ~set)

  let id = Type.name "Jsonrpc.Id.t"

  let is_same_as_id =
    let sort = List.sort ~compare:Poly.compare in
    let constrs =
      [ Prim.String; Number ] |> List.map ~f:(fun s -> Resolved.Ident s) |> sort
    in
    fun cs -> List.equal ( = ) constrs (sort cs)

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

  let make_typ name t =
    let rec type_ (t : Resolved.typ) =
      match t with
      | Ident Number -> Type.int
      | Ident String -> Type.string
      | Ident Bool -> Type.bool
      | Ident Any
      | Ident Object ->
        Type.json
      | Ident Self -> Type.t (* XXX wrong *)
      | Ident Null -> assert false
      | Ident List -> Type.list Type.json
      | Ident (Resolved r) -> Type.module_t r.name
      | List t -> Type.list (type_ t)
      | Tuple ts -> Type.Tuple (List.map ~f:type_ ts)
      | Sum s -> sum s
      | App _
      | Literal _ ->
        Type.void
      | Record r -> record r
    and sum s =
      if is_same_as_json s then
        Type.json
      else
        match remove_null s with
        | `No_null_present ->
          if is_same_as_id s then
            id
          else
            poly s
        | `Null_removed [ s ] -> Type.Optional (type_ s)
        | `Null_removed [] -> assert false
        | `Null_removed cs -> Type.Optional (sum cs)
    and simplify_record (fields : Resolved.field list) =
      match fields with
      | [ { Named.name = _; data = Pattern { pat; typ } } ] ->
        let key = type_ pat in
        let data = type_ typ in
        Some (Type.assoc_list ~key ~data)
      | _ -> None
    and record fields =
      match simplify_record fields with
      | None -> Type.name name
      | Some a -> a
    and poly s : Ml.Type.t =
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
                 | Ident (Resolved r) -> (r.name, [ type_ t ])
                 | Tuple [ Ident Number; Ident Number ] ->
                   ("Offset", [ type_ t ])
                 | Literal (String x) -> (x, [])
                 | _ -> raise Exit
               in
               Type.constr ~name constrs))
      with Exit -> Type.unit
    in
    type_ t

  let make_field (field : Resolved.field) =
    match field.data with
    | Pattern { pat; typ } ->
      let key = make_typ field.name pat in
      let data = make_typ field.name typ in
      Type.assoc_list ~key ~data
    | Resolved.Single { typ; optional } ->
      let typ = make_typ field.name typ in
      if optional then
        Optional typ
      else
        typ

  let record_ name (fields : Resolved.field list) =
    let data =
      match fields with
      | [ { Named.name; data = Pattern { pat; typ } } ] ->
        let key = make_typ name pat in
        let data = make_typ name typ in
        Type.Alias (Type.assoc_list ~key ~data)
      | _ ->
        Type.Record
          (List.map fields ~f:(fun (field : Resolved.field) ->
               let typ = make_field field in
               Ml.Type.field typ ~name:field.name))
    in
    { Named.name; data }

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
          let name = Option.value_exn env in
          let replacement = Ml.Type.name name in
          let constrs, m = self#fold_left_map ~f:(self#constr env) constrs in
          (replacement, self#plus m [ { Named.name; data = constrs } ])
      end
    in
    extract#decl None s
end

module Gen = struct
  module Type = Ml.Type

  let type_ { Named.name; data = typ } =
    let main_type =
      let typ = Mapper.make_typ name typ in
      { Named.name; data = Type.Alias typ }
    in
    [ main_type ]

  let record { Named.name; data = fields } =
    let main_type = Mapper.record_ name fields in
    match fields with
    | [] -> []
    | _ :: _ -> [ main_type ]

  let interface_fields (i : Resolved.interface) =
    let rec interface init (i : Resolved.interface) =
      let init =
        List.fold_left i.extends ~init ~f:(fun init a ->
            match a with
            | Prim.Resolved r -> type_ init r.data
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

  let poly_enum { Named.name; data = _ } : Type.decl Named.t list =
    [ { Named.name; data = Type.Alias Type.unit } ]

  let poly_enum_conv (t : _ Named.t) =
    if List.for_all t.data ~f:(fun (c : Ml.Type.constr) -> List.is_empty c.args)
    then
      (* This is equivalent to an enum *)
      List.map t.data ~f:(fun (c : Ml.Type.constr) ->
          (c.name, Literal.String c.name))
      |> Named.set_data t |> Enum.conv ~poly:true
    else
      [ Poly_variant.of_json t; Poly_variant.to_json t ]

  (* This is the more complex case *)

  let module_ { Ml.Module.name; bindings } : Module.t =
    let type_decls =
      List.concat_map bindings ~f:(fun (r : Expanded.binding Named.t) ->
          match r.data with
          | Record data -> record { r with data }
          | Interface data -> record { r with data = interface_fields data }
          | Poly_enum data -> poly_enum { r with data }
          | Alias data -> type_ { r with data })
    in
    let intf : Ml.Module.sig_ Named.t list =
      List.map type_decls ~f:(fun (td : Ml.Type.decl Named.t) ->
          { td with
            Named.data = (Ml.Module.Type_decl td.data : Ml.Module.sig_)
          })
    in
    let impl : Ml.Module.impl Named.t list =
      (* TODO we should make sure to handle duplicate variants extracted *)
      List.concat_map type_decls ~f:(fun d ->
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
          let literal_wrapper = [] in
          poly_vars_and_convs
          @ [ { d with data = Ml.Module.Type_decl typ_ } ]
          @ literal_wrapper)
    in
    let module_ bindings = { Ml.Module.name; bindings } in
    { Ml.Kind.intf = module_ intf; impl = module_ impl }
end

class idents =
  object
    inherit [Resolved.t list] Resolved.fold

    method! ident i ~init =
      match i with
      | Resolved r -> r :: init
      | _ -> init
  end

let of_typescript (ts : Resolved.t list) =
  match
    Top_closure.String.top_closure ts
      ~key:(fun (x : Resolved.t) -> x.name)
      ~deps:(fun (x : Resolved.t) -> (new idents)#t x ~init:[])
  with
  | Error _ -> Code_error.raise "Unexpected cycle" []
  | Ok ts ->
    List.filter_map ts ~f:(fun (t : Resolved.t) ->
        match t.data with
        | Enum_anon data -> Some (Enum.module_ { t with data })
        | _ ->
          let open Option.O in
          let+ pped = preprocess t in
          let mod_ = Expanded.of_ts pped in
          Gen.module_ mod_)

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

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
  ]

(* Super classes to remove because we handle their concerns differently (or not
   at all) *)
let removed_super_classes = [ "WorkDoneProgressParams"; "PartialResultParams" ]

(* The preprocessing stage maps over the typescript AST. It should only do very
   simple clean ups *)
let preprocess =
  let union_fields l1 l2 ~f =
    let of_map =
      String.Map.of_list_map_exn ~f:(fun (x : _ Named.t) -> (x.name, x))
    in
    String.Map.union (of_map l1) (of_map l2) ~f |> String.Map.values
  in
  let traverse =
    object (self)
      inherit Resolved.map as super

      val mutable current_name = None

      method name =
        match current_name with
        | None -> assert false
        | Some n -> n

      method! field x =
        if x.name = "documentChanges" then
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
        else
          super#field x

      method! typ x =
        (* XXX what does this to? I don't see any sums of records in
           TextDocumentContentChangeEvent *)
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

      (* all toplevel declarations that we decide to skip *)
      method! t x =
        current_name <- Some x.name;
        super#t x

      method! interface i =
        (* we don't handle partial results or progress notifications params (for
           now) *)
        let extends =
          List.filter i.extends ~f:(fun x ->
              match x with
              | Prim.Resolved r ->
                not (List.mem removed_super_classes r.name ~equal:String.equal)
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

module Json : sig
  val pat_of_literal : Literal.t -> Ml.Expr.pat

  val json_error_pat : string -> Ml.Expr.pat * Ml.Expr.t

  val of_json : name:string -> Ml.Expr.t -> Ml.Expr.toplevel Named.t

  val to_json : name:string -> Ml.Expr.t -> Ml.Expr.toplevel Named.t

  val constr_of_literal : Literal.t -> Ml.Expr.t

  val is_json_constr : Ml.Type.constr -> bool

  module Name : sig
    val conv : [ `Of | `To ] -> string -> string
  end
end = struct
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
    List.mem [ "String"; "Int"; "Bool" ] constr.name ~equal:String.equal

  module Name = struct
    let of_ = sprintf "%s_of_yojson"

    let to_ = sprintf "yojson_of_%s"

    let conv = function
      | `To -> to_
      | `Of -> of_
  end

  open Ml.Arg

  let of_json ~name expr =
    let pat = [ (Unnamed "json", Ml.Type.json) ] in
    let data = { Ml.Expr.pat; type_ = Ml.Type.name name; body = expr } in
    let name = Name.of_ name in
    { Named.name; data }

  let to_json ~name expr =
    let pat = [ (Unnamed name, Ml.Type.name name) ] in
    let data = { Ml.Expr.pat; type_ = Ml.Type.json; body = expr } in
    let name = Name.to_ name in
    { Named.name; data }
end

module Module : sig
  open Ml

  type t = (Module.sig_ Module.t, Module.impl Module.t) Kind.pair

  val add_private_values : t -> Expr.toplevel Named.t list -> t

  val type_decls : Module.Name.t -> Type.decl Named.t list Kind.Map.t -> t

  (** Add include Json.Jsonable.t signatures *)
  val add_json_conv_for_t : t -> t

  (** Use Json.Nullable_option or Json.Assoc.t where appropriate *)
  val use_json_conv_types : t -> t

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

  let add_json_conv_for_t (t : t) =
    let conv_t =
      { Named.name = "t"
      ; data =
          Ml.Module.Include
            (Module.Name.of_string "Json.Jsonable.S", [ (Named "t", Named "t") ])
      }
    in
    let intf = { t.intf with bindings = t.intf.bindings @ [ conv_t ] } in
    { t with intf }

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
                | Impl -> Attr ("key", [ sprintf "%S" f.name ]) :: f.attrs
                | Intf -> f.attrs
              in
              { f with name = f.name ^ "_"; attrs }
            else
              f
          in
          super#field x f

        method! assoc x k v = self#t x (App (Named "Json.Assoc.t", [ k; v ]))
      end
    in
    fun kind t -> (map kind)#decl () t |> fst

  let use_json_conv_types =
    let map =
      let open Ml.Type in
      let json = Named "Json.t" in
      object (self)
        inherit [unit, unit] Ml.Type.mapreduce as super

        method empty = ()

        method plus () () = ()

        method! optional x t =
          match t with
          | Named "Json.t" -> super#optional x t
          | _ -> self#t x (App (Named "Json.Nullable_option.t", [ t ]))

        method! field x f =
          let f =
            match f.typ with
            | Optional t ->
              if t = json then
                { f with attrs = Attr ("yojson.option", []) :: f.attrs }
              else
                { f with
                  attrs =
                    Attr ("default", [ "None" ])
                    :: Attr ("yojson_drop_default", [ "( = )" ]) :: f.attrs
                }
            | _ -> f
          in
          super#field x f

        method! assoc x k v = self#t x (App (Named "Json.Assoc.t", [ k; v ]))
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
        (* Gross hack because I was too lazy to allow patterns in toplevel exprs *)
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

module Enum : sig
  (* Convert simple enums. Because enums are so simple, we go straight from TS
     to generated Ml *)

  val conv :
       allow_other:bool
    -> poly:bool
    -> (string * Literal.t) list Named.t
    -> Ml.Expr.toplevel Named.t list

  val module_ :
       allow_other:bool
    -> (string * Literal.t) list Named.t
    -> (Ml.Module.sig_ Ml.Module.t, Ml.Module.impl Ml.Module.t) Ml.Kind.pair
end = struct
  let of_json ~allow_other ~poly { Named.name; data = constrs } =
    let open Ml.Expr in
    let body =
      let clauses =
        List.map constrs ~f:(fun (constr, literal) ->
            let pat = Json.pat_of_literal literal in
            let tag = constr in
            (pat, Create (Constr { tag; poly; args = [] })))
      in
      let clauses =
        if allow_other then
          let s = Ident "s" in
          let pat =
            Pat (Constr { tag = "String"; poly = true; args = [ Pat s ] })
          in
          let make =
            Create (Constr { tag = "Other"; poly; args = [ Create s ] })
          in
          clauses @ [ (pat, make) ]
        else
          clauses
      in
      Match (Create (Ident "json"), clauses @ [ Json.json_error_pat name ])
    in
    Json.of_json ~name body

  let to_json ~allow_other ~poly { Named.name; data = constrs } =
    let open Ml.Expr in
    let body =
      let clauses =
        List.map constrs ~f:(fun (constr, literal) ->
            let pat = Pat (Constr { tag = constr; poly; args = [] }) in
            (pat, Json.constr_of_literal literal))
      in
      let clauses =
        if allow_other then
          let s = Ident "s" in
          let pat = Pat (Constr { tag = "Other"; poly; args = [ Pat s ] }) in
          let make =
            Create (Constr { tag = "String"; poly = true; args = [ Create s ] })
          in
          clauses @ [ (pat, make) ]
        else
          clauses
      in
      Match (Create (Ident name), clauses)
    in
    Json.to_json ~name body

  let conv ~allow_other ~poly t =
    let to_json = to_json ~allow_other ~poly t in
    let of_json = of_json ~allow_other ~poly t in
    [ to_json; of_json ]

  let module_ ~allow_other ({ Named.name; data = constrs } as t) =
    let json_bindings = conv ~allow_other ~poly:false { t with name = "t" } in
    let t =
      let data =
        let constrs =
          List.map constrs ~f:(fun (name, _) -> Ml.Type.constr ~name [])
        in
        let constrs =
          if allow_other then
            (* [String] is a hack but it doesn't matter *)
            constrs @ [ Ml.Type.constr ~name:"Other" [ Ml.Type.Prim String ] ]
          else
            constrs
        in
        Ml.Type.Variant constrs
      in
      { Named.name = "t"; data }
    in
    let type_decls = Ml.Kind.Map.make_both [ t ] in
    let module_ =
      Module.type_decls (Ml.Module.Name.of_string name) type_decls
    in
    Module.add_private_values module_ json_bindings
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

module Mapper : sig
  (* Convert typescript types to OCaml types *)

  val make_typ : string -> Resolved.typ -> Ml.Type.t

  val record_ : string -> Resolved.field list -> Ml.Type.decl Named.t

  val extract_poly_vars :
    Ml.Type.decl -> Ml.Type.decl * Ml.Type.constr list Named.t list
end = struct
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

  let make_typ name t =
    let rec type_ (t : Resolved.typ) =
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
      (* A record with only a pattern field is simplified to an association list *)
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
                 | Tuple [ Ident Uinteger; Ident Uinteger ] ->
                   ("Offset", [ type_ t ])
                 | Literal (String x) -> (x, [])
                 | _ -> raise Exit
               in
               Type.constr ~name constrs))
      with
      | Exit -> Type.unit
    in
    type_ t

  let make_field (field : Resolved.field) =
    match field.data with
    | Pattern { pat; typ } ->
      let key = make_typ field.name pat in
      let data = make_typ field.name typ in
      let typ = Type.assoc_list ~key ~data in
      Ml.Type.field typ ~name:field.name
    | Resolved.Single { typ = Literal s; optional = false } ->
      let literal =
        match s with
        | String s -> s
        | _ -> assert false
      in
      Type.kind_field ~literal
    | Resolved.Single { typ; optional } ->
      let typ = make_typ field.name typ in
      let typ =
        if optional then
          Type.Optional typ
        else
          typ
      in
      Ml.Type.field typ ~name:field.name

  let record_ name (fields : Resolved.field list) =
    let data =
      match fields with
      | [ { Named.name; data = Pattern { pat; typ } } ] ->
        let key = make_typ name pat in
        let data = make_typ name typ in
        Type.Alias (Type.assoc_list ~key ~data)
      | _ -> Type.Record (List.map fields ~f:make_field)
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
          match env with
          | None -> super#poly_variant env constrs
          | Some name ->
            let replacement = Ml.Type.name name in
            let constrs, m = self#fold_left_map ~f:(self#constr env) constrs in
            (replacement, self#plus m [ { Named.name; data = constrs } ])
      end
    in
    extract#decl None s
end

module Gen : sig
  val module_ : Expanded.binding Ml.Module.t -> Module.t
end = struct
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
      |> Named.set_data t
      |> Enum.conv ~allow_other:false ~poly:true
    else
      [ Poly_variant.of_json t; Poly_variant.to_json t ]

  let literal_field { Named.name; data = typ_ } =
    match (typ_ : Ml.Type.decl) with
    | Record fs -> (
      match
        List.partition_map fs ~f:(fun f ->
            match Ml.Type.get_kind f with
            | None -> Right f
            | Some lit -> Left (f, lit))
      with
      | [], _ -> None
      | [ (field, lit) ], normal_fields ->
        Some ((field, lit), { Named.name; data = Ml.Type.Record normal_fields })
      | _ -> assert false)
    | _ -> None

  let literal_wrapper ((field : Ml.Type.field), lit) name =
    (* Some json representations require an extra "kind" field set to some
       string constant *)
    let open Ml.Expr in
    let args = List.map ~f:(fun x -> Ml.Arg.Unnamed (Create x)) in
    let to_ =
      let a =
        [ String field.name
        ; String lit
        ; Ident (Json.Name.conv `To name)
        ; Ident name
        ]
      in
      App (Create (Ident "Json.To.literal_field"), args a)
    in
    let of_ =
      let a =
        [ String name
        ; String field.name
        ; String lit
        ; Ident (Json.Name.conv `Of name)
        ; Ident "json"
        ]
      in
      App (Create (Ident "Json.Of.literal_field"), args a)
    in
    [ Json.to_json ~name to_; Json.of_json ~name of_ ]
    |> List.map ~f:(Named.map ~f:(fun v -> Ml.Module.Value v))

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
      List.concat_map type_decls ~f:(fun (td : Ml.Type.decl Named.t) ->
          let td =
            { td with data = Module.rename_invalid_fields Intf td.data }
          in
          let type_ =
            match literal_field td with
            | None -> td
            | Some (_, typ_) -> typ_
          in
          [ { td with
              Named.data = (Ml.Module.Type_decl td.data : Ml.Module.sig_)
            }
          ]
          @ Create.intf_of_type type_)
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
          let typ_, literal_wrapper =
            let typ_ = { d with data = typ_ } in
            match literal_field typ_ with
            | None -> (typ_, [])
            | Some (f, typ_) -> (typ_, literal_wrapper f d.name)
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

(* extract all resovled identifiers *)
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
  | Error cycle ->
    let cycle = List.map cycle ~f:(fun (x : Resolved.t) -> x.name) in
    Code_error.raise "Unexpected cycle"
      [ ("cycle", Dyn.Encoder.(list string) cycle) ]
  | Ok ts ->
    List.filter_map ts ~f:(fun (t : Resolved.t) ->
        if List.mem skipped_ts_decls t.name ~equal:String.equal then
          None
        else
          match t.data with
          | Enum_anon data ->
            (* "open" enums need an `Other constructor *)
            let allow_other = t.name = "CodeActionKind" in
            let data =
              List.filter_map data ~f:(fun (constr, v) ->
                  match v with
                  | Literal l -> Some (constr, l)
                  | Alias _ ->
                    (* TODO we don't handle these for now *)
                    None)
            in
            Some (Enum.module_ ~allow_other { t with data })
          | Interface _
          | Type _ ->
            let pped = preprocess t in
            let mod_ = Expanded.of_ts pped in
            Some (Gen.module_ mod_))
    |> List.map ~f:(fun decl ->
           Module.add_json_conv_for_t decl |> Module.use_json_conv_types)

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

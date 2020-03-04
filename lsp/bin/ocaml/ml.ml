open Import

module Kind = struct
  type t =
    | Intf
    | Impl

  type ('intf, 'impl) pair =
    { intf : 'intf
    ; impl : 'impl
    }

  module Map = struct
    type 'a t = ('a, 'a) pair

    let get { intf; impl } = function
      | Impl -> impl
      | Intf -> intf

    let make_both a = { intf = a; impl = a }

    let iter { intf; impl } ~f =
      f intf;
      f impl

    let map { intf; impl } ~f = { intf = f intf; impl = f impl }

    let both (type a b) (x : a t) (y : b t) : (a * b) t =
      { intf = (x.intf, y.intf); impl = (x.impl, y.impl) }
  end
end

module Type = struct
  [@@@warning "-30"]

  let ident = function
    | "type" -> "type_"
    | "method" -> "method_"
    | "end" -> "end_"
    | s -> s

  type prim =
    | Unit
    | String
    | Int
    | Bool

  type attr =
    | Option
    | Key of string

  type t =
    | Named of string
    | Var of string
    | Prim of prim
    | Tuple of t list
    | Optional of t
    | List of t
    | Poly_variant of constr list
    | App of t * t list

  and constr =
    { name : string
    ; args : t list
    }

  and field =
    { name : string
    ; typ : t
    ; attrs : attr list
    }

  type decl =
    | Alias of t
    | Record of field list
    | Variant of constr list

  let field typ ~name =
    let ident = ident name in
    let attrs =
      if ident = name then
        []
      else
        [ Key name ]
    in
    let attrs =
      match typ with
      | Optional _ -> Option :: attrs
      | _ -> attrs
    in
    { name = ident; typ; attrs }

  let constr args ~name =
    let name = String.capitalize_ascii name in
    { name; args }

  let list t = List t

  let assoc_list ~key ~data = List (Tuple [ key; data ])

  let t = Named "t"

  let module_t m = Named (String.capitalize_ascii m ^ ".t")

  let string = Named "string"

  let name s = Named s

  let int = Named "int"

  let bool = Named "bool"

  let alpha = Var "a"

  let enum constrs =
    Variant (List.map constrs ~f:(fun constr -> { name = constr; args = [] }))

  let poly_enum constrs =
    Poly_variant
      (List.map constrs ~f:(fun constr -> { name = constr; args = [] }))

  let json = Named "Json.t"

  let unit = Named "unit"

  module Type = W.Type

  let pp_prim (p : prim) : W.t =
    match p with
    | String -> Pp.verbatim "string"
    | Int -> Pp.verbatim "int"
    | Bool -> Pp.verbatim "bool"
    | Unit -> Pp.verbatim "unit"

  let rec pp (a : t) ~(kind : Kind.t) : W.t =
    match a with
    | Prim p -> pp_prim p
    | Var v -> Type.var v
    | Named v -> Type.name v
    | App (f, xs) -> Type.app (pp ~kind f) (List.map ~f:(pp ~kind) xs)
    | Tuple t -> Type.tuple (List.map ~f:(pp ~kind) t)
    | Optional t ->
      let name =
        match kind with
        | Intf -> "option"
        | Impl -> "Json.Nullable_option.t"
      in
      pp ~kind (App (Named name, [ t ]))
    | List t -> pp ~kind (App (Named "list", [ t ]))
    | Poly_variant constrs ->
      List.map constrs ~f:(fun { name; args } ->
          (name, List.map args ~f:(pp ~kind)))
      |> Type.poly

  let pp_decl' ~(kind : Kind.t) (a : decl) =
    match a with
    | Alias a -> pp ~kind a
    | Record r -> (
      let r =
        List.map r ~f:(fun { name; typ; attrs } ->
            let def =
              let field = pp ~kind typ in
              let attrs =
                let attrs =
                  match kind with
                  | Intf -> []
                  | Impl -> attrs
                in
                List.concat_map attrs ~f:(function
                  | Option ->
                    [ W.Attr.make "default" [ Pp.verbatim "None" ]
                    ; W.Attr.make "yojson_drop_default" [ Pp.verbatim "( = )" ]
                    ]
                  | Key s ->
                    [ W.Attr.make "key" [ Pp.verbatim (sprintf "%S" s) ] ])
              in
              Type.field_attrs ~field ~attrs
            in
            (name, def))
        |> Type.record
      in
      match kind with
      | Intf -> r
      | Impl -> W.Type.deriving r )
    | Variant v ->
      List.map v ~f:(fun { name; args } -> (name, List.map ~f:(pp ~kind) args))
      |> Type.variant

  let pp_decl ~name ~kind (a : decl) : W.t =
    let body = pp_decl' ~kind a in
    Type.decl name body
end

module Module = struct
  type 'a t =
    { name : string
    ; bindings : 'a Named.t list
    }

  let empty name = { name; bindings = [] }

  type sig_ =
    | Value of Type.t
    | Type_decl of Type.decl

  type impl = Type_decl of Type.decl

  let pp_sig { name; bindings } =
    let bindings =
      Pp.concat_map bindings ~sep:Pp.newline ~f:(fun { name; data = v } ->
          let lhs =
            match v with
            | Value _ -> Pp.textf "val %s :" name
            | Type_decl _ -> Pp.textf "type %s =" name
          in
          let rhs =
            match v with
            | Value t -> Type.pp ~kind:Intf t
            | Type_decl t -> Type.pp_decl' ~kind:Intf t
          in
          Pp.concat [ lhs; Pp.space; rhs ])
    in
    W.Sig.module_ name bindings

  let pp_impl { name; bindings } =
    let bindings =
      Pp.concat_map bindings ~sep:Pp.newline ~f:(fun { name; data = v } ->
          let lhs =
            match v with
            | Type_decl _ -> Pp.textf "type %s =" name
          in
          let rhs =
            match v with
            | Type_decl t -> Type.pp_decl' ~kind:Impl t
          in
          Pp.concat [ lhs; Pp.space; rhs ])
    in
    W.module_ name bindings
end

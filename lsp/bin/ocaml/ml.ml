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
    | Assoc of t * t
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

  let assoc_list ~key ~data = Assoc (key, data)

  let t = Named "t"

  let module_t m = Named (String.capitalize_ascii m ^ ".t")

  let string = Prim String

  let name s = Named s

  let int = Prim Int

  let bool = Prim Bool

  let alpha = Var "a"

  let enum constrs =
    Variant (List.map constrs ~f:(fun constr -> { name = constr; args = [] }))

  let poly_enum constrs =
    Poly_variant
      (List.map constrs ~f:(fun constr -> { name = constr; args = [] }))

  let json = Named "Json.t"

  let unit = Prim Unit

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
        match (kind, t) with
        | Impl, Named "Json.t"
        | Intf, _ ->
          "option"
        | Impl, _ -> "Json.Nullable_option.t"
      in
      pp ~kind (App (Named name, [ t ]))
    | List t -> pp ~kind (App (Named "list", [ t ]))
    | Poly_variant constrs ->
      List.map constrs ~f:(fun { name; args } ->
          (name, List.map args ~f:(pp ~kind)))
      |> Type.poly
    | Assoc (k, v) -> (
      match kind with
      | Intf -> pp (List (Tuple [ k; v ])) ~kind
      | Impl -> pp (App (Named "Json.Assoc.t", [ k; v ])) ~kind )

  let pp_decl' ~(kind : Kind.t) (a : decl) =
    match a with
    | Alias a -> pp ~kind a
    | Record r -> (
      let r =
        List.filter_map r ~f:(fun { name; typ; attrs } ->
            let open Option.O in
            let+ () =
              match (name, typ) with
              | "kind", Prim Unit -> None
              | _, _ -> Some ()
            in
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
                    if typ = Optional json then
                      [ W.Attr.make "yojson.option" [] ]
                    else
                      [ W.Attr.make "default" [ Pp.verbatim "None" ]
                      ; W.Attr.make "yojson_drop_default"
                          [ Pp.verbatim "( = )" ]
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

module Expr = struct
  [@@@ocaml.warning "-30-32-37"]

  type expr =
    | Ident of string
    | Let of pat * expr * expr
    | Match of expr * (pat * expr) list
    | Fun of pat arg list * expr
    | App of expr * expr arg list
    | Create of expr prim
    | Assert_false

  and 'e prim =
    | Unit
    | Bool of bool
    | Int of int
    | String of string
    | Ident of string
    | Cons of 'e * 'e prim
    | List of 'e list
    | Tuple of 'e list
    | Record of 'e record_
    | Constr of 'e constr

  and 'e arg =
    | Unnamed of 'e
    | Labeled of string * 'e
    | Optional of string * 'e

  and pat =
    | Wildcard
    | Pat of pat prim

  and 'e record_ = (string * 'e) list

  and 'e constr =
    { tag : string
    ; poly : bool
    ; args : 'e list
    }

  type t = expr

  let assert_false_clause = (Wildcard, Assert_false)

  type toplevel =
    { pat : (string arg * Type.t) list
    ; type_ : Type.t
    ; body : t
    }

  let constr ?(poly = false) ?(args = []) tag = { poly; args; tag }

  let pp_constr f { tag; poly; args } =
    let tag =
      let tag = String.capitalize tag in
      Pp.verbatim
        ( if poly then
          "`" ^ tag
        else
          tag )
    in
    match args with
    | [] -> tag
    | args ->
      let sep = Pp.verbatim "," in
      let args = W.surround `Paren (Pp.concat_map ~sep ~f args) in
      Pp.concat [ tag; Pp.space; args ]

  let rec pp_pat = function
    | Wildcard -> Pp.verbatim "_"
    | Pat pat -> (
      match pat with
      | Unit -> Pp.verbatim "()"
      | Bool b -> Pp.textf "%b" b
      | Int i -> Pp.textf "%i" i
      | String s -> Pp.textf "%S" s
      | Ident s -> Pp.verbatim s
      | Cons _ -> assert false
      | List _ -> assert false
      | Tuple _ -> assert false
      | Record _ -> assert false
      | Constr c -> pp_constr pp_pat c )

  let rec pp_create : expr prim -> _ Pp.t = function
    | Unit -> Pp.verbatim "()"
    | Bool b -> Pp.textf "%b" b
    | Int i ->
      let pp = Pp.textf "%i" i in
      if i < 0 then
        W.surround `Paren pp
      else
        pp
    | String s -> Pp.textf "%S" s
    | Ident s -> Pp.verbatim s
    | Cons _ -> assert false
    | List _ -> assert false
    | Tuple _ -> assert false
    | Record _ -> assert false
    | Constr c -> pp_constr pp c

  and pp = function
    | Ident s -> Pp.verbatim s
    | Assert_false -> Pp.verbatim "assert false"
    | Match (expr, patterns) ->
      let with_ =
        Pp.concat
          [ Pp.verbatim "match"
          ; Pp.space
          ; pp expr
          ; Pp.space
          ; Pp.verbatim "with"
          ]
      in
      let clauses =
        Pp.concat_map patterns ~f:(fun (pat, expr) ->
            Pp.concat
              [ Pp.verbatim "| "
              ; pp_pat pat
              ; Pp.space
              ; Pp.verbatim "->"
              ; Pp.space
              ; Pp.verbatim "("
              ; pp expr
              ; Pp.verbatim ")"
              ])
      in
      Pp.concat [ with_; Pp.newline; clauses ]
    | Create c -> pp_create c
    | App (x, args) ->
      let args =
        Pp.concat_map args ~sep:Pp.space ~f:(fun arg ->
            match arg with
            | Unnamed e -> pp e
            | _ -> assert false)
      in
      Pp.concat [ pp x; Pp.space; args ]
    | _ -> assert false

  let pp_toplevel ~kind name { pat; type_; body } =
    let pat =
      Pp.concat_map pat ~f:(fun (pat, typ) ->
          let typ = Type.pp ~kind typ in
          match pat with
          | Unnamed s ->
            Pp.concat
              [ Pp.verbatim "("
              ; Pp.verbatim s
              ; Pp.verbatim " : "
              ; typ
              ; Pp.verbatim ")"
              ]
          | Labeled (l, r) ->
            if l = r then
              Pp.concat [ Pp.textf "~(%s :" l; typ; Pp.verbatim ")" ]
            else
              assert false
          | Optional (_, _) -> assert false)
    in
    let body = pp body in
    let type_ = Type.pp type_ ~kind in
    Pp.concat
      [ Pp.textf "let %s" name
      ; pat
      ; Pp.textf " : "
      ; type_
      ; Pp.textf "="
      ; Pp.newline
      ; body
      ]
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
    | Json_conv_sig

  type impl =
    | Type_decl of Type.decl
    | Value of Expr.toplevel

  let pp_sig { name; bindings } =
    let bindings =
      Pp.concat_map bindings ~sep:Pp.newline ~f:(fun { name; data } ->
          match (data : sig_) with
          | Value t ->
            Pp.concat
              [ Pp.textf "val %s :" name; Pp.space; Type.pp ~kind:Intf t ]
          | Type_decl t ->
            Pp.concat
              [ Pp.textf "type %s =" name
              ; Pp.space
              ; Type.pp_decl' ~kind:Intf t
              ]
          | Json_conv_sig ->
            Pp.textf "include Json.Jsonable.S with type t := %s" name)
    in
    W.Sig.module_ name bindings

  let pp_impl { name; bindings } =
    let bindings =
      Pp.concat_map bindings ~sep:Pp.newline ~f:(fun { name; data = v } ->
          match v with
          | Type_decl t ->
            let lhs = Pp.textf "type %s =" name in
            let rhs = Type.pp_decl' ~kind:Impl t in
            Pp.concat [ lhs; Pp.space; rhs ]
          | Value decl -> Expr.pp_toplevel ~kind:Impl name decl)
    in
    W.module_ name bindings
end

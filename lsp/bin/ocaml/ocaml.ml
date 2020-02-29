open! Import
open! Ts_types

module Json = struct
  let pp_literal (t : Literal.t) =
    match t with
    | String s -> Pp.textf "`String %S" s
    | Int s -> Pp.textf "`Int (%d)" s
    | Float _ -> assert false
end

module Type = struct
  type anon =
    | Named of string
    | Tuple of anon list
    | Var of string
    | App of anon * anon

  and field =
    { name : string
    ; typ : anon
    ; attrs : unit Pp.t list
    }

  type t =
    | Anon of anon
    | Sum of (string * anon option) list
    | Record of field list

  let of_named (r : _ Named.t) = Named (String.capitalize_ascii r.name ^ ".t")

  let list t = App (Named "list", t)

  let t = Named "t"

  let string = Named "string"

  let int = Named "int"

  let bool = Named "bool"

  let alpha = Anon (Var "a")

  let enum constrs = Sum (List.map constrs ~f:(fun constr -> (constr, None)))

  let json = Named "Json.t"

  let unit = Named "unit"

  let rec pp_anon (a : anon) =
    match a with
    | Var v -> Pp.textf "'%s" v
    | Named v -> Pp.verbatim v
    | App (f, x) -> Pp.concat [ pp_anon x; Pp.space; pp_anon f ]
    | Tuple _ -> assert false

  let pp (t : t) =
    let open W in
    match t with
    | Anon a -> pp_anon a
    | Sum xs ->
      List.map xs ~f:(fun (constr, arg) ->
          let arg = Option.map arg ~f:pp_anon in
          (String.capitalize constr, arg))
      |> Type.variant_
    | Record r ->
      List.map r ~f:(fun { name; attrs; typ } ->
          let typ = pp_anon typ in
          let defn = Pp.concat ~sep:Pp.space (typ :: attrs) in
          (name, defn))
      |> Type.record
end

module Type_decl = struct
  type single =
    { name : string
    ; defn : Type.t
    }

  type t =
    { decls : single list
    ; deriving : bool
    }

  let pp { decls; deriving } =
    let pp_decls =
      List.map decls ~f:(fun { name; defn } -> (name, Type.pp defn))
      |> W.Type.rec_decls
    in
    match decls with
    | _ :: _ when deriving -> W.Type.deriving pp_decls
    | _ -> pp_decls
end

module Expr = struct
  [@@@warning "-30"]

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
    ; args : 'e option
    }

  type t = expr

  let constr ?(poly = false) ?args tag = { poly; args; tag }

  let rec pp = function
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
        ignore patterns;
        assert false
      in
      Pp.concat [ with_; Pp.newline; clauses ]
    | _ -> assert false
end

module Gen = struct end

module Texpr = struct
  type t = Expr.t * Type.t

  let assert_false = (Expr.Assert_false, Type.alpha)
end

module Let = struct
  type t =
    { name : string
    ; pat : (Expr.pat Expr.arg * Type.anon) list
    ; body : unit Pp.t * Type.anon
    }

  let pp =
    let intf { name; pat; body } =
      let patterns =
        let open Expr in
        Pp.concat_map pat ~f:(fun (pat, typ) ->
            let typ = Type.pp (Anon typ) in
            let typ = Pp.concat [ typ; Pp.space; Pp.verbatim "->"; Pp.space ] in
            match pat with
            | Unnamed _ -> typ
            | Labeled (name, _) -> Pp.concat [ Pp.textf "%s:" name; typ ]
            | Optional (name, _) -> Pp.concat [ Pp.textf "?%s:" name; typ ])
      in
      Pp.concat
        [ Pp.textf "val %s :" name
        ; Pp.space
        ; patterns
        ; Type.pp (Anon (snd body))
        ]
    in
    let impl { name; pat; body } =
      let pat =
        Pp.concat_map pat ~sep:Pp.space ~f:(fun (pat, _typ) ->
            match pat with
            | Unnamed (Pat (Ident s)) -> Pp.verbatim s
            | Labeled (name, Pat (Ident e)) -> Pp.textf "~%s:%s" name e
            | Optional (name, Pat (Ident e)) -> Pp.textf "?%s:%s" name e
            | _ -> assert false)
      in
      let body = fst body in
      Pp.concat
        [ Pp.textf "let %s" name
        ; Pp.space
        ; Pp.hovbox pat
        ; Pp.space
        ; Pp.text "="
        ; Pp.newline
        ; Pp.hovbox ~indent:2 body
        ]
    in
    { Ml_kind.intf; impl }
end

module Module = struct
  type t =
    { name : string
    ; type_decls : Type_decl.t list
    ; lets : Let.t list
    }

  let empty name = { name; type_decls = []; lets = [] }

  let pp t ~kind =
    let decls = Pp.concat_map ~sep:Pp.newline ~f:Type_decl.pp t.type_decls in
    let lets =
      let f = Ml_kind.get Let.pp kind in
      Pp.concat_map ~sep:Pp.newline t.lets ~f
    in
    let body = Pp.concat [ decls; lets ] in
    match kind with
    | Intf -> W.Sig.module_ t.name body
    | Impl -> W.module_ t.name body

  let pp t = { Ml_kind.intf = pp t ~kind:Intf; impl = pp t ~kind:Impl }
end

let pp_file pp ~fname =
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  Pp.render_ignore_tags fmt pp;
  Format.pp_print_flush fmt ();
  Io.String_path.write_file fname (Buffer.contents buf)

let name = "gprotocol"

module Enum = struct
  let of_json { Named.name = _; data = constrs } =
    let open Expr in
    let name = "of_yojson" in
    let pat = [ (Unnamed (Pat (Ident "json")), Type.json) ] in
    let body =
      let body =
        let constrs =
          Pp.concat_map constrs ~f:(fun (constr, literal) ->
              Pp.concat
                [ Pp.textf "| "
                ; Json.pp_literal literal
                ; Pp.textf "-> %s" (String.capitalize constr)
                ; Pp.newline
                ])
        in
        [ Pp.textf "match json with"
        ; Pp.newline
        ; constrs
        ; Pp.text "| _ -> assert false"
        ; Pp.newline
        ]
      in
      (Pp.concat body, Type.t)
    in
    { Let.name; pat; body }

  let to_json { Named.name = _; data = constrs } =
    let open Expr in
    let name = "to_yojson" in
    let pat = [ (Unnamed (Pat (Ident "t")), Type.t) ] in
    let body =
      let patterns =
        Pp.concat_map constrs ~f:(fun (constr, literal) ->
            Pp.concat
              [ Pp.textf "| %s -> " (String.capitalize constr)
              ; Json.pp_literal literal
              ; Pp.newline
              ])
      in
      (Pp.concat [ Pp.verbatim "match t with"; Pp.newline; patterns ], Type.json)
    in
    { Let.name; pat; body }

  let module_ ({ Named.name; data = constrs } as t) =
    let type_decls =
      let defn = Type.Sum (List.map constrs ~f:(fun (c, _) -> (c, None))) in
      let decls = [ { Type_decl.name = "t"; defn } ] in
      [ { Type_decl.decls; deriving = false } ]
    in
    let lets =
      let to_json = to_json t in
      let of_json = of_json t in
      [ to_json; of_json ]
    in
    Some { Module.name; type_decls; lets }
end

let rename_ident = function
  | "type" -> "type_"
  | "method" -> "method_"
  | "end" -> "end_"
  | s -> s

module Sum = struct
  (* Sum types are a bit involved. All special handling is grouped here.

     The rules are as follows:

     - If the type is only composed of literals it's treated as an enum

     - If there are only two characters and one of them is null. This is treated
     as a nullable option

     - If it's a string | number, it's treated as an Id.

     - string | number | boolean | array | object | null is hard coded to be a
     json type

     - Anonymous sums in field names are treated as polymoprhic variants. The
     tags are chosen mechanically based on the arguments

     - Anonymous sums of records are hardeset to handle. We compose the various
     parsers with <|> *)
end

module Record = struct
  (* The steps to process a record are:

     - extract all inlined types out of a record - every inlined type will be
     named after the field it was used in

     Field handling - optional - key handling - default handling *)
  open Type

  let rec all_types fields =
    List.concat_map fields ~f:(fun (f : Resolved.field) ->
        match f.data with
        | Resolved.Single { typ; optional = _ } -> (
          match typ with
          | Sum _ -> []
          | Record s -> (f, s) :: all_types s
          | _ -> [] )
        | Pattern _ -> [])

  let make_typ name t =
    let rec typ (t : Resolved.typ) =
      match t with
      | Ident Number -> Type.int
      | Ident String -> Type.string
      | Ident Bool -> Type.bool
      | Ident Any
      | Ident Object ->
        Type.json
      | Ident Self -> Type.t (* XXX wrong *)
      | Ident Null -> assert false
      | Ident List -> assert false
      | Ident (Resolved r) -> Type.of_named r
      | List t -> Type.list (typ t)
      | App _
      | Tuple _
      | Sum _
      | Literal _ ->
        Type.unit
      | Record _ -> Type.Named name
    in
    typ t

  let make_field (field : Resolved.field) =
    match field.data with
    | Pattern _ -> Type.unit
    | Resolved.Single { typ; optional = _ } -> make_typ field.name typ

  let attrs_of_field (field : Resolved.field) =
    match field.data with
    | Single s -> (
      let attrs =
        if s.optional then
          [ W.Type.opt_attr ]
        else
          []
      in
      match String.drop_suffix field.name ~suffix:"_" with
      | Some s -> W.Type.key s :: attrs
      | None -> attrs )
    | Pattern _ -> []

  let record_ name fields =
    let defn =
      Record
        (List.map fields ~f:(fun (field : Resolved.field) ->
             { name = field.name
             ; typ = make_field field
             ; attrs = attrs_of_field field
             }))
    in
    { Type_decl.name; defn }
end

module Interface = struct
  let module_
      { Named.name; data = { Resolved.extends = _; fields; params = _ } } =
    let all_types = Record.all_types fields in
    let main_type =
      List.map fields ~f:(fun (n : Resolved.field) ->
          let name = rename_ident n.name in
          { n with name })
      |> Record.record_ "t"
    in
    let rest_types =
      List.map all_types ~f:(fun ((t : Resolved.field), subfields) ->
          Record.record_ t.name subfields)
    in
    let type_decl =
      { Type_decl.deriving = true; decls = main_type :: rest_types }
    in
    let lets = [] in
    match fields with
    | [] -> None
    | _ :: _ -> Some { Module.name; type_decls = [ type_decl ]; lets }
end

module Lsp_type = struct
  let module_ { Named.name; data = typ } =
    let main_type =
      let rec defn typ =
        match (typ : Resolved.typ) with
        | Ident Number -> Type.int
        | Ident String -> Type.string
        | Ident Bool -> Type.bool
        | Ident Any
        | Ident Object ->
          Type.json
        | Ident Self -> Type.t (* XXX wrong *)
        | Ident Null -> assert false
        | Ident List -> assert false
        | Ident (Resolved r) -> Type.of_named r
        | List f -> Type.list (defn f)
        | App _
        | Tuple _
        | Sum _
        | Literal _ ->
          Type.unit
        | Record _ -> Type.Named name
      in
      { Type_decl.name = "t"; defn = Type.Anon (defn typ) }
    in
    let type_decl = { Type_decl.deriving = true; decls = [ main_type ] } in
    Some { Module.name; type_decls = [ type_decl ]; lets = [] }
end

class idents =
  object
    inherit [Resolved.t list] Resolved.fold_ident

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
        | Enum_anon data -> Enum.module_ { t with data }
        | Interface data -> Interface.module_ { t with data }
        | Type data -> Lsp_type.module_ { t with data })

let mli = name ^ ".mli"

let ml = name ^ ".ml"

let output modules =
  let open Ml_kind in
  let intf, impl =
    List.map modules ~f:(fun m ->
        let { intf; impl } = Module.pp m in
        (intf, impl))
    |> List.unzip
  in
  let def = { intf; impl } in
  let def = Ml_kind.map def ~f:(Pp.concat ~sep:Pp.newline) in
  let def =
    let prelude =
      let open W in
      Pp.concat [ opens [ "Import" ]; warnings "-30"; Pp.newline ]
    in
    Ml_kind.map def ~f:(Pp.seq prelude)
  in
  let pp = { intf = pp_file ~fname:mli; impl = pp_file ~fname:ml } in
  Ml_kind.both def pp |> Ml_kind.iter ~f:(fun (def, pp) -> pp def)

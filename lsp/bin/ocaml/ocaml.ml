open! Import
open! Ts_types

(* TODO - Handle the [kind] field everywhere *)

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
        | "Message" ->
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
        super#interface { i with extends }
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

      (* Every record valued field introduces a new type TODO handle the case
         where two fields share a type *)
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
  let pp_literal (t : Literal.t) =
    match t with
    | String s -> Pp.textf "`String %S" s
    | Int s -> Pp.textf "`Int (%d)" s
    | Float _ -> assert false
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

module Texpr = struct
  [@@@ocaml.warning "-34-32-37"]

  type t = Expr.t * Ml.Type.t

  let assert_false = (Expr.Assert_false, Ml.Type.alpha)
end

module Let = struct
  [@@@ocaml.warning "-32-37"]

  type t =
    { name : string
    ; pat : (Expr.pat Expr.arg * Ml.Type.t) list
    ; body : unit Pp.t * Ml.Type.t
    }

  let pp =
    let intf { name; pat; body } =
      let kind = Ml.Kind.Intf in
      let patterns =
        let open Expr in
        Pp.concat_map pat ~f:(fun (pat, typ) ->
            let typ = Ml.Type.pp ~kind typ in
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
        ; Ml.Type.pp ~kind (snd body)
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
    { Ml.Kind.intf; impl }
end

module Module = struct
  module Module = Ml.Module

  type t = (Module.sig_ Module.t, Module.impl Module.t) Ml.Kind.pair

  let _empty name =
    { Ml.Kind.intf = Ml.Module.empty name; impl = Ml.Module.empty name }

  let type_decls name (type_decls : Ml.Type.decl Named.t list) : t =
    let module_ bindings = { Ml.Module.name; bindings } in
    let intf : Module.sig_ Module.t =
      List.map type_decls ~f:(fun (td : Ml.Type.decl Named.t) ->
          { td with
            Named.data = (Ml.Module.Type_decl td.data : Ml.Module.sig_)
          })
      |> module_
    in
    let impl =
      List.map type_decls ~f:(fun (td : Ml.Type.decl Named.t) ->
          { td with Named.data = Ml.Module.Type_decl td.data })
      |> module_
    in
    { Ml.Kind.intf; impl }

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
  let of_json { Named.name = _; data = constrs } =
    let open Expr in
    let name = "of_yojson" in
    let pat = [ (Unnamed (Pat (Ident "json")), Ml.Type.json) ] in
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
      (Pp.concat body, Ml.Type.t)
    in
    { Let.name; pat; body }

  let to_json { Named.name = _; data = constrs } =
    let open Expr in
    let name = "to_yojson" in
    let pat = [ (Unnamed (Pat (Ident "t")), Ml.Type.t) ] in
    let body =
      let patterns =
        Pp.concat_map constrs ~f:(fun (constr, literal) ->
            Pp.concat
              [ Pp.textf "| %s -> " (String.capitalize constr)
              ; Json.pp_literal literal
              ; Pp.newline
              ])
      in
      ( Pp.concat [ Pp.verbatim "match t with"; Pp.newline; patterns ]
      , Ml.Type.json )
    in
    { Let.name; pat; body }

  let module_ ({ Named.name; data = constrs } as t) =
    let _lets =
      let to_json = to_json t in
      let of_json = of_json t in
      ignore [ to_json; of_json ];
      []
    in
    let t =
      let data =
        Ml.Type.Variant
          (List.map constrs ~f:(fun (name, _) -> Ml.Type.constr ~name []))
      in
      { Named.name = "t"; data }
    in
    Module.type_decls name [ t ]
end

module Sum = struct
  (** Sum types are a bit involved. All special handling is grouped here.

      The rules are as follows:

      - If the type is only composed of literals it's treated as an enum

      - If there are only two characters and one of them is null. This is
      treated as a nullable option

      - If it's a string | number, it's treated as an Id.

      - string | number | boolean | array | object | null is hard coded to be a
      json type

      - Anonymous sums in field names are treated as polymoprhic variants. The
      tags are chosen mechanically based on the arguments

      - Anonymous sums of records are hardeset to handle. We compose the various
      parsers with <|> *)
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
        Type.unit
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

  let module_ { Ml.Module.name; bindings } =
    let type_decls =
      List.concat_map bindings ~f:(fun (r : Expanded.binding Named.t) ->
          match r.data with
          | Record data -> record { r with data }
          | Interface data -> record { r with data = interface_fields data }
          | Poly_enum data -> poly_enum { r with data }
          | Alias data -> type_ { r with data })
    in
    Module.type_decls name type_decls
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
  let def =
    let prelude =
      let open W in
      Pp.concat [ opens [ "Import" ]; warnings "-30"; Pp.newline ]
    in
    Ml.Kind.Map.map def ~f:(Pp.seq prelude)
  in
  let pp = Ml.Kind.Map.get def kind in
  pp_file pp out

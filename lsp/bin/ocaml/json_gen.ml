open! Import
open Ml

let json_t = Type.Path (Dot (Ident "Json", "t"))

let pat_of_literal (t : Ts_types.Literal.t) : Expr.pat =
  let open Expr in
  let tag, args =
    match t with
    | Ts_types.Literal.String s -> ("String", Pat (Expr.String s))
    | Int i -> ("Int", Pat (Expr.Int i))
    | Float _ -> assert false
  in
  Pat (Constr { poly = true; tag; args = [ args ] })

let constr_of_literal (t : Ts_types.Literal.t) : Expr.t =
  let open Expr in
  let tag, args =
    match t with
    | Ts_types.Literal.String s -> ("String", Create (Expr.String s))
    | Int i -> ("Int", Create (Expr.Int i))
    | Float _ -> assert false
  in
  Create (Constr { poly = true; tag; args = [ args ] })

let json_error_pat msg =
  let open Expr in
  ( Wildcard
  , App
      ( Create (Ident "Json.error")
      , [ Unnamed (Create (String msg)); Unnamed (Create (Ident "json")) ] ) )

let is_json_constr (constr : Type.constr) =
  List.mem [ "String"; "Int"; "Bool" ] constr.name ~equal:String.equal

module Name = struct
  let of_ = sprintf "%s_of_yojson"

  let to_ = sprintf "yojson_of_%s"

  let conv = function
    | `To -> to_
    | `Of -> of_
end

open Arg

let of_json ~name expr =
  let pat = [ (Unnamed "json", Type.json) ] in
  let data = { Expr.pat; type_ = Type.name name; body = expr } in
  let name = Name.of_ name in
  { Named.name; data }

let to_json ~name expr =
  let pat = [ (Unnamed name, Type.name name) ] in
  let data = { Expr.pat; type_ = Type.json; body = expr } in
  let name = Name.to_ name in
  { Named.name; data }

let add_json_conv_for_t (sig_ : Module.sig_ Module.t) =
  let conv_t =
    { Named.name = "t"
    ; data =
        (let t = Type.Path (Path.Ident "t") in
         Module.Include (Module.Name.of_string "Json.Jsonable.S", [ (t, t) ]))
    }
  in
  { sig_ with bindings = sig_.bindings @ [ conv_t ] }

module Enum = struct
  let of_json ~allow_other ~poly { Named.name; data = constrs } =
    let open Ml.Expr in
    let body =
      let clauses =
        List.map constrs ~f:(fun (constr, literal) ->
            let pat = pat_of_literal literal in
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
      let msg =
        sprintf "Invalid value. Expected one of: %s"
          (List.map constrs ~f:(fun (_, literal) ->
               Ts_types.Literal.to_maybe_quoted_string literal)
          |> String.concat ~sep:", ")
      in
      Match (Create (Ident "json"), clauses @ [ json_error_pat msg ])
    in
    of_json ~name body

  let to_json ~allow_other ~poly { Named.name; data = constrs } =
    let open Ml.Expr in
    let body =
      let clauses =
        List.map constrs ~f:(fun (constr, literal) ->
            let pat = Pat (Constr { tag = constr; poly; args = [] }) in
            (pat, constr_of_literal literal))
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
    to_json ~name body

  let conv ~allow_other ~poly t =
    let to_json = to_json ~allow_other ~poly t in
    let of_json = of_json ~allow_other ~poly t in
    [ to_json; of_json ]
end

module Poly_variant = struct
  type constrs =
    { json_constrs : Ml.Type.constr list
    ; untagged_constrs : Ml.Type.constr list
    }

  let split_clauses constrs =
    let json_constrs, untagged_constrs =
      List.partition_map constrs ~f:(fun x ->
          if is_json_constr x then
            Left x
          else
            Right x)
    in
    { json_constrs; untagged_constrs }

  let conv_of_constr target (utc : Ml.Type.constr) =
    let rec conv (p : Ml.Path.t) =
      match p with
      | Ident name -> Ml.Path.Ident (Name.conv target name)
      | Dot (s, name) -> Ml.Path.Dot (s, Name.conv target name)
      | Apply (s, y) -> Path.Apply (s, conv y)
    in
    let conv p = Ml.Path.to_string (conv p) in
    let open Ml.Expr in
    let json_mod n =
      match target with
      | `To -> Ident ("Json.To." ^ n)
      | `Of -> Ident ("Json.Of." ^ n)
    in
    let conv t = Create (Ident (conv t)) in
    match (utc.args : Ml.Type.t list) with
    | [ Path p ] -> conv p
    | [ List (Path p) ] -> App (Create (json_mod "list"), [ Unnamed (conv p) ])
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
    to_json ~name expr

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
      | _, [] -> Match (Create (Ident "json"), clauses @ [ json_error_pat name ])
      | _ :: _, _ :: _ ->
        Match (Create (Ident "json"), clauses @ [ (Wildcard, untagged) ])
    in
    of_json ~name expr
end

(* [name] is used as the pattern in the "to" converter. In the "of" converter,
   it's used to generate better error messages. *)
let make_literal_wrapper_conv ~field_name ~literal_value ~type_name =
  (* Some json representations require an extra "kind" field set to some string
     constant *)
  let open Ml.Expr in
  let args = List.map ~f:(fun x -> Ml.Arg.Unnamed (Create x)) in
  let to_ =
    let a =
      [ String field_name
      ; String literal_value
      ; Ident (Name.conv `To type_name)
      ; Ident type_name
      ]
    in
    App (Create (Ident "Json.To.literal_field"), args a)
  in
  let of_ =
    let a =
      [ String type_name
      ; String field_name
      ; String literal_value
      ; Ident (Name.conv `Of type_name)
      ; Ident "json"
      ]
    in
    App (Create (Ident "Json.Of.literal_field"), args a)
  in
  [ to_json ~name:type_name to_; of_json ~name:type_name of_ ]
  |> List.map ~f:(Named.map ~f:(fun v -> Ml.Module.Value v))

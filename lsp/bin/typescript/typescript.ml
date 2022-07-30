open Import
open Ts_types

type test =
  { snippet : string
  ; loc : Lexing.position
  ; exn : exn
  }

let name_table (defns : Unresolved.t list) =
  List.map defns ~f:(fun (def : _ Named.t) ->
      (def.name, (def, Ts_types.Ident.make Name def.name)))
  |> String.Map.of_list_reducei ~f:(fun name (v1, id1) (v2, id2) ->
         let open Unresolved in
         match (v1.Named.data, v2.data) with
         | Enum_anon _, _ -> (v1, id1)
         | _, Enum_anon _ -> (v2, id2)
         | _, _ ->
           if v1 = v2 then (v1, id1)
           else
             let open Dyn in
             Code_error.raise "definition conflict" [ ("name", string name) ])

let resolve_all (defns : Unresolved.t list) =
  let names = name_table defns in
  let defns = String.Map.values names |> List.map ~f:fst in
  let names = String.Map.map ~f:snd names in
  (Ts_types.resolve_all defns ~names, names)

let test_snippets s =
  let fails, succs =
    List.partition_map s ~f:(fun s ->
        let lexbuf = Lexing.from_string s in
        try Right (Ts_parser.main Ts_lexer.token lexbuf)
        with exn -> Left { snippet = s; loc = lexbuf.lex_curr_p; exn })
  in
  ignore (resolve_all (List.concat succs));
  fails

let pp_results ppf tests =
  List.iteri tests ~f:(fun i { snippet; loc; exn } ->
      Format.pp_print_string ppf snippet;
      Format.fprintf ppf "line: %d char: %d@." loc.pos_lnum
        (loc.pos_cnum - loc.pos_bol);
      Format.fprintf ppf "%d. exn: %s@.%!" (i + 1) (Printexc.to_string exn);
      Format.fprintf ppf "@.---@.")

let of_snippets s =
  List.concat_map s ~f:(fun s ->
      let lexbuf = Lexing.from_string s in
      try Ts_parser.main Ts_lexer.token lexbuf with _exn -> [])

module Unresolved = Ts_types.Unresolved
open Unresolved
open Metamodel

let rename = function
  | "_InitializeParams" -> "InitializedParams_"
  | s -> s

let reference s =
  match rename s with
  | "LSPAny" -> "object"
  | s -> s

let named ~name s =
  let name = rename name in
  Named.make ~name s

let baseType (baseType : Metamodel.baseType) : Ts_types.Unresolved.typ =
  match baseType with
  | Uri -> Ident "URI"
  | DocumentUri -> Ident "URI"
  | Integer -> Ident "number"
  | Uinteger -> Ident "uinteger"
  | Decimal -> Ident "number"
  | RegExp -> assert false
  | String -> Ident "string"
  | Boolean -> Ident "boolean"
  | Null -> Ident "null"

let rec typ (type_ : Metamodel.type_) : Ts_types.Unresolved.typ =
  match type_ with
  | Reference s -> Ident (reference s)
  | Base b -> baseType b
  | Array t -> List (typ t)
  | Or ts -> Sum (List.map ts ~f:typ)
  | And _ -> failwith "and"
  | Tuple ts -> Tuple (List.map ts ~f:typ)
  | Literal l -> literal l
  | Map m -> mapType m

and mapType { Metamodel.key; value } : Ts_types.Unresolved.typ =
  let pat =
    match key with
    | Uri -> Ident "URI"
    | DocumentUri -> Ident "URI"
    | String -> Ident "string"
    | Integer -> Ident "number"
    | Reference s -> Ident (reference s)
  in
  let typ = typ value in
  let field = named ~name:"" (Pattern { pat; typ }) in
  Record [ field ]

and literal (l : Metamodel.literalType) : Ts_types.Unresolved.typ =
  match l with
  | String s -> Literal (String s)
  | Boolean _ -> assert false
  | Integer i -> Literal (Int i)
  | Record fields -> Record (List.map ~f:field fields)

and field { Metamodel.name; optional; doc = _; type_ } :
    Ts_types.Unresolved.field =
  let field : Ts_types.Unresolved.field_def =
    Single { optional; typ = typ type_ }
  in
  named ~name field

let structure
    ({ doc = _; extends; mixins = _; name; properties } : Metamodel.structure) :
    Ts_types.Unresolved.t =
  let interface : Ts_types.Unresolved.interface =
    let extends =
      List.map extends ~f:(fun (t : Metamodel.type_) ->
          match t with
          | Reference s -> reference s
          | _ -> assert false)
    in
    let fields = List.map properties ~f:field in
    { extends; fields; params = [] }
  in
  named ~name (Interface interface)

let typeAlias ({ name; type_; doc = _ } : Metamodel.typeAlias) =
  named ~name (Type (typ type_))

let enumeration { doc = _; name; supportsCustomValues = _; type_ = _; values } =
  named ~name
  @@ Enum_anon
       (List.map values ~f:(fun ({ name; value; doc = _ } : enumerationEntry) ->
            let case : Enum.case =
              match value with
              | `Int i -> Literal (Int i)
              | `String s -> Literal (String s)
            in
            (name, case)))

let of_metamodel (m : Metamodel.t) : Ts_types.Unresolved.t list =
  let structures = List.map m.structures ~f:structure in
  let type_aliases = List.map m.typeAliases ~f:typeAlias in
  let enumerations = List.map m.enumerations ~f:enumeration in
  List.concat [ structures; type_aliases; enumerations ]

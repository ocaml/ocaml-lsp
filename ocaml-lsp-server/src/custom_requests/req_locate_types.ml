open Import
module P = Query_protocol.Locate_types_result

let meth = "ocamllsp/locateTypes"
let capability = "handleLocateTypes", `Bool true

module Request_params = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; position : Position.t
    }

  let create ~text_document ~position () = { text_document; position }

  let yojson_of_t { text_document; position } =
    match TextDocumentIdentifier.yojson_of_t text_document with
    | `Assoc assoc ->
      let position = "position", Position.yojson_of_t position in
      `Assoc (position :: assoc)
    | _ -> (* unreachable *) assert false
  ;;

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let text_document = json |> TextDocumentIdentifier.t_of_yojson
    and position = json |> member "position" |> Position.t_of_yojson in
    create ~text_document ~position ()
  ;;
end

type payload =
  { ty : string
  ; result :
      [ `Found of DocumentUri.t option * Position.t
      | `Builtin of string
      | `Not_in_env of string
      | `File_not_found of DocumentUri.t
      | `Not_found of DocumentUri.t * string option
      ]
  }

type node =
  | Arrow
  | Tuple
  | Object
  | Poly_variant
  | Other of string
  | Type_ref of payload

type t =
  { data : node
  ; children : t list
  }

module TySet = Set.Make (struct
    type t = string * DocumentUri.t option * Position.t

    let compare (ty_a, doc_a, pos_a) (ty_b, doc_b, pos_b) =
      let c = Option.compare DocumentUri.compare doc_a doc_b in
      if Int.equal 0 c
      then (
        let c = Position.compare pos_a pos_b in
        if Ordering.is_eq c then compare ty_a ty_b else c |> Ordering.to_int)
      else c
    ;;
  end)

let to_set pl =
  let on_node set node =
    match node with
    | Type_ref { ty; result = `Found (doc, pos) } -> TySet.add (ty, doc, pos) set
    | _ -> set
  in
  let rec aux set { data = node; children } =
    let set = on_node set node in
    List.fold_left children ~init:set ~f:aux
  in
  aux TySet.empty pl
;;

let simple constr = `Assoc [ "kind", `String constr ]

let yojson_of_result = function
  | `Found (uri, pos) ->
    let uri, has_uri =
      match uri with
      | Some uri -> DocumentUri.yojson_of_t uri, `Bool true
      | None -> `Null, `Bool false
    in
    `Assoc
      [ "kind", `String "found"
      ; "has_uri", has_uri
      ; "uri", uri
      ; "position", Position.yojson_of_t pos
      ]
  | `Builtin v -> `Assoc [ "kind", `String "built-in"; "type", `String v ]
  | `Not_in_env v -> `Assoc [ "kind", `String "not-in-env"; "type", `String v ]
  | `File_not_found uri ->
    `Assoc [ "kind", `String "file-not-found"; "uri", DocumentUri.yojson_of_t uri ]
  | `Not_found (uri, ty) ->
    `Assoc
      [ "kind", `String "not-found"
      ; "uri", DocumentUri.yojson_of_t uri
      ; ( "type"
        , match ty with
          | None -> `Null
          | Some ty -> `String ty )
      ]
;;

let yojson_type_ref payload =
  `Assoc
    [ "kind", `String "type-ref"
    ; "type", `String payload.ty
    ; "result", yojson_of_result payload.result
    ]
;;

let other s = `Assoc [ "kind", `String "other"; "result", `String s ]

let yojson_of_node = function
  | Arrow -> simple "arrow"
  | Tuple -> simple "tuple"
  | Object -> simple "object"
  | Poly_variant -> simple "poly-variant"
  | Other s -> other s
  | Type_ref p -> yojson_type_ref p
;;

let rec yojson_of_t ?set ({ data; children } as pl) =
  let set =
    match set with
    | Some _ -> []
    | None ->
      let set = to_set pl in
      [ ( "set"
        , `List
            (TySet.to_list set
             |> List.map ~f:(fun (ty, doc, pos) ->
               let uri, has_uri =
                 match doc with
                 | Some uri -> DocumentUri.yojson_of_t uri, `Bool true
                 | None -> `Null, `Bool false
               in
               `Assoc
                 [ "type", `String ty
                 ; "has_uri", has_uri
                 ; "uri", uri
                 ; "position", Position.yojson_of_t pos
                 ])) )
      ]
  in
  `Assoc
    (set
     @ [ "data", yojson_of_node data
       ; "children", `List (List.map ~f:(yojson_of_t ~set) children)
       ])
;;

let map_payload type_ result =
  { ty = type_
  ; result =
      (match result with
       | `Found (file, pos) ->
         let uri = Option.map ~f:DocumentUri.of_string file
         and pos = pos |> Position.of_lexical_position |> Option.value_exn in
         `Found (uri, pos)
       | (`Not_in_env _ | `Builtin _) as s -> s
       | `Not_found (file, ty) -> `Not_found (DocumentUri.of_string file, ty)
       | `File_not_found file -> `File_not_found (DocumentUri.of_string file))
  }
;;

let map_node = function
  | P.Tree.Arrow -> Arrow
  | P.Tree.Tuple -> Tuple
  | P.Tree.Object -> Object
  | P.Tree.Poly_variant -> Poly_variant
  | P.Tree.Other s -> Other s
  | P.Tree.Type_ref { type_; result } -> Type_ref (map_payload type_ result)
;;

let rec map_tree_result result =
  let children = List.map ~f:map_tree_result result.P.Tree.children in
  let data = map_node result.data in
  { data; children }
;;

let with_pipeline state uri f =
  let doc = Document_store.get state.State.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return `Null
  | `Merlin merlin -> Document.Merlin.with_pipeline_exn merlin f
;;

let make_locate_types_command position = Query_protocol.Locate_types position

let dispatch_locate_types position pipeline =
  let position = Position.logical position in
  let command = make_locate_types_command position in
  match Query_commands.dispatch pipeline command with
  | P.Success result ->
    let result = map_tree_result result in
    yojson_of_t result
  | P.Invalid_context ->
    let open Jsonrpc.Response.Error in
    raise @@ make ~code:Code.RequestFailed ~message:"Locate Types: Invalid context" ()
;;

let on_request ~params state =
  Fiber.of_thunk (fun () ->
    let params = (Option.value ~default:(`Assoc []) params :> Json.t) in
    let Request_params.{ text_document; position } = Request_params.t_of_yojson params in
    let uri = text_document.uri in
    with_pipeline state uri @@ dispatch_locate_types position)
;;

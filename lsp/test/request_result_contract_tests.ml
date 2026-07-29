open Lsp
open Types

let print_json json = Yojson.Safe.pretty_to_string json |> Stdlib.print_endline

let check_decode label decode json =
  Stdlib.Printf.printf "%s: " label;
  match decode json with
  | _ -> Stdlib.print_endline "accepted"
  | exception _ -> Stdlib.print_endline "rejected"
;;

let protocol_uri = DocumentUri.of_string "file:///workspace/test.ml"
let protocol_position = Position.create ~line:2 ~character:4
let protocol_range = Range.create ~start:protocol_position ~end_:protocol_position
let protocol_document = TextDocumentIdentifier.create ~uri:protocol_uri
let protocol_location = Location.create ~uri:protocol_uri ~range:protocol_range

let check_response label (Client_request.E request) json =
  check_decode label (Client_request.response_of_json request) json
;;

let round_trip_response label (Client_request.E request) json =
  Stdlib.Printf.printf "%s:\n" label;
  let response = Client_request.response_of_json request json in
  Client_request.yojson_of_result request response |> print_json
;;

let%expect_test "definition-like result wire shapes" =
  let requests =
    [ ( "definition"
      , Client_request.E
          (Client_request.TextDocumentDefinition
             (DefinitionParams.create
                ~position:protocol_position
                ~textDocument:protocol_document
                ())) )
    ; ( "declaration"
      , Client_request.E
          (Client_request.TextDocumentDeclaration
             (DeclarationParams.create
                ~position:protocol_position
                ~textDocument:protocol_document
                ())) )
    ; ( "type definition"
      , Client_request.E
          (Client_request.TextDocumentTypeDefinition
             (TypeDefinitionParams.create
                ~position:protocol_position
                ~textDocument:protocol_document
                ())) )
    ; ( "implementation"
      , Client_request.E
          (Client_request.TextDocumentImplementation
             (ImplementationParams.create
                ~position:protocol_position
                ~textDocument:protocol_document
                ())) )
    ]
  in
  let singular = Location.yojson_of_t protocol_location in
  Stdlib.List.iter
    (fun (label, request) -> round_trip_response label request singular)
    requests;
  [%expect
    {|
    definition:
    [
      {
        "range": {
          "end": { "character": 4, "line": 2 },
          "start": { "character": 4, "line": 2 }
        },
        "uri": "file:///workspace/test.ml"
      }
    ]
    declaration:
    [
      {
        "range": {
          "end": { "character": 4, "line": 2 },
          "start": { "character": 4, "line": 2 }
        },
        "uri": "file:///workspace/test.ml"
      }
    ]
    type definition:
    [
      {
        "range": {
          "end": { "character": 4, "line": 2 },
          "start": { "character": 4, "line": 2 }
        },
        "uri": "file:///workspace/test.ml"
      }
    ]
    implementation:
    [
      {
        "range": {
          "end": { "character": 4, "line": 2 },
          "start": { "character": 4, "line": 2 }
        },
        "uri": "file:///workspace/test.ml"
      }
    ]
    |}]
;;

let%expect_test "workspace symbol and nullable results" =
  let workspace_symbol =
    `List
      [ `Assoc
          [ "data", `String "symbol-id"
          ; "kind", `Int 12
          ; "location", `Assoc [ "uri", DocumentUri.yojson_of_t protocol_uri ]
          ; "name", `String "value"
          ]
      ]
  in
  check_response
    "workspace symbol"
    (Client_request.E
       (Client_request.WorkspaceSymbol (WorkspaceSymbolParams.create ~query:"value" ())))
    workspace_symbol;
  check_response
    "signature help null"
    (Client_request.E
       (Client_request.SignatureHelp
          (SignatureHelpParams.create
             ~position:protocol_position
             ~textDocument:protocol_document
             ())))
    `Null;
  check_response
    "selection range null"
    (Client_request.E
       (Client_request.SelectionRange
          (SelectionRangeParams.create
             ~positions:[ protocol_position ]
             ~textDocument:protocol_document
             ())))
    `Null;
  check_response
    "rename null"
    (Client_request.E
       (Client_request.TextDocumentRename
          (RenameParams.create
             ~newName:"renamed"
             ~position:protocol_position
             ~textDocument:protocol_document
             ())))
    `Null;
  [%expect
    {|
    workspace symbol: rejected
    signature help null: rejected
    selection range null: rejected
    rename null: rejected
    |}]
;;

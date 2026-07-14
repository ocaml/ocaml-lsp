open Test.Import

let print_locations = function
  | None -> print_endline "[]"
  | Some locations -> Test.print_result (Locations.yojson_of_t locations)
;;

let iter_type_definition source position k =
  let makeRequest textDocument =
    Lsp.Client_request.TextDocumentTypeDefinition
      (TypeDefinitionParams.create ~textDocument ~position ())
  in
  Lsp_helpers.iter_lsp_response
    ~path:"file.ml"
    ~language_id:"ocaml"
    ~source
    ~makeRequest
    k
;;

let%expect_test "returns location of a type definition" =
  let source =
    {ocaml|(* type we are jumping on *)
type t = T of int

let x = T 43
|ocaml}
  in
  iter_type_definition source (Position.create ~line:3 ~character:4) print_locations;
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 5, "line": 1 },
          "start": { "character": 5, "line": 1 }
        },
        "uri": "file:///file.ml"
      }
    ]
    |}]
;;

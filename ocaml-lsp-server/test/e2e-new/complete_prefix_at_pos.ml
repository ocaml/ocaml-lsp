open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Complete_prefix_at_pos

let test ~line ~character ~prefix source =
  let request client =
    let open Fiber.O in
    let text_document = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let position = Position.create ~line ~character in
    let params =
      Req.Request_params.create ~text_document ~position ~prefix
      |> Req.Request_params.yojson_of_t
    in
    let+ response = Test.custom_request client Req.meth params in
    Test.print_result response
  in
  Helpers.test source request
;;

let%expect_test "Serialization works as expected" =
  let serialize = Req.For_testing.serialize in
  let print j = Test.print_result j in
  let completions =
    [ CompletionItem.t_of_yojson
        (`Assoc [ "detail", `String "'a option -> bool"; "label", `String "is_none" ])
    ]
  in
  let labels = [ "~key", "'_weak1"; "~data", "'_weak1" ] in
  print (serialize (completions, None));
  [%expect
    {|
    {
      "entries": [ { "detail": "'a option -> bool", "label": "is_none" } ],
      "context": null
    }
    |}];
  print (serialize (completions, Some []));
  [%expect
    {|
    {
      "entries": [ { "detail": "'a option -> bool", "label": "is_none" } ],
      "context": [ "application", { "labels": [] } ]
    }
    |}];
  print (serialize (completions, Some labels));
  [%expect
    {|
    {
      "entries": [ { "detail": "'a option -> bool", "label": "is_none" } ],
      "context": [
        "application",
        {
          "labels": [
            { "name": "~key", "type": "'_weak1" },
            { "name": "~data", "type": "'_weak1" }
          ]
        }
      ]
    }
    |}]
;;

let%expect_test "Can complete with prefix in buffer at position" =
  let source =
    {ocaml|
let go =
  print_endline
  @@ if Option.is_ (Some "blah")
  then "yes" else "no"
;;
|ocaml}
  in
  test ~line:3 ~character:18 ~prefix:"Option.is_" source;
  [%expect
    {|
    {
      "entries": [
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_none",
          "sortText": "0000",
          "textEdit": {
            "newText": "is_none",
            "range": {
              "end": { "character": 18, "line": 3 },
              "start": { "character": 15, "line": 3 }
            }
          }
        },
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_some",
          "sortText": "0001",
          "textEdit": {
            "newText": "is_some",
            "range": {
              "end": { "character": 18, "line": 3 },
              "start": { "character": 15, "line": 3 }
            }
          }
        }
      ],
      "context": null
    }
    |}]
;;

let%expect_test "Can complete with prefix not in buffer at position" =
  let source =
    {ocaml|
let go =
  print_endline
  @@ if  (Some "blah")
  then "yes" else "no"
;;
|ocaml}
  in
  test ~line:3 ~character:8 ~prefix:"Option.is_" source;
  [%expect
    {|
    {
      "entries": [
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_none",
          "sortText": "0000",
          "textEdit": {
            "newText": "is_none",
            "range": {
              "end": { "character": 8, "line": 3 },
              "start": { "character": 8, "line": 3 }
            }
          }
        },
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_some",
          "sortText": "0001",
          "textEdit": {
            "newText": "is_some",
            "range": {
              "end": { "character": 8, "line": 3 },
              "start": { "character": 8, "line": 3 }
            }
          }
        }
      ],
      "context": null
    }
    |}]
;;

let%expect_test "Can complete with different prefix in buffer at position" =
  let source =
    {ocaml|
let go =
  print_endline
  @@ if Or_error.is_some (Some "blah")
  then "yes" else "no"
;;
|ocaml}
  in
  test ~line:3 ~character:24 ~prefix:"Option.is_" source;
  [%expect
    {|
    {
      "entries": [
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_none",
          "sortText": "0000",
          "textEdit": {
            "newText": "is_none",
            "range": {
              "end": { "character": 24, "line": 3 },
              "start": { "character": 17, "line": 3 }
            }
          }
        },
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_some",
          "sortText": "0001",
          "textEdit": {
            "newText": "is_some",
            "range": {
              "end": { "character": 24, "line": 3 },
              "start": { "character": 17, "line": 3 }
            }
          }
        }
      ],
      "context": null
    }
    |}]
;;

let%expect_test "Completion can return function application context" =
  let source =
    {ocaml|
open MoreLabels
let (tbl : (string, int) Hashtbl.t) = Hashtbl.create 1
let () = Hashtbl.add tbl ~key
;;
|ocaml}
  in
  test ~line:3 ~character:29 ~prefix:"~key" source;
  [%expect
    {|
    {
      "entries": [],
      "context": [
        "application",
        {
          "labels": [
            { "name": "~key", "type": "string" },
            { "name": "~data", "type": "int" }
          ]
        }
      ]
    }
    |}]
;;

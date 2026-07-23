open Test.Import

let completion_item_resolve client label position =
  let data =
    `Assoc
      [ "textDocument", `Assoc [ "uri", DocumentUri.yojson_of_t Helpers.uri ]
      ; "position", Position.yojson_of_t position
      ]
  in
  Client.request client (CompletionItemResolve (CompletionItem.create ~label ~data ()))
;;

let print_completion_item item =
  CompletionItem.yojson_of_t item
  |> Yojson.Safe.pretty_to_string ~std:false
  |> print_endline
;;

let%expect_test "can get documentation for the end of document" =
  let source =
    {ocaml|List.ma
|ocaml}
  in
  let req client =
    let* response =
      completion_item_resolve client "map2" (Position.create ~line:0 ~character:5)
    in
    print_completion_item response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "documentation": "[map2 f [a1; ...; an] [b1; ...; bn]] is\n   [[f a1 b1; ...; f an bn]].\n   @raise Invalid_argument if the two lists are determined\n   to have different lengths.",
      "label": "map2"
    }
    |}]
;;

let%expect_test "completion resolve after its document changes" =
  let capabilities =
    let resolveSupport =
      ClientCompletionItemResolveOptions.create ~properties:[ "documentation" ]
    in
    let completionItem = ClientCompletionItemOptions.create ~resolveSupport () in
    let completion = CompletionClientCapabilities.create ~completionItem () in
    let textDocument = TextDocumentClientCapabilities.create ~completion () in
    ClientCapabilities.create ~textDocument ()
  in
  let source = "(** old docs *)\nlet old_value = 1\nlet _ = old_" in
  let req client =
    let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let params =
      CompletionParams.create
        ~textDocument
        ~position:(Position.create ~line:2 ~character:12)
        ()
    in
    let* completions = Client.request client (TextDocumentCompletion params) in
    let items =
      match completions with
      | None -> failwith "missing completions"
      | Some (`CompletionList { items; _ } | `List items) -> items
    in
    let item =
      List.find items ~f:(fun (item : CompletionItem.t) ->
        String.equal item.label "old_value")
      |> Option.value_exn
    in
    print_endline "Completion item before document update:";
    print_completion_item item;
    let textDocument =
      VersionedTextDocumentIdentifier.create ~uri:Helpers.uri ~version:1
    in
    let contentChanges =
      [ `TextDocumentContentChangeWholeDocument
          (TextDocumentContentChangeWholeDocument.create
             ~text:"(** new docs *)\nlet old_value = 1\nlet _ = old_")
      ]
    in
    let* () =
      Client.notification
        client
        (TextDocumentDidChange
           (DidChangeTextDocumentParams.create ~textDocument ~contentChanges))
    in
    let* item = Client.request client (CompletionItemResolve item) in
    print_endline "Same item resolved after document update to version 1:";
    print_completion_item item;
    Fiber.return ()
  in
  Helpers.test ~capabilities source req;
  [%expect
    {|
    Completion item before document update:
    {
      "data": {
        "position": { "character": 12, "line": 2 },
        "textDocument": { "uri": "file:///test.ml" }
      },
      "detail": "int",
      "kind": 12,
      "label": "old_value",
      "sortText": "0000",
      "textEdit": {
        "newText": "old_value",
        "range": {
          "end": { "character": 12, "line": 2 },
          "start": { "character": 8, "line": 2 }
        }
      }
    }
    Same item resolved after document update to version 1:
    {
      "detail": "int",
      "documentation": { "kind": "markdown", "value": "new docs" },
      "kind": 12,
      "label": "old_value",
      "sortText": "0000",
      "textEdit": {
        "newText": "old_value",
        "range": {
          "end": { "character": 12, "line": 2 },
          "start": { "character": 8, "line": 2 }
        }
      }
    }
    |}]
;;

let%expect_test "completion documentation respects the client's preferred format" =
  let capabilities =
    let resolveSupport =
      ClientCompletionItemResolveOptions.create ~properties:[ "documentation" ]
    in
    let completionItem =
      ClientCompletionItemOptions.create
        ~documentationFormat:[ MarkupKind.PlainText; MarkupKind.Markdown ]
        ~resolveSupport
        ()
    in
    let completion = CompletionClientCapabilities.create ~completionItem () in
    let textDocument = TextDocumentClientCapabilities.create ~completion () in
    ClientCapabilities.create ~textDocument ()
  in
  let source = "List.ma" in
  let req client =
    let* response =
      completion_item_resolve client "map2" (Position.create ~line:0 ~character:7)
    in
    print_completion_item response;
    Fiber.return ()
  in
  Helpers.test ~capabilities source req;
  [%expect
    {|
    {
      "documentation": {
        "kind": "markdown",
        "value": "`map2 f [a1; ...; an] [b1; ...; bn]` is `[f a1 b1; ...; f an bn]`.\n\n***@raise*** `Invalid_argument`\nif the two lists are determined to have different lengths."
      },
      "label": "map2"
    }
    |}]
;;

let%expect_test "can get documentation at arbitrary position" =
  let source =
    {ocaml|List.fld((=) 0) [1; 2; 3]
|ocaml}
  in
  let req client =
    let* response =
      completion_item_resolve client "find_all" (Position.create ~line:0 ~character:5)
    in
    print_completion_item response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "documentation": "[find_all] is another name for {!filter}.",
      "label": "find_all"
    }
    |}]
;;

let%expect_test "can resolve a dotted operator from the middle of its name" =
  let source =
    {ocaml|(** combine docs *)
let ( >>. ) x y = x + y
let _ = 1 >>. 2
|ocaml}
  in
  let req client =
    let* response =
      completion_item_resolve client ">>." (Position.create ~line:2 ~character:11)
    in
    print_completion_item response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect {| { "label": ">>." } |}]
;;

let%expect_test "completion resolve converts UTF-16 positions for Merlin" =
  let source = "let café = List.ma" in
  let req client =
    let* response =
      completion_item_resolve client "map2" (Position.create ~line:0 ~character:18)
    in
    print_completion_item response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect {| { "label": "map2" } |}]
;;

let%expect_test "can get documentation at arbitrary position (before dot)" =
  let source =
    {ocaml|module Outer = struct
  (** documentation for [Inner] *)
  module Inner = struct
    let v = ()
  end
end

let _ = ();;

Outer.Inner.v
|ocaml}
  in
  let req client =
    let* response =
      completion_item_resolve client "Inner" (Position.create ~line:9 ~character:10)
    in
    print_completion_item response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect {| { "documentation": "documentation for [Inner]", "label": "Inner" } |}]
;;

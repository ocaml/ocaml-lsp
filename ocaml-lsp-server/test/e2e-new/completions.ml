open Test.Import

let print_completion
  (completions :
    [ `CompletionList of CompletionList.t | `List of CompletionItem.t list ] option)
  =
  let print_items (items : CompletionItem.t list) =
    List.map items ~f:(fun item ->
      CompletionItem.yojson_of_t item |> Yojson.Safe.pretty_to_string ~std:false)
    |> String.concat ~sep:"\n"
    |> print_endline
  in
  match completions with
  | None -> print_endline "no completion response"
  | Some completions ->
    (match completions with
     | `List items -> print_items items
     | `CompletionList completions -> print_items completions.items)
;;

let completion client position =
  Client.request
    client
    (TextDocumentCompletion
       (CompletionParams.create
          ~position
          ~textDocument:(TextDocumentIdentifier.create ~uri:Helpers.uri)
          ()))
;;

let%expect_test "completing optional arguments" =
  let source =
    {ocaml|
let foo ?aaa ?aab ~abb () = 5

let foo_value = foo ~a
let foo_value = foo ?a
|ocaml}
  in
  let req client =
    let* resp = completion client (Position.create ~line:3 ~character:22) in
    let () = print_completion resp in
    print_endline "****************************************";
    let* resp = completion client (Position.create ~line:4 ~character:22) in
    let () = print_completion resp in
    Fiber.return ()
  in
  (* The first three results should respect the [~] prefix and contain "newText" that
     starts with a [~]. The second three should contain the prefix matching the argument
     type. The LSP could filter these to exclude those that don't match the [?] prefix,
     but since the LSP already relies on the clients to do filtering, it feels weird to
     add filtering to the LSP. *)
  Helpers.test source req;
  [%expect
    {|
    {
      "detail": "'a",
      "kind": 5,
      "label": "~aaa",
      "sortText": "0000",
      "textEdit": {
        "newText": "~aaa",
        "range": {
          "end": { "character": 22, "line": 3 },
          "start": { "character": 20, "line": 3 }
        }
      }
    }
    {
      "detail": "'b",
      "kind": 5,
      "label": "~aab",
      "sortText": "0001",
      "textEdit": {
        "newText": "~aab",
        "range": {
          "end": { "character": 22, "line": 3 },
          "start": { "character": 20, "line": 3 }
        }
      }
    }
    {
      "detail": "'c",
      "kind": 5,
      "label": "~abb",
      "sortText": "0002",
      "textEdit": {
        "newText": "~abb",
        "range": {
          "end": { "character": 22, "line": 3 },
          "start": { "character": 20, "line": 3 }
        }
      }
    }
    ****************************************
    {
      "detail": "'a",
      "kind": 5,
      "label": "?aaa",
      "sortText": "0000",
      "textEdit": {
        "newText": "?aaa",
        "range": {
          "end": { "character": 22, "line": 4 },
          "start": { "character": 20, "line": 4 }
        }
      }
    }
    {
      "detail": "'b",
      "kind": 5,
      "label": "?aab",
      "sortText": "0001",
      "textEdit": {
        "newText": "?aab",
        "range": {
          "end": { "character": 22, "line": 4 },
          "start": { "character": 20, "line": 4 }
        }
      }
    }
    {
      "detail": "'c",
      "kind": 5,
      "label": "~abb",
      "sortText": "0002",
      "textEdit": {
        "newText": "~abb",
        "range": {
          "end": { "character": 22, "line": 4 },
          "start": { "character": 20, "line": 4 }
        }
      }
    }
    |}]
;;

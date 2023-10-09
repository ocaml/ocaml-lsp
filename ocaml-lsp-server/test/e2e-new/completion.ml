open Test.Import

let iter_completions ?prep ?path ?(triggerCharacter = "")
    ?(triggerKind = CompletionTriggerKind.Invoked) ~position =
  let makeRequest textDocument =
    let context = CompletionContext.create ~triggerCharacter ~triggerKind () in
    Lsp.Client_request.TextDocumentCompletion
      (CompletionParams.create ~textDocument ~position ~context ())
  in
  Lsp_helpers.iter_LspResponse ?prep ?path ~makeRequest

let print_completions ?(prep = fun _ -> Fiber.return ()) ?(path = "foo.ml")
    ?(filter = fun _ -> true) source position =
  iter_completions ~prep ~path ~source ~position (function
      | None -> print_endline "No completion Items"
      | Some completions -> (
        let items =
          match completions with
          | `CompletionList comp -> comp.items
          | `List comp -> comp
        in
        items |> List.filter ~f:filter |> function
        | [] -> print_endline "No completions"
        | items ->
          print_endline "Completions:";
          List.iter items ~f:(fun item ->
              item |> CompletionItem.yojson_of_t
              |> Yojson.Safe.pretty_to_string ~std:false
              |> print_endline)))

let%expect_test "completions" =
  let source = {ocaml|
let testNum=11 in
test
|ocaml} in
  let position = Position.create ~line:2 ~character:4 in
  print_completions source position;
  [%expect
    {| 
Completions:
{
  "detail": "int",
  "kind": 12,
  "label": "testNum",
  "sortText": "0000",
  "textEdit": {
    "newText": "testNum",
    "range": {
      "end": { "character": 4, "line": 2 },
      "start": { "character": 0, "line": 2 }
    }
  }
}
|}]

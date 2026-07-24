open Test.Import
open Lsp_helpers

let code_action_capabilities resolveSupport =
  let codeActionLiteralSupport =
    let codeActionKind = ClientCodeActionKindOptions.create ~valueSet:[] in
    ClientCodeActionLiteralOptions.create ~codeActionKind
  in
  let codeAction =
    CodeActionClientCapabilities.create
      ~codeActionLiteralSupport
      ~dataSupport:true
      ~resolveSupport
      ()
  in
  let textDocument = TextDocumentClientCapabilities.create ~codeAction () in
  ClientCapabilities.create ~textDocument ()
;;

let print_json label json =
  Printf.printf "%s:\n%s\n" label (Yojson.Safe.pretty_to_string ~std:false json)
;;

let print_code_action_provider = function
  | None -> print_json "code action provider" `Null
  | Some (`Bool enabled) -> print_json "code action provider" (`Bool enabled)
  | Some (`CodeActionOptions options) ->
    print_json "code action provider" (CodeActionOptions.yojson_of_t options)
;;

let print_code_action label action = print_json label (CodeAction.yojson_of_t action)

let%expect_test "inline edit is computed eagerly despite resolve support" =
  let resolveSupport = ClientCodeActionResolveOptions.create ~properties:[ "edit" ] in
  let capabilities = code_action_capabilities resolveSupport in
  let handler = Client.Handler.make ~on_notification:(fun _ _ -> Fiber.return ()) () in
  (Test.run_initialized ~handler ~capabilities
   @@ fun client ->
   let run () =
     let* initialized = Client.initialized client in
     print_code_action_provider initialized.capabilities.codeActionProvider;
     let uri = DocumentUri.of_path "resolve.ml" in
     let source =
       {ocaml|let _ =
  let x = 0 in
  x + 1
|ocaml}
     in
     let* () = open_document ~language_id:"ocaml" ~client ~uri ~source in
     let range =
       Code_actions.range ~start_line:1 ~start_character:6 ~end_line:1 ~end_character:7
     in
     let textDocument = TextDocumentIdentifier.create ~uri in
     let context =
       CodeActionContext.create ~diagnostics:[] ~only:[ CodeActionKind.RefactorInline ] ()
     in
     let params = CodeActionParams.create ~textDocument ~range ~context () in
     let* response = Client.request client (CodeAction params) in
     let action =
       response
       |> Option.value_exn
       |> List.find_map ~f:(function
         | `CodeAction ({ CodeAction.title = "Inline into uses"; _ } as action) ->
           Some action
         | `CodeAction _ | `Command _ -> None)
       |> Option.value_exn
     in
     print_code_action "initial response" action;
     Test.exit_client client
   in
   run ());
  [%expect
    {|
    code action provider:
    {
      "codeActionKinds": [
        "quickfix", "refactor.extract", "refactor.inline", "combine-cases",
        "construct", "destruct (enumerate cases)",
        "destruct-line (enumerate cases, use existing match)", "inferred_intf",
        "merlin-jump-fun", "merlin-jump-let", "merlin-jump-match",
        "merlin-jump-module", "merlin-jump-module-type", "merlin-jump-next-case",
        "merlin-jump-prev-case", "open-dune", "put module name in identifiers",
        "remove module name from identifiers", "remove type annotation",
        "switch", "type-annotate", "update_intf"
      ]
    }
    initial response:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "(0)",
                "range": {
                  "end": { "character": 3, "line": 2 },
                  "start": { "character": 2, "line": 2 }
                }
              }
            ],
            "textDocument": { "uri": "file:///resolve.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "refactor.inline",
      "title": "Inline into uses"
    }
    |}]
;;

open Test.Import
open Lsp_helpers

let code_action_capabilities ?(dataSupport = true) resolveSupport =
  let codeActionLiteralSupport =
    let codeActionKind = ClientCodeActionKindOptions.create ~valueSet:[] in
    ClientCodeActionLiteralOptions.create ~codeActionKind
  in
  let codeAction =
    CodeActionClientCapabilities.create
      ~codeActionLiteralSupport
      ~dataSupport
      ~disabledSupport:true
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

let request_inline_action client ~uri ~range =
  let textDocument = TextDocumentIdentifier.create ~uri in
  let context =
    CodeActionContext.create ~diagnostics:[] ~only:[ CodeActionKind.RefactorInline ] ()
  in
  let params = CodeActionParams.create ~textDocument ~range ~context () in
  let+ response = Client.request client (CodeAction params) in
  Option.bind response ~f:(fun actions ->
    List.find_map actions ~f:(function
      | `CodeAction ({ CodeAction.title = "Inline into uses"; _ } as action) ->
        Some action
      | `CodeAction _ | `Command _ -> None))
;;

let print_optional_code_action label = function
  | None -> print_json label `Null
  | Some action -> print_code_action label action
;;

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
     let* () = Test.open_document ~client ~uri ~source () in
     let range =
       Code_actions.range ~start_line:1 ~start_character:6 ~end_line:1 ~end_character:7
     in
     let* action = request_inline_action client ~uri ~range in
     let action = Option.value_exn action in
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

let%expect_test "resolving after a document change returns the stale eager edit" =
  let resolveSupport = ClientCodeActionResolveOptions.create ~properties:[ "edit" ] in
  let capabilities = code_action_capabilities resolveSupport in
  let handler = Client.Handler.make ~on_notification:(fun _ _ -> Fiber.return ()) () in
  (Test.run_initialized ~handler ~capabilities
   @@ fun client ->
   let run () =
     let uri = DocumentUri.of_path "resolve-stale.ml" in
     let source =
       {ocaml|let _ =
  let x = 0 in
  x + 1
|ocaml}
     in
     let* () = Test.open_document ~client ~uri ~source () in
     let range =
       Code_actions.range ~start_line:1 ~start_character:6 ~end_line:1 ~end_character:7
     in
     let* action = request_inline_action client ~uri ~range in
     let action = Option.value_exn action in
     let textDocument = VersionedTextDocumentIdentifier.create ~uri ~version:1 in
     let contentChanges =
       [ `TextDocumentContentChangeWholeDocument
           (TextDocumentContentChangeWholeDocument.create
              ~text:("(* changed *)\n" ^ source))
       ]
     in
     let* () =
       Client.notification
         client
         (TextDocumentDidChange
            (DidChangeTextDocumentParams.create ~textDocument ~contentChanges))
     in
     let* result =
       Fiber.collect_errors (fun () -> Client.request client (CodeActionResolve action))
     in
     let* () =
       match result with
       | Ok resolved ->
         print_code_action "resolved after change" resolved;
         Fiber.return ()
       | Error
           [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ]
         ->
         print_json "resolve error" (Jsonrpc.Response.Error.yojson_of_t error);
         Fiber.return ()
       | Error errors -> Fiber.reraise_all errors
     in
     Test.exit_client client
   in
   run ());
  [%expect
    {|
    resolved after change:
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
            "textDocument": { "uri": "file:///resolve-stale.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "refactor.inline",
      "title": "Inline into uses"
    }
    |}]
;;

let%expect_test "an unused binding has no inline action" =
  let resolveSupport = ClientCodeActionResolveOptions.create ~properties:[ "edit" ] in
  let capabilities = code_action_capabilities resolveSupport in
  let handler = Client.Handler.make ~on_notification:(fun _ _ -> Fiber.return ()) () in
  (Test.run_initialized ~handler ~capabilities
   @@ fun client ->
   let run () =
     let uri = DocumentUri.of_path "resolve-unused.ml" in
     let source =
       {ocaml|let _ =
  let x = 0 in
  1
|ocaml}
     in
     let* () = Test.open_document ~client ~uri ~source () in
     let range =
       Code_actions.range ~start_line:1 ~start_character:6 ~end_line:1 ~end_character:7
     in
     let* action = request_inline_action client ~uri ~range in
     print_optional_code_action "inline action" action;
     Test.exit_client client
   in
   run ());
  [%expect
    {|
    inline action:
    null
    |}]
;;

let%expect_test "a shadowed inline action is disabled eagerly" =
  let resolveSupport = ClientCodeActionResolveOptions.create ~properties:[ "edit" ] in
  let capabilities = code_action_capabilities resolveSupport in
  let handler = Client.Handler.make ~on_notification:(fun _ _ -> Fiber.return ()) () in
  (Test.run_initialized ~handler ~capabilities
   @@ fun client ->
   let run () =
     let uri = DocumentUri.of_path "resolve-shadowed.ml" in
     let source =
       {ocaml|let _ =
  let y = 1 in
  let x = y in
  let y = 0 in
  x + 1
|ocaml}
     in
     let* () = Test.open_document ~client ~uri ~source () in
     let range =
       Code_actions.range ~start_line:2 ~start_character:6 ~end_line:2 ~end_character:7
     in
     let* action = request_inline_action client ~uri ~range in
     let action = Option.value_exn action in
     print_code_action "initial response" action;
     let* resolved = Client.request client (CodeActionResolve action) in
     print_code_action "after resolve" resolved;
     Test.exit_client client
   in
   run ());
  [%expect
    {|
    initial response:
    {
      "disabled": { "reason": "'y' is shadowed in inlining context" },
      "isPreferred": false,
      "kind": "refactor.inline",
      "title": "Inline into uses"
    }
    after resolve:
    {
      "disabled": { "reason": "'y' is shadowed in inlining context" },
      "isPreferred": false,
      "kind": "refactor.inline",
      "title": "Inline into uses"
    }
    |}]
;;

let%expect_test "inline edits remain eager without complete resolve support" =
  let test label ?(dataSupport = true) properties =
    let resolveSupport = ClientCodeActionResolveOptions.create ~properties in
    let capabilities = code_action_capabilities ~dataSupport resolveSupport in
    let handler = Client.Handler.make ~on_notification:(fun _ _ -> Fiber.return ()) () in
    Test.run_initialized ~handler ~capabilities
    @@ fun client ->
    let run () =
      let uri = DocumentUri.of_path (label ^ ".ml") in
      let source =
        {ocaml|let _ =
  let x = 0 in
  x + 1
|ocaml}
      in
      let* () = Test.open_document ~client ~uri ~source () in
      let range =
        Code_actions.range ~start_line:1 ~start_character:6 ~end_line:1 ~end_character:7
      in
      let* action = request_inline_action client ~uri ~range in
      print_optional_code_action label action;
      Test.exit_client client
    in
    run ()
  in
  test "no-data-support" ~dataSupport:false [ "edit" ];
  test "no-edit-resolve-support" [ "disabled" ];
  [%expect
    {|
    no-data-support:
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
            "textDocument": { "uri": "file:///no-data-support.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "refactor.inline",
      "title": "Inline into uses"
    }
    no-edit-resolve-support:
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
            "textDocument": {
              "uri": "file:///no-edit-resolve-support.ml",
              "version": 0
            }
          }
        ]
      },
      "isPreferred": false,
      "kind": "refactor.inline",
      "title": "Inline into uses"
    }
    |}]
;;

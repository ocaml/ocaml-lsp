open Test.Import

let%expect_test "initialize with empty capabilities" =
  (Test.run
   @@ fun client ->
   let run_client () = Test.start_client client in
   let run =
     let* (_ : InitializeResult.t) = Client.initialized client in
     print_endline "initialized";
     Client.request client Shutdown
   in
   Fiber.fork_and_join_unit run_client (fun () -> run >>> Client.stop client));
  [%expect {| initialized |}]
;;

let code_action_capabilities () =
  let codeActionLiteralSupport =
    let codeActionKind = ClientCodeActionKindOptions.create ~valueSet:[] in
    ClientCodeActionLiteralOptions.create ~codeActionKind
  in
  let codeAction = CodeActionClientCapabilities.create ~codeActionLiteralSupport () in
  let textDocument = TextDocumentClientCapabilities.create ~codeAction () in
  ClientCapabilities.create ~textDocument ()
;;

let%expect_test "advertises every code action kind that the server may return" =
  (Test.run
   @@ fun client ->
   let run_client () =
     Test.start_client ~capabilities:(code_action_capabilities ()) client
   in
   let run () =
     let* initialized = Client.initialized client in
     let advertised =
       match initialized.capabilities.codeActionProvider with
       | Some (`CodeActionOptions { codeActionKinds = Some kinds; _ }) -> kinds
       | None | Some (`Bool _) | Some (`CodeActionOptions _) -> []
     in
     let expected =
       [ "refactor.extract"
       ; "combine-cases"
       ; "destruct-line (enumerate cases, use existing match)"
       ; "update_intf"
       ; "switch"
       ; "merlin-jump-fun"
       ; "merlin-jump-match"
       ; "merlin-jump-let"
       ; "merlin-jump-module"
       ; "merlin-jump-module-type"
       ; "merlin-jump-next-case"
       ; "merlin-jump-prev-case"
       ]
     in
     List.iter expected ~f:(fun name ->
       let kind = CodeActionKind.t_of_yojson (`String name) in
       if not (List.mem advertised kind ~equal:Poly.equal)
       then Printf.printf "missing: %s\n" name);
     Client.request client Shutdown
   in
   Fiber.fork_and_join_unit run_client (fun () -> run () >>> Client.stop client));
  [%expect {| |}]
;;

let%expect_test "start/stop" =
  let notifs = Queue.create () in
  let handler_collecting_notifs =
    Client.Handler.make
      ~on_notification:(fun _ notif -> Queue.push notifs notif |> Fiber.return)
      ()
  in
  (Test.run ~handler:handler_collecting_notifs
   @@ fun client ->
   let run_client () =
     let capabilities =
       let window =
         let showDocument = ShowDocumentClientCapabilities.create ~support:true in
         WindowClientCapabilities.create ~showDocument ()
       in
       let textDocument =
         let codeAction =
           let codeActionLiteralSupport =
             let codeActionKind = ClientCodeActionKindOptions.create ~valueSet:[] in
             ClientCodeActionLiteralOptions.create ~codeActionKind
           in
           CodeActionClientCapabilities.create ~codeActionLiteralSupport ()
         in
         TextDocumentClientCapabilities.create ~codeAction ()
       in
       ClientCapabilities.create ~window ~textDocument ()
     in
     Test.start_client ~capabilities client
   in
   let print_init =
     let+ resp = Client.initialized client in
     print_endline "client: server initialized with:";
     InitializeResult.yojson_of_t resp
     |> Yojson.Safe.pretty_to_string ~std:false
     |> print_endline
   in
   let run =
     let* () = print_init in
     print_endline "client: shutting down server";
     Client.request client Shutdown
   in
   Fiber.fork_and_join_unit run_client (fun () -> run >>> Client.stop client));
  print_endline "\nnotifications received:";
  Queue.iter notifs ~f:(fun notif ->
    Lsp.Server_notification.to_jsonrpc notif
    |> Jsonrpc.Notification.yojson_of_t
    |> Yojson.Safe.pretty_to_string
    |> print_endline);
  [%expect
    {|
    client: server initialized with:
    {
      "capabilities": {
        "codeActionProvider": {
          "codeActionKinds": [
            "quickfix", "refactor.extract", "refactor.inline", "combine-cases",
            "construct", "destruct (enumerate cases)",
            "destruct-line (enumerate cases, use existing match)",
            "inferred_intf", "merlin-jump-fun", "merlin-jump-let",
            "merlin-jump-match", "merlin-jump-module", "merlin-jump-module-type",
            "merlin-jump-next-case", "merlin-jump-prev-case",
            "put module name in identifiers",
            "remove module name from identifiers", "remove type annotation",
            "switch", "type-annotate", "update_intf"
          ]
        },
        "codeLensProvider": { "resolveProvider": false },
        "completionProvider": {
          "resolveProvider": true,
          "triggerCharacters": [ ".", "#" ]
        },
        "declarationProvider": true,
        "definitionProvider": true,
        "documentFormattingProvider": true,
        "documentHighlightProvider": true,
        "documentRangeFormattingProvider": true,
        "documentSymbolProvider": true,
        "executeCommandProvider": {
          "commands": [
            "ocamllsp/view-metrics", "ocamllsp/open-related-source",
            "ocamllsp/merlin-jump-to-target", "ocamllsp/show-document-text",
            "ocamllsp/show-merlin-config", "dune/promote"
          ]
        },
        "experimental": {
          "ocamllsp": {
            "interfaceSpecificLangId": true,
            "handleSwitchImplIntf": true,
            "handleInferIntf": true,
            "handleTypedHoles": true,
            "handleJumpToTypedHole": true,
            "handleWrappingAstNode": true,
            "diagnostic_promotions": true,
            "handleHoverExtended": true,
            "handleMerlinCallCompatible": true,
            "handleTypeEnclosing": true,
            "handleGetDocumentation": true,
            "handleConstruct": true,
            "handleTypeSearch": true,
            "handleJump": true,
            "handlePhrase": true,
            "handleTypeExpression": true,
            "handleLocate": true,
            "handleDestruct": true,
            "handleLocateTypes": true,
            "handleRefactorExtract": true
          }
        },
        "foldingRangeProvider": true,
        "hoverProvider": true,
        "inlayHintProvider": true,
        "referencesProvider": true,
        "renameProvider": { "prepareProvider": true },
        "selectionRangeProvider": true,
        "semanticTokensProvider": {
          "full": { "delta": true },
          "legend": {
            "tokenModifiers": [
              "declaration", "definition", "readonly", "static", "deprecated",
              "abstract", "async", "modification", "documentation",
              "defaultLibrary"
            ],
            "tokenTypes": [
              "namespace", "type", "class", "enum", "interface", "struct",
              "typeParameter", "parameter", "variable", "property", "enumMember",
              "event", "function", "method", "macro", "keyword", "modifier",
              "comment", "string", "number", "regexp", "operator", "decorator"
            ]
          }
        },
        "signatureHelpProvider": {
          "triggerCharacters": [ " ", "~", "?", ":", "(" ]
        },
        "textDocumentSync": {
          "change": 2,
          "openClose": true,
          "save": { "includeText": false },
          "willSave": false,
          "willSaveWaitUntil": false
        },
        "typeDefinitionProvider": true,
        "workspace": {
          "workspaceFolders": { "changeNotifications": true, "supported": true }
        },
        "workspaceSymbolProvider": true
      },
      "serverInfo": { "name": "ocamllsp", "version": "dev" }
    }
    client: shutting down server

    notifications received:
    |}]
;;

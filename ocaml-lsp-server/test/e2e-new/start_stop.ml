open Test.Import

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
             let codeActionKind =
               CodeActionClientCapabilities.create_codeActionKind ~valueSet:[]
             in
             CodeActionClientCapabilities.create_codeActionLiteralSupport ~codeActionKind
           in
           CodeActionClientCapabilities.create ~codeActionLiteralSupport ()
         in
         TextDocumentClientCapabilities.create ~codeAction ()
       in
       ClientCapabilities.create ~window ~textDocument ()
     in
     Client.start client (InitializeParams.create ~capabilities ())
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
            "quickfix", "refactor.inline", "construct",
            "destruct (enumerate cases)", "inferred_intf",
            "put module name in identifiers",
            "remove module name from identifiers", "remove type annotation",
            "type-annotate"
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
        "documentSymbolProvider": true,
        "executeCommandProvider": {
          "commands": [
            "ocamllsp/view-metrics", "ocamllsp/open-related-source",
            "ocamllsp/show-document-text", "ocamllsp/show-merlin-config",
            "dune/promote"
          ]
        },
        "experimental": {
          "ocamllsp": {
            "interfaceSpecificLangId": true,
            "handleSwitchImplIntf": true,
            "handleInferIntf": true,
            "handleTypedHoles": true,
            "handleWrappingAstNode": true,
            "diagnostic_promotions": true,
            "handleHoverExtended": true,
            "handleMerlinCallCompatible": true,
            "handleTypeEnclosing": true,
            "handleGetDocumentation": true
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

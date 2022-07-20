open Test.Import

let semantic_tokens_full_debug = "ocamllsp/textDocument/semanticTokens/full"

let client_capabilities =
  let textDocument =
    let semanticTokens =
      (* copied from vscode v1.69.2 client capabilities for semantic tokens;
         it's easier to read in this form *)
      SemanticTokensClientCapabilities.t_of_yojson
      @@ Yojson.Safe.from_string
           {|
        {
          "dynamicRegistration": true,
          "tokenTypes": [
              "namespace",
              "type",
              "class",
              "enum",
              "interface",
              "struct",
              "typeParameter",
              "parameter",
              "variable",
              "property",
              "enumMember",
              "event",
              "function",
              "method",
              "macro",
              "keyword",
              "modifier",
              "comment",
              "string",
              "number",
              "regexp",
              "operator",
              "decorator"
          ],
          "tokenModifiers": [
              "declaration",
              "definition",
              "readonly",
              "static",
              "deprecated",
              "abstract",
              "async",
              "modification",
              "documentation",
              "defaultLibrary"
          ],
          "formats": [
              "relative"
          ],
          "requests": {
              "range": true,
              "full": {
                  "delta": true
              }
          },
          "multilineTokenSupport": false,
          "overlappingTokenSupport": false,
          "serverCancelSupport": true,
          "augmentsSyntaxTokens": true
        }
            |}
    in
    TextDocumentClientCapabilities.create ~semanticTokens ()
  in
  ClientCapabilities.create ~textDocument ()

let test :
    type resp.
       src:string
    -> (SemanticTokensParams.t -> resp Client.out_request)
    -> (resp -> Yojson.Safe.t)
    -> unit =
 fun ~src req json_of_resp ->
  let wait_for_diagnostics = Fiber.Ivar.create () in
  let handler =
    Client.Handler.make
      ~on_notification:
        (fun client -> function
          | Lsp.Server_notification.PublishDiagnostics _ ->
            (* we don't want to close the connection from client-side before we
               process diagnostics arrived on the channel. TODO: would a better
               solution be to simply flush on closing the connection because now
               semantic tokens tests is coupled to diagnostics *)
            let+ () = Fiber.Ivar.fill wait_for_diagnostics () in
            Client.state client
          | _ -> Fiber.return ())
      ()
  in
  Test.run ~handler ~extra_env:[ "OCAMLLSP_SEMANTIC_HIGHLIGHTING=full/delta" ]
    (fun client ->
      let run_client () =
        Client.start client
          (InitializeParams.create ~capabilities:client_capabilities ())
      in
      let run () =
        let* (_ : InitializeResult.t) = Client.initialized client in
        let uri = DocumentUri.of_path "test.ml" in
        let textDocument =
          TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text:src
        in
        let* () =
          Client.notification client
            (TextDocumentDidOpen
               (DidOpenTextDocumentParams.create ~textDocument))
        in
        let* resp =
          let textDocument = TextDocumentIdentifier.create ~uri in
          let params = SemanticTokensParams.create ~textDocument () in
          Client.request client (req params)
        in
        let* () =
          json_of_resp resp
          |> Yojson.Safe.pretty_to_string ~std:false
          |> print_endline |> Fiber.return
        in
        let* () =
          Fiber.fork_and_join_unit
            (fun () -> Fiber.Ivar.read wait_for_diagnostics)
            (fun () -> Client.request client Shutdown)
        in
        Client.stop client
      in
      Fiber.fork_and_join_unit run_client run)

let test_semantic_tokens_full src =
  test ~src
    (fun p -> SemanticTokensFull p)
    (fun resp ->
      Option.map resp ~f:SemanticTokens.yojson_of_t
      |> Option.value ~default:`Null)

let%expect_test "tokens for ocaml_lsp_server.ml" =
  test_semantic_tokens_full Semantic_hl_data.src0;
  [%expect
    {|
    {
      "resultId": "0",
      "data": [
        1, 7, 3, 0, 2, 1, 7, 1, 1, 1, 2, 7, 3, 3, 1, 1, 6, 3, 10, 1, 0, 7, 6, 1,
        0, 1, 6, 3, 10, 1, 0, 17, 3, 1, 0, 0, 17, 6, 1, 0, 2, 6, 1, 8, 1, 0, 4,
        4, 1, 0, 2, 6, 1, 12, 1, 0, 4, 4, 1, 0, 0, 8, 1, 1, 0, 2, 7, 1, 1, 1, 0,
        4, 3, 1, 0, 2, 7, 3, 3, 1, 1, 6, 3, 10, 1, 0, 7, 6, 1, 0, 1, 6, 3, 10, 1,
        0, 17, 3, 1, 0, 0, 17, 6, 1, 0, 2, 6, 1, 8, 0, 0, 4, 2, 10, 0, 2, 6, 1,
        12, 2, 0, 2, 2, 10, 0, 0, 5, 1, 19, 0, 3, 12, 3, 4, 0, 1, 7, 1, 5, 1, 1,
        6, 3, 9, 0, 0, 6, 3, 0, 0, 0, 4, 1, 1, 0, 1, 6, 3, 9, 0, 0, 6, 3, 1, 0,
        4, 5, 1, 3, 1, 0, 4, 3, 0, 0, 0, 4, 3, 1, 0, 1, 4, 3, 10, 1, 0, 7, 6, 1,
        0, 1, 4, 3, 10, 1, 0, 20, 3, 1, 0, 0, 20, 6, 1, 0, 2, 4, 1, 12, 2, 0, 3,
        3, 8, 0, 0, 6, 1, 1, 0, 1, 8, 3, 8, 0, 1, 4, 3, 0, 0, 0, 4, 3, 10, 0, 0,
        4, 1, 8, 0, 0, 5, 1, 8, 0, 0, 2, 1, 12, 0, 0, 2, 13, 12, 0, 0, 14, 1, 19,
        0, 1, 4, 3, 0, 0, 0, 4, 3, 10, 0, 0, 13, 1, 8, 0, 0, 6, 13, 12, 0, 0, 14,
        1, 8, 0, 1, 4, 3, 0, 0, 0, 4, 3, 10, 0, 0, 16, 1, 8, 0, 0, 6, 1, 8, 0, 2,
        7, 3, 0, 2, 0, 5, 3, 0, 0, 0, 6, 3, 4, 0, 1, 9, 9, 0, 2, 1, 9, 1, 1, 1,
        0, 4, 6, 1, 0, 4, 7, 8, 0, 2, 0, 11, 3, 0, 0, 1, 7, 1, 5, 1, 1, 6, 3, 9,
        0, 0, 6, 3, 0, 0, 0, 4, 1, 1, 0, 1, 6, 3, 9, 0, 0, 6, 3, 1, 0
      ]
    } |}]

let test_semantic_tokens_full_debug src =
  test ~src
    (fun p ->
      UnknownRequest
        { meth = semantic_tokens_full_debug
        ; params =
            Some
              (SemanticTokensParams.yojson_of_t p
              |> Jsonrpc.Structured.t_of_yojson)
        })
    Fun.id

let%expect_test "tokens for ocaml_lsp_server.ml" =
  test_semantic_tokens_full_debug Semantic_hl_data.src0;
  [%expect
    {|
    [
      {
        "start_pos": { "line": 1, "character": 7 },
        "length": 3,
        "type": "namespace",
        "modifiers": [ "definition" ]
      },
      {
        "start_pos": { "line": 2, "character": 7 },
        "length": 1,
        "type": "type",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "line": 4, "character": 7 },
        "length": 3,
        "type": "enum",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "line": 5, "character": 6 },
        "length": 3,
        "type": "enumMember",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "line": 5, "character": 13 },
        "length": 6,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 6, "character": 6 },
        "length": 3,
        "type": "enumMember",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "line": 6, "character": 23 },
        "length": 3,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 6, "character": 40 },
        "length": 6,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 8, "character": 6 },
        "length": 1,
        "type": "variable",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "line": 8, "character": 10 },
        "length": 4,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 10, "character": 6 },
        "length": 1,
        "type": "function",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "line": 10, "character": 10 },
        "length": 4,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 10, "character": 18 },
        "length": 1,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 12, "character": 7 },
        "length": 1,
        "type": "type",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "line": 12, "character": 11 },
        "length": 3,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 14, "character": 7 },
        "length": 3,
        "type": "enum",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "line": 15, "character": 6 },
        "length": 3,
        "type": "enumMember",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "line": 15, "character": 13 },
        "length": 6,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 16, "character": 6 },
        "length": 3,
        "type": "enumMember",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "line": 16, "character": 23 },
        "length": 3,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 16, "character": 40 },
        "length": 6,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 18, "character": 6 },
        "length": 1,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "line": 18, "character": 10 },
        "length": 2,
        "type": "enumMember",
        "modifiers": []
      },
      {
        "start_pos": { "line": 20, "character": 6 },
        "length": 1,
        "type": "function",
        "modifiers": [ "definition" ]
      },
      {
        "start_pos": { "line": 20, "character": 8 },
        "length": 2,
        "type": "enumMember",
        "modifiers": []
      },
      {
        "start_pos": { "line": 20, "character": 13 },
        "length": 1,
        "type": "number",
        "modifiers": []
      },
      {
        "start_pos": { "line": 23, "character": 12 },
        "length": 3,
        "type": "interface",
        "modifiers": []
      },
      {
        "start_pos": { "line": 24, "character": 7 },
        "length": 1,
        "type": "struct",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "line": 25, "character": 6 },
        "length": 3,
        "type": "property",
        "modifiers": []
      },
      {
        "start_pos": { "line": 25, "character": 12 },
        "length": 3,
        "type": "namespace",
        "modifiers": []
      },
      {
        "start_pos": { "line": 25, "character": 16 },
        "length": 1,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 26, "character": 6 },
        "length": 3,
        "type": "property",
        "modifiers": []
      },
      {
        "start_pos": { "line": 26, "character": 12 },
        "length": 3,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 30, "character": 5 },
        "length": 1,
        "type": "enum",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "line": 30, "character": 9 },
        "length": 3,
        "type": "namespace",
        "modifiers": []
      },
      {
        "start_pos": { "line": 30, "character": 13 },
        "length": 3,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 31, "character": 4 },
        "length": 3,
        "type": "enumMember",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "line": 31, "character": 11 },
        "length": 6,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 32, "character": 4 },
        "length": 3,
        "type": "enumMember",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "line": 32, "character": 24 },
        "length": 3,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 32, "character": 44 },
        "length": 6,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 34, "character": 4 },
        "length": 1,
        "type": "function",
        "modifiers": [ "definition" ]
      },
      {
        "start_pos": { "line": 34, "character": 7 },
        "length": 3,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "line": 34, "character": 13 },
        "length": 1,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 35, "character": 8 },
        "length": 3,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "line": 36, "character": 4 },
        "length": 3,
        "type": "namespace",
        "modifiers": []
      },
      {
        "start_pos": { "line": 36, "character": 8 },
        "length": 3,
        "type": "enumMember",
        "modifiers": []
      },
      {
        "start_pos": { "line": 36, "character": 12 },
        "length": 1,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "line": 36, "character": 17 },
        "length": 1,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "line": 36, "character": 19 },
        "length": 1,
        "type": "function",
        "modifiers": []
      },
      {
        "start_pos": { "line": 36, "character": 21 },
        "length": 13,
        "type": "function",
        "modifiers": []
      },
      {
        "start_pos": { "line": 36, "character": 35 },
        "length": 1,
        "type": "number",
        "modifiers": []
      },
      {
        "start_pos": { "line": 37, "character": 4 },
        "length": 3,
        "type": "namespace",
        "modifiers": []
      },
      {
        "start_pos": { "line": 37, "character": 8 },
        "length": 3,
        "type": "enumMember",
        "modifiers": []
      },
      {
        "start_pos": { "line": 37, "character": 21 },
        "length": 1,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "line": 37, "character": 27 },
        "length": 13,
        "type": "function",
        "modifiers": []
      },
      {
        "start_pos": { "line": 37, "character": 41 },
        "length": 1,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "line": 38, "character": 4 },
        "length": 3,
        "type": "namespace",
        "modifiers": []
      },
      {
        "start_pos": { "line": 38, "character": 8 },
        "length": 3,
        "type": "enumMember",
        "modifiers": []
      },
      {
        "start_pos": { "line": 38, "character": 24 },
        "length": 1,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "line": 38, "character": 30 },
        "length": 1,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "line": 40, "character": 7 },
        "length": 3,
        "type": "namespace",
        "modifiers": [ "definition" ]
      },
      {
        "start_pos": { "line": 40, "character": 12 },
        "length": 3,
        "type": "namespace",
        "modifiers": []
      },
      {
        "start_pos": { "line": 40, "character": 18 },
        "length": 3,
        "type": "interface",
        "modifiers": []
      },
      {
        "start_pos": { "line": 41, "character": 9 },
        "length": 9,
        "type": "namespace",
        "modifiers": [ "definition" ]
      },
      {
        "start_pos": { "line": 42, "character": 9 },
        "length": 1,
        "type": "type",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "line": 42, "character": 13 },
        "length": 6,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 46, "character": 7 },
        "length": 8,
        "type": "namespace",
        "modifiers": [ "definition" ]
      },
      {
        "start_pos": { "line": 46, "character": 18 },
        "length": 3,
        "type": "namespace",
        "modifiers": []
      },
      {
        "start_pos": { "line": 47, "character": 7 },
        "length": 1,
        "type": "struct",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "line": 48, "character": 6 },
        "length": 3,
        "type": "property",
        "modifiers": []
      },
      {
        "start_pos": { "line": 48, "character": 12 },
        "length": 3,
        "type": "namespace",
        "modifiers": []
      },
      {
        "start_pos": { "line": 48, "character": 16 },
        "length": 1,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "line": 49, "character": 6 },
        "length": 3,
        "type": "property",
        "modifiers": []
      },
      {
        "start_pos": { "line": 49, "character": 12 },
        "length": 3,
        "type": "type",
        "modifiers": []
      }
    ] |}]

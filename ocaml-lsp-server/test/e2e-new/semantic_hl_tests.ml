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
  Test.run
    ~handler
    ~extra_env:[ "OCAMLLSP_SEMANTIC_HIGHLIGHTING=full/delta" ]
    (fun client ->
      let run_client () =
        Client.start
          client
          (InitializeParams.create ~capabilities:client_capabilities ())
      in
      let run () =
        let* (_ : InitializeResult.t) = Client.initialized client in
        let uri = DocumentUri.of_path "test.ml" in
        let textDocument =
          TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text:src
        in
        let* () =
          Client.notification
            client
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
  test
    ~src
    (fun p -> SemanticTokensFull p)
    (fun resp ->
      Option.map resp ~f:SemanticTokens.yojson_of_t
      |> Option.value ~default:`Null)

let%expect_test "tokens for ocaml_lsp_server.ml" =
  test_semantic_tokens_full Semantic_hl_data.src0;
  [%expect
    {|
    {
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
      ],
      "resultId": "0"
    } |}]

let test_semantic_tokens_full_debug src =
  test
    ~src
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
        "start_pos": { "character": 7, "line": 1 },
        "length": 3,
        "type": "namespace",
        "modifiers": [ "definition" ]
      },
      {
        "start_pos": { "character": 7, "line": 2 },
        "length": 1,
        "type": "type",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "character": 7, "line": 4 },
        "length": 3,
        "type": "enum",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "character": 6, "line": 5 },
        "length": 3,
        "type": "enumMember",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "character": 13, "line": 5 },
        "length": 6,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 6, "line": 6 },
        "length": 3,
        "type": "enumMember",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "character": 23, "line": 6 },
        "length": 3,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 40, "line": 6 },
        "length": 6,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 6, "line": 8 },
        "length": 1,
        "type": "variable",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "character": 10, "line": 8 },
        "length": 4,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 6, "line": 10 },
        "length": 1,
        "type": "function",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "character": 10, "line": 10 },
        "length": 4,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 18, "line": 10 },
        "length": 1,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 7, "line": 12 },
        "length": 1,
        "type": "type",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "character": 11, "line": 12 },
        "length": 3,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 7, "line": 14 },
        "length": 3,
        "type": "enum",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "character": 6, "line": 15 },
        "length": 3,
        "type": "enumMember",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "character": 13, "line": 15 },
        "length": 6,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 6, "line": 16 },
        "length": 3,
        "type": "enumMember",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "character": 23, "line": 16 },
        "length": 3,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 40, "line": 16 },
        "length": 6,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 6, "line": 18 },
        "length": 1,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "character": 10, "line": 18 },
        "length": 2,
        "type": "enumMember",
        "modifiers": []
      },
      {
        "start_pos": { "character": 6, "line": 20 },
        "length": 1,
        "type": "function",
        "modifiers": [ "definition" ]
      },
      {
        "start_pos": { "character": 8, "line": 20 },
        "length": 2,
        "type": "enumMember",
        "modifiers": []
      },
      {
        "start_pos": { "character": 13, "line": 20 },
        "length": 1,
        "type": "number",
        "modifiers": []
      },
      {
        "start_pos": { "character": 12, "line": 23 },
        "length": 3,
        "type": "interface",
        "modifiers": []
      },
      {
        "start_pos": { "character": 7, "line": 24 },
        "length": 1,
        "type": "struct",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "character": 6, "line": 25 },
        "length": 3,
        "type": "property",
        "modifiers": []
      },
      {
        "start_pos": { "character": 12, "line": 25 },
        "length": 3,
        "type": "namespace",
        "modifiers": []
      },
      {
        "start_pos": { "character": 16, "line": 25 },
        "length": 1,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 6, "line": 26 },
        "length": 3,
        "type": "property",
        "modifiers": []
      },
      {
        "start_pos": { "character": 12, "line": 26 },
        "length": 3,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 5, "line": 30 },
        "length": 1,
        "type": "enum",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "character": 9, "line": 30 },
        "length": 3,
        "type": "namespace",
        "modifiers": []
      },
      {
        "start_pos": { "character": 13, "line": 30 },
        "length": 3,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 4, "line": 31 },
        "length": 3,
        "type": "enumMember",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "character": 11, "line": 31 },
        "length": 6,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 4, "line": 32 },
        "length": 3,
        "type": "enumMember",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "character": 24, "line": 32 },
        "length": 3,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 44, "line": 32 },
        "length": 6,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 4, "line": 34 },
        "length": 1,
        "type": "function",
        "modifiers": [ "definition" ]
      },
      {
        "start_pos": { "character": 7, "line": 34 },
        "length": 3,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "character": 13, "line": 34 },
        "length": 1,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 8, "line": 35 },
        "length": 3,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "character": 4, "line": 36 },
        "length": 3,
        "type": "namespace",
        "modifiers": []
      },
      {
        "start_pos": { "character": 8, "line": 36 },
        "length": 3,
        "type": "enumMember",
        "modifiers": []
      },
      {
        "start_pos": { "character": 12, "line": 36 },
        "length": 1,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "character": 17, "line": 36 },
        "length": 1,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "character": 19, "line": 36 },
        "length": 1,
        "type": "function",
        "modifiers": []
      },
      {
        "start_pos": { "character": 21, "line": 36 },
        "length": 13,
        "type": "function",
        "modifiers": []
      },
      {
        "start_pos": { "character": 35, "line": 36 },
        "length": 1,
        "type": "number",
        "modifiers": []
      },
      {
        "start_pos": { "character": 4, "line": 37 },
        "length": 3,
        "type": "namespace",
        "modifiers": []
      },
      {
        "start_pos": { "character": 8, "line": 37 },
        "length": 3,
        "type": "enumMember",
        "modifiers": []
      },
      {
        "start_pos": { "character": 21, "line": 37 },
        "length": 1,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "character": 27, "line": 37 },
        "length": 13,
        "type": "function",
        "modifiers": []
      },
      {
        "start_pos": { "character": 41, "line": 37 },
        "length": 1,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "character": 4, "line": 38 },
        "length": 3,
        "type": "namespace",
        "modifiers": []
      },
      {
        "start_pos": { "character": 8, "line": 38 },
        "length": 3,
        "type": "enumMember",
        "modifiers": []
      },
      {
        "start_pos": { "character": 24, "line": 38 },
        "length": 1,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "character": 30, "line": 38 },
        "length": 1,
        "type": "variable",
        "modifiers": []
      },
      {
        "start_pos": { "character": 7, "line": 40 },
        "length": 3,
        "type": "namespace",
        "modifiers": [ "definition" ]
      },
      {
        "start_pos": { "character": 12, "line": 40 },
        "length": 3,
        "type": "namespace",
        "modifiers": []
      },
      {
        "start_pos": { "character": 18, "line": 40 },
        "length": 3,
        "type": "interface",
        "modifiers": []
      },
      {
        "start_pos": { "character": 9, "line": 41 },
        "length": 9,
        "type": "namespace",
        "modifiers": [ "definition" ]
      },
      {
        "start_pos": { "character": 9, "line": 42 },
        "length": 1,
        "type": "type",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "character": 13, "line": 42 },
        "length": 6,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 7, "line": 46 },
        "length": 8,
        "type": "namespace",
        "modifiers": [ "definition" ]
      },
      {
        "start_pos": { "character": 18, "line": 46 },
        "length": 3,
        "type": "namespace",
        "modifiers": []
      },
      {
        "start_pos": { "character": 7, "line": 47 },
        "length": 1,
        "type": "struct",
        "modifiers": [ "declaration" ]
      },
      {
        "start_pos": { "character": 6, "line": 48 },
        "length": 3,
        "type": "property",
        "modifiers": []
      },
      {
        "start_pos": { "character": 12, "line": 48 },
        "length": 3,
        "type": "namespace",
        "modifiers": []
      },
      {
        "start_pos": { "character": 16, "line": 48 },
        "length": 1,
        "type": "type",
        "modifiers": []
      },
      {
        "start_pos": { "character": 6, "line": 49 },
        "length": 3,
        "type": "property",
        "modifiers": []
      },
      {
        "start_pos": { "character": 12, "line": 49 },
        "length": 3,
        "type": "type",
        "modifiers": []
      }
    ] |}]

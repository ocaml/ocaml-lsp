open Test.Import

let semantic_tokens_full_debug = "ocamllsp/textDocument/semanticTokens/full"

let test_initialize ~capabilities f =
  Test.run (fun client ->
    let run_client () = Test.start_client ~capabilities client in
    let run () =
      let* initialized = Client.initialized client in
      f initialized;
      Client.request client Shutdown
    in
    Fiber.fork_and_join_unit run_client (fun () -> run () >>> Client.stop client))
;;

let semantic_tokens_provider_json (initialized : InitializeResult.t) =
  InitializeResult.yojson_of_t initialized
  |> Yojson.Safe.Util.member "capabilities"
  |> Yojson.Safe.Util.member "semanticTokensProvider"
;;

let print_semantic_tokens_provider initialized =
  print_endline "semanticTokensProvider:";
  semantic_tokens_provider_json initialized |> Test.print_result
;;

let%expect_test "does not advertise semantic tokens without client support" =
  test_initialize
    ~capabilities:(ClientCapabilities.create ())
    print_semantic_tokens_provider;
  [%expect
    {|
    semanticTokensProvider:
    {
      "full": { "delta": true },
      "legend": {
        "tokenModifiers": [
          "declaration", "definition", "readonly", "static", "deprecated",
          "abstract", "async", "modification", "documentation", "defaultLibrary"
        ],
        "tokenTypes": [
          "namespace", "type", "class", "enum", "interface", "struct",
          "typeParameter", "parameter", "variable", "property", "enumMember",
          "event", "function", "method", "macro", "keyword", "modifier",
          "comment", "string", "number", "regexp", "operator", "decorator"
        ]
      }
    }
    |}]
;;

let semantic_tokens_client_capabilities
      ?full
      ?(formats = [ TokenFormat.Relative ])
      ?(token_types = [])
      ?(token_modifiers = [])
      ?multiline_token_support
      ?overlapping_token_support
      ()
  =
  let requests = ClientSemanticTokensRequestOptions.create ?full () in
  let semanticTokens =
    SemanticTokensClientCapabilities.create
      ~formats
      ~requests
      ~tokenTypes:token_types
      ~tokenModifiers:token_modifiers
      ?multilineTokenSupport:multiline_token_support
      ?overlappingTokenSupport:overlapping_token_support
      ()
  in
  let textDocument = TextDocumentClientCapabilities.create ~semanticTokens () in
  ClientCapabilities.create ~textDocument ()
;;

let%expect_test "does not advertise an unsupported semantic token format" =
  let capabilities =
    semantic_tokens_client_capabilities ~full:(`Bool true) ~formats:[] ()
  in
  test_initialize ~capabilities print_semantic_tokens_provider;
  [%expect
    {|
    semanticTokensProvider:
    {
      "full": { "delta": true },
      "legend": {
        "tokenModifiers": [
          "declaration", "definition", "readonly", "static", "deprecated",
          "abstract", "async", "modification", "documentation", "defaultLibrary"
        ],
        "tokenTypes": [
          "namespace", "type", "class", "enum", "interface", "struct",
          "typeParameter", "parameter", "variable", "property", "enumMember",
          "event", "function", "method", "macro", "keyword", "modifier",
          "comment", "string", "number", "regexp", "operator", "decorator"
        ]
      }
    }
    |}]
;;

let%expect_test "does not advertise unsupported full semantic token requests" =
  print_endline "omitted full support:";
  let capabilities = semantic_tokens_client_capabilities () in
  test_initialize ~capabilities print_semantic_tokens_provider;
  print_endline "full = false:";
  let capabilities = semantic_tokens_client_capabilities ~full:(`Bool false) () in
  test_initialize ~capabilities print_semantic_tokens_provider;
  [%expect
    {|
    omitted full support:
    semanticTokensProvider:
    {
      "full": { "delta": true },
      "legend": {
        "tokenModifiers": [
          "declaration", "definition", "readonly", "static", "deprecated",
          "abstract", "async", "modification", "documentation", "defaultLibrary"
        ],
        "tokenTypes": [
          "namespace", "type", "class", "enum", "interface", "struct",
          "typeParameter", "parameter", "variable", "property", "enumMember",
          "event", "function", "method", "macro", "keyword", "modifier",
          "comment", "string", "number", "regexp", "operator", "decorator"
        ]
      }
    }
    full = false:
    semanticTokensProvider:
    {
      "full": { "delta": true },
      "legend": {
        "tokenModifiers": [
          "declaration", "definition", "readonly", "static", "deprecated",
          "abstract", "async", "modification", "documentation", "defaultLibrary"
        ],
        "tokenTypes": [
          "namespace", "type", "class", "enum", "interface", "struct",
          "typeParameter", "parameter", "variable", "property", "enumMember",
          "event", "function", "method", "macro", "keyword", "modifier",
          "comment", "string", "number", "regexp", "operator", "decorator"
        ]
      }
    }
    |}]
;;

let print_semantic_tokens_full_provider initialized =
  print_endline "semanticTokensProvider.full:";
  semantic_tokens_provider_json initialized
  |> Yojson.Safe.Util.member "full"
  |> Test.print_result
;;

let%expect_test "does not advertise unsupported semantic token deltas" =
  let full =
    `ClientSemanticTokensRequestFullDelta
      (ClientSemanticTokensRequestFullDelta.create ~delta:false ())
  in
  let capabilities = semantic_tokens_client_capabilities ~full () in
  test_initialize ~capabilities print_semantic_tokens_full_provider;
  [%expect
    {|
    semanticTokensProvider.full:
    { "delta": true }
    |}]
;;

let%expect_test "advertises supported semantic token request variants" =
  let test label full =
    print_endline label;
    let capabilities = semantic_tokens_client_capabilities ~full () in
    test_initialize ~capabilities print_semantic_tokens_full_provider
  in
  test "full = true:" (`Bool true);
  test
    "full object without delta support:"
    (`ClientSemanticTokensRequestFullDelta
        (ClientSemanticTokensRequestFullDelta.create ()));
  test
    "full object with delta support:"
    (`ClientSemanticTokensRequestFullDelta
        (ClientSemanticTokensRequestFullDelta.create ~delta:true ()));
  [%expect
    {|
    full = true:
    semanticTokensProvider.full:
    { "delta": true }
    full object without delta support:
    semanticTokensProvider.full:
    { "delta": true }
    full object with delta support:
    semanticTokensProvider.full:
    { "delta": true }
    |}]
;;

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
;;

type 'resp req_ctx =
  { initializeResult : InitializeResult.t
  ; resp : 'resp
  }

let test
  : type resp.
    ?capabilities:ClientCapabilities.t
    -> src:string
    -> (SemanticTokensParams.t -> resp Client.out_request)
    -> (resp req_ctx -> unit Fiber.t)
    -> unit
  =
  fun ?(capabilities = client_capabilities) ~src req consume_resp ->
  let wait_for_diagnostics = Fiber.Ivar.create () in
  let handler =
    Client.Handler.make
      ~on_notification:(fun client -> function
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
  Test.run ~handler (fun client ->
    let run_client () = Test.start_client ~capabilities client in
    let run () =
      let* (initializeResult : InitializeResult.t) = Client.initialized client in
      let textDocument =
        TextDocumentItem.create
          ~uri:Helpers.uri
          ~languageId:(LanguageKind.Other "ocaml")
          ~version:0
          ~text:src
      in
      let* () =
        Client.notification
          client
          (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
      in
      let* resp =
        let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
        let params = SemanticTokensParams.create ~textDocument () in
        Client.request client (req params)
      in
      let* () = consume_resp { initializeResult; resp } in
      let* () =
        Fiber.fork_and_join_unit
          (fun () -> Fiber.Ivar.read wait_for_diagnostics)
          (fun () -> Client.request client Shutdown)
      in
      Client.stop client
    in
    Fiber.fork_and_join_unit run_client run)
;;

let print_semantic_tokens_response = function
  | None -> Test.print_result `Null
  | Some tokens ->
    SemanticTokens.yojson_of_t tokens
    |> Yojson.Safe.Util.member "data"
    |> Test.print_result
;;

let%expect_test "direct requests with unsupported client capabilities" =
  let test_request label capabilities =
    print_endline label;
    test
      ~capabilities
      ~src:"let x = 1\n"
      (fun params -> SemanticTokensFull params)
      (fun { initializeResult; resp } ->
         print_semantic_tokens_full_provider initializeResult;
         print_endline "semantic token response data:";
         print_semantic_tokens_response resp;
         Fiber.return ())
  in
  test_request "missing semantic token capability:" (ClientCapabilities.create ());
  test_request
    "unsupported token format:"
    (semantic_tokens_client_capabilities
       ~full:(`Bool true)
       ~formats:[]
       ~token_types:[ "variable"; "number" ]
       ());
  test_request
    "unsupported full requests:"
    (semantic_tokens_client_capabilities ~token_types:[ "variable"; "number" ] ());
  [%expect
    {|
    missing semantic token capability:
    semanticTokensProvider.full:
    { "delta": true }
    semantic token response data:
    [ 0, 4, 1, 8, 0, 0, 4, 1, 19, 0 ]
    unsupported token format:
    semanticTokensProvider.full:
    { "delta": true }
    semantic token response data:
    [ 0, 4, 1, 8, 0, 0, 4, 1, 19, 0 ]
    unsupported full requests:
    semanticTokensProvider.full:
    { "delta": true }
    semantic token response data:
    [ 0, 4, 1, 8, 0, 0, 4, 1, 19, 0 ]
    |}]
;;

let semantic_token_data_json data =
  `List (Array.to_list data |> List.map ~f:(fun value -> `Int value))
;;

let apply_semantic_token_edit
      source
      ({ SemanticTokensEdit.start; deleteCount; data } : SemanticTokensEdit.t)
  =
  let replacement = Option.value data ~default:[||] in
  Array.concat
    [ Array.sub source ~pos:0 ~len:start
    ; replacement
    ; Array.sub
        source
        ~pos:(start + deleteCount)
        ~len:(Array.length source - start - deleteCount)
    ]
;;

let%expect_test "semantic token deltas reconstruct a fresh full response" =
  let on_notification, diagnostics = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  let source = "let x = 1\n" in
  let updated_source = "let x = 1\nlet y = x + 2\n" in
  (Test.run_initialized ~handler ~capabilities:client_capabilities
   @@ fun client ->
   let uri = Helpers.uri in
   let* () = Test.open_document ~client ~uri ~source () in
   let textDocument = TextDocumentIdentifier.create ~uri in
   let* initial =
     Client.request
       client
       (SemanticTokensFull (SemanticTokensParams.create ~textDocument ()))
   in
   let initial_result_id, initial_data =
     match initial with
     | Some { SemanticTokens.resultId = Some result_id; data } -> result_id, data
     | None | Some { resultId = None; _ } -> failwith "full response has no result id"
   in
   let changed_document = VersionedTextDocumentIdentifier.create ~uri ~version:1 in
   let content_change =
     `TextDocumentContentChangeWholeDocument
       (TextDocumentContentChangeWholeDocument.create ~text:updated_source)
   in
   let* () =
     Client.notification
       client
       (TextDocumentDidChange
          (DidChangeTextDocumentParams.create
             ~textDocument:changed_document
             ~contentChanges:[ content_change ]))
   in
   let delta_params =
     SemanticTokensDeltaParams.create ~previousResultId:initial_result_id ~textDocument ()
   in
   let* delta = Client.request client (SemanticTokensDelta delta_params) in
   let* fresh =
     Client.request
       client
       (SemanticTokensFull (SemanticTokensParams.create ~textDocument ()))
   in
   print_endline "initial data:";
   semantic_token_data_json initial_data |> Test.print_result;
   (match delta with
    | None -> print_endline "empty delta response"
    | Some (`SemanticTokens tokens) ->
      print_endline "delta fell back to full data:";
      semantic_token_data_json tokens.data |> Test.print_result
    | Some (`SemanticTokensDelta delta) ->
      print_endline "delta edits:";
      SemanticTokensDelta.yojson_of_t delta
      |> Yojson.Safe.Util.member "edits"
      |> Test.print_result;
      let reconstructed =
        List.fold_left delta.edits ~init:initial_data ~f:apply_semantic_token_edit
      in
      print_endline "reconstructed data:";
      semantic_token_data_json reconstructed |> Test.print_result);
   print_endline "fresh full data:";
   (match fresh with
    | None -> Test.print_result `Null
    | Some tokens -> semantic_token_data_json tokens.data |> Test.print_result);
   let* () = Fiber.Ivar.read diagnostics in
   Test.exit_client client);
  [%expect
    {|
    initial data:
    [ 0, 4, 1, 8, 0, 0, 4, 1, 19, 0 ]
    delta edits:
    [
      {
        "data": [ 1, 4, 1, 8, 0, 0, 4, 1, 8, 0, 0, 2, 1, 12, 0, 0, 2, 1, 19, 0 ],
        "deleteCount": 0,
        "start": 10
      }
    ]
    reconstructed data:
    [
      0, 4, 1, 8, 0, 0, 4, 1, 19, 0, 1, 4, 1, 8, 0, 0, 4, 1, 8, 0, 0, 2, 1, 12,
      0, 0, 2, 1, 19, 0
    ]
    fresh full data:
    [
      0, 4, 1, 8, 0, 0, 4, 1, 19, 0, 1, 4, 1, 8, 0, 0, 4, 1, 8, 0, 0, 2, 1, 12,
      0, 0, 2, 1, 19, 0
    ]
    |}]
;;

let semantic_tokens_legend (initialize_result : InitializeResult.t) =
  match initialize_result.capabilities.semanticTokensProvider with
  | None -> failwith "no server capabilities for semantic tokens"
  | Some (`SemanticTokensOptions { legend; _ }) -> legend
  | Some (`SemanticTokensRegistrationOptions { legend; _ }) -> legend
;;

let print_semantic_tokens_legend_field field legend =
  Printf.printf "semanticTokensProvider.legend.%s:\n" field;
  SemanticTokensLegend.yojson_of_t legend
  |> Yojson.Safe.Util.member field
  |> Test.print_result
;;

let test_semantic_tokens_full src =
  let print_resp { initializeResult; resp } =
    Fiber.return
    @@
    match resp with
    | None -> print_endline "empty response"
    | Some { SemanticTokens.data; _ } ->
      let legend = semantic_tokens_legend initializeResult in
      print_endline
      @@ Semantic_hl_helpers.annotate_src_with_tokens
           ~legend
           ~encoded_tokens:data
           ~annot_mods:true
           src
  in
  test ~src (fun p -> SemanticTokensFull p) print_resp
;;

let%expect_test "tokens are single-line and non-overlapping when required" =
  let src =
    {|module M = struct
  let value = 1
  let f x = x + value
  let text = "first
second"
end
|}
  in
  test
    ~src
    (fun params -> SemanticTokensFull params)
    (fun { resp; _ } ->
       print_endline "protocol violations:";
       (match resp with
        | None -> Test.print_result (`String "empty semantic token response")
        | Some { SemanticTokens.data; _ } ->
          Semantic_hl_helpers.single_line_non_overlapping_violations
            ~source:src
            ~encoded_tokens:data
          |> List.map ~f:(fun violation -> `String violation)
          |> fun violations -> Test.print_result (`List violations));
       Fiber.return ());
  [%expect
    {|
    protocol violations:
    []
    |}]
;;

let%expect_test "does not advertise or send unsupported semantic token types" =
  let src = "let x = 1\n" in
  let capabilities =
    semantic_tokens_client_capabilities ~full:(`Bool true) ~token_types:[ "variable" ] ()
  in
  test
    ~capabilities
    ~src
    (fun params -> SemanticTokensFull params)
    (fun { initializeResult; resp } ->
       let legend = semantic_tokens_legend initializeResult in
       print_semantic_tokens_legend_field "tokenTypes" legend;
       (match resp with
        | None -> print_endline "empty response"
        | Some { SemanticTokens.data; _ } ->
          Semantic_hl_helpers.annotate_src_with_tokens
            ~legend
            ~encoded_tokens:data
            ~annot_mods:false
            src
          |> print_string);
       Fiber.return ());
  [%expect
    {|
    semanticTokensProvider.legend.tokenTypes:
    [
      "namespace", "type", "class", "enum", "interface", "struct",
      "typeParameter", "parameter", "variable", "property", "enumMember",
      "event", "function", "method", "macro", "keyword", "modifier", "comment",
      "string", "number", "regexp", "operator", "decorator"
    ]
    let <variable-0>x</0> = <number-1>1</1>
    |}]
;;

let%expect_test "does not advertise or send unsupported semantic token modifiers" =
  let src = "let f () = 0\n" in
  let capabilities =
    semantic_tokens_client_capabilities
      ~full:(`Bool true)
      ~token_types:[ "function"; "number" ]
      ()
  in
  test
    ~capabilities
    ~src
    (fun params -> SemanticTokensFull params)
    (fun { initializeResult; resp } ->
       let legend = semantic_tokens_legend initializeResult in
       print_semantic_tokens_legend_field "tokenModifiers" legend;
       (match resp with
        | None -> print_endline "empty response"
        | Some { SemanticTokens.data; _ } ->
          Semantic_hl_helpers.annotate_src_with_tokens
            ~legend
            ~encoded_tokens:data
            ~annot_mods:true
            src
          |> print_string);
       Fiber.return ());
  [%expect
    {|
    semanticTokensProvider.legend.tokenModifiers:
    [
      "declaration", "definition", "readonly", "static", "deprecated",
      "abstract", "async", "modification", "documentation", "defaultLibrary"
    ]
    let <function|definition-0>f</0> () = <number|-1>0</1>
    |}]
;;

let%expect_test "semantic tokens use UTF-16 positions" =
  let src = "let café = 1\n" in
  test
    ~src
    (fun params -> SemanticTokensFull params)
    (fun { resp; _ } ->
       (match resp with
        | None -> print_endline "empty response"
        | Some { SemanticTokens.data; _ } ->
          Array.iteri data ~f:(fun index value ->
            if index > 0 then print_string "; ";
            print_int value);
          print_newline ());
       Fiber.return ());
  [%expect {| 0; 4; 5; 8; 0; 0; 8; 1; 19; 0 |}]
;;

let%expect_test "tokens for ocaml_lsp_server.ml" =
  test_semantic_tokens_full Semantic_hl_data.src0;
  [%expect
    {|
    module <namespace|definition-0>Moo</0> : sig
      type <type|definition-1>t</1>

      type <enum|definition-2>koo</2> =
        | <enumMember|definition-3>Foo</3> of <type|-4>string</4>
        | <enumMember|definition-5>Bar</5> of [ `Int of <type|-6>int</6> | `String of <type|-7>string</7> ]

      val <variable|definition-8>u</8> : <type|-9>unit</9>

      val <function|definition-10>f</10> : <type|-11>unit</11> -> <type|-12>t</12>
    end = struct
      type <type|definition-13>t</13> = <type|-14>int</14>

      type <enum|definition-15>koo</15> =
        | <enumMember|definition-16>Foo</16> of <type|-17>string</17>
        | <enumMember|definition-18>Bar</18> of [ `Int of <type|-19>int</19> | `String of <type|-20>string</20> ]

      let <variable|-21>u</21> = ()

      let <function|definition-22>f</22> () = <number|-23>0</23>
    end

    module type <interface|-24>Bar</24> = sig
      type <struct|definition-25>t</25> =
        { <property|-26>foo</26> : <namespace|-27>Moo</27>.<type|-28>t</28>
        ; <property|-29>bar</29> : <type|-30>int</30>
        }
    end

    type <enum|definition-31>t</31> = <namespace|-32>Moo</32>.<type|-33>koo</33> =
      | <enumMember|definition-34>Foo</34> of <type|-35>string</35>
      | <enumMember|definition-36>Bar</36> of [ `BarInt of <type|-37>int</37> | `BarString of <type|-38>string</38> ]

    let <function|definition-39>f</39> (<variable|-40>foo</40> : <type|-41>t</41>) =
      match <variable|-42>foo</42> with
      | <namespace|-43>Moo</43>.<enumMember|-44>Foo</44> <variable|-45>s</45> -> <variable|-46>s</46> <function|-47>^</47> <function|-48>string_of_int</48> <number|-49>0</49>
      | <namespace|-50>Moo</50>.<enumMember|-51>Bar</51> (`BarInt <variable|-52>i</52>) -> <function|-53>string_of_int</53> <variable|-54>i</54>
      | <namespace|-55>Moo</55>.<enumMember|-56>Bar</56> (`BarString <variable|-57>s</57>) -> <variable|-58>s</58>

    module <namespace|definition-59>Foo</59> (<namespace|-60>Arg</60> : <interface|-61>Bar</61>) = struct
      module <namespace|definition-62>Inner_foo</62> = struct
        type <type|definition-63>t</63> = <type|-64>string</64>
      end
    end

    module <namespace|definition-65>Foo_inst</65> = <namespace|-66>Foo</66> (struct
      type <struct|definition-67>t</67> =
        { <property|-68>foo</68> : <namespace|-69>Moo</69>.<type|-70>t</70>
        ; <property|-71>bar</71> : <type|-72>int</72>
        }
    end) |}]
;;

let test_semantic_tokens_full_debug src =
  test
    ~src
    (fun p ->
       UnknownRequest
         { meth = semantic_tokens_full_debug
         ; params =
             Some (SemanticTokensParams.yojson_of_t p |> Jsonrpc.Structured.t_of_yojson)
         })
    (fun { resp; _ } ->
       resp |> Yojson.Safe.pretty_to_string ~std:false |> print_endline |> Fiber.return)
;;

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
        "start_pos": { "character": 6, "line": 20 },
        "length": 1,
        "type": "function",
        "modifiers": [ "definition" ]
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
;;

let%expect_test "highlighting longidents with space between identifiers" =
  test_semantic_tokens_full
  @@ String.trim
       {|
let foo = Bar.jar

let joo = Bar.   jar
  |};
  [%expect
    {|
    let <variable|-0>foo</0> = <namespace|-1>Bar</1>.<variable|-2>jar</2>

    let <variable|-3>joo</3> = <namespace|-4>Bar</4>.   <variable|-5>jar</5> |}]
;;

let%expect_test "highlighting longidents with space between identifiers and infix fns" =
  test_semantic_tokens_full
  @@ String.trim
       {|
Bar.(+) ;;

Bar.( + ) ;;

Bar. (+) ;;

Bar. ( + ) ;;
    |};
  [%expect
    {|
    <namespace|-0>Bar</0>.<variable|-1>(+)</1> ;;

    <namespace|-2>Bar</2>.<namespace|-3>(</3> <namespace|-4>+</4> <variable|-5>)</5> ;;

    <namespace|-6>Bar</6>. <variable|-7>(+)</7> ;;

    <namespace|-8>Bar</8>. <namespace|-9>(</9> <namespace|-10>+</10> <variable|-11>)</11> ;; |}]
;;

let%expect_test "longidents in records" =
  test_semantic_tokens_full
  @@ String.trim
       {|
module M = struct type r = { foo : int ; bar : string } end

let x = { M . foo = 0 ; bar = "bar"}
      |};
  [%expect
    {|
    module <namespace|definition-0>M</0> = struct type <struct|definition-1>r</1> = { <property|-2>foo</2> : <type|-3>int</3> ; <property|-4>bar</4> : <type|-5>string</5> } end

    let <variable|-6>x</6> = { <namespace|-7>M</7> . <property|-8>foo</8> = <number|-9>0</9> ; <property|-10>bar</10> = <string|-11>"bar"</11>} |}]
;;

let%expect_test "operators" =
  test_semantic_tokens_full
  @@ String.trim
       {|
let x = 1.0 *. 2.0
let y = 1 * 2
let z = 0 >>= 1
      |};
  [%expect
    {|
    let <variable|-0>x</0> = <number|-1>1.0</1> <function|-2>*.</2> <number|-3>2.0</3>
    let <variable|-4>y</4> = <number|-5>1</5> <function|-6>*</6> <number|-7>2</7>
    let <variable|-8>z</8> = <number|-9>0</9> <function|-10>>>=</10> <number|-11>1</11> |}]
;;

let%expect_test "operator syntax variants" =
  test_semantic_tokens_full
  @@ String.trim
       {|
module type Operators = sig
  val ( ++ ) : int -> int -> int
end

let ( ++ ) : int -> int -> int = fun left right -> left + right

let modulo = (mod)

let ( let* ) option continuation =
  match option with
  | None -> None
  | Some value -> continuation value

let ( and* ) left right =
  match left, right with
  | Some left, Some right -> Some (left, right)
  | _ -> None

let binding =
  let* left = Some 1
  and* right = Some 2 in
  Some (left + right)

let dereference reference = !reference

let ( ~! ) value = value
let prefixed = ~!1
      |};
  [%expect
    {|
    module type <interface|-0>Operators</0> = sig
      val <function|definition-1>( ++ )</1> : <type|-2>int</2> -> <type|-3>int</3> -> <type|-4>int</4>
    end

    let <function|definition-5>( ++ )</5> : int -> int -> int = fun <variable|-6>left</6> <variable|-7>right</7> -> <variable|-8>left</8> <function|-9>+</9> <variable|-10>right</10>

    let <variable|-11>modulo</11> = <variable|-12>(mod)</12>

    let <function|definition-13>( let* )</13> <variable|-14>option</14> <variable|-15>continuation</15> =
      match <variable|-16>option</16> with
      | <enumMember|-17>None</17> -> <enumMember|-18>None</18>
      | <enumMember|-19>Some</19> <variable|-20>value</20> -> <function|-21>continuation</21> <variable|-22>value</22>

    let <function|definition-23>( and* )</23> <variable|-24>left</24> <variable|-25>right</25> =
      match <variable|-26>left</26>, <variable|-27>right</27> with
      | <enumMember|-28>Some</28> <variable|-29>left</29>, <enumMember|-30>Some</30> <variable|-31>right</31> -> <enumMember|-32>Some</32> (<variable|-33>left</33>, <variable|-34>right</34>)
      | _ -> <enumMember|-35>None</35>

    let <variable|-36>binding</36> =
      let* <variable|-37>left</37> = <enumMember|-38>Some</38> <number|-39>1</39>
      and* <variable|-40>right</40> = <enumMember|-41>Some</41> <number|-42>2</42> in
      <enumMember|-43>Some</43> (<variable|-44>left</44> <function|-45>+</45> <variable|-46>right</46>)

    let <function|definition-47>dereference</47> <variable|-48>reference</48> = <function|-49>!</49><variable|-50>reference</50>

    let <function|definition-51>( ~! )</51> <variable|-52>value</52> = <variable|-53>value</53>
    let <variable|-54>prefixed</54> = <function|-55>~!</55><number|-56>1</56>
    |}]
;;

let%expect_test "function parameters" =
  test_semantic_tokens_full
  @@ String.trim
       {|
let f ~labeled ?(optional = 1) unlabeled ~renamed:local (left, right) =
  labeled + optional + unlabeled + local + left + right

let g = function
  | Some value -> value
  | None -> 0

let h (type item) (value : item) = value

let apply continuation value = continuation value

let capture parameter =
  let nested () = parameter in
  nested ()

let shadow parameter =
  let before = parameter in
  let parameter = 0 in
  before + parameter

let alias ((left, right) as pair) = left, right, pair

let constrained parameter : int = parameter

module type S = sig
  val f : labeled:int -> ?optional:string -> float -> unit
end
      |};
  [%expect
    {|
    let <function|definition-0>f</0> ~<variable|-1>labeled</1> ?(optional = <number|-2>1</2>) unlabeled ~renamed:local (left, right) =
      labeled + optional + unlabeled + local + left + right

    let g = function
      | Some value -> value
      | None -> 0

    let h (type item) (value : item) = value

    let apply continuation value = continuation value

    let capture parameter =
      let nested () = parameter in
      nested ()

    let shadow parameter =
      let before = parameter in
      let parameter = 0 in
      before + parameter

    let alias ((left, right) as pair) = left, right, pair

    let constrained parameter : int = parameter

    module type S = sig
      val f : labeled:int -> ?optional:string -> float -> unit
    end
    |}]
;;

let%expect_test "comment in unit" =
  test_semantic_tokens_full
  @@ String.trim
       {|
let y = (* comment *) 0
let x = ((* comment *))
let ((*comment*)) = ()
      |};
  [%expect
    {|
    let <variable|-0>y</0> = (* comment *) <number|-1>0</1>
    let <variable|-2>x</2> = ((* comment *))
    let ((*comment*)) = () |}]
;;

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
    null
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
    null
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
    null
    full = false:
    semanticTokensProvider:
    null
    |}]
;;

let print_semantic_tokens_full_provider initialized =
  print_endline "semanticTokensProvider.full:";
  (match semantic_tokens_provider_json initialized with
   | `Null -> `Null
   | provider -> Yojson.Safe.Util.member "full" provider)
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
    true
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
    true
    full object without delta support:
    semanticTokensProvider.full:
    true
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
    -> ?uri:DocumentUri.t
    -> ?expect_errors:bool
    -> src:string
    -> (SemanticTokensParams.t -> resp Client.out_request)
    -> (resp req_ctx -> unit Fiber.t)
    -> unit
  =
  fun ?(capabilities = client_capabilities)
    ?(uri = Helpers.uri)
    ?expect_errors
    ~src
    req
    consume_resp ->
  let wait_for_diagnostics = Fiber.Ivar.create () in
  let handler =
    Client.Handler.make
      ~on_notification:(fun client -> function
         | Lsp.Server_notification.PublishDiagnostics params ->
           let+ () =
             (* we don't want to close the connection from client-side before we
             process diagnostics arrived on the channel. TODO: would a better
             solution be to simply flush on closing the connection because now
             semantic tokens tests is coupled to diagnostics *)
             let has_errors =
               List.exists params.diagnostics ~f:(fun diagnostic ->
                 diagnostic.severity = Some DiagnosticSeverity.Error)
             in
             Fiber.Ivar.fill wait_for_diagnostics has_errors
           in
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
          ~uri
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
        let textDocument = TextDocumentIdentifier.create ~uri in
        let params = SemanticTokensParams.create ~textDocument () in
        Client.request client (req params)
      in
      let* () = consume_resp { initializeResult; resp } in
      let* () =
        Fiber.fork_and_join_unit
          (fun () ->
             let+ has_errors = Fiber.Ivar.read wait_for_diagnostics in
             Option.iter expect_errors ~f:(fun expected ->
               if not (Bool.equal expected has_errors)
               then
                 failwith
                   (Printf.sprintf
                      "expected type errors: %b, got: %b"
                      expected
                      has_errors)))
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
    null
    semantic token response data:
    [ 0, 4, 1, 8, 0, 0, 4, 1, 19, 0 ]
    unsupported token format:
    semanticTokensProvider.full:
    null
    semantic token response data:
    [ 0, 4, 1, 0, 0, 0, 4, 1, 1, 0 ]
    unsupported full requests:
    semanticTokensProvider.full:
    null
    semantic token response data:
    [ 0, 4, 1, 0, 0, 0, 4, 1, 1, 0 ]
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
        "data": [ 1, 4, 1, 8, 0, 0, 4, 1, 8, 0, 0, 2, 1, 21, 0, 0, 2, 1, 19, 0 ],
        "deleteCount": 0,
        "start": 10
      }
    ]
    reconstructed data:
    [
      0, 4, 1, 8, 0, 0, 4, 1, 19, 0, 1, 4, 1, 8, 0, 0, 4, 1, 8, 0, 0, 2, 1, 21,
      0, 0, 2, 1, 19, 0
    ]
    fresh full data:
    [
      0, 4, 1, 8, 0, 0, 4, 1, 19, 0, 1, 4, 1, 8, 0, 0, 4, 1, 8, 0, 0, 2, 1, 21,
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

let test_semantic_tokens_full ?capabilities ?uri ?expect_errors src =
  let print_resp { initializeResult; resp } =
    Fiber.return
    @@
    match resp with
    | None -> print_endline "empty response"
    | Some { SemanticTokens.data; _ } ->
      (* Overlaps can make the annotation helper silently omit later tokens. *)
      Semantic_hl_helpers.single_line_non_overlapping_violations
        ~source:src
        ~encoded_tokens:data
      |> List.iter ~f:(Printf.printf "invalid token: %s\n");
      let legend = semantic_tokens_legend initializeResult in
      print_endline
      @@ Semantic_hl_helpers.annotate_src_with_tokens
           ~legend
           ~encoded_tokens:data
           ~annot_mods:true
           src
  in
  test ?capabilities ?uri ?expect_errors ~src (fun p -> SemanticTokensFull p) print_resp
;;

let%expect_test "typed value binding produces ordered semantic tokens" =
  test
    ~src:"let a:b=0"
    (fun params -> SemanticTokensFull params)
    (fun { resp; _ } ->
       (match resp with
        | None -> print_endline "empty response"
        | Some { SemanticTokens.data; _ } ->
          semantic_token_data_json data |> Test.print_result);
       Fiber.return ());
  [%expect {| [ 0, 4, 1, 8, 0, 0, 2, 1, 1, 0, 0, 2, 1, 19, 0 ] |}]
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
    [ "variable" ]
    let <variable-0>x</0> = 1
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
    []
    let <function|-0>f</0> () = <number|-1>0</1>
    |}]
;;

let%expect_test "remaps supported semantic token modifiers" =
  let src = "let f () = 0\n" in
  let capabilities =
    semantic_tokens_client_capabilities
      ~full:(`Bool true)
      ~token_types:[ "function"; "number" ]
      ~token_modifiers:[ "definition" ]
      ()
  in
  test
    ~capabilities
    ~src
    (fun params -> SemanticTokensFull params)
    (fun { initializeResult; resp } ->
       let legend = semantic_tokens_legend initializeResult in
       Printf.printf "modifiers: %s\n" (String.concat ~sep:", " legend.tokenModifiers);
       (match resp with
        | None -> print_endline "empty response"
        | Some { SemanticTokens.data; _ } ->
          Semantic_hl_helpers.annotate_src_with_tokens
            ~legend
            ~encoded_tokens:data
            ~annot_mods:false
            src
          |> print_string;
          let enabled_modifiers =
            List.mapi legend.tokenModifiers ~f:(fun index modifier ->
              let mask = Int.shift_left 1 index in
              if Int.bit_and data.(4) mask = 0 then None else Some modifier)
            |> List.filter_opt
          in
          Printf.printf
            "function token modifiers: %s\n"
            (String.concat ~sep:", " enabled_modifiers));
       Fiber.return ());
  [%expect
    {|
    modifiers: definition
    let <function-0>f</0> () = <number-1>0</1>
    function token modifiers: definition
    |}]
;;

let%expect_test "semantic tokens use UTF-16 positions" =
  let src = "let café = 1\n" in
  test
    ~src
    (fun params -> SemanticTokensFull params)
    (fun { initializeResult; resp } ->
       (match resp with
        | None -> print_endline "empty response"
        | Some { SemanticTokens.data; _ } ->
          let legend = semantic_tokens_legend initializeResult in
          Semantic_hl_helpers.annotate_src_with_tokens
            ~legend
            ~encoded_tokens:data
            ~annot_mods:true
            src
          |> print_string);
       Fiber.return ());
  [%expect {| let <variable|-0>café</0> = <number|-1>1</1> |}]
;;

let%expect_test "tokens for ocaml_lsp_server.ml" =
  test_semantic_tokens_full Semantic_hl_data.src0;
  [%expect
    {|
    module <namespace|definition-0>Moo</0> : sig
      type <type|declaration-1>t</1>

      type <enum|declaration-2>koo</2> =
        | <enumMember|declaration-3>Foo</3> of <type|defaultLibrary-4>string</4>
        | <enumMember|declaration-5>Bar</5> of [ `Int of <type|defaultLibrary-6>int</6> | `String of <type|defaultLibrary-7>string</7> ]

      val <variable|declaration-8>u</8> : <type|defaultLibrary-9>unit</9>

      val <function|declaration-10>f</10> : <type|defaultLibrary-11>unit</11> -> <type|-12>t</12>
    end = struct
      type <type|declaration-13>t</13> = <type|defaultLibrary-14>int</14>

      type <enum|declaration-15>koo</15> =
        | <enumMember|declaration-16>Foo</16> of <type|defaultLibrary-17>string</17>
        | <enumMember|declaration-18>Bar</18> of [ `Int of <type|defaultLibrary-19>int</19> | `String of <type|defaultLibrary-20>string</20> ]

      let <variable|-21>u</21> = ()

      let <function|definition-22>f</22> () = <number|-23>0</23>
    end

    module type <interface|-24>Bar</24> = sig
      type <struct|declaration-25>t</25> =
        { <property|-26>foo</26> : <namespace|-27>Moo</27>.<type|-28>t</28>
        ; <property|-29>bar</29> : <type|defaultLibrary-30>int</30>
        }
    end

    type <enum|declaration-31>t</31> = <namespace|-32>Moo</32>.<type|-33>koo</33> =
      | <enumMember|declaration-34>Foo</34> of <type|-35>string</35>
      | <enumMember|declaration-36>Bar</36> of [ `BarInt of <type|-37>int</37> | `BarString of <type|-38>string</38> ]

    let <function|definition-39>f</39> (<parameter|-40>foo</40> : <type|-41>t</41>) =
      match <parameter|-42>foo</42> with
      | <namespace|-43>Moo</43>.<enumMember|-44>Foo</44> <variable|-45>s</45> -> <variable|-46>s</46> <operator|-47>^</47> <function|-48>string_of_int</48> <number|-49>0</49>
      | <namespace|-50>Moo</50>.<enumMember|-51>Bar</51> (`BarInt <variable|-52>i</52>) -> <function|-53>string_of_int</53> <variable|-54>i</54>
      | <namespace|-55>Moo</55>.<enumMember|-56>Bar</56> (`BarString <variable|-57>s</57>) -> <variable|-58>s</58>

    module <namespace|definition-59>Foo</59> (<namespace|-60>Arg</60> : <interface|-61>Bar</61>) = struct
      module <namespace|definition-62>Inner_foo</62> = struct
        type <type|declaration-63>t</63> = <type|defaultLibrary-64>string</64>
      end
    end

    module <namespace|definition-65>Foo_inst</65> = <namespace|-66>Foo</66> (struct
      type <struct|declaration-67>t</67> =
        { <property|-68>foo</68> : <namespace|-69>Moo</69>.<type|-70>t</70>
        ; <property|-71>bar</71> : <type|defaultLibrary-72>int</72>
        }
    end)
    |}]
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
        "modifiers": [ "defaultLibrary" ]
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
        "modifiers": [ "defaultLibrary" ]
      },
      {
        "start_pos": { "character": 40, "line": 6 },
        "length": 6,
        "type": "type",
        "modifiers": [ "defaultLibrary" ]
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
        "modifiers": [ "defaultLibrary" ]
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
        "modifiers": [ "defaultLibrary" ]
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
        "modifiers": [ "defaultLibrary" ]
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
        "modifiers": [ "defaultLibrary" ]
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
        "modifiers": [ "defaultLibrary" ]
      },
      {
        "start_pos": { "character": 40, "line": 16 },
        "length": 6,
        "type": "type",
        "modifiers": [ "defaultLibrary" ]
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
        "modifiers": [ "defaultLibrary" ]
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
        "type": "parameter",
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
        "type": "parameter",
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
        "type": "operator",
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
        "modifiers": [ "defaultLibrary" ]
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
        "modifiers": [ "defaultLibrary" ]
      }
    ]
    |}]
;;

let%expect_test "highlighting longidents with space between identifiers" =
  test_semantic_tokens_full
  @@ String.strip
       {|
let foo = Bar.jar

let joo = Bar.   jar
  |};
  [%expect
    {|
    let <variable|-0>foo</0> = <namespace|-1>Bar</1>.<variable|-2>jar</2>

    let <variable|-3>joo</3> = <namespace|-4>Bar</4>.   <variable|-5>jar</5>
    |}]
;;

let%expect_test "highlighting longidents with space between identifiers and infix fns" =
  test_semantic_tokens_full
  @@ String.strip
       {|
Bar.(+) ;;

Bar.( + ) ;;

Bar. (+) ;;

Bar. ( + ) ;;
    |};
  [%expect
    {|
    <namespace|-0>Bar</0>.(<operator|-1>+</1>) ;;

    <namespace|-2>Bar</2>.( <operator|-3>+</3> ) ;;

    <namespace|-4>Bar</4>. (<operator|-5>+</5>) ;;

    <namespace|-6>Bar</6>. ( <operator|-7>+</7> ) ;;
    |}]
;;

let%expect_test "longidents in records" =
  test_semantic_tokens_full
  @@ String.strip
       {|
module M = struct type r = { foo : int ; bar : string } end

let x = { M . foo = 0 ; bar = "bar"}
      |};
  [%expect
    {|
    module <namespace|definition-0>M</0> = struct type <struct|declaration-1>r</1> = { <property|-2>foo</2> : <type|defaultLibrary-3>int</3> ; <property|-4>bar</4> : <type|defaultLibrary-5>string</5> } end

    let <variable|-6>x</6> = { <namespace|-7>M</7> . <property|-8>foo</8> = <number|-9>0</9> ; <property|-10>bar</10> = <string|-11>"bar"</11>}
    |}]
;;

let%expect_test "parenthesized operator with spaces (#1533)" =
  test_semantic_tokens_full "let add = ( + )";
  [%expect {| let <variable|-0>add</0> = ( <operator|-1>+</1> ) |}]
;;

let%expect_test "operators" =
  test_semantic_tokens_full
  @@ String.strip
       {|
let x = 1.0 *. 2.0
let y = 1 * 2
let z = 0 >>= 1
let plus = (+)
let ( ++ ) left right = left + right
let sum = ( ++ ) 1 2
      |};
  [%expect
    {|
    let <variable|-0>x</0> = <number|-1>1.0</1> <operator|-2>*.</2> <number|-3>2.0</3>
    let <variable|-4>y</4> = <number|-5>1</5> <operator|-6>*</6> <number|-7>2</7>
    let <variable|-8>z</8> = <number|-9>0</9> <operator|-10>>>=</10> <number|-11>1</11>
    let <variable|-12>plus</12> = (<operator|-13>+</13>)
    let ( <operator|definition-14>++</14> ) <parameter|-15>left</15> <parameter|-16>right</16> = <parameter|-17>left</17> <operator|-18>+</18> <parameter|-19>right</19>
    let <variable|-20>sum</20> = ( <operator|-21>++</21> ) <number|-22>1</22> <number|-23>2</23>
    |}]
;;

let%expect_test "operator syntax variants" =
  test_semantic_tokens_full
  @@ String.strip
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
      val ( <operator|declaration-1>++</1> ) : <type|defaultLibrary-2>int</2> -> <type|defaultLibrary-3>int</3> -> <type|defaultLibrary-4>int</4>
    end

    let ( <operator|definition-5>++</5> ) : int -> int -> int = fun <parameter|-6>left</6> <parameter|-7>right</7> -> <parameter|-8>left</8> <operator|-9>+</9> <parameter|-10>right</10>

    let <variable|-11>modulo</11> = (<operator|-12>mod</12>)

    let ( <operator|definition-13>let*</13> ) <parameter|-14>option</14> <parameter|-15>continuation</15> =
      match <parameter|-16>option</16> with
      | <enumMember|-17>None</17> -> <enumMember|-18>None</18>
      | <enumMember|-19>Some</19> <variable|-20>value</20> -> <parameter|-21>continuation</21> <variable|-22>value</22>

    let ( <operator|definition-23>and*</23> ) <parameter|-24>left</24> <parameter|-25>right</25> =
      match <parameter|-26>left</26>, <parameter|-27>right</27> with
      | <enumMember|-28>Some</28> <variable|-29>left</29>, <enumMember|-30>Some</30> <variable|-31>right</31> -> <enumMember|-32>Some</32> (<variable|-33>left</33>, <variable|-34>right</34>)
      | _ -> <enumMember|-35>None</35>

    let <variable|-36>binding</36> =
      <operator|-37>let*</37> <variable|-38>left</38> = <enumMember|-39>Some</39> <number|-40>1</40>
      <operator|-41>and*</41> <variable|-42>right</42> = <enumMember|-43>Some</43> <number|-44>2</44> in
      <enumMember|-45>Some</45> (<variable|-46>left</46> <operator|-47>+</47> <variable|-48>right</48>)

    let <function|definition-49>dereference</49> <parameter|-50>reference</50> = <operator|-51>!</51><parameter|-52>reference</52>

    let ( <operator|definition-53>~!</53> ) <parameter|-54>value</54> = <parameter|-55>value</55>
    let <variable|-56>prefixed</56> = <operator|-57>~!</57><number|-58>1</58>
    |}]
;;

let%expect_test "function parameters" =
  test_semantic_tokens_full
  @@ String.strip
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
    let <function|definition-0>f</0> ~<parameter|-1>labeled</1> ?(<parameter|-2>optional</2> = <number|-3>1</3>) <parameter|-4>unlabeled</4> ~<parameter|-5>renamed</5>:<parameter|-6>local</6> (<parameter|-7>left</7>, <parameter|-8>right</8>) =
      <parameter|-9>labeled</9> <operator|-10>+</10> <parameter|-11>optional</11> <operator|-12>+</12> <parameter|-13>unlabeled</13> <operator|-14>+</14> <parameter|-15>local</15> <operator|-16>+</16> <parameter|-17>left</17> <operator|-18>+</18> <parameter|-19>right</19>

    let <function|definition-20>g</20> = function
      | <enumMember|-21>Some</21> <parameter|-22>value</22> -> <parameter|-23>value</23>
      | <enumMember|-24>None</24> -> <number|-25>0</25>

    let <function|definition-26>h</26> (type <typeParameter|-27>item</27>) (<parameter|-28>value</28> : <type|-29>item</29>) = <parameter|-30>value</30>

    let <function|definition-31>apply</31> <parameter|-32>continuation</32> <parameter|-33>value</33> = <parameter|-34>continuation</34> <parameter|-35>value</35>

    let <function|definition-36>capture</36> <parameter|-37>parameter</37> =
      let <function|definition-38>nested</38> () = <parameter|-39>parameter</39> in
      <function|-40>nested</40> ()

    let <function|definition-41>shadow</41> <parameter|-42>parameter</42> =
      let <variable|-43>before</43> = <parameter|-44>parameter</44> in
      let <variable|-45>parameter</45> = <number|-46>0</46> in
      <variable|-47>before</47> <operator|-48>+</48> <variable|-49>parameter</49>

    let <function|definition-50>alias</50> ((<parameter|-51>left</51>, <parameter|-52>right</52>) as <parameter|-53>pair</53>) = <parameter|-54>left</54>, <parameter|-55>right</55>, <parameter|-56>pair</56>

    let <function|definition-57>constrained</57> <parameter|-58>parameter</58> : <type|defaultLibrary-59>int</59> = <parameter|-60>parameter</60>

    module type <interface|-61>S</61> = sig
      val <function|declaration-62>f</62> : <parameter|-63>labeled</63>:<type|defaultLibrary-64>int</64> -> ?<parameter|-65>optional</65>:<type|defaultLibrary-66>string</66> -> <type|defaultLibrary-67>float</67> -> <type|defaultLibrary-68>unit</68>
    end
    |}]
;;

let%expect_test "built-in types" =
  test_semantic_tokens_full ~expect_errors:false
  @@ String.strip
       {|
type uses_builtin = int * string * bool

type int = Shadowed

type uses_shadowed = int

module M = struct type int = User end

type uses_qualified = M.int
      |};
  [%expect
    {|
    type <type|declaration-0>uses_builtin</0> = <type|defaultLibrary-1>int</1> * <type|defaultLibrary-2>string</2> * <type|defaultLibrary-3>bool</3>

    type <enum|declaration-4>int</4> = <enumMember|declaration-5>Shadowed</5>

    type <type|declaration-6>uses_shadowed</6> = <type|-7>int</7>

    module <namespace|definition-8>M</8> = struct type <enum|declaration-9>int</9> = <enumMember|declaration-10>User</10> end

    type <type|declaration-11>uses_qualified</11> = <namespace|-12>M</12>.<type|-13>int</13>
    |}]
;;

let%expect_test "predefined constructors and ordinary library aliases" =
  test_semantic_tokens_full
    ~uri:(DocumentUri.of_string "file:///foo.mli")
    ~expect_errors:false
    {ocaml|val scalars : int * char * string * bytes * float * bool * unit * exn * nativeint * int32 * int64 * floatarray
val containers : int array * string list * bool option * unit lazy_t * float iarray
val library_types : int ref * (int, string) result * int List.t
|ocaml};
  [%expect
    {|
    val <variable|declaration-0>scalars</0> : <type|defaultLibrary-1>int</1> * <type|defaultLibrary-2>char</2> * <type|defaultLibrary-3>string</3> * <type|defaultLibrary-4>bytes</4> * <type|defaultLibrary-5>float</5> * <type|defaultLibrary-6>bool</6> * <type|defaultLibrary-7>unit</7> * <type|defaultLibrary-8>exn</8> * <type|defaultLibrary-9>nativeint</9> * <type|defaultLibrary-10>int32</10> * <type|defaultLibrary-11>int64</11> * <type|defaultLibrary-12>floatarray</12>
    val <variable|declaration-13>containers</13> : <type|defaultLibrary-14>int</14> <type|defaultLibrary-15>array</15> * <type|defaultLibrary-16>string</16> <type|defaultLibrary-17>list</17> * <type|defaultLibrary-18>bool</18> <type|defaultLibrary-19>option</19> * <type|defaultLibrary-20>unit</20> <type|defaultLibrary-21>lazy_t</21> * <type|defaultLibrary-22>float</22> <type|defaultLibrary-23>iarray</23>
    val <variable|declaration-24>library_types</24> : <type|defaultLibrary-25>int</25> <type|-26>ref</26> * (<type|defaultLibrary-27>int</27>, <type|defaultLibrary-28>string</28>) <type|-29>result</29> * <type|defaultLibrary-30>int</30> <namespace|-31>List</31>.<type|-32>t</32>
    |}]
;;

let%expect_test "built-in spellings in unresolved qualified types" =
  test_semantic_tokens_full
    ~expect_errors:true
    {ocaml|type unbound_member = Stdlib.int
type unbound_module = Missing.string
type later = int
|ocaml};
  [%expect
    {|
    type <type|declaration-0>unbound_member</0> = <namespace|-1>Stdlib</1>.<type|-2>int</2>
    type <type|declaration-3>unbound_module</3> = <namespace|-4>Missing</4>.<type|-5>string</5>
    type <type|declaration-6>later</6> = <type|defaultLibrary-7>int</7>
    |}]
;;

let%expect_test "built-ins respect aliases and nonrec shadowing" =
  test_semantic_tokens_full
    ~expect_errors:false
    {ocaml|type alias = int
type nonrec int = int
type uses_aliases = alias * int * string
type string = C of string
type nonrec 'a list = 'a list
type container_alias = bool list
|ocaml};
  [%expect
    {|
    type <type|declaration-0>alias</0> = <type|defaultLibrary-1>int</1>
    type nonrec <type|declaration-2>int</2> = <type|defaultLibrary-3>int</3>
    type <type|declaration-4>uses_aliases</4> = <type|-5>alias</5> * <type|-6>int</6> * <type|defaultLibrary-7>string</7>
    type <enum|declaration-8>string</8> = <enumMember|declaration-9>C</9> of <type|-10>string</10>
    type nonrec <typeParameter|-11>'a</11> <type|declaration-12>list</12> = <typeParameter|-13>'a</13> <type|defaultLibrary-14>list</14>
    type <type|declaration-15>container_alias</15> = <type|defaultLibrary-16>bool</16> <type|-17>list</17>
    |}]
;;

let%expect_test "built-ins respect opens and locally abstract types" =
  test_semantic_tokens_full
    ~expect_errors:false
    {ocaml|module M = struct type int = User end
module N = M
let local (type int) (x : int) (y : string) = x, y
let opened = let open N in fun (x : int) (y : string) -> x, y
type outside = int
type locally_opened = N.(int * string)
open N
type shadowed = int * string * M.int
|ocaml};
  [%expect
    {|
    module <namespace|definition-0>M</0> = struct type <enum|declaration-1>int</1> = <enumMember|declaration-2>User</2> end
    module <namespace|definition-3>N</3> = <namespace|-4>M</4>
    let <function|definition-5>local</5> (type <typeParameter|-6>int</6>) (<parameter|-7>x</7> : <type|-8>int</8>) (<parameter|-9>y</9> : <type|defaultLibrary-10>string</10>) = <parameter|-11>x</11>, <parameter|-12>y</12>
    let <variable|-13>opened</13> = let open <namespace|-14>N</14> in fun (<parameter|-15>x</15> : <type|-16>int</16>) (<parameter|-17>y</17> : <type|defaultLibrary-18>string</18>) -> <parameter|-19>x</19>, <parameter|-20>y</20>
    type <type|declaration-21>outside</21> = <type|defaultLibrary-22>int</22>
    type <type|declaration-23>locally_opened</23> = N.(<type|-24>int</24> * <type|defaultLibrary-25>string</25>)
    open <namespace|-26>N</26>
    type <type|declaration-27>shadowed</27> = <type|-28>int</28> * <type|defaultLibrary-29>string</29> * <namespace|-30>M</30>.<type|-31>int</31>
    |}]
;;

let%expect_test "built-ins in expression annotations and class type arguments" =
  test_semantic_tokens_full
    ~expect_errors:false
    {ocaml|class ['a] box = object method id (x : 'a) = x end
type shape = < count : int; text : string >
let value (x : int #box) = x
let coerce (x : int box) = (x :> int #box)
let annotated = (1 : int)
|ocaml};
  [%expect
    {|
    class [<typeParameter|-0>'a</0>] box = object method id (<parameter|-1>x</1> : <typeParameter|-2>'a</2>) = <parameter|-3>x</3> end
    type <type|declaration-4>shape</4> = < count : <type|defaultLibrary-5>int</5>; text : <type|defaultLibrary-6>string</6> >
    let <function|definition-7>value</7> (<parameter|-8>x</8> : <type|defaultLibrary-9>int</9> #<type|-10>box</10>) = <parameter|-11>x</11>
    let <function|definition-12>coerce</12> (<parameter|-13>x</13> : <type|defaultLibrary-14>int</14> <type|-15>box</15>) = (<parameter|-16>x</16> :> <type|defaultLibrary-17>int</17> #<type|-18>box</18>)
    let <variable|-19>annotated</19> = (<number|-20>1</20> : <type|defaultLibrary-21>int</21>)
    |}]
;;

let%expect_test "built-ins in signature constraints and packages" =
  test_semantic_tokens_full
    ~uri:(DocumentUri.of_string "file:///foo.mli")
    ~expect_errors:false
    {ocaml|module type S = sig type t val value : t end
module type T = S with type t = int
module type U = S with type t := string
val packed : (module S with type t = bool)
module M : sig type int val value : int * string end
val outside : int
|ocaml};
  [%expect
    {|
    module type <interface|-0>S</0> = sig type <type|declaration-1>t</1> val <variable|declaration-2>value</2> : <type|-3>t</3> end
    module type <interface|-4>T</4> = <interface|-5>S</5> with type <type|-6>t</6> = <type|defaultLibrary-7>int</7>
    module type <interface|-8>U</8> = <interface|-9>S</9> with type <type|-10>t</10> := <type|defaultLibrary-11>string</11>
    val <namespace|declaration-12>packed</12> : (module S with type t = <type|defaultLibrary-13>bool</13>)
    module <namespace|declaration-14>M</14> : sig type <type|declaration-15>int</15> val <variable|declaration-16>value</16> : <type|-17>int</17> * <type|defaultLibrary-18>string</18> end
    val <variable|declaration-19>outside</19> : <type|defaultLibrary-20>int</20>
    |}]
;;

(* Merlin can discard an entire wrong-arity type. In that case even its
   built-in arguments must fall back to plain type tokens, not name guessing. *)
let%expect_test "built-ins in ill-typed type declarations" =
  test_semantic_tokens_full
    ~expect_errors:true
    {ocaml|type before = int
type missing_first = missing_type * string
type missing_last = bool * missing_type
type wrong_arity = (int, string) list
type int = missing_type
type shadowed = int * bytes
type later = unit option
|ocaml};
  [%expect
    {|
    type <type|declaration-0>before</0> = <type|defaultLibrary-1>int</1>
    type <type|declaration-2>missing_first</2> = <type|-3>missing_type</3> * <type|defaultLibrary-4>string</4>
    type <type|declaration-5>missing_last</5> = <type|defaultLibrary-6>bool</6> * <type|-7>missing_type</7>
    type <type|declaration-8>wrong_arity</8> = (<type|-9>int</9>, <type|-10>string</10>) <type|-11>list</11>
    type <type|declaration-12>int</12> = <type|-13>missing_type</13>
    type <type|declaration-14>shadowed</14> = <type|-15>int</15> * <type|defaultLibrary-16>bytes</16>
    type <type|declaration-17>later</17> = <type|defaultLibrary-18>unit</18> <type|defaultLibrary-19>option</19>
    |}]
;;

let%expect_test "built-ins in ill-typed signatures respect shadowing" =
  test_semantic_tokens_full
    ~uri:(DocumentUri.of_string "file:///foo.mli")
    ~expect_errors:true
    {ocaml|val before : int
val broken : missing_type -> string
module M : sig
  type int
  val inside : int * bool
  val broken : missing_type * int * bytes
end
val outside : int
|ocaml};
  [%expect
    {|
    val <variable|declaration-0>before</0> : <type|defaultLibrary-1>int</1>
    val <function|declaration-2>broken</2> : <type|-3>missing_type</3> -> <type|defaultLibrary-4>string</4>
    module <namespace|declaration-5>M</5> : sig
      type <type|declaration-6>int</6>
      val <variable|declaration-7>inside</7> : <type|-8>int</8> * <type|defaultLibrary-9>bool</9>
      val <variable|declaration-10>broken</10> : <type|-11>missing_type</11> * <type|-12>int</12> * <type|defaultLibrary-13>bytes</13>
    end
    val <variable|declaration-14>outside</14> : <type|defaultLibrary-15>int</15>
    |}]
;;

let%expect_test "built-in modifier uses the negotiated legend" =
  List.iter
    [ []; [ "defaultLibrary" ]; [ "definition"; "defaultLibrary" ] ]
    ~f:(fun token_modifiers ->
      let capabilities =
        semantic_tokens_client_capabilities
          ~full:(`Bool true)
          ~token_types:[ "type" ]
          ~token_modifiers
          ()
      in
      Test.print_result (`List (List.map token_modifiers ~f:(fun s -> `String s)));
      test_semantic_tokens_full ~capabilities ~expect_errors:false "type t = int\n");
  [%expect
    {|
    []
    type <type|-0>t</0> = <type|-1>int</1>

    [ "defaultLibrary" ]
    type <type|-0>t</0> = <type|defaultLibrary-1>int</1>

    [ "definition", "defaultLibrary" ]
    type <type|-0>t</0> = <type|defaultLibrary-1>int</1>
    |}]
;;

let%expect_test "parameter tokens in debug output" =
  test_semantic_tokens_full_debug
  @@ String.strip
       {|
let f ~labeled ?optional () = ()
      |};
  [%expect
    {|
    [
      {
        "start_pos": { "character": 4, "line": 0 },
        "length": 1,
        "type": "function",
        "modifiers": [ "definition" ]
      },
      {
        "start_pos": { "character": 7, "line": 0 },
        "length": 7,
        "type": "parameter",
        "modifiers": []
      },
      {
        "start_pos": { "character": 16, "line": 0 },
        "length": 8,
        "type": "parameter",
        "modifiers": []
      }
    ]
    |}]
;;

let%expect_test "does not advertise OCaml-specific argument modifiers" =
  let capabilities =
    semantic_tokens_client_capabilities
      ~full:(`Bool true)
      ~token_types:[ "function"; "parameter" ]
      ~token_modifiers:[ "definition"; "labeled"; "optional" ]
      ()
  in
  test_initialize ~capabilities (fun initialized ->
    semantic_tokens_legend initialized
    |> print_semantic_tokens_legend_field "tokenModifiers");
  [%expect
    {|
    semanticTokensProvider.legend.tokenModifiers:
    [ "definition" ]
    |}]
;;

let%expect_test "ill-typed buffers retain semantic tokens" =
  test_semantic_tokens_full
  @@ String.strip
       {|
type builtin_before_error = int

let mismatched parameter = parameter + "not an int"

let unbound_callee parameter = missing_function parameter

let annotated (parameter : int) : missing_type = parameter

let later parameter = parameter

type int = Shadowed

type shadowed_after_error = int

type builtin_after_error = string
      |};
  [%expect
    {|
    type <type|declaration-0>builtin_before_error</0> = <type|defaultLibrary-1>int</1>

    let <function|definition-2>mismatched</2> <parameter|-3>parameter</3> = <parameter|-4>parameter</4> <operator|-5>+</5> <string|-6>"not an int"</6>

    let <function|definition-7>unbound_callee</7> <parameter|-8>parameter</8> = <function|-9>missing_function</9> <parameter|-10>parameter</10>

    let <function|definition-11>annotated</11> (<parameter|-12>parameter</12> : <type|defaultLibrary-13>int</13>) : <type|-14>missing_type</14> = <parameter|-15>parameter</15>

    let <function|definition-16>later</16> <parameter|-17>parameter</17> = <parameter|-18>parameter</18>

    type <enum|declaration-19>int</19> = <enumMember|declaration-20>Shadowed</20>

    type <type|declaration-21>shadowed_after_error</21> = <type|-22>int</22>

    type <type|declaration-23>builtin_after_error</23> = <type|defaultLibrary-24>string</24>
    |}]
;;

(* Bindings are syntactic, but references depend on the recovered typed tree.
   Pin both the surviving parameter references and the variable/function fallback. *)
let%expect_test "parameters with an unbound type annotation" =
  test_semantic_tokens_full
    {ocaml|let f (parameter : missing_type) = parameter
let later parameter = parameter
|ocaml};
  [%expect
    {|
    let <function|definition-0>f</0> (<parameter|-1>parameter</1> : <type|-2>missing_type</2>) = <parameter|-3>parameter</3>
    let <function|definition-4>later</4> <parameter|-5>parameter</5> = <parameter|-6>parameter</6>
    |}]
;;

let%expect_test "rejected parameter patterns retain syntactic bindings" =
  test_semantic_tokens_full
    {ocaml|let f (((left, right) as pair) : int) = left, right, pair
let later parameter = parameter
|ocaml};
  [%expect
    {|
    let <function|definition-0>f</0> (((<parameter|-1>left</1>, <parameter|-2>right</2>) as <parameter|-3>pair</3>) : <type|defaultLibrary-4>int</4>) = <variable|-5>left</5>, <variable|-6>right</6>, <parameter|-7>pair</7>
    let <function|definition-8>later</8> <parameter|-9>parameter</9> = <parameter|-10>parameter</10>
    |}]
;;

let%expect_test "rejected function cases fall back for unresolved references" =
  test_semantic_tokens_full
    {ocaml|let f = function
  | 0 -> 0
  | Some (value, apply) -> apply value
let later parameter = parameter
|ocaml};
  [%expect
    {|
    let <function|definition-0>f</0> = function
      | <number|-1>0</1> -> <number|-2>0</2>
      | <enumMember|-3>Some</3> (<parameter|-4>value</4>, <parameter|-5>apply</5>) -> <function|-6>apply</6> <variable|-7>value</7>
    let <function|definition-8>later</8> <parameter|-9>parameter</9> = <parameter|-10>parameter</10>
    |}]
;;

let%expect_test "ill-typed guards retain branch parameters" =
  test_semantic_tokens_full
    {ocaml|let f = function
  | Some value when value + "bad" -> value
  | Some value -> value
  | None -> 0
|ocaml};
  [%expect
    {|
    let <function|definition-0>f</0> = function
      | <enumMember|-1>Some</1> <parameter|-2>value</2> when <variable|-3>value</3> <operator|-4>+</4> <string|-5>"bad"</5> -> <parameter|-6>value</6>
      | <enumMember|-7>Some</7> <parameter|-8>value</8> -> <parameter|-9>value</9>
      | <enumMember|-10>None</10> -> <number|-11>0</11>
    |}]
;;

let%expect_test "ill-typed defaults and annotations preserve parameter labels" =
  test_semantic_tokens_full
    {ocaml|let f outer ?(optional : int = "bad") ~renamed:local () =
  outer, optional, local
let optional = 0
let g ?(optional = missing optional) () = optional
let h ~(labeled : missing_type) = labeled
let i ~renamed:(local : missing_type) = local
let later parameter = parameter
|ocaml};
  [%expect
    {|
    let <function|definition-0>f</0> <parameter|-1>outer</1> ?(<parameter|-2>optional</2> : <type|defaultLibrary-3>int</3> = <string|-4>"bad"</4>) ~<parameter|-5>renamed</5>:<parameter|-6>local</6> () =
      <parameter|-7>outer</7>, <parameter|-8>optional</8>, <parameter|-9>local</9>
    let <variable|-10>optional</10> = <number|-11>0</11>
    let <function|definition-12>g</12> ?(<parameter|-13>optional</13> = <function|-14>missing</14> <variable|-15>optional</15>) () = <parameter|-16>optional</16>
    let <function|definition-17>h</17> ~(<parameter|-18>labeled</18> : <type|-19>missing_type</19>) = <parameter|-20>labeled</20>
    let <function|definition-21>i</21> ~<parameter|-22>renamed</22>:(<parameter|-23>local</23> : <type|-24>missing_type</24>) = <parameter|-25>local</25>
    let <function|definition-26>later</26> <parameter|-27>parameter</27> = <parameter|-28>parameter</28>
    |}]
;;

let%expect_test "ill-typed interfaces retain parameter labels" =
  test_semantic_tokens_full
    ~uri:(DocumentUri.of_string "file:///foo.mli")
    {ocaml|val before : int
val f : labeled:missing_type -> ?optional:int -> unit -> int
val later : other:int -> int
|ocaml};
  [%expect
    {|
    val <variable|declaration-0>before</0> : <type|defaultLibrary-1>int</1>
    val <function|declaration-2>f</2> : <parameter|-3>labeled</3>:<type|-4>missing_type</4> -> ?<parameter|-5>optional</5>:<type|defaultLibrary-6>int</6> -> <type|defaultLibrary-7>unit</7> -> <type|defaultLibrary-8>int</8>
    val <function|declaration-9>later</9> : <parameter|-10>other</10>:<type|defaultLibrary-11>int</11> -> <type|defaultLibrary-12>int</12>
    |}]
;;

let%expect_test "ill-typed bodies preserve captured parameters and shadowing" =
  test_semantic_tokens_full
    {ocaml|let f parameter =
  let nested () = parameter + "bad" in
  let before = parameter in
  let parameter = missing in
  let after () = parameter in
  nested (), before, after ()
let g parameter =
  match missing parameter with
  | Some parameter -> parameter
  | None -> parameter
let later parameter = parameter
|ocaml};
  [%expect
    {|
    let <function|definition-0>f</0> <parameter|-1>parameter</1> =
      let <function|definition-2>nested</2> () = <parameter|-3>parameter</3> <operator|-4>+</4> <string|-5>"bad"</5> in
      let <variable|-6>before</6> = <parameter|-7>parameter</7> in
      let <variable|-8>parameter</8> = <variable|-9>missing</9> in
      let <function|definition-10>after</10> () = <variable|-11>parameter</11> in
      <function|-12>nested</12> (), <variable|-13>before</13>, <function|-14>after</14> ()
    let <function|definition-15>g</15> <parameter|-16>parameter</16> =
      match <function|-17>missing</17> <parameter|-18>parameter</18> with
      | <enumMember|-19>Some</19> <variable|-20>parameter</20> -> <variable|-21>parameter</21>
      | <enumMember|-22>None</22> -> <parameter|-23>parameter</23>
    let <function|definition-24>later</24> <parameter|-25>parameter</25> = <parameter|-26>parameter</26>
    |}]
;;

let test_semantic_tokens_edits ~source changes =
  let diagnostics = Fiber.Mvar.create () in
  let handler =
    Client.Handler.make
      ~on_notification:(fun _ -> function
         | PublishDiagnostics params -> Fiber.Mvar.write diagnostics params
         | _ -> Fiber.return ())
      ()
  in
  Test.run_initialized ~handler ~capabilities:client_capabilities
  @@ fun client ->
  let* initialized = Client.initialized client in
  let legend = semantic_tokens_legend initialized in
  let uri = Helpers.uri in
  let textDocument = TextDocumentIdentifier.create ~uri in
  let full () =
    let+ response =
      Client.request
        client
        (SemanticTokensFull (SemanticTokensParams.create ~textDocument ()))
    in
    match response with
    | Some { SemanticTokens.resultId = Some result_id; data } -> result_id, data
    | None | Some { resultId = None; _ } -> failwith "full response has no result id"
  in
  let print_snapshot source data =
    let* diagnostic = Fiber.Mvar.read diagnostics in
    let has_errors =
      List.exists diagnostic.diagnostics ~f:(fun diagnostic ->
        diagnostic.severity = Some DiagnosticSeverity.Error)
    in
    Printf.printf "type errors: %b\n" has_errors;
    (match
       Semantic_hl_helpers.single_line_non_overlapping_violations
         ~source
         ~encoded_tokens:data
     with
     | [] -> ()
     | violations -> failwith (String.concat ~sep:"\n" violations));
    print_endline
      (Semantic_hl_helpers.annotate_src_with_tokens
         ~legend
         ~encoded_tokens:data
         ~annot_mods:true
         source);
    Fiber.return ()
  in
  let* () = Test.open_document ~client ~uri ~source () in
  let* initial_id, initial_data = full () in
  print_endline "initial:";
  let* () = print_snapshot source initial_data in
  let rec edit source previous_id previous_data = function
    | [] ->
      if not (Array.equal Int.equal initial_data previous_data)
      then failwith "repair did not restore the initial tokens";
      Test.exit_client client
    | (version, label, line, character, old_text, newText) :: rest ->
      let range =
        Range.create
          ~start:(Position.create ~line ~character)
          ~end_:(Position.create ~line ~character:(character + String.length old_text))
      in
      let source = Test.apply_edits source [ TextEdit.create ~range ~newText ] in
      let* () =
        Client.notification
          client
          (TextDocumentDidChange
             (DidChangeTextDocumentParams.create
                ~textDocument:(VersionedTextDocumentIdentifier.create ~uri ~version)
                ~contentChanges:
                  [ `TextDocumentContentChangePartial
                      (TextDocumentContentChangePartial.create ~range ~text:newText ())
                  ]))
      in
      let* delta =
        Client.request
          client
          (SemanticTokensDelta
             (SemanticTokensDeltaParams.create
                ~previousResultId:previous_id
                ~textDocument
                ()))
      in
      let reconstructed =
        match delta with
        | Some (`SemanticTokensDelta { edits; _ }) ->
          List.fold_left edits ~init:previous_data ~f:apply_semantic_token_edit
        | None | Some (`SemanticTokens _) -> failwith "expected a delta response"
      in
      let* fresh_id, fresh_data = full () in
      if not (Array.equal Int.equal reconstructed fresh_data)
      then failwith "delta differs from fresh full response";
      Printf.printf "%s (delta matches full):\n" label;
      let* () = print_snapshot source reconstructed in
      edit source fresh_id fresh_data rest
  in
  edit source initial_id initial_data changes
;;

let%expect_test
    "semantic tokens and deltas recover after breaking and repairing a pattern"
  =
  test_semantic_tokens_edits
    ~source:
      {ocaml|let f = function
  | None -> 0
  | Some (value, apply) -> apply value
let later parameter = parameter
|ocaml}
    [ 1, "broken", 1, 4, "None", "0"; 2, "repaired", 1, 4, "0", "None" ];
  [%expect
    {|
    initial:
    type errors: false
    let <function|definition-0>f</0> = function
      | <enumMember|-1>None</1> -> <number|-2>0</2>
      | <enumMember|-3>Some</3> (<parameter|-4>value</4>, <parameter|-5>apply</5>) -> <parameter|-6>apply</6> <parameter|-7>value</7>
    let <function|definition-8>later</8> <parameter|-9>parameter</9> = <parameter|-10>parameter</10>

    broken (delta matches full):
    type errors: true
    let <function|definition-0>f</0> = function
      | <number|-1>0</1> -> <number|-2>0</2>
      | <enumMember|-3>Some</3> (<parameter|-4>value</4>, <parameter|-5>apply</5>) -> <function|-6>apply</6> <variable|-7>value</7>
    let <function|definition-8>later</8> <parameter|-9>parameter</9> = <parameter|-10>parameter</10>

    repaired (delta matches full):
    type errors: false
    let <function|definition-0>f</0> = function
      | <enumMember|-1>None</1> -> <number|-2>0</2>
      | <enumMember|-3>Some</3> (<parameter|-4>value</4>, <parameter|-5>apply</5>) -> <parameter|-6>apply</6> <parameter|-7>value</7>
    let <function|definition-8>later</8> <parameter|-9>parameter</9> = <parameter|-10>parameter</10>
    |}]
;;

let%expect_test "built-in modifiers and deltas track shadowing and error recovery" =
  test_semantic_tokens_edits
    ~source:
      {ocaml|type num = Other
type uses = int * string
|ocaml}
    [ 1, "shadowed", 0, 5, "num", "int"
    ; 2, "broken", 0, 11, "Other", "missing_type"
    ; 3, "repaired", 0, 11, "missing_type", "Other"
    ; 4, "unshadowed", 0, 5, "int", "num"
    ];
  [%expect
    {|
    initial:
    type errors: false
    type <enum|declaration-0>num</0> = <enumMember|declaration-1>Other</1>
    type <type|declaration-2>uses</2> = <type|defaultLibrary-3>int</3> * <type|defaultLibrary-4>string</4>

    shadowed (delta matches full):
    type errors: false
    type <enum|declaration-0>int</0> = <enumMember|declaration-1>Other</1>
    type <type|declaration-2>uses</2> = <type|-3>int</3> * <type|defaultLibrary-4>string</4>

    broken (delta matches full):
    type errors: true
    type <type|declaration-0>int</0> = <type|-1>missing_type</1>
    type <type|declaration-2>uses</2> = <type|-3>int</3> * <type|defaultLibrary-4>string</4>

    repaired (delta matches full):
    type errors: false
    type <enum|declaration-0>int</0> = <enumMember|declaration-1>Other</1>
    type <type|declaration-2>uses</2> = <type|-3>int</3> * <type|defaultLibrary-4>string</4>

    unshadowed (delta matches full):
    type errors: false
    type <enum|declaration-0>num</0> = <enumMember|declaration-1>Other</1>
    type <type|declaration-2>uses</2> = <type|defaultLibrary-3>int</3> * <type|defaultLibrary-4>string</4>
    |}]
;;

let%expect_test "semantic tokens for GADTs, objects, fields, and functors" =
  test_semantic_tokens_full
    {ocaml|type 'a cell = { mutable field : 'a }
type packed = Pack : 'a * ('a -> string) -> packed
class virtual base = object (self)
  method virtual value : int
  method get = self#value
end
class child = object
  inherit base
  method value = 1
end
let update cell = cell.field <- cell.field
let use_object object_ = object_#get; new child
module type S = functor (M : sig type t end) -> sig type u = M.t end
|ocaml};
  [%expect
    {|
    type <typeParameter|-0>'a</0> <struct|declaration-1>cell</1> = { mutable <property|-2>field</2> : <typeParameter|-3>'a</3> }
    type <enum|declaration-4>packed</4> = <enumMember|declaration-5>Pack</5> : <typeParameter|-6>'a</6> * (<typeParameter|-7>'a</7> -> <type|defaultLibrary-8>string</8>) -> <type|-9>packed</9>
    class virtual base = object (<variable|-10>self</10>)
      method virtual value : <type|defaultLibrary-11>int</11>
      method get = <parameter|-12>self</12>#<method|-13>value</13>
    end
    class child = object
      inherit base
      method value = <number|-14>1</14>
    end
    let <function|definition-15>update</15> <parameter|-16>cell</16> = <parameter|-17>cell</17>.<variable|-18>field</18> <- <parameter|-19>cell</19>.<property|-20>field</20>
    let <function|definition-21>use_object</21> <parameter|-22>object_</22> = <parameter|-23>object_</23>#<method|-24>get</24>; new <class|-25>child</25>
    module type <interface|-26>S</26> = functor (<namespace|-27>M</27> : sig type <type|declaration-28>t</28> end) -> sig type <type|declaration-29>u</29> = <namespace|-30>M</30>.<type|-31>t</31> end
    |}]
;;

let%expect_test "comment in unit" =
  test_semantic_tokens_full
  @@ String.strip
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

let%expect_test "module type with constraints" =
  test_semantic_tokens_full
  @@ String.strip
       {|
module type S = sig module M : sig type t end end
module N = struct type t = int end
module type T = S with module M = N
      |};
  [%expect
    {|
    module type <interface|-0>S</0> = sig module <namespace|declaration-1>M</1> : sig type <type|declaration-2>t</2> end end
    module <namespace|definition-3>N</3> = struct type <type|declaration-4>t</4> = <type|defaultLibrary-5>int</5> end
    module type <interface|-6>T</6> = <interface|-7>S</7> with module <namespace|-8>M</8> = <namespace|-9>N</9>
    |}]
;;

let%expect_test "with type constraints produce non-overlapping semantic tokens" =
  (* Check ranges as well as annotations: an overlapping token makes the source
     annotation helper omit later tokens. *)
  List.iter
    [ "type 'a t = 'a list"; "type 'a t := 'a list"; "type M.t = int"; "type M.t := int" ]
    ~f:(fun constraint_ ->
      let src =
        Printf.sprintf
          "module type S = sig type 'a t module M : sig type t end end\n\
           module type T = S with %s\n\
           let after = 0\n"
          constraint_
      in
      Printf.printf "%s:\n" constraint_;
      test
        ~src
        (fun params -> SemanticTokensFull params)
        (fun { initializeResult; resp } ->
           let { SemanticTokens.data; _ } = Option.value_exn resp in
           print_endline "protocol violations:";
           Semantic_hl_helpers.single_line_non_overlapping_violations
             ~source:src
             ~encoded_tokens:data
           |> Test.print_list (fun violation -> `String violation);
           print_endline
             (Semantic_hl_helpers.annotate_src_with_tokens
                ~legend:(semantic_tokens_legend initializeResult)
                ~encoded_tokens:data
                ~annot_mods:false
                src);
           Fiber.return ()));
  [%expect
    {|
    type 'a t = 'a list:
    protocol violations:
    []
    module type <interface-0>S</0> = sig type <typeParameter-1>'a</1> <type-2>t</2> module <namespace-3>M</3> : sig type <type-4>t</4> end end
    module type <interface-5>T</5> = <interface-6>S</6> with type <typeParameter-7>'a</7> <type-8>t</8> = <typeParameter-9>'a</9> <type-10>list</10>
    let <variable-11>after</11> = <number-12>0</12>

    type 'a t := 'a list:
    protocol violations:
    []
    module type <interface-0>S</0> = sig type <typeParameter-1>'a</1> <type-2>t</2> module <namespace-3>M</3> : sig type <type-4>t</4> end end
    module type <interface-5>T</5> = <interface-6>S</6> with type <typeParameter-7>'a</7> <type-8>t</8> := <typeParameter-9>'a</9> <type-10>list</10>
    let <variable-11>after</11> = <number-12>0</12>

    type M.t = int:
    protocol violations:
    []
    module type <interface-0>S</0> = sig type <typeParameter-1>'a</1> <type-2>t</2> module <namespace-3>M</3> : sig type <type-4>t</4> end end
    module type <interface-5>T</5> = <interface-6>S</6> with type <namespace-7>M</7>.<type-8>t</8> = <type-9>int</9>
    let <variable-10>after</10> = <number-11>0</11>

    type M.t := int:
    protocol violations:
    []
    module type <interface-0>S</0> = sig type <typeParameter-1>'a</1> <type-2>t</2> module <namespace-3>M</3> : sig type <type-4>t</4> end end
    module type <interface-5>T</5> = <interface-6>S</6> with type <namespace-7>M</7>.<type-8>t</8> := <type-9>int</9>
    let <variable-10>after</10> = <number-11>0</11>
    |}]
;;

let%expect_test "open in a signature" =
  test_semantic_tokens_full
  @@ String.strip
       {|
module M = struct type t = int end
module type S = sig
  open M
  val x : t
end
      |};
  [%expect
    {|
    module <namespace|definition-0>M</0> = struct type <type|declaration-1>t</1> = <type|defaultLibrary-2>int</2> end
    module type <interface|-3>S</3> = sig
      open <namespace|-4>M</4>
      val <variable|declaration-5>x</5> : <type|-6>t</6>
    end
    |}]
;;

let%expect_test "syntax matrix covers remaining highlighting branches" =
  test_semantic_tokens_full
    {ocaml|type 'a poly = 'a list
type r = C of { x : int }
type packed = Pack : 'a * ('a -> string) -> packed
let id : int -> int = fun x -> x
let poly : 'a. 'a -> 'a = fun x -> x
let f = function 0 -> 1 | Pack (x, g) -> g x | _ -> 0
let () = print_int (f 1)
let () = ignore (Array.get [|1|] 0)
let xs = 1 :: 2 :: []
let x = 1 in
let r = { x }
let { x; y } = { x = 1; y = 2 }
let (module M : S) = m
let () = s#m
let o = object end
let p = (module M : S)
let () = [%foo bar]
let g (type a) (v : a) = v
let h = (fun z -> z : int -> int)
let* b = m in b
let () = let module L = struct end in ()
module F (X : S) = struct end
module M2 : S = struct end
module type T = S
module type Alias = M2
module type Constrained = S with type t = int and module N = M2
let long = M2(F).x
let openpat = match m with M2.(C) -> 1 | #t -> 2 | _ -> 0
|ocaml};
  [%expect
    {|
    type <typeParameter|-0>'a</0> <type|declaration-1>poly</1> = <typeParameter|-2>'a</2> <type|defaultLibrary-3>list</3>
    type <enum|declaration-4>r</4> = <enumMember|declaration-5>C</5> of { <property|-6>x</6> : <type|defaultLibrary-7>int</7> }
    type <enum|declaration-8>packed</8> = <enumMember|declaration-9>Pack</9> : <typeParameter|-10>'a</10> * (<typeParameter|-11>'a</11> -> <type|defaultLibrary-12>string</12>) -> <type|-13>packed</13>
    let <function|definition-14>id</14> : int -> int = fun <parameter|-15>x</15> -> <parameter|-16>x</16>
    let <function|definition-17>poly</17> : 'a. 'a -> 'a = fun <parameter|-18>x</18> -> <parameter|-19>x</19>
    let <function|definition-20>f</20> = function <number|-21>0</21> -> <number|-22>1</22> | <enumMember|-23>Pack</23> (<parameter|-24>x</24>, <parameter|-25>g</25>) -> <function|-26>g</26> <variable|-27>x</27> | _ -> <number|-28>0</28>
    let () = <function|-29>print_int</29> (<function|-30>f</30> <number|-31>1</31>)
    let () = <function|-32>ignore</32> (Array.get [|<number|-33>1</33>|] <number|-34>0</34>)
    let <variable|-35>xs</35> = <number|-36>1</36> :: <number|-37>2</37> :: []
    let <variable|-38>x</38> = <number|-39>1</39> in
    let <variable|-40>r</40> = { x }
    let { x; y } = { <property|-41>x</41> = <number|-42>1</42>; <property|-43>y</43> = <number|-44>2</44> }
    let (module <namespace|-45>M</45> : S) = <variable|-46>m</46>
    let () = <variable|-47>s</47>#<method|-48>m</48>
    let <variable|-49>o</49> = object end
    let <variable|-50>p</50> = (module <namespace|-51>M</51> : S)
    let () = [%foo <variable|-52>bar</52>]
    let <function|definition-53>g</53> (type <typeParameter|-54>a</54>) (<parameter|-55>v</55> : <type|-56>a</56>) = <parameter|-57>v</57>
    let <variable|-58>h</58> = (fun <parameter|-59>z</59> -> <parameter|-60>z</60> : <type|defaultLibrary-61>int</61> -> <type|defaultLibrary-62>int</62>)
    let* b <operator|-63>=</63> <function|-64>m</64> in <variable|-65>b</65>
    let () = let module <namespace|-66>L</66> = struct end in ()
    module <namespace|definition-67>F</67> (<namespace|-68>X</68> : <interface|-69>S</69>) = struct end
    module <namespace|definition-70>M2</70> : <interface|-71>S</71> = struct end
    module type <interface|-72>T</72> = <interface|-73>S</73>
    module type <interface|-74>Alias</74> = <interface|-75>M2</75>
    module type <interface|-76>Constrained</76> = <interface|-77>S</77> with type <type|-78>t</78> = <type|-79>int</79> and module <namespace|-80>N</80> = <namespace|-81>M2</81>
    let <variable|-82>long</82> = <enumMember|-83>M2</83>(<enumMember|-84>F</84>).<property|-85>x</85>
    let <variable|-86>openpat</86> = match <variable|-87>m</87> with <namespace|-88>M2</88>.(<enumMember|-89>C</89>) -> <number|-90>1</90> | #<type|-91>t</91> -> <number|-92>2</92> | _ -> <number|-93>0</93>
    |}]
;;

let%expect_test "semantic token delta with a middle edit" =
  let on_notification, diagnostics = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  let source = "let x = 1\nlet y = 2\nlet z = 3\n" in
  let updated_source = "let x = 1\nlet y = \"s\"\nlet z = 3\n" in
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
      |> Test.print_result);
   let* () = Fiber.Ivar.read diagnostics in
   Test.exit_client client);
  [%expect
    {|
    initial data:
    [
      0, 4, 1, 8, 0, 0, 4, 1, 19, 0, 1, 4, 1, 8, 0, 0, 4, 1, 19, 0, 1, 4, 1, 8,
      0, 0, 4, 1, 19, 0
    ]
    delta edits:
    [ { "data": [ 3, 18 ], "deleteCount": 2, "start": 17 } ]
    |}]
;;

let%expect_test "semantic token debug request rejects bad arguments" =
  (Test.run_initialized ~capabilities:client_capabilities
   @@ fun client ->
   let uri = DocumentUri.of_path "cram.t" in
   let* () =
     Test.open_document ~language_id:"cram" ~client ~uri ~source:"  $ echo hi\n" ()
   in
   let* missing =
     Fiber.collect_errors (fun () ->
       Client.request
         client
         (UnknownRequest { meth = semantic_tokens_full_debug; params = None }))
   in
   let* () =
     match missing with
     | Error
         [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ] ->
       Printf.printf "missing params: %s\n" error.message;
       Fiber.return ()
     | Error errors -> Fiber.reraise_all errors
     | Ok _ ->
       print_endline "missing params unexpectedly succeeded";
       Fiber.return ()
   in
   let textDocument = TextDocumentIdentifier.create ~uri in
   let params = SemanticTokensParams.create ~textDocument () in
   let* non_merlin =
     Fiber.collect_errors (fun () ->
       Client.request
         client
         (UnknownRequest
            { meth = semantic_tokens_full_debug
            ; params =
                Some
                  (SemanticTokensParams.yojson_of_t params
                   |> Jsonrpc.Structured.t_of_yojson)
            }))
   in
   let* () =
     match non_merlin with
     | Error
         [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ] ->
       Printf.printf "non-merlin doc: %s\n" error.message;
       Fiber.return ()
     | Error errors -> Fiber.reraise_all errors
     | Ok _ ->
       print_endline "non-merlin doc unexpectedly succeeded";
       Fiber.return ()
   in
   Test.shutdown_client client);
  [%expect
    {|
    missing params: ocamllsp/textDocument/semanticTokens/full expects an argument but didn't receive any
    non-merlin doc: expected a merlin document
    |}]
;;

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
        | <enumMember|declaration-3>Foo</3> of <type|-4>string</4>
        | <enumMember|declaration-5>Bar</5> of [ `Int of <type|-6>int</6> | `String of <type|-7>string</7> ]

      val <variable|declaration-8>u</8> : <type|-9>unit</9>

      val <function|declaration-10>f</10> : <type|-11>unit</11> -> <type|-12>t</12>
    end = struct
      type <type|declaration-13>t</13> = <type|-14>int</14>

      type <enum|declaration-15>koo</15> =
        | <enumMember|declaration-16>Foo</16> of <type|-17>string</17>
        | <enumMember|declaration-18>Bar</18> of [ `Int of <type|-19>int</19> | `String of <type|-20>string</20> ]

      let <variable|-21>u</21> = ()

      let <function|definition-22>f</22> () = <number|-23>0</23>
    end

    module type <interface|-24>Bar</24> = sig
      type <struct|declaration-25>t</25> =
        { <property|-26>foo</26> : <namespace|-27>Moo</27>.<type|-28>t</28>
        ; <property|-29>bar</29> : <type|-30>int</30>
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
        type <type|declaration-63>t</63> = <type|-64>string</64>
      end
    end

    module <namespace|definition-65>Foo_inst</65> = <namespace|-66>Foo</66> (struct
      type <struct|declaration-67>t</67> =
        { <property|-68>foo</68> : <namespace|-69>Moo</69>.<type|-70>t</70>
        ; <property|-71>bar</71> : <type|-72>int</72>
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
    module <namespace|definition-0>M</0> = struct type <struct|declaration-1>r</1> = { <property|-2>foo</2> : <type|-3>int</3> ; <property|-4>bar</4> : <type|-5>string</5> } end

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
      val ( <operator|declaration-1>++</1> ) : <type|-2>int</2> -> <type|-3>int</3> -> <type|-4>int</4>
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

    let <function|definition-57>constrained</57> <parameter|-58>parameter</58> : <type|-59>int</59> = <parameter|-60>parameter</60>

    module type <interface|-61>S</61> = sig
      val <function|declaration-62>f</62> : <parameter|-63>labeled</63>:<type|-64>int</64> -> ?<parameter|-65>optional</65>:<type|-66>string</66> -> <type|-67>float</67> -> <type|-68>unit</68>
    end
    |}]
;;

let%expect_test "built-in types" =
  test_semantic_tokens_full
  @@ String.strip
       {|
type uses_builtin = int * string * bool

type int = Shadowed

type uses_shadowed = int

type uses_qualified_builtin = Stdlib.int
      |};
  [%expect
    {|
    type <type|declaration-0>uses_builtin</0> = <type|-1>int</1> * <type|-2>string</2> * <type|-3>bool</3>

    type <enum|declaration-4>int</4> = <enumMember|declaration-5>Shadowed</5>

    type <type|declaration-6>uses_shadowed</6> = <type|-7>int</7>

    type <type|declaration-8>uses_qualified_builtin</8> = <namespace|-9>Stdlib</9>.<type|-10>int</10>
    |}]
;;

let%expect_test "parameter modifiers in debug output" =
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
        "modifiers": [ "labeled" ]
      },
      {
        "start_pos": { "character": 16, "line": 0 },
        "length": 8,
        "type": "parameter",
        "modifiers": [ "optional" ]
      }
    ]
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
    type <type|declaration-0>builtin_before_error</0> = <type|-1>int</1>

    let <function|definition-2>mismatched</2> <parameter|-3>parameter</3> = <parameter|-4>parameter</4> <operator|-5>+</5> <string|-6>"not an int"</6>

    let <function|definition-7>unbound_callee</7> <parameter|-8>parameter</8> = <function|-9>missing_function</9> <parameter|-10>parameter</10>

    let <function|definition-11>annotated</11> (<parameter|-12>parameter</12> : <type|-13>int</13>) : <type|-14>missing_type</14> = <parameter|-15>parameter</15>

    let <function|definition-16>later</16> <parameter|-17>parameter</17> = <parameter|-18>parameter</18>

    type <enum|declaration-19>int</19> = <enumMember|declaration-20>Shadowed</20>

    type <type|declaration-21>shadowed_after_error</21> = <type|-22>int</22>

    type <type|declaration-23>builtin_after_error</23> = <type|-24>string</24>
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
    type <enum|declaration-4>packed</4> = <enumMember|declaration-5>Pack</5> : <typeParameter|-6>'a</6> * (<typeParameter|-7>'a</7> -> <type|-8>string</8>) -> <type|-9>packed</9>
    class virtual base = object (<variable|-10>self</10>)
      method virtual value : <type|-11>int</11>
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

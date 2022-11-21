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

type 'resp req_ctx =
  { initializeResult : InitializeResult.t
  ; resp : 'resp
  }

let test :
    type resp.
       src:string
    -> (SemanticTokensParams.t -> resp Client.out_request)
    -> (resp req_ctx -> unit Fiber.t)
    -> unit =
 fun ~src req consume_resp ->
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
  Test.run ~handler (fun client ->
      let run_client () =
        Client.start
          client
          (InitializeParams.create ~capabilities:client_capabilities ())
      in
      let run () =
        let* (initializeResult : InitializeResult.t) =
          Client.initialized client
        in
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
        let* () = consume_resp { initializeResult; resp } in
        let* () =
          Fiber.fork_and_join_unit
            (fun () -> Fiber.Ivar.read wait_for_diagnostics)
            (fun () -> Client.request client Shutdown)
        in
        Client.stop client
      in
      Fiber.fork_and_join_unit run_client run)

let test_semantic_tokens_full src =
  let print_resp { initializeResult; resp } =
    Fiber.return
    @@
    match resp with
    | None -> print_endline "empty response"
    | Some { SemanticTokens.data; _ } ->
      let legend =
        match
          initializeResult.InitializeResult.capabilities
            .ServerCapabilities.semanticTokensProvider
        with
        | None -> failwith "no server capabilities for semantic tokens"
        | Some (`SemanticTokensOptions { legend; _ }) -> legend
        | Some (`SemanticTokensRegistrationOptions { legend; _ }) -> legend
      in
      print_endline
      @@ Semantic_hl_helpers.annotate_src_with_tokens
           ~legend
           ~encoded_tokens:data
           ~annot_mods:true
           src
  in
  test ~src (fun p -> SemanticTokensFull p) print_resp

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

      let <variable|-21>u</21> = <enumMember|-22>()</22>

      let <function|definition-23>f</23> <enumMember|-24>()</24> = <number|-25>0</25>
    end

    module type <interface|-26>Bar</26> = sig
      type <struct|definition-27>t</27> =
        { <property|-28>foo</28> : <namespace|-29>Moo</29>.<type|-30>t</30>
        ; <property|-31>bar</31> : <type|-32>int</32>
        }
    end

    type <enum|definition-33>t</33> = <namespace|-34>Moo</34>.<type|-35>koo</35> =
      | <enumMember|definition-36>Foo</36> of <type|-37>string</37>
      | <enumMember|definition-38>Bar</38> of [ `BarInt of <type|-39>int</39> | `BarString of <type|-40>string</40> ]

    let <function|definition-41>f</41> (<variable|-42>foo</42> : <type|-43>t</43>) =
      match <variable|-44>foo</44> with
      | <namespace|-45>Moo</45>.<enumMember|-46>Foo</46> <variable|-47>s</47> -> <variable|-48>s</48> <function|-49>^</49> <function|-50>string_of_int</50> <number|-51>0</51>
      | <namespace|-52>Moo</52>.<enumMember|-53>Bar</53> (`BarInt <variable|-54>i</54>) -> <function|-55>string_of_int</55> <variable|-56>i</56>
      | <namespace|-57>Moo</57>.<enumMember|-58>Bar</58> (`BarString <variable|-59>s</59>) -> <variable|-60>s</60>

    module <namespace|definition-61>Foo</61> (<namespace|-62>Arg</62> : <interface|-63>Bar</63>) = struct
      module <namespace|definition-64>Inner_foo</64> = struct
        type <type|definition-65>t</65> = <type|-66>string</66>
      end
    end

    module <namespace|definition-67>Foo_inst</67> = <namespace|-68>Foo</68> (struct
      type <struct|definition-69>t</69> =
        { <property|-70>foo</70> : <namespace|-71>Moo</71>.<type|-72>t</72>
        ; <property|-73>bar</73> : <type|-74>int</74>
        }
    end) |}]

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
    (fun { resp; _ } ->
      resp
      |> Yojson.Safe.pretty_to_string ~std:false
      |> print_endline |> Fiber.return)

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

let%expect_test "highlighting longidents with space between identifiers" =
  test_semantic_tokens_full
  @@ String.trim {|
let foo = Bar.jar

let joo = Bar.   jar
  |};
  [%expect
    {|
    let <variable|-0>foo</0> = <namespace|-1>Bar</1>.<variable|-2>jar</2>

    let <variable|-3>joo</3> = <namespace|-4>Bar</4>.   <variable|-5>jar</5> |}]

let%expect_test "highlighting longidents with space between identifiers and \
                 infix fns" =
  test_semantic_tokens_full
  @@ String.trim {|
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

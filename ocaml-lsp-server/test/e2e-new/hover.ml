open Test.Import

let print_hover hover =
  match hover with
  | None -> print_endline "no hover response"
  | Some hover ->
    hover |> Hover.yojson_of_t |> Yojson.Safe.pretty_to_string ~std:false |> print_endline
;;

let hover client position =
  Client.request
    client
    (TextDocumentHover
       { HoverParams.position
       ; textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri
       ; workDoneToken = None
       })
;;

let%expect_test "object method call" =
  let source =
    {ocaml|
let f (o : <  g : int -> unit >) = o#g 4
|ocaml}
  in
  let position = Position.create ~line:1 ~character:38 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "int -> unit" },
      "range": {
        "end": { "character": 38, "line": 1 },
        "start": { "character": 35, "line": 1 }
      }
    }
    |}]
;;

let%expect_test "hover warning description" =
  let source =
    {ocaml|
let f x = 1
|ocaml}
  in
  let position = Position.create ~line:1 ~character:6 in
  let on_notification, diagnostics_ivar = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  Test.run ~handler (fun client ->
    let run_client () =
      Client.start
        client
        (InitializeParams.create ~capabilities:(ClientCapabilities.create ()) ())
    in
    let run () =
      let* (_ : InitializeResult.t) = Client.initialized client in
      let textDocument =
        TextDocumentItem.create
          ~uri:Helpers.uri
          ~languageId:"ocaml"
          ~version:0
          ~text:source
      in
      let* () =
        Client.notification
          client
          (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
      in
      let* () = Fiber.Ivar.read diagnostics_ivar in
      let* resp = hover client position in
      let () = print_hover resp in
      let* () = Client.request client Shutdown in
      Client.stop client
    in
    Fiber.fork_and_join_unit run_client run);
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "'a" },
      "range": {
        "end": { "character": 7, "line": 1 },
        "start": { "character": 6, "line": 1 }
      }
    }
    |}]
;;

let%expect_test "hover warning description 2" =
  let source =
    {ocaml|
let f x = match x with 1 -> 1
|ocaml}
  in
  let position = Position.create ~line:1 ~character:12 in
  let diagnostics = Fiber.Ivar.create () in
  let on_notification _ = function
    | Lsp.Server_notification.PublishDiagnostics d ->
      if d.diagnostics <> []
      then
        let* diag = Fiber.Ivar.peek diagnostics in
        match diag with
        | Some _ -> Fiber.return ()
        | None -> Fiber.Ivar.fill diagnostics d
      else Fiber.return ()
    | _ -> Fiber.return ()
  in
  let handler = Client.Handler.make ~on_notification () in
  Test.run ~handler (fun client ->
    let run_client () =
      Client.start
        client
        (InitializeParams.create ~capabilities:(ClientCapabilities.create ()) ())
    in
    let run () =
      let* (_ : InitializeResult.t) = Client.initialized client in
      let textDocument =
        TextDocumentItem.create
          ~uri:Helpers.uri
          ~languageId:"ocaml"
          ~version:0
          ~text:source
      in
      let* () =
        Client.notification
          client
          (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
      in
      let* d = Fiber.Ivar.read diagnostics in
      let () =
        d
        |> PublishDiagnosticsParams.yojson_of_t
        |> Yojson.Safe.pretty_to_string ~std:false
        |> print_endline
      in
      let* resp = hover client position in
      let () = print_hover resp in
      let* () = Client.request client Shutdown in
      Client.stop client
    in
    Fiber.fork_and_join_unit run_client run);
  [%expect
    {|
    {
      "diagnostics": [
        {
          "code": 8,
          "message": "Warning 8: this pattern-matching is not exhaustive.\n  Here is an example of a case that is not matched: 0",
          "range": {
            "end": { "character": 29, "line": 1 },
            "start": { "character": 10, "line": 1 }
          },
          "severity": 2,
          "source": "ocamllsp"
        }
      ],
      "uri": "file:///test.ml"
    }
    {
      "contents": {
        "kind": "plaintext",
        "value": "int\n***\n**Warning 8**: Partial match: missing cases in pattern-matching."
      },
      "range": {
        "end": { "character": 29, "line": 1 },
        "start": { "character": 10, "line": 1 }
      }
    }
    |}]
;;

let%expect_test "hover over warning attribute" =
  let source =
    {ocaml|
let () =
  let x [@warning "-27+4..6@a"] = 1 in
  ()
|ocaml}
  in
  let position = Position.create ~line:2 ~character:20 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": "Disables warning 27: Innocuous unused variable: unused variable that is not bound with\n    \"let\" nor \"as\", and doesn't start with an underscore (\"_\")\n    character.\nEnables warnings 4 to 6\nEnables warning set 'a' as errors"
    }
    |}]
;;

let%expect_test "hover over warning attribute comprehensive" =
  let source =
    {ocaml|
let () =
  let x [@warning "+9-26@3+10..15-20..25@30..35+a-z@b"] = 1 in
  ()
|ocaml}
  in
  let position = Position.create ~line:2 ~character:20 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": "Enables warning 9: Missing fields in a record pattern.\nDisables warning 26: Suspicious unused variable: unused variable that is bound\n    with \"let\" or \"as\", and doesn't start with an underscore (\"_\")\n    character.\nEnables warning 3 as an error: Deprecated synonym for the 'deprecated' alert.\nEnables warnings 10 to 15\nDisables warnings 20 to 25\nEnables warnings 30 to 35 as errors\nEnables warning set 'a'\nDisables warning set 'z'\nEnables warning set 'b' as errors"
    }
    |}]
;;

let%expect_test "hover over warning attribute invalid formats" =
  let source =
    {ocaml|
let () =
  let x [@warning "+-@++1..+ab"] = 1 in
  ()
|ocaml}
  in
  let position = Position.create ~line:2 ~character:20 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect {| { "contents": "Enables warning set 'a'" } |}]
;;

let%expect_test "parse_warning_payload unit tests" =
  let open Ocaml_lsp_server in
  let show_action = function
    | Hover_req.Enable n -> Printf.sprintf "Enable %d" n
    | Disable n -> Printf.sprintf "Disable %d" n
    | Enable_as_error n -> Printf.sprintf "Enable_as_error %d" n
    | Enable_range (n1, n2) -> Printf.sprintf "Enable_range (%d, %d)" n1 n2
    | Disable_range (n1, n2) -> Printf.sprintf "Disable_range (%d, %d)" n1 n2
    | Enable_as_error_range (n1, n2) ->
      Printf.sprintf "Enable_as_error_range (%d, %d)" n1 n2
    | Enable_letter c -> Printf.sprintf "Enable_letter '%c'" c
    | Disable_letter c -> Printf.sprintf "Disable_letter '%c'" c
    | Enable_as_error_letter c -> Printf.sprintf "Enable_as_error_letter '%c'" c
  in
  let run payload =
    let actions = Hover_req.parse_warning_payload payload in
    List.iter actions ~f:(fun act -> print_endline (show_action act))
  in
  print_endline "Test 1: Empty and basic inputs";
  run "";
  run "+9";
  run "-26";
  run "@3";
  print_endline "Test 2: Ranges";
  run "+10..15";
  run "-20..25";
  run "@30..35";
  print_endline "Test 3: Letters";
  run "+a";
  run "-z";
  run "@b";
  print_endline "Test 4: Combined and malformed/ignored parts";
  run "+9-26@3+10..15-20..25@30..35+a-z@b";
  run "+-@++1..+ab";
  [%expect
    {|
    Test 1: Empty and basic inputs
    Enable 9
    Disable 26
    Enable_as_error 3
    Test 2: Ranges
    Enable_range (10, 15)
    Disable_range (20, 25)
    Enable_as_error_range (30, 35)
    Test 3: Letters
    Enable_letter 'a'
    Disable_letter 'z'
    Enable_as_error_letter 'b'
    Test 4: Combined and malformed/ignored parts
    Enable 9
    Disable 26
    Enable_as_error 3
    Enable_range (10, 15)
    Disable_range (20, 25)
    Enable_as_error_range (30, 35)
    Enable_letter 'a'
    Disable_letter 'z'
    Enable_as_error_letter 'b'
    Enable_letter 'a'
    |}]
;;

open Test.Import

let print_diagnostics params =
  print_endline "textDocument/publishDiagnostics";
  PublishDiagnosticsParams.yojson_of_t params |> Test.print_result
;;

let test source =
  let diagnostics = Fiber.Ivar.create () in
  let handler =
    Client.Handler.make
      ~on_notification:(fun _ -> function
         | PublishDiagnostics params ->
           let* filled = Fiber.Ivar.peek diagnostics in
           (match filled with
            | Some _ -> Fiber.return ()
            | None -> Fiber.Ivar.fill diagnostics params)
         | _ -> Fiber.return ())
      ()
  in
  Test.run_initialized ~handler (fun client ->
    let textDocument =
      TextDocumentItem.create
        ~uri:Helpers.uri
        ~languageId:(LanguageKind.Other "ocaml")
        ~version:0
        ~text:source
    in
    let* () =
      Client.notification
        client
        (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
    in
    let* params = Fiber.Ivar.read diagnostics in
    print_diagnostics params;
    Test.shutdown_client client)
;;

let%expect_test "has related diagnostics" =
  let source =
    {ocaml|(* " *)
|ocaml}
  in
  test source;
  [%expect
    {|
    textDocument/publishDiagnostics
    {
      "diagnostics": [
        {
          "message": "This comment contains an unterminated string literal",
          "range": {
            "end": { "character": 2, "line": 0 },
            "start": { "character": 0, "line": 0 }
          },
          "severity": 1,
          "source": "ocamllsp"
        }
      ],
      "uri": "file:///test.ml"
    }
    |}]
;;

let%expect_test "unused values have diagnostic tags" =
  let source =
    {ocaml|let () =
  let x = 123 in
  ()
|ocaml}
  in
  test source;
  [%expect
    {|
    textDocument/publishDiagnostics
    {
      "diagnostics": [
        {
          "message": "Warning 26: unused variable x.",
          "range": {
            "end": { "character": 7, "line": 1 },
            "start": { "character": 6, "line": 1 }
          },
          "severity": 2,
          "source": "ocamllsp"
        }
      ],
      "uri": "file:///test.ml"
    }
    |}]
;;

let%expect_test "deprecated values have diagnostic tags" =
  let source =
    {ocaml|module X : sig
  val x : unit
  [@@ocaml.deprecated "do not use"]
end = struct
  let x = ()
end
let () = ignore X.x
|ocaml}
  in
  test source;
  [%expect
    {|
    textDocument/publishDiagnostics
    {
      "diagnostics": [
        {
          "message": "Alert deprecated: X.x\ndo not use",
          "range": {
            "end": { "character": 19, "line": 6 },
            "start": { "character": 16, "line": 6 }
          },
          "severity": 2,
          "source": "ocamllsp"
        }
      ],
      "uri": "file:///test.ml"
    }
    |}]
;;

let%expect_test "related diagnostics for mismatched signatures" =
  let source =
    {ocaml|module X : sig
  val x : unit
end = struct
  let x = 123
end
|ocaml}
  in
  test source;
  [%expect
    {|
    textDocument/publishDiagnostics
    {
      "diagnostics": [
        {
          "message": "Signature mismatch:\nModules do not match:\n  sig val x : int end\nis not included in\n  sig val x : unit end\nValues do not match: val x : int is not included in val x : unit\nThe type int is not compatible with the type unit\nFile \"test.ml\", line 2, characters 2-14: Expected declaration\nFile \"test.ml\", line 4, characters 6-7: Actual declaration",
          "range": {
            "end": { "character": 3, "line": 4 },
            "start": { "character": 6, "line": 2 }
          },
          "severity": 1,
          "source": "ocamllsp"
        }
      ],
      "uri": "file:///test.ml"
    }
    |}]
;;

let%expect_test "shortening diagnostics republishes existing Merlin diagnostics" =
  let source =
    {ocaml|module X : sig
  val x : unit
end = struct
  let x = 123
end
|ocaml}
  in
  let first_diagnostics = Fiber.Ivar.create () in
  let publications = ref 0 in
  let handler =
    Client.Handler.make
      ~on_notification:(fun _ -> function
         | PublishDiagnostics _ ->
           incr publications;
           let* filled = Fiber.Ivar.peek first_diagnostics in
           (match filled with
            | Some _ -> Fiber.return ()
            | None -> Fiber.Ivar.fill first_diagnostics ())
         | _ -> Fiber.return ())
      ()
  in
  Test.run_initialized ~handler (fun client ->
    let textDocument =
      TextDocumentItem.create
        ~uri:Helpers.uri
        ~languageId:(LanguageKind.Other "ocaml")
        ~version:0
        ~text:source
    in
    let* () =
      Client.notification
        client
        (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
    in
    let* () = Fiber.Ivar.read first_diagnostics in
    let settings =
      `Assoc [ "shortenMerlinDiagnostics", `Assoc [ "enable", `Bool true ] ]
    in
    let* () = Client.notification client (ChangeConfiguration { settings }) in
    let* () = Lev_fiber.Timer.sleepf 0.05 in
    print_endline ("diagnostic publications: " ^ Int.to_string !publications);
    Test.shutdown_client client);
  [%expect {| diagnostic publications: 1 |}]
;;

let%expect_test "no diagnostics for valid files" =
  let source =
    {ocaml|let num = 42
|ocaml}
  in
  test source;
  [%expect
    {|
    textDocument/publishDiagnostics
    { "diagnostics": [], "uri": "file:///test.ml" }
    |}]
;;

let%expect_test "should have diagnostics for a hole only" =
  let source =
    {ocaml|let a : int = _
|ocaml}
  in
  test source;
  [%expect
    {|
    textDocument/publishDiagnostics
    {
      "diagnostics": [
        {
          "code": "hole",
          "message": "This typed hole should be replaced with an expression of type int",
          "range": {
            "end": { "character": 15, "line": 0 },
            "start": { "character": 14, "line": 0 }
          },
          "severity": 1,
          "source": "ocamllsp"
        }
      ],
      "uri": "file:///test.ml"
    }
    |}]
;;

let%expect_test "should have diagnostics for holes only" =
  let source =
    {ocaml|let _a : int = _ in
let b : string = match Some 1 with None -> _ | Some _ -> _ in
()
|ocaml}
  in
  test source;
  [%expect
    {|
    textDocument/publishDiagnostics
    {
      "diagnostics": [
        {
          "code": "hole",
          "message": "This typed hole should be replaced with an expression of type int",
          "range": {
            "end": { "character": 16, "line": 0 },
            "start": { "character": 15, "line": 0 }
          },
          "severity": 1,
          "source": "ocamllsp"
        },
        {
          "message": "Warning 26: unused variable b.",
          "range": {
            "end": { "character": 5, "line": 1 },
            "start": { "character": 4, "line": 1 }
          },
          "severity": 2,
          "source": "ocamllsp"
        },
        {
          "code": "hole",
          "message": "This typed hole should be replaced with an expression of type string",
          "range": {
            "end": { "character": 44, "line": 1 },
            "start": { "character": 43, "line": 1 }
          },
          "severity": 1,
          "source": "ocamllsp"
        },
        {
          "code": "hole",
          "message": "This typed hole should be replaced with an expression of type string",
          "range": {
            "end": { "character": 58, "line": 1 },
            "start": { "character": 57, "line": 1 }
          },
          "severity": 1,
          "source": "ocamllsp"
        }
      ],
      "uri": "file:///test.ml"
    }
    |}]
;;

let%expect_test "different diagnostics, including holes, sorted by range" =
  let source =
    {ocaml|let _u =
  let _i = List.map (fun i -> i) [1; 2; ()] in
  let b = 234 in
  let _k = _ in
  let _c = _ in
  b + "a"
|ocaml}
  in
  test source;
  [%expect
    {|
    textDocument/publishDiagnostics
    {
      "diagnostics": [
        {
          "message": "This expression should not be a unit literal, the expected type is\nint",
          "range": {
            "end": { "character": 42, "line": 1 },
            "start": { "character": 40, "line": 1 }
          },
          "severity": 1,
          "source": "ocamllsp"
        },
        {
          "code": "hole",
          "message": "This typed hole should be replaced with an expression of type 'a",
          "range": {
            "end": { "character": 12, "line": 3 },
            "start": { "character": 11, "line": 3 }
          },
          "severity": 1,
          "source": "ocamllsp"
        },
        {
          "code": "hole",
          "message": "This typed hole should be replaced with an expression of type 'a",
          "range": {
            "end": { "character": 12, "line": 4 },
            "start": { "character": 11, "line": 4 }
          },
          "severity": 1,
          "source": "ocamllsp"
        },
        {
          "message": "This constant has type string but an expression was expected of type int",
          "range": {
            "end": { "character": 9, "line": 5 },
            "start": { "character": 6, "line": 5 }
          },
          "severity": 1,
          "source": "ocamllsp"
        }
      ],
      "uri": "file:///test.ml"
    }
    |}]
;;

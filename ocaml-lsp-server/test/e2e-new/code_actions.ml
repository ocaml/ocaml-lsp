open Test.Import

let iter_code_actions ?(path = "foo.ml") ~source range k =
  let diagnostics = Fiber.Ivar.create () in
  let handler =
    Client.Handler.make
      ~on_notification:
        (fun _ -> function
          | PublishDiagnostics _ -> Fiber.Ivar.fill diagnostics ()
          | _ -> Fiber.return ())
      ()
  in
  Test.run ~handler @@ fun client ->
  let run_client () =
    let capabilities =
      let window =
        let showDocument =
          ShowDocumentClientCapabilities.create ~support:true
        in
        WindowClientCapabilities.create ~showDocument ()
      in
      ClientCapabilities.create ~window ()
    in
    Client.start client (InitializeParams.create ~capabilities ())
  in
  let run =
    let* (_ : InitializeResult.t) = Client.initialized client in
    let uri = DocumentUri.of_path path in
    let* () =
      let textDocument =
        TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text:source
      in
      Client.notification
        client
        (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
    in
    let+ resp =
      let context = CodeActionContext.create ~diagnostics:[] () in
      let request =
        let textDocument = TextDocumentIdentifier.create ~uri in
        CodeActionParams.create ~textDocument ~range ~context ()
      in
      Client.request client (CodeAction request)
    in
    k resp
  in
  Fiber.fork_and_join_unit run_client (fun () ->
      run >>> Fiber.Ivar.read diagnostics >>> Client.stop client)

let print_code_actions ?(path = "foo.ml") ?(filter = fun _ -> true) source range
    =
  iter_code_actions ~path ~source range (function
      | None -> print_endline "No code actions"
      | Some code_actions -> (
        code_actions |> List.filter ~f:filter |> function
        | [] -> print_endline "No code actions"
        | actions ->
          print_endline "Code actions:";
          List.iter actions ~f:(fun ca ->
              let json =
                match ca with
                | `Command command -> Command.yojson_of_t command
                | `CodeAction ca -> CodeAction.yojson_of_t ca
              in
              Yojson.Safe.pretty_to_string ~std:false json |> print_endline)))

let find_annotate_action =
  let open CodeAction in
  function
  | `CodeAction { kind = Some (Other "type-annotate"); _ } -> true
  | _ -> false

let find_remove_annotation_action =
  let open CodeAction in
  function
  | `CodeAction { kind = Some (Other "remove type annotation"); _ } -> true
  | _ -> false

let%expect_test "code actions" =
  let source = {ocaml|
let foo = 123
|ocaml} in
  let range =
    let start = Position.create ~line:1 ~character:5 in
    let end_ = Position.create ~line:1 ~character:7 in
    Range.create ~start ~end_
  in
  print_code_actions source range;
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "(foo : int)",
                "range": {
                  "end": { "character": 7, "line": 1 },
                  "start": { "character": 4, "line": 1 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "type-annotate",
      "title": "Type-annotate"
    }
    {
      "command": {
        "arguments": [ "file:///foo.mli" ],
        "command": "ocamllsp/open-related-source",
        "title": "Create foo.mli"
      },
      "edit": {
        "documentChanges": [ { "kind": "create", "uri": "file:///foo.mli" } ]
      },
      "kind": "switch",
      "title": "Create foo.mli"
    } |}]

let%expect_test "can type-annotate a function argument" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool
let f x = Foo x
|ocaml}
  in
  let range =
    let start = Position.create ~line:2 ~character:6 in
    let end_ = Position.create ~line:2 ~character:7 in
    Range.create ~start ~end_
  in
  print_code_actions source range ~filter:find_annotate_action;
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "(x : int)",
                "range": {
                  "end": { "character": 7, "line": 2 },
                  "start": { "character": 6, "line": 2 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "type-annotate",
      "title": "Type-annotate"
    } |}]

let%expect_test "can type-annotate a toplevel value" =
  let source = {ocaml|
let iiii = 3 + 4
|ocaml} in
  let range =
    let start = Position.create ~line:1 ~character:4 in
    let end_ = Position.create ~line:1 ~character:5 in
    Range.create ~start ~end_
  in
  print_code_actions source range;
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "(iiii : int)",
                "range": {
                  "end": { "character": 8, "line": 1 },
                  "start": { "character": 4, "line": 1 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "type-annotate",
      "title": "Type-annotate"
    }
    {
      "command": {
        "arguments": [ "file:///foo.mli" ],
        "command": "ocamllsp/open-related-source",
        "title": "Create foo.mli"
      },
      "edit": {
        "documentChanges": [ { "kind": "create", "uri": "file:///foo.mli" } ]
      },
      "kind": "switch",
      "title": "Create foo.mli"
    }
     |}]

let%expect_test "can type-annotate an argument in a function call" =
  let source =
    {ocaml|
let f x = x + 1
let () =
  let i = 8 in
  print_int (f i)
|ocaml}
  in
  let range =
    let start = Position.create ~line:1 ~character:7 in
    let end_ = Position.create ~line:1 ~character:8 in
    Range.create ~start ~end_
  in
  print_code_actions source range ~filter:find_annotate_action;
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "(x : int)",
                "range": {
                  "end": { "character": 7, "line": 1 },
                  "start": { "character": 6, "line": 1 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "type-annotate",
      "title": "Type-annotate"
    } |}]

let%expect_test "can type-annotate a variant with its name only" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool

let f (x : t) = x
|ocaml}
  in
  let range =
    let start = Position.create ~line:3 ~character:16 in
    let end_ = Position.create ~line:3 ~character:17 in
    Range.create ~start ~end_
  in
  print_code_actions source range ~filter:find_annotate_action;
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "(x : t)",
                "range": {
                  "end": { "character": 17, "line": 3 },
                  "start": { "character": 16, "line": 3 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "type-annotate",
      "title": "Type-annotate"
    } |}]

let%expect_test "does not type-annotate in a non expression context" =
  let source = {ocaml|
type x =
   | Foo of int
   | Baz of string
|ocaml} in
  let range =
    let start = Position.create ~line:3 ~character:5 in
    let end_ = Position.create ~line:3 ~character:6 in
    Range.create ~start ~end_
  in
  print_code_actions source range ~filter:find_annotate_action;
  [%expect {| No code actions |}]

let%expect_test "does not type-annotate already annotated argument" =
  let source = {ocaml|
let f (x : int) = 1
|ocaml} in
  let range =
    let start = Position.create ~line:1 ~character:7 in
    let end_ = Position.create ~line:1 ~character:8 in
    Range.create ~start ~end_
  in
  print_code_actions source range ~filter:find_annotate_action;
  [%expect {| No code actions |}]

let%expect_test "does not type-annotate already annotated expression" =
  let source = {ocaml|
let f x = (1 : int)
|ocaml} in
  let range =
    let start = Position.create ~line:1 ~character:11 in
    let end_ = Position.create ~line:1 ~character:12 in
    Range.create ~start ~end_
  in
  print_code_actions source range ~filter:find_annotate_action;
  [%expect {| No code actions |}]

let%expect_test "does not type-annotate already annotated and coerced \
                 expression" =
  let source = {ocaml|
let f x = (1 : int :> int)
|ocaml} in
  let range =
    let start = Position.create ~line:1 ~character:11 in
    let end_ = Position.create ~line:1 ~character:12 in
    Range.create ~start ~end_
  in
  print_code_actions source range ~filter:find_annotate_action;
  [%expect {| No code actions |}]

let%expect_test "can remove type annotation from a function argument" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) = Foo x
|ocaml}
  in
  let range =
    let start = Position.create ~line:2 ~character:7 in
    let end_ = Position.create ~line:2 ~character:8 in
    Range.create ~start ~end_
  in
  print_code_actions source range ~filter:find_remove_annotation_action;
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "x",
                "range": {
                  "end": { "character": 13, "line": 2 },
                  "start": { "character": 6, "line": 2 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "remove type annotation",
      "title": "Remove type annotation"
    } |}]

let%expect_test "can remove type annotation from a toplevel value" =
  let source = {ocaml|
let (iiii : int) = 3 + 4
|ocaml} in
  let range =
    let start = Position.create ~line:1 ~character:5 in
    let end_ = Position.create ~line:1 ~character:6 in
    Range.create ~start ~end_
  in
  print_code_actions source range ~filter:find_remove_annotation_action;
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "iiii",
                "range": {
                  "end": { "character": 16, "line": 1 },
                  "start": { "character": 4, "line": 1 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "remove type annotation",
      "title": "Remove type annotation"
    } |}]

let%expect_test "can remove type annotation from an argument in a function call"
    =
  let source =
    {ocaml|
let f (x : int) = x + 1
 let () =
   let i = 8 in
   print_int (f i)
|ocaml}
  in
  let range =
    let start = Position.create ~line:1 ~character:7 in
    let end_ = Position.create ~line:1 ~character:8 in
    Range.create ~start ~end_
  in
  print_code_actions source range ~filter:find_remove_annotation_action;
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "x",
                "range": {
                  "end": { "character": 15, "line": 1 },
                  "start": { "character": 6, "line": 1 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "remove type annotation",
      "title": "Remove type annotation"
    } |}]

let%expect_test "can remove type annotation from a coerced expression" =
  let source = {ocaml|
let x = (7 : int :> int)
|ocaml} in
  let range =
    let start = Position.create ~line:1 ~character:9 in
    let end_ = Position.create ~line:1 ~character:10 in
    Range.create ~start ~end_
  in
  print_code_actions source range ~filter:find_remove_annotation_action;
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "7",
                "range": {
                  "end": { "character": 16, "line": 1 },
                  "start": { "character": 9, "line": 1 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "remove type annotation",
      "title": "Remove type annotation"
    } |}]

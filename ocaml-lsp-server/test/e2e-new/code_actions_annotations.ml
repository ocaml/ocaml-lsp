open Test.Import
open Lsp_helpers
open Code_actions

let%expect_test "no code actions for dune documents" =
  let source = "(library (name foo))\n" in
  let range = range ~start_line:0 ~start_character:0 ~end_line:0 ~end_character:0 in
  let makeRequest textDocument =
    let context = CodeActionContext.create ~diagnostics:[] () in
    Lsp.Client_request.CodeAction
      (CodeActionParams.create ~textDocument ~range ~context ())
  in
  iter_lsp_response
    ~path:"dune"
    ~language_id:"dune"
    ~makeRequest
    ~source
    print_code_action_result;
  [%expect {| No code actions |}]
;;

let%expect_test "code actions" =
  let source =
    {ocaml|
let foo = 123
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:5 ~end_line:1 ~end_character:7 in
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
;;

let%expect_test "code action only includes nested kinds" =
  let source =
    {ocaml|let _ =
  let x = 0 in
  x + 1
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:6 ~end_line:1 ~end_character:7 in
  print_code_actions ~only:[ CodeActionKind.Refactor ] source range;
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "(0)",
                "range": {
                  "end": { "character": 3, "line": 2 },
                  "start": { "character": 2, "line": 2 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "refactor.inline",
      "title": "Inline into uses"
    }
    |}]
;;

let%expect_test "can type-annotate a function argument" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool
let f x = Foo x
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:6 ~end_line:2 ~end_character:7 in
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
;;

let%expect_test "can type-annotate a toplevel value" =
  let source =
    {ocaml|
let iiii = 3 + 4
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:4 ~end_line:1 ~end_character:5 in
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
;;

let%expect_test "does not type-annotate function" =
  let source =
    {ocaml|
let my_fun x y = 1
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:5 ~end_line:1 ~end_character:6 in
  print_code_actions source range ~filter:find_annotate_action;
  [%expect {| No code actions |}]
;;

let%expect_test "can type-annotate an argument in a function call" =
  let source =
    {ocaml|
let f x = x + 1
let () =
  let i = 8 in
  print_int (f i)
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:7 ~end_line:1 ~end_character:8 in
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
;;

let%expect_test "can type-annotate a variant with its name only" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool

let f (x : t) = x
|ocaml}
  in
  let range = range ~start_line:3 ~start_character:16 ~end_line:3 ~end_character:17 in
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
;;

let%expect_test "does not type-annotate in a non expression context" =
  let source =
    {ocaml|
type x =
   | Foo of int
   | Baz of string
|ocaml}
  in
  let range = range ~start_line:3 ~start_character:5 ~end_line:3 ~end_character:6 in
  print_code_actions source range ~filter:find_annotate_action;
  [%expect {| No code actions |}]
;;

let%expect_test "does not type-annotate already annotated argument" =
  let source =
    {ocaml|
let f (x : int) = 1
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:7 ~end_line:1 ~end_character:8 in
  print_code_actions source range ~filter:find_annotate_action;
  [%expect {| No code actions |}]
;;

let%expect_test "does not type-annotate already annotated expression" =
  let source =
    {ocaml|
let f x = (1 : int)
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:11 ~end_line:1 ~end_character:12 in
  print_code_actions source range ~filter:find_annotate_action;
  [%expect {| No code actions |}]
;;

let%expect_test "does not type-annotate already annotated and coerced expression" =
  let source =
    {ocaml|
let f x = (1 : int :> int)
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:11 ~end_line:1 ~end_character:12 in
  print_code_actions source range ~filter:find_annotate_action;
  [%expect {| No code actions |}]
;;

let%expect_test "can remove type annotation from a function argument" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) = Foo x
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:7 ~end_line:2 ~end_character:8 in
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
;;

let%expect_test "can remove type annotation from a toplevel value" =
  let source =
    {ocaml|
let (iiii : int) = 3 + 4
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:5 ~end_line:1 ~end_character:6 in
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
;;

let%expect_test "can remove type annotation from an argument in a function call" =
  let source =
    {ocaml|
let f (x : int) = x + 1
 let () =
   let i = 8 in
   print_int (f i)
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:7 ~end_line:1 ~end_character:8 in
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
;;

let%expect_test "can remove type annotation from a coerced expression" =
  let source =
    {ocaml|
let x = (7 : int :> int)
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:9 ~end_line:1 ~end_character:10 in
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
;;

let%expect_test "does not remove type annotation from function" =
  let source =
    {ocaml|
let my_fun x y : int = 1
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:5 ~end_line:1 ~end_character:6 in
  print_code_actions source range ~filter:find_remove_annotation_action;
  [%expect {| No code actions |}]
;;

let capabilities_with_create_file =
  let window =
    let showDocument = ShowDocumentClientCapabilities.create ~support:true in
    WindowClientCapabilities.create ~showDocument ()
  in
  let workspace =
    let workspaceEdit =
      WorkspaceEditClientCapabilities.create
        ~documentChanges:true
        ~resourceOperations:[ ResourceOperationKind.Create ]
        ()
    in
    WorkspaceClientCapabilities.create ~workspaceEdit ()
  in
  ClientCapabilities.create ~window ~workspace ()
;;

let%expect_test "create counterpart action, client supports resource operations" =
  let range = range ~start_line:0 ~start_character:4 ~end_line:0 ~end_character:5 in
  print_code_actions
    ~capabilities:capabilities_with_create_file
    ~filter:(find_action "switch")
    "let x = 1\n"
    range;
  [%expect
    {|
    Code actions:
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
;;

let%expect_test "create counterpart action, client lacks resource operations" =
  let range = range ~start_line:0 ~start_character:4 ~end_line:0 ~end_character:5 in
  print_code_actions ~filter:(find_action "switch") "let x = 1\n" range;
  [%expect
    {|
    Code actions:
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
;;

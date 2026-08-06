open Test.Import
open Lsp_helpers
open Code_actions

let rec censor_backtraces = function
  | `Assoc fields ->
    `Assoc
      (List.map fields ~f:(fun (name, value) ->
         match name, value with
         | "backtrace", _ -> name, `String "<censored>"
         | "exn", `String message
           when String.is_prefix message ~prefix:"File \"src/analysis/destruct.ml\"" ->
           name, `String "Assert_failure(\"src/analysis/destruct.ml\", _, _)"
         | _ -> name, censor_backtraces value))
  | `List values -> `List (List.map values ~f:censor_backtraces)
  | json -> json
;;

let%expect_test "malformed object method leaks a destruct assertion" =
  let source = "object method x with|0" in
  let range = range ~start_line:0 ~start_character:21 ~end_line:0 ~end_character:21 in
  let makeRequest textDocument =
    let only = [ CodeActionKind.Other "destruct (enumerate cases)" ] in
    let context = CodeActionContext.create ~diagnostics:[] ~only () in
    Lsp.Client_request.CodeAction
      (CodeActionParams.create ~textDocument ~range ~context ())
  in
  Lsp_helpers.iter_lsp_response_result ~language_id:"ocaml" ~makeRequest ~source (function
    | Error error ->
      Jsonrpc.Response.Error.yojson_of_t error |> censor_backtraces |> Test.print_result
    | Ok response -> print_code_action_result response);
  [%expect
    {|
    {
      "data": {
        "exn": "Assert_failure(\"src/analysis/destruct.ml\", _, _)",
        "backtrace": "<censored>"
      },
      "code": -32603,
      "message": "uncaught exception"
    }
    |}]
;;

let%expect_test "destruct-line returns an edit inside a UTF-8 scalar" =
  let source = "𐐀 = 1\nmatch th | 0 -)et x = 1" in
  let range = range ~start_line:1 ~start_character:11 ~end_line:1 ~end_character:23 in
  let general =
    GeneralClientCapabilities.create ~positionEncodings:[ PositionEncodingKind.UTF8 ] ()
  in
  let capabilities = ClientCapabilities.create ~general () in
  Helpers.test ~capabilities source (fun client ->
    let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let context = CodeActionContext.create ~diagnostics:[] () in
    let* response =
      Client.request
        client
        (CodeAction (CodeActionParams.create ~textDocument ~range ~context ()))
    in
    print_code_action_result
      response
      ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
    Fiber.return ());
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "match ((1 0) - (( *type-error* ) ( *type-error* ))) = 1 with\n| false -> _\n| true -> _",
                "range": {
                  "end": { "character": 23, "line": 1 },
                  "start": { "character": 1, "line": 0 }
                }
              }
            ],
            "textDocument": { "uri": "file:///test.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct-line (enumerate cases, use existing match)",
      "title": "Destruct-line (enumerate cases, use existing match)"
    }
    |}]
;;

let%expect_test "can destruct sum types" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) = x
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:16 ~end_line:2 ~end_character:17 in
  print_code_actions source range ~filter:(find_action "destruct (enumerate cases)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "match x with | Foo _ -> _ | Bar _ -> _",
                "range": {
                  "end": { "character": 17, "line": 2 },
                  "start": { "character": 16, "line": 2 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct (enumerate cases)",
      "title": "Destruct (enumerate cases)"
    }
    |}]
;;

let%expect_test "can destruct match line" =
  let source =
    {ocaml|
let f (x:bool) =
  match x
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:5 ~end_line:2 ~end_character:5 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "match x with\n  | false -> _\n  | true -> _",
                "range": {
                  "end": { "character": 9, "line": 2 },
                  "start": { "character": 2, "line": 2 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct-line (enumerate cases, use existing match)",
      "title": "Destruct-line (enumerate cases, use existing match)"
    }
    |}]
;;

let%expect_test "destruct-line is available on a whole inline match expression" =
  let source =
    {ocaml|
type t = A | B | C
let x = match A with
  | A -> _
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:8 ~end_line:3 ~end_character:10 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "\n  | B -> _\n  | C -> _",
                "range": {
                  "end": { "character": 10, "line": 3 },
                  "start": { "character": 10, "line": 3 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct-line (enumerate cases, use existing match)",
      "title": "Destruct-line (enumerate cases, use existing match)"
    }
    |}]
;;

let%expect_test "destruct-line is available when a match case is on the same line" =
  let source =
    {ocaml|
type t = A | B | C
let x = match A with | A -> _
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:8 ~end_line:2 ~end_character:13 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "\n                     | B -> _\n                     | C -> _",
                "range": {
                  "end": { "character": 29, "line": 2 },
                  "start": { "character": 29, "line": 2 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct-line (enumerate cases, use existing match)",
      "title": "Destruct-line (enumerate cases, use existing match)"
    }
    |}]
;;

let%expect_test "destruct-line expands an inline match without cases" =
  let source =
    {ocaml|
type t = A | B | C
let x = match A with
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:8 ~end_line:2 ~end_character:13 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "match A with\n        | A -> _\n        | B -> _\n        | C -> _",
                "range": {
                  "end": { "character": 20, "line": 2 },
                  "start": { "character": 8, "line": 2 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct-line (enumerate cases, use existing match)",
      "title": "Destruct-line (enumerate cases, use existing match)"
    }
    |}]
;;

let%expect_test
    "destruct-line is not offered when the selection starts before an inline match"
  =
  let source =
    {ocaml|
type t = A | B | C
let x = match A with
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:0 ~end_line:2 ~end_character:20 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect {| No code actions |}]
;;

let%expect_test
    "destruct-line is not offered when a multiline selection starts before an inline \
     match"
  =
  let source =
    {ocaml|
type t = A | B | C
let x = match A with
  | A -> _
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:0 ~end_line:3 ~end_character:10 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect {| No code actions |}]
;;

let%expect_test "destruct-line finds a case after a record update" =
  let source =
    {ocaml|
type t = A | B | C
type r = { value : t }
let f r = match { r with value = A }.value with | A -> _
|ocaml}
  in
  let range = range ~start_line:3 ~start_character:10 ~end_line:3 ~end_character:15 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "\n                                                | B -> _\n                                                | C -> _",
                "range": {
                  "end": { "character": 56, "line": 3 },
                  "start": { "character": 56, "line": 3 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct-line (enumerate cases, use existing match)",
      "title": "Destruct-line (enumerate cases, use existing match)"
    }
    |}]
;;

let%expect_test "destruct-line finds the outer case of a nested match" =
  let source =
    {ocaml|
type t = A | B | C
let x = match (match A with | A -> B | B -> C | C -> A) with | A -> _
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:8 ~end_line:2 ~end_character:13 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "\n                                                             | B -> _\n                                                             | C -> _",
                "range": {
                  "end": { "character": 69, "line": 2 },
                  "start": { "character": 69, "line": 2 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct-line (enumerate cases, use existing match)",
      "title": "Destruct-line (enumerate cases, use existing match)"
    }
    |}]
;;

let%expect_test
    "destruct-line finds the outer case of a nested match containing a record update"
  =
  let source =
    {ocaml|
type t = A | B | C
type r = { value : t }
let f r = match (match { r with value = A } with | A -> B | B -> C | C -> A) with | C -> _
|ocaml}
  in
  let range = range ~start_line:3 ~start_character:10 ~end_line:3 ~end_character:15 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "\n                                                                                  | A -> _\n                                                                                  | B -> _",
                "range": {
                  "end": { "character": 90, "line": 3 },
                  "start": { "character": 90, "line": 3 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct-line (enumerate cases, use existing match)",
      "title": "Destruct-line (enumerate cases, use existing match)"
    }
    |}]
;;

let%expect_test "destruct-line finds a case after a module-type with" =
  let source =
    {ocaml|
type t = A | B | C
module type S = sig type u end
module M : S with type u = int = struct type u = int end
let f (x : t) = match (x, (module M : S with type u = int)) with | A, _ -> _
|ocaml}
  in
  let range = range ~start_line:4 ~start_character:16 ~end_line:4 ~end_character:21 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "\n                                                                 | (B -> _\n                                                                 | C), _ -> _",
                "range": {
                  "end": { "character": 76, "line": 4 },
                  "start": { "character": 76, "line": 4 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct-line (enumerate cases, use existing match)",
      "title": "Destruct-line (enumerate cases, use existing match)"
    }
    |}]
;;

let%expect_test "destruct-line returns UTF-16 edit ranges" =
  let source =
    {ocaml|
let f (café : bool) =
  match café
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:10 ~end_line:2 ~end_character:10 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "match café with\n  | false -> _\n  | true -> _",
                "range": {
                  "end": { "character": 13, "line": 2 },
                  "start": { "character": 2, "line": 2 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct-line (enumerate cases, use existing match)",
      "title": "Destruct-line (enumerate cases, use existing match)"
    }
    |}]
;;

let%expect_test "destruct-line selects an expression repeated in match" =
  let source =
    {ocaml|
let f (a : bool) =
  match a
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:8 ~end_line:2 ~end_character:8 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect {| No code actions |}]
;;

let%expect_test "can destruct match-with line" =
  let source =
    {ocaml|
    match (Ok 0) with
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:0 ~end_line:1 ~end_character:0 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "match Ok 0 with\n    | Ok _ -> _\n    | Error _ -> _",
                "range": {
                  "end": { "character": 21, "line": 1 },
                  "start": { "character": 4, "line": 1 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct-line (enumerate cases, use existing match)",
      "title": "Destruct-line (enumerate cases, use existing match)"
    }
    |}]
;;

let%expect_test "can destruct case line" =
  let source =
    {ocaml|
type q =
| A
| B
| C
| D
let f (x: q) =
  match x with
  | C -> _
|ocaml}
  in
  let range = range ~start_line:8 ~start_character:0 ~end_line:8 ~end_character:0 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "\n  | A -> _\n  | B -> _\n  | D -> _",
                "range": {
                  "end": { "character": 10, "line": 8 },
                  "start": { "character": 10, "line": 8 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct-line (enumerate cases, use existing match)",
      "title": "Destruct-line (enumerate cases, use existing match)"
    }
    |}]
;;

let%expect_test "can destruct hole" =
  let source =
    {ocaml|
let zip (type a b) (xs : a list) (ys : b list) : (a * b) list =
  match (xs, ys) with
  | (_, _) -> _
|ocaml}
  in
  let range = range ~start_line:3 ~start_character:5 ~end_line:3 ~end_character:5 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "([], _) -> _\n  | (_::_, _)",
                "range": {
                  "end": { "character": 10, "line": 3 },
                  "start": { "character": 4, "line": 3 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct-line (enumerate cases, use existing match)",
      "title": "Destruct-line (enumerate cases, use existing match)"
    }
    |}]
;;

let%expect_test "destruct hole spacing" =
  let source =
    {ocaml|
type q =
| A
| B
| C
| D
let f (x: q) =
  match x with
  | _ -> _
|ocaml}
  in
  let range = range ~start_line:8 ~start_character:5 ~end_line:8 ~end_character:5 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "A -> _\n  | B -> _\n  | C -> _\n  | D",
                "range": {
                  "end": { "character": 5, "line": 8 },
                  "start": { "character": 4, "line": 8 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct-line (enumerate cases, use existing match)",
      "title": "Destruct-line (enumerate cases, use existing match)"
    }
    |}]
;;

let%expect_test "destruct a case with a hole but not on the hole" =
  let source =
    {ocaml|
type q =
| A
| B
| C
| D
let f (x: q) =
  match x with
  | _ -> _
|ocaml}
  in
  let range = range ~start_line:8 ~start_character:2 ~end_line:8 ~end_character:2 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "A -> _\n  | B -> _\n  | C -> _\n  | D",
                "range": {
                  "end": { "character": 5, "line": 8 },
                  "start": { "character": 4, "line": 8 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct-line (enumerate cases, use existing match)",
      "title": "Destruct-line (enumerate cases, use existing match)"
    }
    |}]
;;

let%expect_test "destruct uses the right number of newlines" =
  let source =
    {ocaml|
type t =
  | Very_long_name_for_for_the_first_case_so_that_merlin_will_use_multiple_lines
  | Almost_as_long_name_for_for_the_second_case
  | Another_long_name_for_for_the_third_case
;;
let f (x: t) =
  match x with
  |ocaml}
  in
  let range = range ~start_line:7 ~start_character:7 ~end_line:7 ~end_character:7 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect
    {|
      Code actions:
      {
        "edit": {
          "documentChanges": [
            {
              "edits": [
                {
                  "newText": "match x with\n  | Very_long_name_for_for_the_first_case_so_that_merlin_will_use_multiple_lines -> _\n  | Almost_as_long_name_for_for_the_second_case -> _\n  | Another_long_name_for_for_the_third_case -> _",
                  "range": {
                    "end": { "character": 14, "line": 7 },
                    "start": { "character": 2, "line": 7 }
                  }
                }
              ],
              "textDocument": { "uri": "file:///foo.ml", "version": 0 }
            }
          ]
        },
        "isPreferred": false,
        "kind": "destruct-line (enumerate cases, use existing match)",
        "title": "Destruct-line (enumerate cases, use existing match)"
      }
      |}]
;;

let%expect_test "destruct strips parentheses even on long lines" =
  let source =
    {ocaml|
type q =
  | Very_long_name_for_for_the_first_case_so_that_merlin_will_be_forced_to_use_multiple_lines
  | Almost_as_long_name_for_for_the_second_case
  | Another_long_name_for_for_the_third_case
  | Very_long_name_for_for_the_last_case_so_that_we_can_make_sure_we_handle_both_parens_and_line_breaks of int
;;
let f (x: q) =
  match x with
  | Almost_as_long_name_for_for_the_second_case -> _
|ocaml}
  in
  let range = range ~start_line:9 ~start_character:22 ~end_line:9 ~end_character:22 in
  print_code_actions
    source
    range
    ~filter:(find_action "destruct-line (enumerate cases, use existing match)");
  [%expect
    {|
      Code actions:
      {
        "edit": {
          "documentChanges": [
            {
              "edits": [
                {
                  "newText": "\n  | Very_long_name_for_for_the_first_case_so_that_merlin_will_be_forced_to_use_multiple_lines -> _\n  | Another_long_name_for_for_the_third_case -> _\n  | Very_long_name_for_for_the_last_case_so_that_we_can_make_sure_we_handle_both_parens_and_line_breaks\n     _ -> _",
                  "range": {
                    "end": { "character": 52, "line": 9 },
                    "start": { "character": 52, "line": 9 }
                  }
                }
              ],
              "textDocument": { "uri": "file:///foo.ml", "version": 0 }
            }
          ]
        },
        "isPreferred": false,
        "kind": "destruct-line (enumerate cases, use existing match)",
        "title": "Destruct-line (enumerate cases, use existing match)"
      }
      |}]
;;

let%expect_test "can destruct on sub-expression" =
  let source =
    {ocaml|
let defered_peek x = if x >= 0 then Some (`Foo x) else None
let job_reader = 10

let _ = defered_peek job_reader
|ocaml}
  in
  let range = range ~start_line:4 ~start_character:8 ~end_line:4 ~end_character:31 in
  print_code_actions source range ~filter:(find_action "destruct (enumerate cases)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "match defered_peek job_reader with | None -> _ | Some _ -> _",
                "range": {
                  "end": { "character": 31, "line": 4 },
                  "start": { "character": 8, "line": 4 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct (enumerate cases)",
      "title": "Destruct (enumerate cases)"
    }
    |}]
;;

let%expect_test "can destruct on sub-expression that need parenthesis" =
  let source =
    {ocaml|
let defered_peek x = if x >= 0 then Some (`Foo x) else None
let job_reader = 10

let _ = defered_peek job_reader
|ocaml}
  in
  let range = range ~start_line:4 ~start_character:21 ~end_line:4 ~end_character:31 in
  print_code_actions source range ~filter:(find_action "destruct (enumerate cases)");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "(match job_reader with | 0 -> _ | _ -> _)",
                "range": {
                  "end": { "character": 31, "line": 4 },
                  "start": { "character": 21, "line": 4 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "destruct (enumerate cases)",
      "title": "Destruct (enumerate cases)"
    }
    |}]
;;

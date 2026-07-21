open Test.Import
open Lsp_helpers

let range ~start_line ~start_character ~end_line ~end_character =
  let start = Position.create ~line:start_line ~character:start_character in
  let end_ = Position.create ~line:end_line ~character:end_character in
  Range.create ~start ~end_
;;

let iter_code_actions ?prep ?path ?(diagnostics = []) ?only ~source range =
  let makeRequest textDocument =
    let context = CodeActionContext.create ~diagnostics ?only () in
    Lsp.Client_request.CodeAction
      (CodeActionParams.create ~textDocument ~range ~context ())
  in
  iter_lsp_response ?prep ?path ~language_id:"ocaml" ~makeRequest ~source
;;

let print_code_actions
      ?(prep = fun _ -> Fiber.return ())
      ?(path = "foo.ml")
      ?(diagnostics = [])
      ?only
      ?(filter = fun _ -> true)
      source
      range
  =
  iter_code_actions ~prep ~path ~diagnostics ?only ~source range (function
    | None -> print_endline "No code actions"
    | Some code_actions ->
      code_actions
      |> List.filter ~f:filter
      |> (function
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
;;

let find_action action_name action =
  match action with
  | `CodeAction { CodeAction.kind = Some (Other name); _ } -> name = action_name
  | _ -> false
;;

let find_annotate_action = find_action "type-annotate"
let find_remove_annotation_action = find_action "remove type annotation"

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

let%expect_test "can infer module interfaces" =
  let impl_source =
    {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) = x
|ocaml}
  in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.openDocument ~client ~uri ~source:impl_source in
  let intf_source = "" in
  let range = range ~start_line:0 ~start_character:0 ~end_line:0 ~end_character:0 in
  print_code_actions
    intf_source
    range
    ~prep
    ~path:"foo.mli"
    ~filter:(find_action "inferred_intf");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "type t = Foo of int | Bar of bool\n\nval f : t -> t\n",
                "range": {
                  "end": { "character": 0, "line": 0 },
                  "start": { "character": 0, "line": 0 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.mli", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "inferred_intf",
      "title": "Insert inferred interface"
    } |}]
;;

let%expect_test "inferred interface excludes existing names" =
  let impl_source =
    {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) = x
|ocaml}
  in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.openDocument ~client ~uri ~source:impl_source in
  let intf_source =
    {ocaml|
val f : t -> t
|ocaml}
  in
  let range = range ~start_line:0 ~start_character:0 ~end_line:0 ~end_character:0 in
  print_code_actions
    intf_source
    range
    ~prep
    ~path:"foo.mli"
    ~filter:(find_action "inferred_intf");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "type t = Foo of int | Bar of bool\n",
                "range": {
                  "end": { "character": 0, "line": 0 },
                  "start": { "character": 0, "line": 0 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.mli", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "inferred_intf",
      "title": "Insert inferred interface"
    }
    |}]
;;

let%expect_test "update-signatures adds new function args" =
  let impl_source =
    {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) (d : bool) =
  match x with
  |Bar x -> x
  |Foo _ -> d
|ocaml}
  in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.openDocument ~client ~uri ~source:impl_source in
  let intf_source =
    {ocaml|
type t = Foo of int | Bar of bool
val f : t -> bool
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:0 ~end_line:2 ~end_character:0 in
  print_code_actions
    intf_source
    range
    ~prep
    ~path:"foo.mli"
    ~filter:(find_action "update_intf");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "val f : t -> bool -> bool\n",
                "range": {
                  "end": { "character": 17, "line": 2 },
                  "start": { "character": 0, "line": 2 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.mli", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "update_intf",
      "title": "Update signature(s) to match implementation"
    }
    |}]
;;

let%expect_test "update-signatures removes old function args" =
  let impl_source =
    {ocaml|
let f i s b =
  if b then String.length s > i else String.length s < i
|ocaml}
  in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.openDocument ~client ~uri ~source:impl_source in
  let intf_source =
    {ocaml|
val f : int -> string -> 'a list -> bool -> bool
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:10 ~end_line:1 ~end_character:10 in
  print_code_actions
    intf_source
    range
    ~prep
    ~path:"foo.mli"
    ~filter:(find_action "update_intf");
  [%expect
    {|
  Code actions:
  {
    "edit": {
      "documentChanges": [
        {
          "edits": [
            {
              "newText": "val f : int -> string -> bool -> bool\n",
              "range": {
                "end": { "character": 48, "line": 1 },
                "start": { "character": 0, "line": 1 }
              }
            }
          ],
          "textDocument": { "uri": "file:///foo.mli", "version": 0 }
        }
      ]
    },
    "isPreferred": false,
    "kind": "update_intf",
    "title": "Update signature(s) to match implementation"
  }
  |}]
;;

let%expect_test "update-signatures updates parameter types" =
  let impl_source =
    {ocaml|
let f i s l b =
  if b then List.length s > i else List.length l < i
  |ocaml}
  in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.openDocument ~client ~uri ~source:impl_source in
  let intf_source =
    {ocaml|
val f : int -> string -> 'a list -> bool -> bool
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:1 ~end_line:1 ~end_character:12 in
  print_code_actions
    intf_source
    range
    ~prep
    ~path:"foo.mli"
    ~filter:(find_action "update_intf");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "val f : int -> 'a list -> 'b list -> bool -> bool\n",
                "range": {
                  "end": { "character": 48, "line": 1 },
                  "start": { "character": 0, "line": 1 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.mli", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "update_intf",
      "title": "Update signature(s) to match implementation"
    }
    |}]
;;

let%expect_test "update-signatures preserves functions and their comments" =
  let impl_source =
    {ocaml|
let f x = x + 1;;

let g x y z ~another_arg ~yet_another_arg ~keep_them_coming = x - y + z + another_arg + yet_another_arg + keep_them_coming;;

let h x = x *. 2.0;;
  |ocaml}
  in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.openDocument ~client ~uri ~source:impl_source in
  let intf_source =
    {ocaml|
val f :
    int  (* This comment should stay. *)
    -> int

val g : int
    -> int (* This comment should disappear since the function changes. *)
    -> int

(* This comment should stay even though the function changes. *)
val h : int -> bool
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:0 ~end_line:10 ~end_character:19 in
  print_code_actions
    intf_source
    range
    ~prep
    ~path:"foo.mli"
    ~filter:(find_action "update_intf");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "val g :\n  int ->\n  int ->\n  int ->\n  another_arg:int -> yet_another_arg:int -> keep_them_coming:int -> int\n",
                "range": {
                  "end": { "character": 10, "line": 7 },
                  "start": { "character": 0, "line": 5 }
                }
              },
              {
                "newText": "val h : float -> float\n",
                "range": {
                  "end": { "character": 19, "line": 10 },
                  "start": { "character": 0, "line": 10 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.mli", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "update_intf",
      "title": "Update signature(s) to match implementation"
    }
    |}]
;;

let%expect_test "update-signatures updates modules" =
  let impl_source =
    {ocaml|
module M = struct
  type t =
    | I of int
    | F of float
  ;;
  let f (x : t) ~long_name_for_an_integer_argument =
    match x with
    | I i -> i
    | F f -> long_name_for_an_integer_argument
  ;;
end
|ocaml}
  in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.openDocument ~client ~uri ~source:impl_source in
  let intf_source =
    {ocaml|
module M : sig type t = I of int | B of bool end
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:0 ~end_line:1 ~end_character:0 in
  print_code_actions
    intf_source
    range
    ~prep
    ~path:"foo.mli"
    ~filter:(find_action "update_intf");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "module M :\n  sig\n    type t = I of int | F of float\n    val f : t -> long_name_for_an_integer_argument:int -> int\n  end\n",
                "range": {
                  "end": { "character": 48, "line": 1 },
                  "start": { "character": 0, "line": 1 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.mli", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "update_intf",
      "title": "Update signature(s) to match implementation"
    }
    |}]
;;

let activate_jump client =
  let config =
    DidChangeConfigurationParams.create
      ~settings:(`Assoc [ "merlinJumpCodeActions", `Assoc [ "enable", `Bool true ] ])
  in
  change_config ~client config
;;

let%expect_test "can jump to match target" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool
let square x = x * x
let f (x : t) (d : bool) =
  match x with
  |Bar x -> x
  |Foo _ -> d
|ocaml}
  in
  let range = range ~start_line:5 ~start_character:5 ~end_line:5 ~end_character:5 in
  print_code_actions
    ~prep:activate_jump
    source
    range
    ~filter:(find_action "merlin-jump-match");
  [%expect
    {|
    Code actions:
    {
      "command": {
        "arguments": [
          "file:///foo.ml",
          {
            "end": { "character": 2, "line": 4 },
            "start": { "character": 2, "line": 4 }
          }
        ],
        "command": "ocamllsp/merlin-jump-to-target",
        "title": "Match jump"
      },
      "kind": "merlin-jump-match",
      "title": "Match jump"
    }

       |}]
;;

let%expect_test "can jump to match-next-case target" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool
let square x = x * x
let f (x : t) (d : bool) =
  match x with
  |Bar x -> x
  |Foo _ -> d
|ocaml}
  in
  let range = range ~start_line:5 ~start_character:5 ~end_line:5 ~end_character:5 in
  print_code_actions
    ~prep:activate_jump
    source
    range
    ~filter:(find_action "merlin-jump-next-case");
  [%expect
    {|
    Code actions:
    {
      "command": {
        "arguments": [
          "file:///foo.ml",
          {
            "end": { "character": 3, "line": 6 },
            "start": { "character": 3, "line": 6 }
          }
        ],
        "command": "ocamllsp/merlin-jump-to-target",
        "title": "Next-case jump"
      },
      "kind": "merlin-jump-next-case",
      "title": "Next-case jump"
    } |}]
;;

let%expect_test "can jump to  match-prev-case target" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool
let square x = x * x
let f (x : t) (d : bool) =
  match x with
  |Bar x -> x
  |Foo _ -> d
|ocaml}
  in
  let range = range ~start_line:5 ~start_character:5 ~end_line:5 ~end_character:5 in
  print_code_actions
    ~prep:activate_jump
    source
    range
    ~filter:(find_action "merlin-jump-prev-case");
  [%expect
    {|
    Code actions:
    {
      "command": {
        "arguments": [
          "file:///foo.ml",
          {
            "end": { "character": 3, "line": 5 },
            "start": { "character": 3, "line": 5 }
          }
        ],
        "command": "ocamllsp/merlin-jump-to-target",
        "title": "Prev-case jump"
      },
      "kind": "merlin-jump-prev-case",
      "title": "Prev-case jump"
    } |}]
;;

let%expect_test "can jump to let target" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool
let square x = x * x
let f (x : t) (d : bool) =
  match x with
  |Bar x -> x
  |Foo _ -> d
|ocaml}
  in
  let range = range ~start_line:5 ~start_character:5 ~end_line:5 ~end_character:5 in
  print_code_actions
    ~prep:activate_jump
    source
    range
    ~filter:(find_action "merlin-jump-let");
  [%expect
    {|
    Code actions:
    {
      "command": {
        "arguments": [
          "file:///foo.ml",
          {
            "end": { "character": 0, "line": 3 },
            "start": { "character": 0, "line": 3 }
          }
        ],
        "command": "ocamllsp/merlin-jump-to-target",
        "title": "Let jump"
      },
      "kind": "merlin-jump-let",
      "title": "Let jump"
    } |}]
;;

let%expect_test "can jump to fun target" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool
let square x = x * x
let f (x : t) (d : bool) =
  match x with
  |Bar x -> x
  |Foo _ -> d
|ocaml}
  in
  let range = range ~start_line:5 ~start_character:5 ~end_line:5 ~end_character:5 in
  print_code_actions
    ~prep:activate_jump
    source
    range
    ~filter:(find_action "merlin-jump-fun");
  [%expect
    {|
    Code actions:
    {
      "command": {
        "arguments": [
          "file:///foo.ml",
          {
            "end": { "character": 0, "line": 3 },
            "start": { "character": 0, "line": 3 }
          }
        ],
        "command": "ocamllsp/merlin-jump-to-target",
        "title": "Fun jump"
      },
      "kind": "merlin-jump-fun",
      "title": "Fun jump"
    } |}]
;;

let%expect_test "can jump to module target" =
  let source =
    {ocaml|
module FooBar = struct
  type t = Foo of int | Bar of bool
end
let f (x : t) (d : bool) =
  match x with
  |Bar x -> x
  |Foo _ -> d
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:5 ~end_line:2 ~end_character:5 in
  print_code_actions
    ~prep:activate_jump
    source
    range
    ~filter:(find_action "merlin-jump-module");
  [%expect
    {|
    Code actions:
    {
      "command": {
        "arguments": [
          "file:///foo.ml",
          {
            "end": { "character": 0, "line": 1 },
            "start": { "character": 0, "line": 1 }
          }
        ],
        "command": "ocamllsp/merlin-jump-to-target",
        "title": "Module jump"
      },
      "kind": "merlin-jump-module",
      "title": "Module jump"
    } |}]
;;

let%expect_test "can jump to module-type target" =
  let source =
    {ocaml|
  module type ORDER = sig
    type t
    val leq : t -> t -> bool
    val equal : t -> t -> bool
  end

  let f (x : t) (d : bool) =
    match x with
    |Bar x -> x
    |Foo _ -> d
  |ocaml}
  in
  let range = range ~start_line:4 ~start_character:5 ~end_line:4 ~end_character:5 in
  print_code_actions
    ~prep:activate_jump
    source
    range
    ~filter:(find_action "merlin-jump-module-type");
  [%expect
    {|
      Code actions:
      {
        "command": {
          "arguments": [
            "file:///foo.ml",
            {
              "end": { "character": 2, "line": 1 },
              "start": { "character": 2, "line": 1 }
            }
          ],
          "command": "ocamllsp/merlin-jump-to-target",
          "title": "Module-type jump"
        },
        "kind": "merlin-jump-module-type",
        "title": "Module-type jump"
      } |}]
;;

let%expect_test "shouldn't find the jump target on the same line" =
  let source =
    {ocaml|
  let square x = x * x
  let f (x : t) (d : bool) =
    match x with
    |Bar x -> x
    |Foo _ -> d
  |ocaml}
  in
  let range = range ~start_line:0 ~start_character:5 ~end_line:0 ~end_character:5 in
  print_code_actions
    ~prep:activate_jump
    source
    range
    ~filter:(find_action "merlin-jump-fun");
  [%expect
    {|
      No code actions |}]
;;

let%expect_test "can combine cases with multiple RHSes" =
  let source =
    {ocaml|
    match card with
    | Ace -> _
    | King -> _
    | Queen -> "Face card!"
    | Jack -> "Face card?"
    | Number _ -> _
|ocaml}
  in
  let range = range ~start_line:3 ~start_character:3 ~end_line:6 ~end_character:6 in
  print_code_actions source range ~filter:(find_action "combine-cases");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "    | King | Queen | Jack | Number _ -> _\n",
                "range": {
                  "end": { "character": 0, "line": 7 },
                  "start": { "character": 0, "line": 3 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "combine-cases",
      "title": "Combine-cases"
    }
    |}]
;;

let%expect_test "can combine cases with one unique RHS" =
  let source =
    {ocaml|
    match card with
    | Ace -> _
    | King -> _
    | Queen -> "Face card!"
    | Jack -> "Face card?"
    | Number _ -> _
|ocaml}
  in
  let range = range ~start_line:3 ~start_character:3 ~end_line:4 ~end_character:4 in
  print_code_actions source range ~filter:(find_action "combine-cases");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "    | King | Queen -> \"Face card!\"\n",
                "range": {
                  "end": { "character": 0, "line": 5 },
                  "start": { "character": 0, "line": 3 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "combine-cases",
      "title": "Combine-cases"
    }
    |}]
;;

let position_of_offset src x =
  assert (0 <= x && x < String.length src);
  let cnum = ref 0
  and lnum = ref 0 in
  for i = 0 to x - 1 do
    if src.[i] = '\n'
    then (
      incr lnum;
      cnum := 0)
    else incr cnum
  done;
  Position.create ~character:!cnum ~line:!lnum
;;

let parse_selection src =
  let start_pos =
    match String.index src '$' with
    | Some x -> x
    | None -> failwith "expected a selection opening mark"
  in
  let end_pos =
    match String.index_from src (start_pos + 1) '$' with
    | Some x ->
      if Option.is_some (String.index_from src (x + 1) '$')
      then failwith "unexpected third selection mark";
      x - 1 (* account for opening mark *)
    | None -> start_pos
  in
  let start = position_of_offset src start_pos in
  let end_ = position_of_offset src end_pos in
  let src' =
    String.filter_map src ~f:(function
      | '$' -> None
      | c -> Some c)
  in
  src', Range.create ~start ~end_
;;

let apply_code_action ?diagnostics title source range =
  let open Option.O in
  (* collect code action results *)
  let code_actions = ref None in
  iter_code_actions ?diagnostics ~source range (fun ca -> code_actions := Some ca);
  let* m_code_actions = !code_actions in
  let* code_actions = m_code_actions in
  let* edit =
    List.find_map code_actions ~f:(function
      | `CodeAction { title = t; edit = Some edit; _ } when t = title -> Some edit
      | _ -> None)
  in
  let+ changes = edit.documentChanges in
  List.concat_map changes ~f:(function
    | `TextDocumentEdit x ->
      List.map x.edits ~f:(function
        | `AnnotatedTextEdit (a : AnnotatedTextEdit.t) ->
          TextEdit.create ~newText:a.newText ~range:a.range
        | `SnippetTextEdit (s : SnippetTextEdit.t) ->
          TextEdit.create ~newText:s.snippet.value ~range:s.range
        | `TextEdit e -> e)
    | `CreateFile _ | `DeleteFile _ | `RenameFile _ -> [])
  |> Test.apply_edits source
;;

let code_action_test ~title source =
  let src, range = parse_selection source in
  Option.iter (apply_code_action title src range) ~f:print_string
;;

let setup_inferred_intf_workspace () =
  let dir = Test.temp_dir "ocamllsp-code-action-" in
  Test.write_file (Stdlib.Filename.concat dir "dune-project") "(lang dune 2.5)\n";
  Test.write_file
    (Stdlib.Filename.concat dir "dune")
    "(library\n (name code_action_intf)\n (flags :standard -w -32))\n";
  Test.write_file (Stdlib.Filename.concat dir "lib.ml") "let x = 1\n";
  Test.write_file (Stdlib.Filename.concat dir "lib.mli") "";
  Test.run_command ~cwd:dir "dune build";
  dir
;;

let action_title expected = function
  | `CodeAction { CodeAction.title; _ } -> String.equal expected title
  | _ -> false
;;

let add_rec_action = action_title "Add missing `rec` keyword"
let mark_unused_action = action_title "Mark as unused"
let remove_unused_action = action_title "Remove unused"

let diagnostic ?(severity = DiagnosticSeverity.Error) message range =
  Diagnostic.create ~message:(`String message) ~range ~severity ~source:"ocamllsp" ()
;;

let print_inferred_intf_edits source path range =
  iter_code_actions ~path ~source range (function
    | None -> print_endline "No code actions"
    | Some code_actions ->
      (match List.find code_actions ~f:(find_action "inferred_intf") with
       | None -> print_endline "No inferred interface action"
       | Some (`Command _) -> print_endline "Inferred interface action was a command"
       | Some (`CodeAction { edit = None; _ }) -> print_endline "No edit"
       | Some (`CodeAction { edit = Some edit; _ }) ->
         let edits =
           Option.value edit.documentChanges ~default:[]
           |> List.filter_map ~f:(function
             | `TextDocumentEdit (text_document_edit : TextDocumentEdit.t) ->
               Some
                 (`List
                     (List.map text_document_edit.edits ~f:(function
                        | `TextEdit edit -> TextEdit.yojson_of_t edit
                        | `AnnotatedTextEdit edit -> AnnotatedTextEdit.yojson_of_t edit
                        | `SnippetTextEdit edit -> SnippetTextEdit.yojson_of_t edit)))
             | `CreateFile _ | `RenameFile _ | `DeleteFile _ -> None)
         in
         Test.print_result (`List edits)))
;;

let%expect_test "opens the implementation if not in store" =
  let dir = setup_inferred_intf_workspace () in
  let path = Stdlib.Filename.concat dir "lib.mli" in
  let range = range ~start_line:0 ~start_character:0 ~end_line:0 ~end_character:0 in
  print_inferred_intf_edits "" path range;
  [%expect
    {|
    [
      [
        {
          "newText": "val x : int\n",
          "range": {
            "end": { "character": 0, "line": 0 },
            "start": { "character": 0, "line": 0 }
          }
        }
      ]
    ]
    |}]
;;

let%expect_test "offers Construct an expression code action" =
  let source =
    {ocaml|let x = _
|ocaml}
  in
  let range = range ~start_line:0 ~start_character:8 ~end_line:0 ~end_character:9 in
  print_code_actions ~path:"test.ml" ~filter:(find_action "construct") source range;
  [%expect
    {|
    Code actions:
    {
      "command": {
        "command": "editor.action.triggerSuggest",
        "title": "Trigger Suggest"
      },
      "kind": "construct",
      "title": "Construct an expression"
    }
    |}]
;;

let%expect_test "refactor-open unqualify in-file module" =
  let source =
    {ocaml|module M = struct
  let a = 1
  let f x = x + 1
end

open M

let y = M.f M.a
|ocaml}
  in
  let range = range ~start_line:6 ~start_character:5 ~end_line:6 ~end_character:5 in
  print_code_actions
    ~path:"test.ml"
    ~filter:(action_title "Remove module name from identifiers")
    source
    range;
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "changes": {
          "file:///test.ml": [
            {
              "newText": "f",
              "range": {
                "end": { "character": 11, "line": 7 },
                "start": { "character": 8, "line": 7 }
              }
            },
            {
              "newText": "a",
              "range": {
                "end": { "character": 15, "line": 7 },
                "start": { "character": 12, "line": 7 }
              }
            }
          ]
        }
      },
      "isPreferred": false,
      "kind": "remove module name from identifiers",
      "title": "Remove module name from identifiers"
    }
    |}]
;;

let%expect_test "refactor-open qualify in-file module" =
  let source =
    {ocaml|module M = struct
  let a = 1
  let f x = x + 1
end

open M

let y = f a
|ocaml}
  in
  let range = range ~start_line:6 ~start_character:5 ~end_line:6 ~end_character:5 in
  print_code_actions
    ~path:"test.ml"
    ~filter:(action_title "Put module name in identifiers")
    source
    range;
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "changes": {
          "file:///test.ml": [
            {
              "newText": "M.f",
              "range": {
                "end": { "character": 9, "line": 7 },
                "start": { "character": 8, "line": 7 }
              }
            },
            {
              "newText": "M.a",
              "range": {
                "end": { "character": 11, "line": 7 },
                "start": { "character": 10, "line": 7 }
              }
            }
          ]
        }
      },
      "isPreferred": false,
      "kind": "put module name in identifiers",
      "title": "Put module name in identifiers"
    }
    |}]
;;

let%expect_test "add missing rec in toplevel let" =
  let source =
    {ocaml|let needs_rec x = 1 + (needs_rec x)
|ocaml}
  in
  let diagnostics =
    [ diagnostic
        "Unbound value"
        (range ~start_line:0 ~start_character:23 ~end_line:0 ~end_character:32)
    ]
  in
  let range = range ~start_line:0 ~start_character:31 ~end_line:0 ~end_character:32 in
  print_code_actions
    ~path:"missing-rec-1.ml"
    ~diagnostics
    ~filter:add_rec_action
    source
    range;
  [%expect
    {|
    Code actions:
    {
      "diagnostics": [
        {
          "message": "Unbound value",
          "range": {
            "end": { "character": 32, "line": 0 },
            "start": { "character": 23, "line": 0 }
          },
          "severity": 1,
          "source": "ocamllsp"
        }
      ],
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "rec ",
                "range": {
                  "end": { "character": 4, "line": 0 },
                  "start": { "character": 4, "line": 0 }
                }
              }
            ],
            "textDocument": { "uri": "file:///missing-rec-1.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "quickfix",
      "title": "Add missing `rec` keyword"
    }
    |}]
;;

let%expect_test "add missing rec in expression let" =
  let source =
    {ocaml|let outer =
  let inner x =
    1 + (inner
|ocaml}
  in
  let diagnostics =
    [ diagnostic
        "Unbound value"
        (range ~start_line:2 ~start_character:9 ~end_line:2 ~end_character:14)
    ]
  in
  let range = range ~start_line:2 ~start_character:14 ~end_line:2 ~end_character:15 in
  print_code_actions
    ~path:"missing-rec-2.ml"
    ~diagnostics
    ~filter:add_rec_action
    source
    range;
  [%expect
    {|
    Code actions:
    {
      "diagnostics": [
        {
          "message": "Unbound value",
          "range": {
            "end": { "character": 14, "line": 2 },
            "start": { "character": 9, "line": 2 }
          },
          "severity": 1,
          "source": "ocamllsp"
        }
      ],
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "rec ",
                "range": {
                  "end": { "character": 6, "line": 1 },
                  "start": { "character": 6, "line": 1 }
                }
              }
            ],
            "textDocument": { "uri": "file:///missing-rec-2.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "quickfix",
      "title": "Add missing `rec` keyword"
    }
    |}]
;;

let%expect_test "add missing rec in expression let-and" =
  let source =
    {ocaml|let outer =
  let inner1 = 0
  and inner x =
    1 + (inner
|ocaml}
  in
  let diagnostics =
    [ diagnostic
        "Unbound value"
        (range ~start_line:3 ~start_character:9 ~end_line:3 ~end_character:14)
    ]
  in
  let range = range ~start_line:3 ~start_character:14 ~end_line:3 ~end_character:15 in
  print_code_actions
    ~path:"missing-rec-3.ml"
    ~diagnostics
    ~filter:add_rec_action
    source
    range;
  [%expect
    {|
    Code actions:
    {
      "diagnostics": [
        {
          "message": "Unbound value",
          "range": {
            "end": { "character": 14, "line": 3 },
            "start": { "character": 9, "line": 3 }
          },
          "severity": 1,
          "source": "ocamllsp"
        }
      ],
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "rec ",
                "range": {
                  "end": { "character": 6, "line": 1 },
                  "start": { "character": 6, "line": 1 }
                }
              }
            ],
            "textDocument": { "uri": "file:///missing-rec-3.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "quickfix",
      "title": "Add missing `rec` keyword"
    }
    |}]
;;

let%expect_test "don't add rec when rec exists" =
  let source =
    {ocaml|let outer =
  let rec inner x =
    1 + (inner
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:14 ~end_line:2 ~end_character:15 in
  print_code_actions ~path:"has-rec-2.ml" ~filter:add_rec_action source range;
  [%expect {| No code actions |}]
;;

let%expect_test "don't add rec to pattern bindings" =
  let source =
    {ocaml|let (f, x) = 1 + (f x)
|ocaml}
  in
  let diagnostics =
    [ diagnostic
        "Unbound value"
        (range ~start_line:0 ~start_character:18 ~end_line:0 ~end_character:19)
    ]
  in
  let range = range ~start_line:0 ~start_character:18 ~end_line:0 ~end_character:19 in
  print_code_actions ~path:"no-rec-1.ml" ~diagnostics ~filter:add_rec_action source range;
  [%expect {| No code actions |}]
;;

let unused_source =
  {ocaml|let f x =
  let y = [
    1;
    2;
  ] in
  0
|ocaml}
;;

let unused_diagnostics =
  [ diagnostic
      ~severity:DiagnosticSeverity.Warning
      "Error (warning 26): unused variable"
      (range ~start_line:1 ~start_character:6 ~end_line:1 ~end_character:7)
  ]
;;

let%expect_test "mark variable as unused" =
  let range = range ~start_line:1 ~start_character:6 ~end_line:1 ~end_character:7 in
  print_code_actions
    ~path:"mark-unused-variable.ml"
    ~diagnostics:unused_diagnostics
    ~filter:mark_unused_action
    unused_source
    range;
  [%expect
    {|
    Code actions:
    {
      "diagnostics": [
        {
          "message": "Error (warning 26): unused variable",
          "range": {
            "end": { "character": 7, "line": 1 },
            "start": { "character": 6, "line": 1 }
          },
          "severity": 2,
          "source": "ocamllsp"
        }
      ],
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "_",
                "range": {
                  "end": { "character": 6, "line": 1 },
                  "start": { "character": 6, "line": 1 }
                }
              }
            ],
            "textDocument": {
              "uri": "file:///mark-unused-variable.ml",
              "version": 0
            }
          }
        ]
      },
      "isPreferred": true,
      "kind": "quickfix",
      "title": "Mark as unused"
    }
    |}]
;;

let%expect_test "remove unused variable" =
  let range = range ~start_line:1 ~start_character:6 ~end_line:1 ~end_character:7 in
  print_code_actions
    ~path:"remove-unused-variable.ml"
    ~diagnostics:unused_diagnostics
    ~filter:remove_unused_action
    unused_source
    range;
  [%expect
    {|
    Code actions:
    {
      "diagnostics": [
        {
          "message": "Error (warning 26): unused variable",
          "range": {
            "end": { "character": 7, "line": 1 },
            "start": { "character": 6, "line": 1 }
          },
          "severity": 2,
          "source": "ocamllsp"
        }
      ],
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "",
                "range": {
                  "end": { "character": 2, "line": 5 },
                  "start": { "character": 2, "line": 1 }
                }
              }
            ],
            "textDocument": {
              "uri": "file:///remove-unused-variable.ml",
              "version": 0
            }
          }
        ]
      },
      "isPreferred": false,
      "kind": "quickfix",
      "title": "Remove unused"
    }
    |}]
;;

let%expect_test "don't remove unused value in let-and binding" =
  let source =
    {ocaml|let f x =
  let y = 0 and z = 0 in
  0
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:6 ~end_line:1 ~end_character:7 in
  print_code_actions
    ~path:"remove-unused-variable-2.ml"
    ~diagnostics:unused_diagnostics
    ~filter:remove_unused_action
    source
    range;
  [%expect {| No code actions |}]
;;

let%expect_test "next-hole range ends at the last inserted line's character" =
  let source =
    {ocaml|
let f (x:bool) =
  match x
|ocaml}
  in
  let capabilities =
    ClientCapabilities.create ~experimental:(`Assoc [ "jumpToNextHole", `Bool true ]) ()
  in
  let req client =
    let query_range =
      range ~start_line:2 ~start_character:5 ~end_line:2 ~end_character:5
    in
    let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let context =
      CodeActionContext.create
        ~diagnostics:[]
        ~only:
          [ CodeActionKind.Other "destruct-line (enumerate cases, use existing match)" ]
        ()
    in
    let params = CodeActionParams.create ~textDocument ~range:query_range ~context () in
    let* response = Client.request client (CodeAction params) in
    let in_range =
      let open Option.O in
      let* actions = response in
      let* action =
        List.find
          actions
          ~f:(find_action "destruct-line (enumerate cases, use existing match)")
      in
      let* command =
        match action with
        | `Command _ -> None
        | `CodeAction action -> action.command
      in
      let* arguments = command.arguments in
      match arguments with
      | [ `Assoc fields ] ->
        List.find_map fields ~f:(fun (name, value) ->
          Option.some_if (String.equal name "inRange") value)
      | _ -> None
    in
    Option.iter in_range ~f:Test.print_result;
    Fiber.return ()
  in
  Helpers.test ~capabilities source req;
  [%expect
    {|
    {
      "end": { "character": 15, "line": 4 },
      "start": { "character": 2, "line": 2 }
    }
    |}]
;;

let%expect_test "combine-cases survives an incremental edit" =
  let source =
    {ocaml|type t = A | B
let f = function
  | A -> 1
  | B -> 1
|ocaml}
  in
  let first_diagnostics = Fiber.Ivar.create () in
  let handler =
    Client.Handler.make
      ~on_notification:(fun _ -> function
         | PublishDiagnostics _ ->
           let* filled = Fiber.Ivar.peek first_diagnostics in
           (match filled with
            | Some _ -> Fiber.return ()
            | None -> Fiber.Ivar.fill first_diagnostics ())
         | _ -> Fiber.return ())
      ()
  in
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
      let settings = `Assoc [ "diagnostics_delay", `Float 10.0 ] in
      let* () = Client.notification client (ChangeConfiguration { settings }) in
      let edit_range =
        range ~start_line:2 ~start_character:8 ~end_line:2 ~end_character:8
      in
      let contentChanges =
        [ `TextDocumentContentChangePartial
            (TextDocumentContentChangePartial.create ~range:edit_range ~text:" " ())
        ]
      in
      let textDocument =
        VersionedTextDocumentIdentifier.create ~uri:Helpers.uri ~version:1
      in
      let change = DidChangeTextDocumentParams.create ~textDocument ~contentChanges in
      let* () = Client.notification client (TextDocumentDidChange change) in
      let query_range =
        range ~start_line:2 ~start_character:0 ~end_line:4 ~end_character:0
      in
      let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
      let context =
        CodeActionContext.create
          ~diagnostics:[]
          ~only:[ CodeActionKind.Other "combine-cases" ]
          ()
      in
      let params = CodeActionParams.create ~textDocument ~range:query_range ~context () in
      let* response = Client.request client (CodeAction params) in
      let available =
        response
        |> Option.value ~default:[]
        |> List.exists ~f:(find_action "combine-cases")
      in
      Printf.printf "combine-cases available: %b\n" available;
      let* () = Client.request client Shutdown in
      Client.notification client Exit
    in
    Fiber.fork_and_join_unit run_client run);
  [%expect {| combine-cases available: false |}]
;;

open Test.Import
open Lsp_helpers

let iter_code_actions ?prep ?path ?(diagnostics = []) ~source range =
  let makeRequest textDocument =
    let context = CodeActionContext.create ~diagnostics () in
    Lsp.Client_request.CodeAction
      (CodeActionParams.create ~textDocument ~range ~context ())
  in
  iter_lsp_response ?prep ?path ~makeRequest ~source
;;

let print_code_actions
  ?(prep = fun _ -> Fiber.return ())
  ?(path = "foo.ml")
  ?(filter = fun _ -> true)
  source
  range
  =
  iter_code_actions ~prep ~path ~source range (function
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
;;

let%expect_test "can type-annotate a function argument" =
  let source = {ocaml|
type t = Foo of int | Bar of bool
let f x = Foo x
|ocaml} in
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
;;

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
;;

let%expect_test "does not type-annotate function" =
  let source = {ocaml|
let my_fun x y = 1
|ocaml} in
  let range =
    let start = Position.create ~line:1 ~character:5 in
    let end_ = Position.create ~line:1 ~character:6 in
    Range.create ~start ~end_
  in
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
;;

let%expect_test "can type-annotate a variant with its name only" =
  let source = {ocaml|
type t = Foo of int | Bar of bool

let f (x : t) = x
|ocaml} in
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
;;

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
;;

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
;;

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
;;

let%expect_test "does not type-annotate already annotated and coerced expression" =
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
;;

let%expect_test "can remove type annotation from a function argument" =
  let source = {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) = Foo x
|ocaml} in
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
;;

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
;;

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
;;

let%expect_test "does not remove type annotation from function" =
  let source = {ocaml|
let my_fun x y : int = 1
|ocaml} in
  let range =
    let start = Position.create ~line:1 ~character:5 in
    let end_ = Position.create ~line:1 ~character:6 in
    Range.create ~start ~end_
  in
  print_code_actions source range ~filter:find_remove_annotation_action;
  [%expect {| No code actions |}]
;;

let%expect_test "can destruct sum types" =
  let source = {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) = x
|ocaml} in
  let range =
    let start = Position.create ~line:2 ~character:16 in
    let end_ = Position.create ~line:2 ~character:17 in
    Range.create ~start ~end_
  in
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
                "newText": "match x with Foo _ -> _ | Bar _ -> _\n",
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
  let source = {ocaml|
let f (x:bool) =
  match x
|ocaml} in
  let range =
    let start = Position.create ~line:2 ~character:5 in
    let end_ = Position.create ~line:2 ~character:5 in
    Range.create ~start ~end_
  in
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

let%expect_test "can destruct match-with line" =
  let source = {ocaml|
    match (Ok 0) with
|ocaml} in
  let range =
    let start = Position.create ~line:1 ~character:0 in
    let end_ = Position.create ~line:1 ~character:0 in
    Range.create ~start ~end_
  in
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
  let range =
    let start = Position.create ~line:8 ~character:0 in
    let end_ = Position.create ~line:8 ~character:0 in
    Range.create ~start ~end_
  in
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
  let range =
    let start = Position.create ~line:3 ~character:5 in
    let end_ = Position.create ~line:3 ~character:5 in
    Range.create ~start ~end_
  in
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
  let range =
    let start = Position.create ~line:8 ~character:5 in
    let end_ = Position.create ~line:8 ~character:5 in
    Range.create ~start ~end_
  in
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
  let range =
    let start = Position.create ~line:8 ~character:2 in
    let end_ = Position.create ~line:8 ~character:2 in
    Range.create ~start ~end_
  in
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
  let range =
    let start = Position.create ~line:7 ~character:7 in
    let end_ = Position.create ~line:7 ~character:7 in
    Range.create ~start ~end_
  in
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
  let range =
    let start = Position.create ~line:9 ~character:22 in
    let end_ = Position.create ~line:9 ~character:22 in
    Range.create ~start ~end_
  in
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

let%expect_test "can infer module interfaces" =
  let impl_source = {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) = x
|ocaml} in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.openDocument ~client ~uri ~source:impl_source in
  let intf_source = "" in
  let range =
    let start = Position.create ~line:0 ~character:0 in
    let end_ = Position.create ~line:0 ~character:0 in
    Range.create ~start ~end_
  in
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
  let impl_source = {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) = x
|ocaml} in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.openDocument ~client ~uri ~source:impl_source in
  let intf_source = {ocaml|
val f : t -> t
|ocaml} in
  let range =
    let start = Position.create ~line:0 ~character:0 in
    let end_ = Position.create ~line:0 ~character:0 in
    Range.create ~start ~end_
  in
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
  let intf_source = {ocaml|
type t = Foo of int | Bar of bool
val f : t -> bool
|ocaml} in
  let range =
    let start = Position.create ~line:2 ~character:0 in
    let end_ = Position.create ~line:2 ~character:0 in
    Range.create ~start ~end_
  in
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
  let intf_source = {ocaml|
val f : int -> string -> 'a list -> bool -> bool
|ocaml} in
  let range =
    let start = Position.create ~line:1 ~character:10 in
    let end_ = Position.create ~line:1 ~character:10 in
    Range.create ~start ~end_
  in
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
  let intf_source = {ocaml|
val f : int -> string -> 'a list -> bool -> bool
|ocaml} in
  let range =
    let start = Position.create ~line:1 ~character:1 in
    let end_ = Position.create ~line:1 ~character:12 in
    Range.create ~start ~end_
  in
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
  let range =
    let start = Position.create ~line:1 ~character:0 in
    let end_ = Position.create ~line:10 ~character:19 in
    Range.create ~start ~end_
  in
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
  let intf_source = {ocaml|
module M : sig type t = I of int | B of bool end
|ocaml} in
  let range =
    let start = Position.create ~line:1 ~character:0 in
    let end_ = Position.create ~line:1 ~character:0 in
    Range.create ~start ~end_
  in
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
        | `TextEdit e -> e)
    | `CreateFile _ | `DeleteFile _ | `RenameFile _ -> [])
  |> Test.apply_edits source
;;

let code_action_test ~title source =
  let src, range = parse_selection source in
  Option.iter (apply_code_action title src range) ~f:print_string
;;

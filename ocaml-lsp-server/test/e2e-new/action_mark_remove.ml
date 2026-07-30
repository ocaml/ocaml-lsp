open Test.Import

let run_test ~title ~message source =
  let src, range = Code_actions.parse_selection source in
  Code_actions.apply_code_action
    ~diagnostics:[ Diagnostic.create ~message:(`String message) ~range () ]
    title
    src
    range
  |> Option.iter ~f:print_string
;;

let mark_test = function
  | `Value -> run_test ~title:"Mark as unused" ~message:"unused value"
  | `Open -> run_test ~title:"Replace with open!" ~message:"unused open"
  | `For_loop_index ->
    run_test ~title:"Mark for-loop index as unused" ~message:"unused for-loop index"
;;

let remove_test = function
  | `Value -> run_test ~title:"Remove unused" ~message:"unused value"
  | `Open -> run_test ~title:"Remove unused open" ~message:"unused open"
  | `Open_bang -> run_test ~title:"Remove unused open!" ~message:"unused open!"
  | `Type -> run_test ~title:"Remove unused type" ~message:"unused type"
  | `Module -> run_test ~title:"Remove unused module" ~message:"unused module"
  | `Case -> run_test ~title:"Remove unused case" ~message:"this match case is unused"
  | `Rec -> run_test ~title:"Remove unused rec" ~message:"unused rec flag"
  | `Constructor ->
    run_test ~title:"Remove unused constructor" ~message:"unused constructor"
;;

let%expect_test "mark value in let" =
  mark_test
    `Value
    {|
let f =
  let $x$ = 1 in
  0
|};
  [%expect
    {|
    let f =
      let _x = 1 in
      0 |}]
;;

(* todo *)
let%expect_test "mark value in top level let" =
  mark_test
    `Value
    {|
let $f$ =
  let x = 1 in
  0
|};
  [%expect
    {|
    let _f =
      let x = 1 in
      0 |}]
;;

let%expect_test "mark value in match" =
  mark_test
    `Value
    {|
let f = function
  | $x$ -> 0
|};
  [%expect
    {|
    let f = function
      | _x -> 0 |}]
;;

let%expect_test "remove value in let" =
  remove_test
    `Value
    {|
let f =
  let $x$ = 1 in
  0
|};
  [%expect
    {|
    let f =
      0 |}]
;;

(* todo *)
let%expect_test "remove value in top level let" =
  remove_test
    `Value
    {|
let $f$ =
  let x = 1 in
  0
|}
;;

let%expect_test "mark open" =
  mark_test
    `Open
    {|
$open M$
|};
  [%expect {| open! M |}]
;;

let%expect_test "mark for loop index" =
  mark_test
    `For_loop_index
    {|
let () =
  for $i$ = 0 to 10 do
    ()
  done
|};
  [%expect
    {|
    let () =
      for _i = 0 to 10 do
        ()
      done |}]
;;

let%expect_test "remove open" =
  remove_test
    `Open
    {|
open A
$open B$
|};
  [%expect {| open A |}]
;;

let%expect_test "remove open!" =
  remove_test
    `Open_bang
    {|
open A
$open! B$
|}
;;

let%expect_test "remove type" =
  remove_test
    `Type
    {|
$type t = int$
type s = bool
|};
  [%expect {| type s = bool |}]
;;

let%expect_test "remove module" =
  remove_test
    `Module
    {|
$module A = struct end$
module B = struct end
|};
  [%expect {| module B = struct end |}]
;;

let%expect_test "remove case" =
  remove_test
    `Case
    {|
let f = function
 | 0 -> 0
 | $0 -> 1$
|};
  [%expect
    {|
    let f = function
     | 0 -> 0 |}]
;;

let%expect_test "remove case after Unicode" =
  let source, range =
    Code_actions.parse_selection
      {|
let f = function
 | 0 -> 0
 | $"😀" -> 1$
|}
  in
  let diagnostics =
    [ Diagnostic.create ~message:(`String "this match case is unused") ~range () ]
  in
  Code_actions.print_code_actions
    ~diagnostics
    ~filter:(function
      | `CodeAction { title; _ } -> String.equal title "Remove unused case"
      | `Command _ -> false)
    source
    range;
  [%expect
    {|
    Code actions:
    {
      "diagnostics": [
        {
          "message": "this match case is unused",
          "range": {
            "end": { "character": 12, "line": 3 },
            "start": { "character": 3, "line": 3 }
          }
        }
      ],
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "",
                "range": {
                  "end": { "character": 14, "line": 3 },
                  "start": { "character": 1, "line": 3 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": true,
      "kind": "quickfix",
      "title": "Remove unused case"
    }
    |}]
;;

let%expect_test "remove rec flag after Unicode" =
  let source, range =
    Code_actions.parse_selection
      {|
let café = let rec $f$ = 0 in f
|}
  in
  let diagnostics =
    [ Diagnostic.create ~message:(`String "unused rec flag") ~range () ]
  in
  Code_actions.print_code_actions
    ~diagnostics
    ~filter:(function
      | `CodeAction { title; _ } -> String.equal title "Remove unused rec"
      | `Command _ -> false)
    source
    range;
  [%expect
    {|
    Code actions:
    {
      "diagnostics": [
        {
          "message": "unused rec flag",
          "range": {
            "end": { "character": 20, "line": 1 },
            "start": { "character": 19, "line": 1 }
          }
        }
      ],
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "",
                "range": {
                  "end": { "character": 19, "line": 1 },
                  "start": { "character": 16, "line": 1 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "quickfix",
      "title": "Remove unused rec"
    }
    |}]
;;

let%expect_test "remove constructor" =
  remove_test
    `Constructor
    {|
type t = A $| B$
|};
  [%expect {| type t = A |}]
;;

let%expect_test "remove constructor" =
  remove_test
    `Constructor
    {|
type t =
  | A
 $| B$
|};
  [%expect
    {|
    type t =
      | A |}]
;;

let%expect_test "remove constructor" =
  remove_test
    `Constructor
    {|
type t =
 $| A$
 | B
|};
  [%expect
    {|
    type t =

     | B |}]
;;

let%expect_test "remove constructor" =
  remove_test
    `Constructor
    {|
type t =
 $A$
 | B
|};
  [%expect
    {|
    type t =

     | B |}]
;;

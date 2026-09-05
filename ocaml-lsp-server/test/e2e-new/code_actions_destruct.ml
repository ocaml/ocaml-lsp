open Test.Import
open Lsp_helpers
open Code_actions

let destruct = code_action_test ~print_none:true ~title:"Destruct (enumerate cases)"

let destruct_line =
  code_action_test
    ~print_none:true
    ~title:"Destruct-line (enumerate cases, use existing match)"
;;

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

let%expect_test "destruct-line rejects a cross-line recovery location" =
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
  [%expect {| No code actions |}]
;;

let%expect_test "can destruct sum types" =
  destruct
    {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) = $x$
|ocaml};
  [%expect
    {|
    type t = Foo of int | Bar of bool
    let f (x : t) = match x with | Foo _ -> _ | Bar _ -> _
    |}]
;;

let%expect_test "can destruct match line" =
  destruct_line
    {ocaml|
let f (x:bool) =
  mat$ch x
|ocaml};
  [%expect
    {|
    let f (x:bool) =
      match x with
      | false -> _
      | true -> _
    |}]
;;

let%expect_test "destruct-line is available on a whole inline match expression" =
  destruct_line
    {ocaml|
type t = A | B | C
let x = $match A with
  | A -> _$
|ocaml};
  [%expect
    {|
    type t = A | B | C
    let x = match A with
      | A -> _
      | B -> _
      | C -> _
    |}]
;;

let%expect_test "destruct-line is available when a match case is on the same line" =
  destruct_line
    {ocaml|
type t = A | B | C
let x = $match$ A with | A -> _
|ocaml};
  [%expect
    {|
    type t = A | B | C
    let x = match A with | A -> _
                         | B -> _
                         | C -> _
    |}]
;;

let%expect_test "destruct-line expands an inline match without cases" =
  destruct_line
    {ocaml|
type t = A | B | C
let x = $match$ A with
|ocaml};
  [%expect
    {|
    type t = A | B | C
    let x = match A with
            | A -> _
            | B -> _
            | C -> _
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
  destruct_line
    {ocaml|
type t = A | B | C
type r = { value : t }
let f r = $match$ { r with value = A }.value with | A -> _
|ocaml};
  [%expect
    {|
    type t = A | B | C
    type r = { value : t }
    let f r = match { r with value = A }.value with | A -> _
                                                    | B -> _
                                                    | C -> _
    |}]
;;

let%expect_test "destruct-line finds the outer case of a nested match" =
  destruct_line
    {ocaml|
type t = A | B | C
let x = $match$ (match A with | A -> B | B -> C | C -> A) with | A -> _
|ocaml};
  [%expect
    {|
    type t = A | B | C
    let x = match (match A with | A -> B | B -> C | C -> A) with | A -> _
                                                                 | B -> _
                                                                 | C -> _
    |}]
;;

let%expect_test
    "destruct-line finds the outer case of a nested match containing a record update"
  =
  destruct_line
    {ocaml|
type t = A | B | C
type r = { value : t }
let f r = $match$ (match { r with value = A } with | A -> B | B -> C | C -> A) with | C -> _
|ocaml};
  [%expect
    {|
    type t = A | B | C
    type r = { value : t }
    let f r = match (match { r with value = A } with | A -> B | B -> C | C -> A) with | C -> _
                                                                                      | A -> _
                                                                                      | B -> _
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
  destruct_line
    {ocaml|
$    match (Ok 0) with
|ocaml};
  [%expect
    {|
    match Ok 0 with
    | Ok _ -> _
    | Error _ -> _
    |}]
;;

let%expect_test "can destruct case line" =
  destruct_line
    {ocaml|
type q =
| A
| B
| C
| D
let f (x: q) =
  match x with
$  | C -> _
|ocaml};
  [%expect
    {|
    type q =
    | A
    | B
    | C
    | D
    let f (x: q) =
      match x with
      | C -> _
      | A -> _
      | B -> _
      | D -> _
    |}]
;;

let%expect_test "can destruct hole" =
  destruct_line
    {ocaml|
let zip (type a b) (xs : a list) (ys : b list) : (a * b) list =
  match (xs, ys) with
  | ($_, _) -> _
|ocaml};
  [%expect
    {|
    let zip (type a b) (xs : a list) (ys : b list) : (a * b) list =
      match (xs, ys) with
      | ([], _) -> _
      | (_::_, _) -> _
    |}]
;;

let%expect_test "destruct hole spacing" =
  destruct_line
    {ocaml|
type q =
| A
| B
| C
| D
let f (x: q) =
  match x with
  | _$ -> _
|ocaml};
  [%expect
    {|
    type q =
    | A
    | B
    | C
    | D
    let f (x: q) =
      match x with
      | A -> _
      | B -> _
      | C -> _
      | D -> _
    |}]
;;

let%expect_test "destruct a case with a hole but not on the hole" =
  destruct_line
    {ocaml|
type q =
| A
| B
| C
| D
let f (x: q) =
  match x with
  $| _ -> _
|ocaml};
  [%expect
    {|
    type q =
    | A
    | B
    | C
    | D
    let f (x: q) =
      match x with
      | A -> _
      | B -> _
      | C -> _
      | D -> _
    |}]
;;

let%expect_test "destruct uses the right number of newlines" =
  destruct_line
    {ocaml|
type t =
  | Very_long_name_for_for_the_first_case_so_that_merlin_will_use_multiple_lines
  | Almost_as_long_name_for_for_the_second_case
  | Another_long_name_for_for_the_third_case
;;
let f (x: t) =
  match$ x with
  |ocaml};
  [%expect
    {|
    type t =
      | Very_long_name_for_for_the_first_case_so_that_merlin_will_use_multiple_lines
      | Almost_as_long_name_for_for_the_second_case
      | Another_long_name_for_for_the_third_case
    ;;
    let f (x: t) =
      match x with
      | Very_long_name_for_for_the_first_case_so_that_merlin_will_use_multiple_lines -> _
      | Almost_as_long_name_for_for_the_second_case -> _
      | Another_long_name_for_for_the_third_case -> _
    |}]
;;

let%expect_test "destruct strips parentheses even on long lines" =
  destruct_line
    {ocaml|
type q =
  | Very_long_name_for_for_the_first_case_so_that_merlin_will_be_forced_to_use_multiple_lines
  | Almost_as_long_name_for_for_the_second_case
  | Another_long_name_for_for_the_third_case
  | Very_long_name_for_for_the_last_case_so_that_we_can_make_sure_we_handle_both_parens_and_line_breaks of int
;;
let f (x: q) =
  match x with
  | Almost_as_long_nam$e_for_for_the_second_case -> _
|ocaml};
  [%expect
    {|
    type q =
      | Very_long_name_for_for_the_first_case_so_that_merlin_will_be_forced_to_use_multiple_lines
      | Almost_as_long_name_for_for_the_second_case
      | Another_long_name_for_for_the_third_case
      | Very_long_name_for_for_the_last_case_so_that_we_can_make_sure_we_handle_both_parens_and_line_breaks of int
    ;;
    let f (x: q) =
      match x with
      | Almost_as_long_name_for_for_the_second_case -> _
      | Very_long_name_for_for_the_first_case_so_that_merlin_will_be_forced_to_use_multiple_lines -> _
      | Another_long_name_for_for_the_third_case -> _
      | Very_long_name_for_for_the_last_case_so_that_we_can_make_sure_we_handle_both_parens_and_line_breaks
         _ -> _
    |}]
;;

let%expect_test "can destruct on sub-expression" =
  destruct
    {ocaml|
let defered_peek x = if x >= 0 then Some (`Foo x) else None
let job_reader = 10

let _ = $defered_peek job_reader$
|ocaml};
  [%expect
    {|
    let defered_peek x = if x >= 0 then Some (`Foo x) else None
    let job_reader = 10

    let _ = match defered_peek job_reader with | None -> _ | Some _ -> _
    |}]
;;

let%expect_test "can destruct on sub-expression that need parenthesis" =
  destruct
    {ocaml|
let defered_peek x = if x >= 0 then Some (`Foo x) else None
let job_reader = 10

let _ = defered_peek $job_reader$
|ocaml};
  [%expect
    {|
    let defered_peek x = if x >= 0 then Some (`Foo x) else None
    let job_reader = 10

    let _ = defered_peek (match job_reader with | 0 -> _ | _ -> _)
    |}]
;;

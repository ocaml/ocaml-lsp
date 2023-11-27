open Test.Import
open Lsp_helpers

let iter_code_actions ?prep ?path ?(diagnostics = []) ~source range =
  let makeRequest textDocument =
    let context = CodeActionContext.create ~diagnostics () in
    Lsp.Client_request.CodeAction
      (CodeActionParams.create ~textDocument ~range ~context ())
  in
  iter_lsp_response ?prep ?path ~makeRequest ~source

let print_code_actions ?(prep = fun _ -> Fiber.return ()) ?(path = "foo.ml")
    ?(filter = fun _ -> true) source range =
  iter_code_actions ~prep ~path ~source range (function
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

let find_action action_name action =
  match action with
  | `CodeAction { CodeAction.kind = Some (Other name); _ } -> name = action_name
  | _ -> false

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

let%expect_test "can destruct sum types" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) = x
|ocaml}
  in
  let range =
    let start = Position.create ~line:2 ~character:16 in
    let end_ = Position.create ~line:2 ~character:17 in
    Range.create ~start ~end_
  in
  print_code_actions source range ~filter:(find_action "destruct");
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
      "kind": "destruct",
      "title": "Destruct"
    } |}]

let%expect_test "can infer module interfaces" =
  let impl_source =
    {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) = x
|ocaml}
  in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = open_document ~client ~uri ~source:impl_source in
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

let position_of_offset src x =
  assert (0 <= x && x < String.length src);
  let cnum = ref 0
  and lnum = ref 0 in
  for i = 0 to x - 1 do
    if src.[i] = '\n' then (
      incr lnum;
      cnum := 0)
    else incr cnum
  done;
  Position.create ~character:!cnum ~line:!lnum

let parse_selection src =
  let start_pos =
    match String.index src '$' with
    | Some x -> x
    | None -> failwith "expected a selection opening mark"
  in
  let end_pos =
    match String.index_from src (start_pos + 1) '$' with
    | Some x ->
      if Option.is_some (String.index_from src (x + 1) '$') then
        failwith "unexpected third selection mark";
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
  (src', Range.create ~start ~end_)

let offset_of_position src (pos : Position.t) =
  let line_offset =
    String.split_lines src |> List.take pos.line
    |> List.fold_left ~init:0 ~f:(fun s l -> s + String.length l)
  in
  line_offset + pos.line (* account for line endings *) + pos.character

let apply_edits src edits =
  let rec apply src = function
    | [] -> src
    | (new_text, start, end_) :: edits ->
      (* apply edit *)
      let src' = String.take src start ^ new_text ^ String.drop src end_ in

      (* calculate amount of text added (or removed) *)
      let diff_len = String.length new_text - (end_ - start) in

      (* offset positions of remaining edits *)
      let edits' =
        List.map edits ~f:(fun (new_text, start, end_) ->
            (new_text, start + diff_len, end_ + diff_len))
      in
      apply src' edits'
  in
  let edits =
    List.map edits ~f:(fun (e : TextEdit.t) ->
        ( e.newText
        , offset_of_position src e.range.start
        , offset_of_position src e.range.end_ ))
  in
  apply src edits

let apply_code_action ?diagnostics title source range =
  let open Option.O in
  (* collect code action results *)
  let code_actions = ref None in
  iter_code_actions ?diagnostics ~source range (fun ca ->
      code_actions := Some ca);
  let* m_code_actions = !code_actions in
  let* code_actions = m_code_actions in

  let* edit =
    List.find_map code_actions ~f:(function
        | `CodeAction { title = t; edit = Some edit; _ } when t = title ->
          Some edit
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
  |> apply_edits source

let code_action_test ~title source =
  let src, range = parse_selection source in
  Option.iter (apply_code_action title src range) ~f:print_string

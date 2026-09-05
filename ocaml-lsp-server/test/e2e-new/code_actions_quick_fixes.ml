open Test.Import
open Lsp_helpers
open Code_actions

let setup_inferred_intf_workspace () =
  let dir = Test.temp_dir "ocamllsp-code-action-" in
  Test.write_file (Filename.concat dir "dune-project") "(lang dune 2.5)\n";
  Test.write_file
    (Filename.concat dir "dune")
    "(library\n (name code_action_intf)\n (flags :standard -w -32))\n";
  Test.write_file (Filename.concat dir "lib.ml") "let x = 1\n";
  Test.write_file (Filename.concat dir "lib.mli") "";
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

let print_applied_action ?diagnostics ~title source range =
  match apply_code_action ~path:"test.ml" ?diagnostics title source range with
  | None -> print_endline "None"
  | Some source -> print_string source
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
         Test.apply_workspace_edit source edit |> print_string))
;;

let%expect_test "opens the implementation if not in store" =
  let dir = setup_inferred_intf_workspace () in
  let path = Filename.concat dir "lib.mli" in
  let range = range ~start_line:0 ~start_character:0 ~end_line:0 ~end_character:0 in
  print_inferred_intf_edits "" path range;
  [%expect {| val x : int |}]
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
  print_applied_action ~title:"Remove module name from identifiers" source range;
  [%expect
    {|
    module M = struct
      let a = 1
      let f x = x + 1
    end

    open M

    let y = f a
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
  print_applied_action ~title:"Put module name in identifiers" source range;
  [%expect
    {|
    module M = struct
      let a = 1
      let f x = x + 1
    end

    open M

    let y = M.f M.a
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
  print_applied_action ~diagnostics ~title:"Add missing `rec` keyword" source range;
  [%expect {| let rec needs_rec x = 1 + (needs_rec x) |}]
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
  print_applied_action ~diagnostics ~title:"Add missing `rec` keyword" source range;
  [%expect
    {|
    let outer =
      let rec inner x =
        1 + (inner
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
  print_applied_action ~diagnostics ~title:"Add missing `rec` keyword" source range;
  [%expect
    {|
    let outer =
      let rec inner1 = 0
      and inner x =
        1 + (inner
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
      | [ `Assoc fields ] -> List.Assoc.find fields "inRange" ~equal:String.equal
      | _ -> None
    in
    Option.iter in_range ~f:Test.print_result;
    Fiber.return ()
  in
  Helpers.test ~capabilities source req;
  [%expect
    {|
    {
      "end": { "character": 13, "line": 4 },
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
  let on_notification, first_diagnostics = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  Test.run_initialized ~handler (fun client ->
    let* () = Test.open_document ~client ~uri:Helpers.uri ~source () in
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
    let edit =
      Option.value_exn response
      |> List.find_map ~f:(function
        | `CodeAction { CodeAction.title = "Combine-cases"; edit = Some edit; _ } ->
          Some edit
        | `CodeAction _ | `Command _ -> None)
      |> Option.value_exn
    in
    let version =
      match edit.documentChanges with
      | Some [ `TextDocumentEdit { textDocument = { version; _ }; _ } ] ->
        Option.value_exn version
      | None | Some _ -> failwith "expected one versioned document edit"
    in
    Printf.printf "edit version: %d\n" version;
    let source =
      Test.apply_edits source [ TextEdit.create ~range:edit_range ~newText:" " ]
    in
    Test.apply_workspace_edit source edit |> print_string;
    Test.exit_client client);
  [%expect
    {|
    edit version: 1
    type t = A | B
    let f = function
      | A | B -> 1
    |}]
;;

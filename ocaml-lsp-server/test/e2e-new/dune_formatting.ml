open Test.Import
open Dune_rpc_test

let formatting_params uri =
  let textDocument = TextDocumentIdentifier.create ~uri in
  let options = FormattingOptions.create ~tabSize:2 ~insertSpaces:true () in
  DocumentFormattingParams.create ~textDocument ~options ()
;;

let change_document client ~uri ~version ~text =
  let textDocument = VersionedTextDocumentIdentifier.create ~uri ~version in
  let contentChanges =
    [ `TextDocumentContentChangeWholeDocument
        (TextDocumentContentChangeWholeDocument.create ~text)
    ]
  in
  Client.notification
    client
    (TextDocumentDidChange
       (DidChangeTextDocumentParams.create ~textDocument ~contentChanges))
;;

let print_edits = function
  | None -> print_endline "no edits"
  | Some edits -> `List (List.map edits ~f:TextEdit.yojson_of_t) |> Test.print_result
;;

let%expect_test "format a Dune file and report malformed input" =
  let project = create_project "format" in
  Fun.protect
    ~finally:(fun () -> destroy_project project)
    (fun () ->
       let events = Lifecycle_events.create () in
       run project events ~f:(fun client _workspace ->
         let* () = Signal.wait (Events.dune_ready events.dune) in
         let uri = Uri.of_path (Filename.concat project.root "dune") in
         let source = "(rule(alias formatted)(action(echo hello)))\n" in
         let* () = Test.open_document ~language_id:"dune" ~client ~uri ~source () in
         let* edits =
           Client.request client (TextDocumentFormatting (formatting_params uri))
         in
         print_endline "valid formatting:";
         print_edits edits;
         let* () = change_document client ~uri ~version:1 ~text:"(rule" in
         let* result =
           Fiber.collect_errors (fun () ->
             Client.request client (TextDocumentFormatting (formatting_params uri)))
         in
         match result with
         | Error
             [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ }
             ] ->
           Printf.printf "malformed formatting: %s\n" error.message;
           Fiber.return ()
         | Error errors -> Fiber.reraise_all errors
         | Ok edits ->
           print_endline "malformed formatting unexpectedly succeeded:";
           print_edits edits;
           Fiber.return ()));
  [%expect
    {|
    valid formatting:
    [
      {
        "newText": "(rule\n (alias formatted)\n (action\n  (echo hello)))\n",
        "range": {
          "end": { "character": 0, "line": 1 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ]
    malformed formatting: dune failed to format
    |}]
;;

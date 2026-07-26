open Test.Import
open Dune_rpc_test

let%expect_test "a nested Dune instance ignores parent documents" =
  let project = create_project "nr" in
  let outside_dune = Filename.concat project.temp "dune" in
  let source = "(library(name outside))\n" in
  Test.write_file outside_dune source;
  Fun.protect
    ~finally:(fun () -> destroy_project project)
    (fun () ->
       let events = Lifecycle_events.create () in
       run ~workspace_root:project.temp project events ~f:(fun client _workspace ->
         let uri = Uri.of_path outside_dune in
         let* () = Signal.wait (Events.dune_ready events.dune) in
         Test.write_file project.gate "";
         let* (_ : PublishDiagnosticsParams.t) =
           Events.wait_for_diagnostics events.dune ~f:has_dune_diagnostic
         in
         let* () = Test.open_document ~language_id:"dune" ~client ~uri ~source () in
         let textDocument = TextDocumentIdentifier.create ~uri in
         let options = FormattingOptions.create ~tabSize:2 ~insertSpaces:true () in
         let request =
           Lsp.Client_request.TextDocumentFormatting
             (DocumentFormattingParams.create ~textDocument ~options ())
         in
         let* result = Fiber.collect_errors (fun () -> Client.request client request) in
         match result with
         | Error
             [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ }
             ] ->
           print_payload
             project
             "textDocument/formatting response:"
             (Jsonrpc.Response.Error.yojson_of_t error);
           Fiber.return ()
         | Error errors -> Fiber.reraise_all errors
         | Ok edits ->
           let json =
             match edits with
             | None -> `Null
             | Some edits -> `List (List.map edits ~f:TextEdit.yojson_of_t)
           in
           print_payload project "textDocument/formatting response:" json;
           Fiber.return ()));
  [%expect
    {|
    textDocument/formatting response:
    {
      "code": -32600,
      "message": "No dune instance found. Please run dune in watch mode for <test-dir>/dune"
    }
    |}]
;;

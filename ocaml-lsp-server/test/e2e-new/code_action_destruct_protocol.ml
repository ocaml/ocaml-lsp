open Test.Import
open Lsp_helpers

let code_action_capabilities () =
  let window =
    let showDocument = ShowDocumentClientCapabilities.create ~support:true in
    WindowClientCapabilities.create ~showDocument ()
  in
  let codeActionLiteralSupport =
    let codeActionKind = ClientCodeActionKindOptions.create ~valueSet:[] in
    ClientCodeActionLiteralOptions.create ~codeActionKind
  in
  let codeAction =
    CodeActionClientCapabilities.create ~codeActionLiteralSupport ~dataSupport:true ()
  in
  let textDocument = TextDocumentClientCapabilities.create ~codeAction () in
  ClientCapabilities.create ~window ~textDocument ()
;;

let metrics_handler contents =
  let on_request (type r) _ (request : r Lsp.Server_request.t)
    : (r Lsp_fiber.Rpc.Reply.t * unit) Fiber.t
    =
    match request with
    | ShowDocumentRequest params ->
      let metrics = Stdune.Io.String_path.read_file (Uri.to_path params.uri) in
      let* () = Fiber.Ivar.fill contents metrics in
      let result = ShowDocumentResult.create ~success:true in
      Fiber.return (Lsp_fiber.Rpc.Reply.now result, ())
    | _ -> assert false
  in
  let on_request = { Client.Handler.on_request } in
  Client.Handler.make ~on_request ~on_notification:(fun _ _ -> Fiber.return ()) ()
;;

let metric_count metrics name =
  match Yojson.Safe.from_string metrics with
  | `Assoc fields ->
    (match
       List.find_map fields ~f:(fun (field, value) ->
         Option.some_if (String.equal field "traceEvents") value)
     with
     | Some (`List events) ->
       List.fold_left events ~init:0 ~f:(fun count -> function
         | `Assoc fields ->
           (match
              List.find_map fields ~f:(fun (field, value) ->
                Option.some_if (String.equal field "name") value)
            with
            | Some (`String event_name) when String.equal event_name name -> count + 1
            | Some _ | None -> count)
         | _ -> count)
     | _ -> 0)
  | _ -> 0
;;

let print_pipeline_count ~name ~only ~source range =
  let contents = Fiber.Ivar.create () in
  let handler = metrics_handler contents in
  Test.run ~handler
  @@ fun client ->
  let run_client () =
    Test.start_client ~capabilities:(code_action_capabilities ()) client
  in
  let run () =
    let* (_ : InitializeResult.t) = Client.initialized client in
    let uri = DocumentUri.of_path "metrics.ml" in
    let* () = open_document ~language_id:"ocaml" ~client ~uri ~source in
    let textDocument = TextDocumentIdentifier.create ~uri in
    let context = CodeActionContext.create ~diagnostics:[] ~only () in
    let params = CodeActionParams.create ~textDocument ~range ~context () in
    let* (_ : CodeActionResult.t) = Client.request client (CodeAction params) in
    let view_metrics = ExecuteCommandParams.create ~command:"ocamllsp/view-metrics" () in
    let* _ = Client.request client (ExecuteCommand view_metrics) in
    let+ metrics = Fiber.Ivar.read contents in
    Printf.printf "%s pipelines: %d\n" name (metric_count metrics name)
  in
  let shutdown () =
    let* () = Client.request client Shutdown in
    Client.notification client Exit
  in
  Fiber.fork_and_join_unit run_client (fun () -> Fiber.finalize run ~finally:shutdown)
;;

let%expect_test "destruct actions duplicate case analysis" =
  let source =
    {ocaml|let f (x : bool) =
  match x
|ocaml}
  in
  let range =
    Code_actions.range ~start_line:1 ~start_character:8 ~end_line:1 ~end_character:9
  in
  let only =
    [ CodeActionKind.Other "destruct (enumerate cases)"
    ; CodeActionKind.Other "destruct-line (enumerate cases, use existing match)"
    ]
  in
  print_pipeline_count ~name:"destruct" ~only ~source range;
  [%expect {| destruct pipelines: 2 |}]
;;

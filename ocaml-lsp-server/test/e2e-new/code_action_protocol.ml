open Test.Import
open Lsp_helpers

let code_action_capabilities ?resolveSupport () =
  let window =
    let showDocument = ShowDocumentClientCapabilities.create ~support:true in
    WindowClientCapabilities.create ~showDocument ()
  in
  let codeActionLiteralSupport =
    let codeActionKind = ClientCodeActionKindOptions.create ~valueSet:[] in
    ClientCodeActionLiteralOptions.create ~codeActionKind
  in
  let codeAction =
    CodeActionClientCapabilities.create
      ~codeActionLiteralSupport
      ~dataSupport:true
      ?resolveSupport
      ()
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
      let metrics = Fs_io.read_file (Uri.to_path params.uri) |> Result.ok_exn in
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
    (match List.Assoc.find fields "traceEvents" ~equal:String.equal with
     | Some (`List events) ->
       List.count events ~f:(function
         | `Assoc fields ->
           (match List.Assoc.find fields "name" ~equal:String.equal with
            | Some (`String event_name) -> String.equal event_name name
            | Some _ | None -> false)
         | _ -> false)
     | _ -> 0)
  | _ -> 0
;;

let print_pipeline_count ?(prep = fun _ -> Fiber.return ()) ~name ~only ~source range =
  let contents = Fiber.Ivar.create () in
  let handler = metrics_handler contents in
  Test.run_initialized ~handler ~capabilities:(code_action_capabilities ())
  @@ fun client ->
  let run () =
    let* () = prep client in
    let uri = DocumentUri.of_path "metrics.ml" in
    let* () = Test.open_document ~client ~uri ~source () in
    let textDocument = TextDocumentIdentifier.create ~uri in
    let context = CodeActionContext.create ~diagnostics:[] ~only () in
    let params = CodeActionParams.create ~textDocument ~range ~context () in
    let* (_ : CodeActionResult.t) = Client.request client (CodeAction params) in
    let view_metrics = ExecuteCommandParams.create ~command:"ocamllsp/view-metrics" () in
    let* _ = Client.request client (ExecuteCommand view_metrics) in
    let+ metrics = Fiber.Ivar.read contents in
    Printf.printf "%s pipelines: %d\n" name (metric_count metrics name)
  in
  Fiber.finalize run ~finally:(fun () -> Test.exit_client client)
;;

let jump_kinds =
  [ "fun"; "match"; "let"; "module"; "module-type"; "next-case"; "prev-case" ]
  |> List.map ~f:(fun target -> CodeActionKind.Other ("merlin-jump-" ^ target))
;;

let activate_jump client =
  let settings = `Assoc [ "merlinJumpCodeActions", `Assoc [ "enable", `Bool true ] ] in
  change_config ~client (DidChangeConfigurationParams.create ~settings)
;;

let%expect_test "Merlin jump actions open a pipeline for every target" =
  let source =
    {ocaml|type t = Foo of int | Bar of bool
let f (x : t) =
  match x with
  | Foo x -> x
  | Bar _ -> 0
|ocaml}
  in
  let range =
    Code_actions.range ~start_line:3 ~start_character:4 ~end_line:3 ~end_character:4
  in
  print_pipeline_count ~prep:activate_jump ~name:"unknown" ~only:jump_kinds ~source range;
  [%expect {| unknown pipelines: 7 |}]
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

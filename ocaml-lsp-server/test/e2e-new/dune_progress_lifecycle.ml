open Test.Import
open Dune_rpc_test

let print_creation label params =
  print_endline label;
  WorkDoneProgressCreateParams.yojson_of_t params |> Test.print_result
;;

let print_progress label params =
  print_endline label;
  ProgressParams.yojson_of_t Lsp.Progress.yojson_of_t params |> Test.print_result
;;

let rec wait_for_begin (events : Lifecycle_events.t) label =
  let* params = Mailbox.wait (Events.progress events.dune) in
  match params.value with
  | Begin _ ->
    let+ creation = Mailbox.wait events.progress_creations in
    print_creation (label ^ " create:") creation;
    print_progress (label ^ " begin:") params
  | Report _ | End _ -> wait_for_begin events label
;;

let rec wait_for_end (events : Lifecycle_events.t) label expected =
  let* params = Mailbox.wait (Events.progress events.dune) in
  match params.value with
  | End { message = Some message } when String.equal message expected ->
    print_progress (label ^ " end:") params;
    Fiber.return ()
  | Report _ -> wait_for_end events label expected
  | Begin _ | End _ ->
    failwith (Printf.sprintf "expected progress end message %S" expected)
;;

let remove path =
  match Sys.remove path with
  | () -> ()
  | exception Sys_error _ -> ()
;;

let%expect_test "report terminal states from real Dune builds" =
  let project = create_project "progress" in
  Fun.protect
    ~finally:(fun () -> destroy_project project)
    (fun () ->
       let events = Lifecycle_events.create () in
       run project events ~f:(fun _client _workspace ->
         let* () = Signal.wait (Events.dune_ready events.dune) in
         let* () = wait_for_begin events "failed build" in
         Test.write_file project.gate "";
         let* () = wait_for_end events "failed build" "Build failed" in
         remove project.gate;
         Test.write_file project.expected "let answer = 42";
         let* () = wait_for_begin events "successful build" in
         Test.write_file project.gate "";
         wait_for_end events "successful build" "Build finished"));
  [%expect
    {|
    failed build create:
    { "token": "dune-build-0" }
    failed build begin:
    {
      "token": "dune-build-0",
      "value": { "kind": "begin", "message": "started", "title": "Build" }
    }
    failed build end:
    {
      "token": "dune-build-0",
      "value": { "kind": "end", "message": "Build failed" }
    }
    successful build create:
    { "token": "dune-build-1" }
    successful build begin:
    {
      "token": "dune-build-1",
      "value": { "kind": "begin", "message": "started", "title": "Build" }
    }
    successful build end:
    {
      "token": "dune-build-1",
      "value": { "kind": "end", "message": "Build finished" }
    }
    |}]
;;

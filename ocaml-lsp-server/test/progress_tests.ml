open Lsp.Types
module Dune_progress = Ocaml_lsp_server.Testing.Progress

let run fiber = Lev_fiber.run (fun () -> fiber) |> Lev_fiber.Error.ok_exn

let make ?window events =
  let capabilities = ClientCapabilities.create ?window () in
  Dune_progress.create
    capabilities
    ~create_task:(fun params ->
      events := ("create", WorkDoneProgressCreateParams.yojson_of_t params) :: !events;
      Fiber.return ())
    ~report_progress:(fun params ->
      events
      := ("report", ProgressParams.yojson_of_t Lsp.Progress.yojson_of_t params) :: !events;
      Fiber.return ())
;;

let print_events events =
  List.rev !events
  |> List.iter (fun (kind, json) ->
    Printf.printf "%s: %s\n" kind (Yojson.Safe.to_string json))
;;

let in_progress ~complete ~remaining =
  Dune_rpc.V1.Progress.In_progress { complete; remaining; failed = 0 }
;;

let%expect_test "report every Dune terminal progress state" =
  let events = ref [] in
  let window = WindowClientCapabilities.create ~workDoneProgress:true () in
  let progress = make ~window events in
  let update progress_update =
    run (Dune_progress.build_progress progress progress_update)
  in
  update (in_progress ~complete:0 ~remaining:4);
  update (in_progress ~complete:1 ~remaining:3);
  update Dune_rpc.V1.Progress.Waiting;
  update (in_progress ~complete:2 ~remaining:2);
  update Dune_rpc.V1.Progress.Interrupted;
  update (in_progress ~complete:3 ~remaining:1);
  update Dune_rpc.V1.Progress.Failed;
  update (in_progress ~complete:1 ~remaining:0);
  update Dune_rpc.V1.Progress.Success;
  update (in_progress ~complete:0 ~remaining:2);
  run (Dune_progress.end_build_if_running progress);
  print_events events;
  [%expect
    {|
    create: {"token":"dune-build-0"}
    report: {"token":"dune-build-0","value":{"kind":"begin","message":"started","title":"Build"}}
    report: {"token":"dune-build-0","value":{"kind":"report","message":"Building [0/4]","percentage":0}}
    report: {"token":"dune-build-0","value":{"kind":"report","message":"Building [1/4]","percentage":25}}
    report: {"token":"dune-build-0","value":{"kind":"end","message":"Waiting for changes"}}
    create: {"token":"dune-build-1"}
    report: {"token":"dune-build-1","value":{"kind":"begin","message":"started","title":"Build"}}
    report: {"token":"dune-build-1","value":{"kind":"report","message":"Building [2/4]","percentage":50}}
    report: {"token":"dune-build-1","value":{"kind":"end","message":"Build interrupted"}}
    create: {"token":"dune-build-2"}
    report: {"token":"dune-build-2","value":{"kind":"begin","message":"started","title":"Build"}}
    report: {"token":"dune-build-2","value":{"kind":"report","message":"Building [3/4]","percentage":75}}
    report: {"token":"dune-build-2","value":{"kind":"end","message":"Build failed"}}
    create: {"token":"dune-build-3"}
    report: {"token":"dune-build-3","value":{"kind":"begin","message":"started","title":"Build"}}
    report: {"token":"dune-build-3","value":{"kind":"report","message":"Building [1/1]","percentage":100}}
    report: {"token":"dune-build-3","value":{"kind":"end","message":"Build finished"}}
    create: {"token":"dune-build-4"}
    report: {"token":"dune-build-4","value":{"kind":"begin","message":"started","title":"Build"}}
    report: {"token":"dune-build-4","value":{"kind":"report","message":"Building [0/2]","percentage":0}}
    report: {"token":"dune-build-4","value":{"kind":"end","message":"Build interrupted"}}
    |}]
;;

open Test.Import
open Dune_rpc_test

let dune_file ~left_ready ~right_ready ~gate =
  Printf.sprintf
    {|
(rule
 (alias repro)
 (deps expected.ml left.ml barrier.sh)
 (target left.actual.ml)
 (action
  (progn
   (run %%{dep:barrier.sh} %S %S %S)
   (copy %%{dep:left.ml} %%{target})
   (diff expected.ml %%{target}))))

(rule
 (alias repro)
 (deps expected.ml right.ml barrier.sh)
 (target right.actual.ml)
 (action
  (progn
   (run %%{dep:barrier.sh} %S %S %S)
   (copy %%{dep:right.ml} %%{target})
   (diff expected.ml %%{target}))))
|}
    left_ready
    right_ready
    gate
    right_ready
    left_ready
    gate
;;

let normalize_error (params : LogMessageParams.t) =
  let message =
    if
      String.is_substring params.message ~substring:"Assert_failure"
      || String.is_substring params.message ~substring:"Assertion failed"
    then "promotion registration assertion failed"
    else params.message
  in
  { params with message }
;;

let%expect_test "duplicate shared-source promotions do not hang the server" =
  let project = create_project "shared-promotion" in
  stop_dune project;
  let left_ready = Filename.concat project.temp "left-ready" in
  let right_ready = Filename.concat project.temp "right-ready" in
  let barrier = Filename.concat project.root "barrier.sh" in
  (* Hold both actions until they are running and ocamllsp has subscribed to
     Dune, so both diagnostics reliably exercise the shared source. *)
  Test.write_file
    barrier
    "#!/bin/sh\n\
     set -eu\n\
     touch \"$1\"\n\
     while [ ! -e \"$2\" ]; do sleep 0.01; done\n\
     while [ ! -e \"$3\" ]; do sleep 0.01; done\n";
  Unix.chmod barrier 0o755;
  Test.write_file
    (Filename.concat project.root "dune")
    (dune_file ~left_ready ~right_ready ~gate:project.gate);
  Test.write_file (Filename.concat project.root "left.ml") "let answer = 1\n";
  Test.write_file (Filename.concat project.root "right.ml") "let answer = 2\n";
  let dune_pid = start_dune ~jobs:2 project.root project.runtime_dir in
  Fun.protect
    ~finally:(fun () ->
      stop_process dune_pid;
      destroy_project project)
    (fun () ->
       wait_for_rpc_registration project.runtime_dir dune_pid;
       let events = Lifecycle_events.create () in
       run ~trace:Verbose project events ~f:(fun client _workspace ->
         let* () = Signal.wait (Events.dune_ready events.dune) in
         let* () = Signal.wait (Events.dune_progress events.dune) in
         Test.write_file project.gate "";
         let* error = Mailbox.wait (Events.errors events.dune) in
         let* echo = Client.request client (DebugEcho { message = "still alive" }) in
         print_payloads
           project
           "Dune errors:"
           LogMessageParams.yojson_of_t
           [ normalize_error error ];
         Printf.printf "echo: %s\n" echo.message;
         Fiber.return ()));
  [%expect
    {|
    Dune errors:
    [ { "message": "promotion registration assertion failed", "type": 1 } ]
    echo: still alive
    |}]
;;

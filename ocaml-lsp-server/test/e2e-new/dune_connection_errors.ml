open Test.Import
open Dune_rpc_test

let mkdir path =
  match Unix.mkdir path 0o700 with
  | () -> ()
  | exception Unix.Unix_error (Unix.EEXIST, _, _) -> ()
;;

let register runtime_dir dune =
  let env variable =
    if String.equal variable "XDG_RUNTIME_DIR"
    then Some runtime_dir
    else Sys.getenv_opt variable
  in
  let config = Dune_rpc.Private.Registry.Config.create (Xdg.create ~env ()) in
  let watch_dir = Dune_rpc.Private.Registry.Config.watch_dir config in
  mkdir (Filename.dirname watch_dir);
  mkdir watch_dir;
  let (`Caller_should_write file) =
    Dune_rpc.Private.Registry.Config.register config dune
  in
  Test.write_file file.path file.contents
;;

let%expect_test "an unexpected Dune connection error is logged without stopping LSP" =
  let temp = Test.temp_dir "ocamllsp-dune-connect-error-" in
  let root = Filename.concat temp "workspace" in
  let runtime_dir = Filename.concat temp "runtime" in
  let blocked = Filename.concat temp "blocked" in
  List.iter [ root; runtime_dir; blocked ] ~f:mkdir;
  let socket = Filename.concat blocked "rpc.sock" in
  let dune =
    Dune_rpc.Private.Registry.Dune.create ~where:(`Unix socket) ~root ~pid:4242
  in
  register runtime_dir dune;
  Unix.chmod blocked 0o000;
  Fun.protect
    ~finally:(fun () ->
      Unix.chmod blocked 0o700;
      ignore (Sys.command ("rm -rf -- " ^ Filename.quote temp) : int))
    (fun () ->
       let logged = Fiber.Ivar.create () in
       let on_notification _ = function
         | Lsp.Server_notification.LogMessage { message; _ }
           when String.is_prefix message ~prefix:"unable to connect to dune at " ->
           let* current = Fiber.Ivar.peek logged in
           (match current with
            | Some _ -> Fiber.return ()
            | None -> Fiber.Ivar.fill logged ())
         | _ -> Fiber.return ()
       in
       let handler = Client.Handler.make ~on_notification () in
       let workspace =
         WorkspaceFolder.create ~uri:(Uri.of_path root) ~name:"connection"
       in
       Test.run_initialized
         ~extra_env:[ "OCAMLLSP_TEST=false"; "XDG_RUNTIME_DIR=" ^ runtime_dir ]
         ~timeout:10.0
         ~handler
         ~workspaceFolders:(Some [ workspace ])
       @@ fun client ->
       let* () = Fiber.Ivar.read logged in
       print_endline "unexpected connection error logged";
       let* echo = Client.request client (DebugEcho { message = "still alive" }) in
       Printf.printf "echo: %s\n" echo.message;
       let arguments =
         [ `Assoc
             [ "dune", `String root
             ; "in_source", `String (Filename.concat root "source.ml")
             ]
         ]
       in
       let command = ExecuteCommandParams.create ~command:"dune/promote" ~arguments () in
       let* promotion =
         Fiber.collect_errors (fun () -> Client.request client (ExecuteCommand command))
       in
       match promotion with
       | Error
           [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ]
         ->
         print_endline
           (String.substr_replace_all error.message ~pattern:temp ~with_:"<test-dir>");
         Test.shutdown_client client
       | Error errors -> Fiber.reraise_all errors
       | Ok _ -> failwith "promotion unexpectedly found a disconnected Dune instance");
  [%expect
    {|
    unexpected connection error logged
    echo: still alive
    dune "<test-dir>/workspace" already disconected
    |}]
;;

let%expect_test "a connected but uninitialized Dune rejects promotion commands" =
  let temp = Test.temp_dir "ocamllsp-dune-uninitialized-" in
  let root = Filename.concat temp "workspace" in
  let runtime_dir = Filename.concat temp "runtime" in
  List.iter [ root; runtime_dir ] ~f:mkdir;
  let socket_path = Filename.concat temp "fake-rpc.sock" in
  let listener = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind listener (Unix.ADDR_UNIX socket_path);
  Unix.listen listener 1;
  let fake_pid =
    match Unix.fork () with
    | 0 ->
      let connection, _ = Unix.accept listener in
      Unix.sleep 30;
      Unix.close connection;
      Unix._exit 0
    | pid -> pid
  in
  let dune =
    Dune_rpc.Private.Registry.Dune.create ~where:(`Unix socket_path) ~root ~pid:fake_pid
  in
  register runtime_dir dune;
  Fun.protect
    ~finally:(fun () ->
      (match Unix.kill fake_pid Sys.sigterm with
       | () -> ()
       | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ());
      ignore (Test.waitpid fake_pid : Unix.process_status);
      Unix.close listener;
      ignore (Sys.command ("rm -rf -- " ^ Filename.quote temp) : int))
    (fun () ->
       let connecting = Fiber.Ivar.create () in
       let on_notification _ = function
         | Lsp.Server_notification.LogMessage { message; _ }
           when String.is_suffix message ~suffix:": connecting..." ->
           let* current = Fiber.Ivar.peek connecting in
           (match current with
            | Some _ -> Fiber.return ()
            | None -> Fiber.Ivar.fill connecting ())
         | _ -> Fiber.return ()
       in
       let handler = Client.Handler.make ~on_notification () in
       let workspace =
         WorkspaceFolder.create ~uri:(Uri.of_path root) ~name:"connecting"
       in
       Test.run_initialized
         ~extra_env:[ "OCAMLLSP_TEST=false"; "XDG_RUNTIME_DIR=" ^ runtime_dir ]
         ~timeout:10.0
         ~handler
         ~workspaceFolders:(Some [ workspace ])
       @@ fun client ->
       let* () = Fiber.Ivar.read connecting in
       let arguments =
         [ `Assoc
             [ "dune", `String root
             ; "in_source", `String (Filename.concat root "source.ml")
             ]
         ]
       in
       let command = ExecuteCommandParams.create ~command:"dune/promote" ~arguments () in
       let* result =
         Fiber.collect_errors (fun () -> Client.request client (ExecuteCommand command))
       in
       match result with
       | Error
           [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ]
         ->
         print_endline
           (String.substr_replace_all error.message ~pattern:temp ~with_:"<test-dir>");
         Test.shutdown_client client
       | Error errors -> Fiber.reraise_all errors
       | Ok _ -> failwith "promotion unexpectedly used an uninitialized Dune client");
  [%expect {| dune "<test-dir>/workspace" is not initialized |}]
;;

let%expect_test "multiple Dune instances for one workspace are reported" =
  let project = create_project "multiple" in
  let build_dir = Filename.concat project.temp "second-build" in
  let second_dune = start_dune ~build_dir project.root project.runtime_dir in
  Fun.protect
    ~finally:(fun () ->
      stop_process second_dune;
      destroy_project project)
    (fun () ->
       let events = Lifecycle_events.create () in
       run project events ~f:(fun client _workspace ->
         let* () = Signal.wait (Events.multiple_instances events.dune) in
         print_endline "multiple-instance warning observed";
         let* echo = Client.request client (DebugEcho { message = "still alive" }) in
         Printf.printf "echo: %s\n" echo.message;
         Fiber.return ()));
  [%expect
    {|
    multiple-instance warning observed
    echo: still alive
    |}]
;;

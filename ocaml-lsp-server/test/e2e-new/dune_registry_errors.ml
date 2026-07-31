open Test.Import

let%expect_test "repeated Dune registry polling errors are logged once" =
  let temp = Test.temp_dir "ocamllsp-dune-registry-error-" in
  let root = Filename.concat temp "workspace" in
  let runtime_dir = Filename.concat temp "runtime" in
  Unix.mkdir root 0o700;
  Unix.mkdir runtime_dir 0o700;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command ("rm -rf -- " ^ Filename.quote temp) : int))
    (fun () ->
       let first_warning = Fiber.Ivar.create () in
       let warning_count = ref 0 in
       let on_notification _ = function
         | Lsp.Server_notification.LogMessage { message; _ }
           when String.is_prefix message ~prefix:"failed to poll dune registry." ->
           incr warning_count;
           let* current = Fiber.Ivar.peek first_warning in
           (match current with
            | Some _ -> Fiber.return ()
            | None -> Fiber.Ivar.fill first_warning ())
         | _ -> Fiber.return ()
       in
       let handler = Client.Handler.make ~on_notification () in
       let workspace = WorkspaceFolder.create ~uri:(Uri.of_path root) ~name:"registry" in
       Test.run_initialized
         ~extra_env:[ "OCAMLLSP_TEST=false"; "XDG_RUNTIME_DIR=" ^ runtime_dir ]
         ~timeout:10.0
         ~handler
         ~workspaceFolders:(Some [ workspace ])
       @@ fun client ->
       let* () = Fiber.Ivar.read first_warning in
       let* () = Lev_fiber.Timer.sleepf 0.65 in
       print_endline "registry warning observed";
       Printf.printf "warning count: %d\n" !warning_count;
       let* echo = Client.request client (DebugEcho { message = "still alive" }) in
       Printf.printf "echo: %s\n" echo.message;
       Test.shutdown_client client);
  [%expect
    {|
    registry warning observed
    warning count: 1
    echo: still alive
    |}]
;;

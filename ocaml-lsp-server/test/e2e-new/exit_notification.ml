open Test.Import

let client_capabilities = ClientCapabilities.create ()

(* Mostly a copy of Test.run, but without optional args and with some extra
   printing to track how `ocamllsp` executable exited *)
module T : sig
  val run : (unit Client.t -> 'a Fiber.t) -> 'a
end = struct
  let _PATH =
    Bin.parse_path (Option.value ~default:"" @@ Env.get Env.initial "PATH")

  let bin =
    Bin.which "ocamllsp" ~path:_PATH |> Option.value_exn |> Path.to_string

  let run f =
    let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
    let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
    let pid =
      let env =
        let current = Unix.environment () in
        Array.to_list current |> Spawn.Env.of_list
      in
      Spawn.spawn
        ~env
        ~prog:bin
        ~argv:[ bin ]
        ~stdin:stdin_i
        ~stdout:stdout_o
        ()
    in
    Unix.close stdin_i;
    Unix.close stdout_o;
    let handler = Client.Handler.make () in
    let init =
      let blockity =
        if Sys.win32 then `Blocking
        else (
          Unix.set_nonblock stdout_i;
          Unix.set_nonblock stdin_o;
          `Non_blocking true)
      in
      let make fd what =
        let fd = Lev_fiber.Fd.create fd blockity in
        Lev_fiber.Io.create fd what
      in
      let* in_ = make stdout_i Input in
      let* out = make stdin_o Output in
      let io = Lsp_fiber.Fiber_io.make in_ out in
      let client = Client.make handler io () in
      f client
    in
    (* TODO replace the wheel once we can cancel sleep *)
    let waitpid wheel =
      let* timeout = Lev_fiber.Timer.Wheel.task wheel in
      Fiber.finalize ~finally:(fun () -> Lev_fiber.Timer.Wheel.stop wheel)
      @@ fun () ->
      let cancelled = ref false in
      Fiber.fork_and_join_unit
        (fun () ->
          let+ timeout = Lev_fiber.Timer.Wheel.await timeout in
          match timeout with
          | `Ok ->
            Unix.kill pid Sys.sigkill;
            Printf.printf "ocamllsp process timed out\n";
            cancelled := true
          | `Cancelled ->
            Printf.printf "ocamllsp process exited\n";
            ())
        (fun () ->
          let* status = Lev_fiber.waitpid ~pid in
          (match status with
          | WEXITED n -> Format.eprintf "ocamllsp finished with code = %d@.%!" n
          | WSIGNALED s ->
            Format.eprintf "ocamllsp killed with signal = %d@.%!" s
          | WSTOPPED s ->
            Format.eprintf "ocamllsp stopped with signal = %d@.%!" s);
          if !cancelled then Fiber.return ()
          else Lev_fiber.Timer.Wheel.cancel timeout)
    in
    Lev_fiber.run (fun () ->
        let* wheel = Lev_fiber.Timer.Wheel.create ~delay:1.0 in
        let+ res = init
        and+ () =
          Fiber.all_concurrently_unit
            [ waitpid wheel; Lev_fiber.Timer.Wheel.run wheel ]
        in
        res)
    |> Lev_fiber.Error.ok_exn
end

let test run =
  T.run (fun client ->
      let run_client () =
        Client.start
          client
          (InitializeParams.create ~capabilities:client_capabilities ())
      in
      Fiber.fork_and_join_unit run_client (run client))

let%expect_test "ocamllsp process exits with code 0 after Shutdown and Exit \
                 notifications are sent" =
  let run client () =
    let* (_ : InitializeResult.t) = Client.initialized client in
    let* () = Client.request client Shutdown in
    Client.notification client Exit
  in
  test run;
  [%expect
    {|
    ocamllsp finished with code = 0
    ocamllsp process exited  |}]

let%expect_test "ocamllsp does not exit if only Shutdown notification is sent" =
  let run client () =
    let* (_ : InitializeResult.t) = Client.initialized client in
    Client.request client Shutdown
  in
  test run;
  [%expect
    {|
    ocamllsp killed with signal = -7
    ocamllsp process timed out  |}]

let%expect_test "ocamllsp process exits with code 0 after Exit notification is \
                 sent (should be 1)" =
  let run client () =
    let* (_ : InitializeResult.t) = Client.initialized client in
    Client.notification client Exit
  in
  test run;
  [%expect
    {|
    ocamllsp finished with code = 0
    ocamllsp process exited  |}]

open Test.Import

let client_capabilities = ClientCapabilities.create ()

let print_status (status : Unix.process_status) =
  match status with
  | WEXITED n -> Format.eprintf "ocamllsp finished with code = %d@.%!" n
  | WSIGNALED s -> Format.eprintf "ocamllsp killed with signal = %d@.%!" s
  | WSTOPPED s -> Format.eprintf "ocamllsp stopped with signal = %d@.%!" s
;;

module T : sig
  val run : ?on_spawn:(int -> unit) -> (unit Client.t -> 'a Fiber.t) -> 'a
end = struct
  let run ?on_spawn f =
    let status, a = Test.run_with_status ?on_spawn f in
    print_status status;
    a
  ;;
end

let send_exit_before_initialize () =
  let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
  let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
  let env = Unix.environment () |> Array.to_list |> Spawn.Env.of_list in
  let pid =
    Spawn.spawn ~env ~prog:Test.bin ~argv:[ Test.bin ] ~stdin:stdin_i ~stdout:stdout_o ()
  in
  Unix.close stdin_i;
  Unix.close stdout_o;
  let json = {|{"jsonrpc":"2.0","method":"exit"}|} in
  let packet = sprintf "Content-Length: %d\r\n\r\n%s" (String.length json) json in
  let oc = Unix.out_channel_of_descr stdin_o in
  output_string oc packet;
  close_out oc;
  let _, status = Unix.waitpid [] pid in
  Unix.close stdout_i;
  status
;;

let test ?on_spawn run =
  T.run ?on_spawn (fun client ->
    let run_client () = Test.start_client ~capabilities:client_capabilities client in
    Fiber.fork_and_join_unit run_client (run client))
;;

let%expect_test "ocamllsp process exits with code 0 after Exit before Initialize" =
  send_exit_before_initialize () |> print_status;
  [%expect
    {|
    ocamllsp finished with code = 0  |}]
;;

let%expect_test
    "ocamllsp process exits with code 0 after Shutdown and Exit notifications are sent"
  =
  let run client () =
    let* (_ : InitializeResult.t) = Client.initialized client in
    let* () = Client.request client Shutdown in
    Client.notification client Exit
  in
  test run;
  [%expect
    {|
    ocamllsp finished with code = 0  |}]
;;

let%expect_test "ocamllsp does not exit if only Shutdown notification is sent" =
  let pid = ref None in
  let run client () =
    let* (_ : InitializeResult.t) = Client.initialized client in
    let+ () = Client.request client Shutdown in
    Unix.kill (Option.value_exn !pid) Sys.sigterm
  in
  test ~on_spawn:(fun process -> pid := Some process) run;
  [%expect {| ocamllsp killed with signal = -11 |}]
;;

let%expect_test
    "ocamllsp process exits with code 0 after Exit notification is sent (should be 1)"
  =
  let run client () =
    let* (_ : InitializeResult.t) = Client.initialized client in
    Client.notification client Exit
  in
  test run;
  [%expect
    {|
    ocamllsp finished with code = 0  |}]
;;

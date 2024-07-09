open Test.Import

let client_capabilities = ClientCapabilities.create ()

module T : sig
  val run : (unit Client.t -> 'a Fiber.t) -> 'a
end = struct
  let run f =
    let status, a = Test.run_with_status f in
    let () =
      match status with
      | WEXITED n -> Format.eprintf "ocamllsp finished with code = %d@.%!" n
      | WSIGNALED s -> Format.eprintf "ocamllsp killed with signal = %d@.%!" s
      | WSTOPPED s -> Format.eprintf "ocamllsp stopped with signal = %d@.%!" s
    in
    a
  ;;
end

let test run =
  T.run (fun client ->
    let run_client () =
      Client.start client (InitializeParams.create ~capabilities:client_capabilities ())
    in
    Fiber.fork_and_join_unit run_client (run client))
;;

let%expect_test "ocamllsp process exits with code 0 after Shutdown and Exit \
                 notifications are sent"
  =
  let run client () =
    let* (_ : InitializeResult.t) = Client.initialized client in
    let* () = Client.request client Shutdown in
    Client.notification client Exit
  in
  test run;
  [%expect {|
    ocamllsp finished with code = 0  |}]
;;

let%expect_test "ocamllsp does not exit if only Shutdown notification is sent" =
  let run client () =
    let* (_ : InitializeResult.t) = Client.initialized client in
    Client.request client Shutdown
  in
  test run;
  [%expect {|
    ocamllsp killed with signal = -7  |}]
;;

let%expect_test "ocamllsp process exits with code 0 after Exit notification is sent \
                 (should be 1)"
  =
  let run client () =
    let* (_ : InitializeResult.t) = Client.initialized client in
    Client.notification client Exit
  in
  test run;
  [%expect {|
    ocamllsp finished with code = 0  |}]
;;

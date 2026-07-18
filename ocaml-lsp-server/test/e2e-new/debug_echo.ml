open Test.Import

let%expect_test "debug/echo" =
  (Test.run
   @@ fun client ->
   let run_client () = Test.start_client client in
   let run =
     let* (_ : InitializeResult.t) = Client.initialized client in
     let* response = Client.request client (DebugEcho { message = "testing" }) in
     print_endline response.message;
     Client.request client Shutdown
   in
   Fiber.fork_and_join_unit run_client (fun () -> run >>> Client.stop client));
  [%expect {| testing |}]
;;

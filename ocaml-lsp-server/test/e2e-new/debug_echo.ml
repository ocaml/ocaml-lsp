open Test.Import

let%expect_test "debug/echo" =
  (Test.run_initialized
   @@ fun client ->
   let* response = Client.request client (DebugEcho { message = "testing" }) in
   print_endline response.message;
   Test.shutdown_client client);
  [%expect {| testing |}]
;;

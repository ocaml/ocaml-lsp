open Test.Import

let%expect_test "unsupported standard requests are reported as unavailable" =
  (Test.run_initialized
   @@ fun client ->
   let textDocument =
     TextDocumentIdentifier.create ~uri:(DocumentUri.of_path "unsupported.ml")
   in
   let position = Position.create ~line:0 ~character:0 in
   let params = LinkedEditingRangeParams.create ~textDocument ~position () in
   let* response =
     Fiber.collect_errors (fun () -> Client.request client (LinkedEditingRange params))
   in
   let* () =
     match response with
     | Error
         [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ] ->
       Jsonrpc.Response.Error.yojson_of_t error |> Test.print_result;
       Fiber.return ()
     | Error errors -> Fiber.reraise_all errors
     | Ok _ -> failwith "unsupported request unexpectedly succeeded"
   in
   Test.shutdown_client client);
  [%expect
    {|
    { "code": -32601, "message": "Request not supported yet!" }
    |}]
;;

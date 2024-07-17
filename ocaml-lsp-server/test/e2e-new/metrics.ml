open Test.Import

let%expect_test "metrics" =
  let handler =
    let on_request (type r) _ (r : r Lsp.Server_request.t)
      : (r Lsp_fiber.Rpc.Reply.t * unit) Fiber.t
      =
      match r with
      | ShowDocumentRequest p ->
        print_endline "client: received show document params";
        let json =
          ShowDocumentParams.yojson_of_t { p with uri = Uri.of_path "<redacted>" }
        in
        Yojson.Safe.to_channel stdout json;
        print_endline "";
        print_endline "metrics contents:";
        print_endline (Stdune.Io.String_path.read_file (Uri.to_path p.uri));
        let res = ShowDocumentResult.create ~success:true in
        Fiber.return (Lsp_fiber.Rpc.Reply.now res, ())
      | _ -> assert false
    in
    let on_request = { Client.Handler.on_request } in
    let on_notification (_ : _ Client.t) (_ : Client.in_notification) =
      (* ignore notifications *)
      Fiber.return ()
    in
    Client.Handler.make ~on_request ~on_notification ()
  in
  (Test.run ~handler
   @@ fun client ->
   let run_client () =
     let capabilities = ClientCapabilities.create () in
     Client.start client (InitializeParams.create ~capabilities ())
   in
   let run =
     let* (_ : InitializeResult.t) = Client.initialized client in
     let view_metrics = ExecuteCommandParams.create ~command:"ocamllsp/view-metrics" () in
     let+ res = Client.request client (ExecuteCommand view_metrics) in
     print_endline "server: receiving response";
     Yojson.Safe.to_channel stdout res;
     print_endline ""
   in
   Fiber.fork_and_join_unit run_client (fun () -> run >>> Client.stop client));
  [%expect
    {|
      client: received show document params
      {"takeFocus":true,"uri":"file:///%3Credacted%3E"}
      metrics contents:
      {"traceEvents":[]}
      server: receiving response
      null |}]
;;

open Test.Import

let%expect_test "show the merlin config for open documents" =
  let diagnostics = Fiber.Mvar.create () in
  let handler =
    let on_request (type r) _ (request : r Lsp.Server_request.t)
      : (r Lsp_fiber.Rpc.Reply.t * unit) Fiber.t
      =
      match request with
      | ShowDocumentRequest params ->
        let path = Uri.to_path params.uri in
        let json =
          ShowDocumentParams.yojson_of_t
            { params with uri = Uri.of_path "<redacted>.json" }
        in
        Test.print_result json;
        let contents = Fs_io.read_file path |> Result.ok_exn in
        (* The dumped config embeds filesystem paths; keep only the top-level
           shape stable by printing the first keys of the JSON. *)
        let json = Yojson.Safe.from_string contents in
        (match json with
         | `Assoc fields ->
           List.iter fields ~f:(fun (key, config) ->
             print_endline ("document: " ^ key);
             match config with
             | `Assoc config_fields ->
               List.iter config_fields ~f:(fun (config_key, _) ->
                 print_endline ("config key: " ^ config_key))
             | config -> Test.print_result config)
         | json -> Test.print_result json);
        Sys.remove path;
        let response = ShowDocumentResult.create ~success:true in
        Fiber.return (Lsp_fiber.Rpc.Reply.now response, ())
      | _ -> assert false
    in
    Client.Handler.make
      ~on_request:{ Client.Handler.on_request }
      ~on_notification:(fun _ -> function
         | PublishDiagnostics params -> Fiber.Mvar.write diagnostics params
         | _ -> Fiber.return ())
      ()
  in
  let capabilities =
    let showDocument = ShowDocumentClientCapabilities.create ~support:true in
    let window = WindowClientCapabilities.create ~showDocument () in
    ClientCapabilities.create ~window ()
  in
  (Test.run_initialized ~handler ~capabilities
   @@ fun client ->
   let uri = DocumentUri.of_path "merlin-config.ml" in
   let* () = Test.open_document ~client ~uri ~source:"let answer = 0\n" () in
   let* (_ : PublishDiagnosticsParams.t) = Fiber.Mvar.read diagnostics in
   let params =
     ExecuteCommandParams.create ~command:"ocamllsp/show-merlin-config" ~arguments:[] ()
   in
   let* response = Client.request client (ExecuteCommand params) in
   Test.print_result response;
   Client.stop client);
  [%expect
    {|
    { "takeFocus": true, "uri": "file:///%3Credacted%3E.json" }
    document: file:///merlin-config.ml
    config key: ocaml
    config key: merlin
    config key: query
    null
    |}]
;;

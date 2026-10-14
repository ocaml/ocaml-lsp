open Test.Import

let client_capabilities = ClientCapabilities.create ()
let uri = DocumentUri.of_path "test.ml"

let test
      ?extra_env
      ?(capabilities = client_capabilities)
      ?(uri = uri)
      ?wire_uri
      ?(language_id = "ocaml")
      text
      req
  =
  let on_notification, diagnostics = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  Test.run_initialized ~handler ~capabilities ?extra_env (fun client ->
    let* () =
      match wire_uri with
      | None -> Test.open_document ~language_id ~client ~uri ~source:text ()
      | Some uri -> Test.open_document_raw ~language_id ~client ~uri ~source:text ()
    in
    let* () = req client in
    let* () = Client.request client Shutdown in
    let* () = Fiber.Ivar.read diagnostics in
    Client.notification client Exit)
;;

open Test.Import
open Lsp_helpers

let kind = CodeActionKind.Other "open-dune"

let capabilities =
  let window =
    let showDocument = ShowDocumentClientCapabilities.create ~support:true in
    WindowClientCapabilities.create ~showDocument ()
  in
  let textDocument =
    let codeAction =
      let codeActionLiteralSupport =
        let codeActionKind = ClientCodeActionKindOptions.create ~valueSet:[] in
        ClientCodeActionLiteralOptions.create ~codeActionKind
      in
      CodeActionClientCapabilities.create ~codeActionLiteralSupport ()
    in
    TextDocumentClientCapabilities.create ~codeAction ()
  in
  ClientCapabilities.create ~window ~textDocument ()
;;

let handler opened =
  let on_request (type r) _ (request : r Lsp.Server_request.t)
    : (r Lsp_fiber.Rpc.Reply.t * unit) Fiber.t
    =
    match request with
    | ShowDocumentRequest params ->
      let* () = Fiber.Ivar.fill opened params.uri in
      let result = ShowDocumentResult.create ~success:true in
      Fiber.return (Lsp_fiber.Rpc.Reply.now result, ())
    | _ -> assert false
  in
  Client.Handler.make
    ~on_request:{ Client.Handler.on_request }
    ~on_notification:(fun _ _ -> Fiber.return ())
    ()
;;

let request_actions client path =
  let uri = DocumentUri.of_path path in
  let* () = open_document ~language_id:"ocaml" ~client ~uri ~source:"let x = 1\n" in
  let params =
    let range =
      let position = Position.create ~line:0 ~character:0 in
      Range.create ~start:position ~end_:position
    in
    let context = CodeActionContext.create ~diagnostics:[] ~only:[ kind ] () in
    let textDocument = TextDocumentIdentifier.create ~uri in
    CodeActionParams.create ~textDocument ~range ~context ()
  in
  Client.request client (CodeAction params)
;;

let find_open_dune_action actions =
  Option.value actions ~default:[]
  |> List.find_map ~f:(function
    | `CodeAction ({ CodeAction.kind = Some action_kind; _ } as action)
      when Poly.equal kind action_kind -> Some action
    | `CodeAction _ | `Command _ -> None)
;;

let%expect_test "open the closest dune file without crossing a project boundary" =
  let root = Test.temp_dir "ocamllsp-open-dune" in
  let lib = Filename.concat root "lib" in
  let deep = Filename.concat lib "deep" in
  let nested_project = Filename.concat root "nested-project" in
  let nested_src = Filename.concat nested_project "src" in
  List.iter [ lib; deep; nested_project; nested_src ] ~f:(fun dir -> Unix.mkdir dir 0o700);
  Test.write_file (Filename.concat root "dune-project") "(lang dune 2.5)\n";
  Test.write_file (Filename.concat root "dune") "";
  let closest_dune = Filename.concat lib "dune" in
  Test.write_file closest_dune "";
  Test.write_file (Filename.concat nested_project "dune-project") "(lang dune 2.5)\n";
  let source = Filename.concat deep "source.ml" in
  let opened = Fiber.Ivar.create () in
  let stderr = Unix.openfile Test.null_device [ O_WRONLY ] 0 in
  (let handler = handler opened in
   let workspace = WorkspaceFolder.create ~uri:(Uri.of_path root) ~name:"test" in
   Test.run_initialized
     ~stderr
     ~handler
     ~capabilities
     ~workspaceFolders:(Some [ workspace ])
   @@ fun client ->
   let* actions = request_actions client source in
   let action = find_open_dune_action actions in
   let* () =
     match action with
     | None ->
       print_endline "closest action: missing";
       Fiber.return ()
     | Some { title; command = None; _ } ->
       Printf.printf "%s: missing command\n" title;
       Fiber.return ()
     | Some { title; command = Some command; _ } ->
       let params =
         let arguments = command.arguments in
         ExecuteCommandParams.create ~command:command.command ?arguments ()
       in
       let* _ = Client.request client (ExecuteCommand params) in
       let+ opened = Fiber.Ivar.read opened in
       Printf.printf "%s: %b\n" title (String.equal (Uri.to_path opened) closest_dune)
   in
   let* actions =
     let nested_source = Filename.concat nested_src "source.ml" in
     request_actions client nested_source
   in
   Printf.printf
     "nested project action: %s\n"
     (match find_open_dune_action actions with
      | None -> "none"
      | Some _ -> "unexpected");
   Test.exit_client client);
  Unix.close stderr;
  [%expect
    {|
    Open dune file: true
    nested project action: none
    |}]
;;

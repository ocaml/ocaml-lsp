open Test.Import
open Lsp_helpers

let code_action_capabilities resolveSupport =
  let codeActionLiteralSupport =
    let codeActionKind = ClientCodeActionKindOptions.create ~valueSet:[] in
    ClientCodeActionLiteralOptions.create ~codeActionKind
  in
  let codeAction =
    CodeActionClientCapabilities.create
      ~codeActionLiteralSupport
      ~dataSupport:true
      ~resolveSupport
      ()
  in
  let textDocument = TextDocumentClientCapabilities.create ~codeAction () in
  ClientCapabilities.create ~textDocument ()
;;

let print_resolve_state prefix (action : CodeAction.t) =
  Printf.printf
    "%s: edit=%b data=%b\n"
    prefix
    (Option.is_some action.edit)
    (Option.is_some action.data)
;;

let%expect_test "inline edit is computed eagerly despite resolve support" =
  let resolveSupport = ClientCodeActionResolveOptions.create ~properties:[ "edit" ] in
  let capabilities = code_action_capabilities resolveSupport in
  let handler = Client.Handler.make ~on_notification:(fun _ _ -> Fiber.return ()) () in
  (Test.run_initialized ~handler ~capabilities
   @@ fun client ->
   let run () =
     let* initialized = Client.initialized client in
     let resolveProvider =
       match initialized.capabilities.codeActionProvider with
       | Some (`CodeActionOptions { resolveProvider = Some true; _ }) -> true
       | None | Some (`Bool _) | Some (`CodeActionOptions _) -> false
     in
     Printf.printf "resolve provider: %b\n" resolveProvider;
     let uri = DocumentUri.of_path "resolve.ml" in
     let source =
       {ocaml|let _ =
  let x = 0 in
  x + 1
|ocaml}
     in
     let* () = open_document ~language_id:"ocaml" ~client ~uri ~source in
     let range =
       Code_actions.range ~start_line:1 ~start_character:6 ~end_line:1 ~end_character:7
     in
     let textDocument = TextDocumentIdentifier.create ~uri in
     let context =
       CodeActionContext.create ~diagnostics:[] ~only:[ CodeActionKind.RefactorInline ] ()
     in
     let params = CodeActionParams.create ~textDocument ~range ~context () in
     let* response = Client.request client (CodeAction params) in
     let action =
       response
       |> Option.value_exn
       |> List.find_map ~f:(function
         | `CodeAction ({ CodeAction.title = "Inline into uses"; _ } as action) ->
           Some action
         | `CodeAction _ | `Command _ -> None)
       |> Option.value_exn
     in
     print_resolve_state "initial response" action;
     Test.exit_client client
   in
   run ());
  [%expect
    {|
    resolve provider: false
    initial response: edit=true data=false |}]
;;

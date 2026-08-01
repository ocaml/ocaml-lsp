open Test.Import
open Dune_rpc_test

let request_promotion client uri =
  let position = Position.create ~line:0 ~character:0 in
  let range = Range.create ~start:position ~end_:position in
  let textDocument = TextDocumentIdentifier.create ~uri in
  let context =
    CodeActionContext.create ~diagnostics:[] ~only:[ CodeActionKind.QuickFix ] ()
  in
  let params = CodeActionParams.create ~textDocument ~range ~context () in
  let* actions = Client.request client (CodeAction params) in
  match Option.value actions ~default:[] with
  | [ `CodeAction action ] -> Fiber.return action
  | actions ->
    failwith
      (Printf.sprintf "expected exactly one Dune promotion, got %d" (List.length actions))
;;

let execute client ?arguments command =
  let params = ExecuteCommandParams.create ~command ?arguments () in
  Client.request client (ExecuteCommand params)
;;

let print_command_error label = function
  | Error [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ]
    ->
    Printf.printf
      "%s (%s): %s\n"
      label
      (Jsonrpc.Response.Error.Code.to_string error.code)
      error.message;
    Fiber.return ()
  | Error errors -> Fiber.reraise_all errors
  | Ok _ ->
    Printf.printf "%s unexpectedly succeeded\n" label;
    Fiber.return ()
;;

let%expect_test "request and execute a Dune promotion" =
  let project = create_project "promote" in
  Fun.protect
    ~finally:(fun () -> destroy_project project)
    (fun () ->
       let events = Lifecycle_events.create () in
       run project events ~f:(fun client _workspace ->
         let* () = Signal.wait (Events.dune_ready events.dune) in
         Test.write_file project.gate "";
         let* (_ : PublishDiagnosticsParams.t) =
           Events.wait_for_diagnostics events.dune ~f:has_dune_diagnostic
         in
         let* (_ : RegistrationParams.t) = Mailbox.wait events.registrations in
         let uri = Uri.of_path project.expected in
         let* action = request_promotion client uri in
         print_payload project "promotion code action:" (CodeAction.yojson_of_t action);
         let command = Option.value_exn action.command in
         let* result = execute client ?arguments:command.arguments command.command in
         if not (Poly.equal result `Null) then failwith "unexpected command result";
         let contents = Fs_io.read_file project.expected |> Result.ok_exn in
         Printf.printf "promoted contents: %S\n" contents;
         let missing_arguments =
           [ `Assoc
               [ "dune", `String project.root
               ; "in_source", `String (Filename.concat project.root "missing.ml")
               ]
           ]
         in
         let* result = execute client ~arguments:missing_arguments "dune/promote" in
         if not (Poly.equal result `Null)
         then failwith "unexpected failed-promotion result";
         print_endline "missing promotion request: ignored RPC error";
         let invalid_arguments label ?arguments () =
           let* result =
             Fiber.collect_errors (fun () -> execute client ?arguments "dune/promote")
           in
           print_command_error label result
         in
         let* () = invalid_arguments "missing arguments" () in
         let* () = invalid_arguments "extra arguments" ~arguments:[ `Null; `Null ] () in
         let* () = invalid_arguments "null argument" ~arguments:[ `Null ] () in
         let* () =
           invalid_arguments
             "incomplete argument"
             ~arguments:[ `Assoc [ "dune", `String project.root ] ]
             ()
         in
         let* invalid =
           Fiber.collect_errors (fun () -> execute client "dune/not-a-command")
         in
         print_command_error "invalid command" invalid));
  [%expect
    {|
    promotion code action:
    {
      "command": {
        "arguments": [
          { "dune": "<workspace-path>", "in_source": "<document-path>" }
        ],
        "command": "dune/promote",
        "title": "Promote"
      },
      "kind": "quickfix",
      "title": "Promote"
    }
    promoted contents: "let answer = 42"
    missing promotion request: ignored RPC error
    missing arguments (InvalidParams): invalid Dune promotion arguments
    extra arguments (InvalidParams): invalid Dune promotion arguments
    null argument (InvalidParams): invalid Dune promotion arguments
    incomplete argument (InvalidParams): invalid Dune promotion arguments
    invalid command (InvalidRequest): invalid command
    |}]
;;

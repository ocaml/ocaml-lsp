open Test.Import

module Signal = struct
  type t =
    { mutable pending : int
    ; mutable waiter : unit Fiber.Ivar.t option
    }

  let create () = { pending = 0; waiter = None }

  let notify t =
    match t.waiter with
    | None ->
      t.pending <- t.pending + 1;
      Fiber.return ()
    | Some waiter ->
      t.waiter <- None;
      Fiber.Ivar.fill waiter ()
  ;;

  let wait t =
    if t.pending > 0
    then (
      t.pending <- t.pending - 1;
      Fiber.return ())
    else (
      assert (Option.is_none t.waiter);
      let waiter = Fiber.Ivar.create () in
      t.waiter <- Some waiter;
      Fiber.Ivar.read waiter)
  ;;
end

module Events = struct
  type diagnostic_waiter =
    (PublishDiagnosticsParams.t -> bool) * PublishDiagnosticsParams.t Fiber.Ivar.t

  type t =
    { dune_connected : Signal.t
    ; dune_connection_traced : Signal.t
    ; build_finished : Signal.t
    ; build_finished_traced : Signal.t
    ; merlin_config_started : Signal.t
    ; merlin_config_request : Signal.t
    ; merlin_config_response : Signal.t
    ; merlin_config_stopping : Signal.t
    ; merlin_config_exited : Signal.t
    ; mutable diagnostics : PublishDiagnosticsParams.t list
    ; mutable diagnostic_waiter : diagnostic_waiter option
    }

  let create () =
    { dune_connected = Signal.create ()
    ; dune_connection_traced = Signal.create ()
    ; build_finished = Signal.create ()
    ; build_finished_traced = Signal.create ()
    ; merlin_config_started = Signal.create ()
    ; merlin_config_request = Signal.create ()
    ; merlin_config_response = Signal.create ()
    ; merlin_config_stopping = Signal.create ()
    ; merlin_config_exited = Signal.create ()
    ; diagnostics = []
    ; diagnostic_waiter = None
    }
  ;;

  let rec take_matching ~f rev_prefix = function
    | [] -> None
    | diagnostic :: rest ->
      if f diagnostic
      then Some (diagnostic, List.rev_append rev_prefix rest)
      else take_matching ~f (diagnostic :: rev_prefix) rest
  ;;

  let wait_for_diagnostics t ~f =
    match take_matching ~f [] t.diagnostics with
    | Some (diagnostic, diagnostics) ->
      t.diagnostics <- diagnostics;
      Fiber.return diagnostic
    | None ->
      assert (Option.is_none t.diagnostic_waiter);
      let waiter = Fiber.Ivar.create () in
      t.diagnostic_waiter <- Some (f, waiter);
      Fiber.Ivar.read waiter
  ;;

  let publish_diagnostics t diagnostic =
    match t.diagnostic_waiter with
    | Some (f, waiter) when f diagnostic ->
      t.diagnostic_waiter <- None;
      Fiber.Ivar.fill waiter diagnostic
    | None | Some _ ->
      t.diagnostics <- diagnostic :: t.diagnostics;
      Fiber.return ()
  ;;

  let on_notification t _ (notification : Lsp.Server_notification.t) =
    match notification with
    | PublishDiagnostics diagnostic -> publish_diagnostics t diagnostic
    | LogMessage { message; _ }
      when Stdlib.String.starts_with ~prefix:"Connected to dune " message ->
      Signal.notify t.dune_connected
    | LogTrace { message = "Connected to Dune RPC"; verbose = None } ->
      Signal.notify t.dune_connection_traced
    | LogTrace { message = "Connected to Dune RPC"; verbose = Some _ } ->
      failwith "messages-level Dune connection trace included verbose details"
    | LogTrace { message = "Dune build finished"; verbose = Some verbose }
      when Stdlib.String.starts_with ~prefix:"Dune root: " verbose ->
      Signal.notify t.build_finished_traced
    | LogTrace { message = "Dune build finished"; verbose = None } ->
      failwith "verbose Dune build trace omitted details"
    | LogTrace { message = "Merlin configuration process started"; verbose = Some _ } ->
      Signal.notify t.merlin_config_started
    | LogTrace { message = "Merlin configuration request sent"; verbose = Some _ } ->
      Signal.notify t.merlin_config_request
    | LogTrace { message = "Merlin configuration response received"; verbose = Some _ } ->
      Signal.notify t.merlin_config_response
    | LogTrace { message = "Stopping Merlin configuration process"; verbose = Some _ } ->
      Signal.notify t.merlin_config_stopping
    | LogTrace { message = "Merlin configuration process exited"; verbose = Some _ } ->
      Signal.notify t.merlin_config_exited
    | LogTrace
        { message =
            ( "Merlin configuration process started"
            | "Merlin configuration request sent"
            | "Merlin configuration response received"
            | "Stopping Merlin configuration process"
            | "Merlin configuration process exited" )
        ; verbose = None
        } -> failwith "verbose Merlin configuration process trace omitted details"
    | WorkDoneProgress { token = `String token; value = Lsp.Progress.End _ }
      when Stdlib.String.starts_with ~prefix:"dune-build-" token ->
      Signal.notify t.build_finished
    | _ -> Fiber.return ()
  ;;
end

let open_document client ~uri ~text =
  let textDocument =
    TextDocumentItem.create ~uri ~languageId:(LanguageKind.Other "ocaml") ~version:0 ~text
  in
  Client.notification
    client
    (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
;;

let close_document client ~uri =
  let textDocument = TextDocumentIdentifier.create ~uri in
  Client.notification
    client
    (TextDocumentDidClose (DidCloseTextDocumentParams.create ~textDocument))
;;

let change_document client ~uri ~text =
  let textDocument = VersionedTextDocumentIdentifier.create ~uri ~version:1 in
  let contentChanges =
    [ `TextDocumentContentChangeWholeDocument
        (TextDocumentContentChangeWholeDocument.create ~text)
    ]
  in
  Client.notification
    client
    (TextDocumentDidChange
       (DidChangeTextDocumentParams.create ~textDocument ~contentChanges))
;;

let diagnostic_message (diagnostic : Diagnostic.t) =
  match diagnostic.message with
  | `String message -> message
  | `MarkupContent { value; _ } -> value
;;

let has_unbound_bar (params : PublishDiagnosticsParams.t) =
  List.exists params.diagnostics ~f:(fun diagnostic ->
    String.equal (diagnostic_message diagnostic) "Unbound module Bar")
;;

let for_uri uri (params : PublishDiagnosticsParams.t) = Uri.equal uri params.uri

let start_dune root runtime_dir =
  let prog = Bin.which "dune" |> Option.value_exn in
  let output = Unix.openfile Test.null_device [ Unix.O_WRONLY ] 0o666 in
  let env =
    let is_runtime_dir value =
      Stdlib.String.starts_with ~prefix:"XDG_RUNTIME_DIR=" value
    in
    Unix.environment ()
    |> Array.to_list
    |> List.filter ~f:(fun value -> not (is_runtime_dir value))
    |> List.cons ("XDG_RUNTIME_DIR=" ^ runtime_dir)
    |> Spawn.Env.of_list
  in
  let pid =
    Spawn.spawn
      ~env
      ~cwd:(Path root)
      ~prog
      ~argv:[ prog; "build"; "--root"; root; "-w"; "@repro" ]
      ~stdout:output
      ~stderr:output
      ()
  in
  Unix.close output;
  pid
;;

let stop_dune pid gate =
  if not (Sys.file_exists gate) then Test.write_file gate "";
  (match Unix.kill pid Sys.sigterm with
   | () -> ()
   | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ());
  ignore (Unix.waitpid [] pid : int * Unix.process_status)
;;

let%expect_test "Dune success refreshes load paths in every Merlin state" =
  let temp = Test.temp_dir "ocamllsp-stale-load-path-" in
  let root = Filename.concat temp "project" in
  let lib = Filename.concat temp "lib" in
  let runtime_dir = Filename.concat temp "runtime" in
  let gate = Filename.concat temp "gate" in
  let trigger = Filename.concat root "trigger" in
  Unix.mkdir root 0o700;
  Unix.mkdir lib 0o700;
  Unix.mkdir runtime_dir 0o700;
  Test.write_file (Filename.concat root "dune-project") "(lang dune 3.24)\n(name repro)\n";
  Test.write_file trigger "0\n";
  let wait_script = Filename.concat root "wait.sh" in
  Test.write_file wait_script "#!/bin/sh\nwhile [ ! -e \"$1\" ]; do sleep 0.05; done\n";
  Unix.chmod wait_script 0o755;
  Test.write_file
    (Filename.concat root "dune")
    (Printf.sprintf
       {jbuild|
(executable
 (name one)
 (modules one two)
 (flags (:standard -I %s)))

(rule
 (alias repro)
 (deps trigger)
 (action (run %%{dep:wait.sh} %s)))
|jbuild}
       lib
       gate);
  Test.write_file (Filename.concat lib "bar.ml") "let x = 42\n";
  let initial_source = "open Bar\nlet y = x\n" in
  let updated_source = "open! Bar\nlet y = x\n" in
  let one = Filename.concat root "one.ml" in
  let two = Filename.concat root "two.ml" in
  Test.write_file one initial_source;
  Test.write_file two initial_source;
  (* Generate Dune's Merlin configuration. The build is expected to fail while
     [Bar.cmi] is absent. *)
  Test.run_command
    ~cwd:root
    (Printf.sprintf
       "dune build --root %s one.exe > %s 2>&1 || true"
       (Filename.quote root)
       Test.null_device);
  let dune_pid = start_dune root runtime_dir in
  let ocamllsp_stderr = Unix.openfile Test.null_device [ Unix.O_WRONLY ] 0o666 in
  Fun.protect
    ~finally:(fun () ->
      stop_dune dune_pid gate;
      Unix.close ocamllsp_stderr;
      ignore (Sys.command ("rm -rf -- " ^ Filename.quote temp) : int))
    (fun () ->
       let events = Events.create () in
       let handler =
         let on_request
               (type response state)
               (client : state Client.t)
               (request : response Lsp.Server_request.t)
           : (response Lsp_fiber.Rpc.Reply.t * state) Fiber.t
           =
           match request with
           | Lsp.Server_request.WorkDoneProgressCreate _ ->
             Fiber.return (Lsp_fiber.Rpc.Reply.now (), Client.state client)
           | _ -> assert false
         in
         Client.Handler.make
           ~on_request:{ Client.Handler.on_request }
           ~on_notification:(Events.on_notification events)
           ()
       in
       let window = WindowClientCapabilities.create ~workDoneProgress:true () in
       let capabilities = ClientCapabilities.create ~window () in
       let workspace =
         WorkspaceFolder.create ~uri:(Uri.of_path root) ~name:"stale-load-path"
       in
       Test.run_initialized
         ~extra_env:[ "OCAMLLSP_TEST=false"; "XDG_RUNTIME_DIR=" ^ runtime_dir ]
         ~timeout:30.0
         ~handler
         ~stderr:ocamllsp_stderr
         ~capabilities
         ~workspaceFolders:(Some [ workspace ])
         ~trace:Messages
       @@ fun client ->
       let* () = Client.notification client Initialized in
       (* The blocked initial build makes Dune RPC connection deterministic. *)
       let* () = Signal.wait events.dune_connected in
       let* () = Signal.wait events.dune_connection_traced in
       print_endline "Dune trace: Connected to Dune RPC";
       let* () =
         Client.notification client (SetTrace (SetTraceParams.create ~value:Verbose))
       in
       let* (_ : Lsp.Extension.DebugEcho.Result.t) =
         Client.request client (DebugEcho { message = "trace barrier" })
       in
       let one_uri = Uri.of_path one in
       let two_uri = Uri.of_path two in
       let* () = open_document client ~uri:one_uri ~text:initial_source in
       let* (_ : PublishDiagnosticsParams.t) =
         Events.wait_for_diagnostics events ~f:(fun params ->
           for_uri one_uri params && has_unbound_bar params)
       in
       let* () = open_document client ~uri:two_uri ~text:initial_source in
       let* (_ : PublishDiagnosticsParams.t) =
         Events.wait_for_diagnostics events ~f:(fun params ->
           for_uri two_uri params && has_unbound_bar params)
       in
       Test.run_command ~cwd:lib "ocamlc -c bar.ml";
       let mtime = Unix.gettimeofday () +. 2.0 in
       Unix.utimes lib mtime mtime;
       (* Refresh one state first. This makes the shared directory cache current
          while the other state still has a stale local snapshot. *)
       Test.write_file one updated_source;
       let* () = change_document client ~uri:one_uri ~text:updated_source in
       let* (_ : PublishDiagnosticsParams.t) =
         Events.wait_for_diagnostics events ~f:(fun params ->
           for_uri one_uri params && not (has_unbound_bar params))
       in
       (* Finish the build that was blocked while setting up the two Merlin
          states, then use its Success notification to refresh both documents. *)
       let two_after_build = Events.wait_for_diagnostics events ~f:(for_uri two_uri) in
       let build_finished = Signal.wait events.build_finished in
       let build_finished_traced = Signal.wait events.build_finished_traced in
       Test.write_file gate "";
       let* two_after_build = two_after_build in
       let* () = build_finished in
       let* () = build_finished_traced in
       print_endline "Dune trace: Dune build finished (verbose details present)";
       List.iter two_after_build.diagnostics ~f:(fun diagnostic ->
         print_endline (diagnostic_message diagnostic));
       let* () = Signal.wait events.merlin_config_started in
       print_endline "Dune trace: Merlin configuration process started";
       let* () = Signal.wait events.merlin_config_request in
       let* () = Signal.wait events.merlin_config_response in
       print_endline "Dune trace: Merlin configuration request completed";
       let stopping = Signal.wait events.merlin_config_stopping in
       let exited = Signal.wait events.merlin_config_exited in
       let* () = close_document client ~uri:one_uri in
       let* () = close_document client ~uri:two_uri in
       let* () = stopping in
       let* () = exited in
       print_endline "Dune trace: Merlin configuration process stopped and exited";
       Test.shutdown_client client);
  [%expect
    {|
    Dune trace: Connected to Dune RPC
    Dune trace: Dune build finished (verbose details present)
    Unbound module Bar
    Unbound value x
    Dune trace: Merlin configuration process started
    Dune trace: Merlin configuration request completed
    Dune trace: Merlin configuration process stopped and exited
    |}]
;;

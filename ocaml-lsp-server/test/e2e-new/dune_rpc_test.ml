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

module Mailbox = struct
  type 'a t =
    { mutable pending_rev : 'a list
    ; mutable waiter : 'a Fiber.Ivar.t option
    }

  let create () = { pending_rev = []; waiter = None }

  let push t value =
    match t.waiter with
    | None ->
      t.pending_rev <- value :: t.pending_rev;
      Fiber.return ()
    | Some waiter ->
      t.waiter <- None;
      Fiber.Ivar.fill waiter value
  ;;

  let wait t =
    match List.rev t.pending_rev with
    | value :: rest ->
      t.pending_rev <- List.rev rest;
      Fiber.return value
    | [] ->
      assert (Option.is_none t.waiter);
      let waiter = Fiber.Ivar.create () in
      t.waiter <- Some waiter;
      Fiber.Ivar.read waiter
  ;;

  let take_pending t =
    let pending = List.rev t.pending_rev in
    t.pending_rev <- [];
    pending
  ;;
end

module Events = struct
  type diagnostic_waiter =
    (PublishDiagnosticsParams.t -> bool) * PublishDiagnosticsParams.t Fiber.Ivar.t

  type t =
    { dune_ready : Signal.t
    ; dune_progress : Signal.t
    ; multiple_instances : Signal.t
    ; errors : LogMessageParams.t Mailbox.t
    ; progress : Lsp.Progress.t ProgressParams.t Mailbox.t
    ; mutable diagnostics : PublishDiagnosticsParams.t list
    ; mutable diagnostic_waiter : diagnostic_waiter option
    }

  let create () =
    { dune_ready = Signal.create ()
    ; dune_progress = Signal.create ()
    ; multiple_instances = Signal.create ()
    ; errors = Mailbox.create ()
    ; progress = Mailbox.create ()
    ; diagnostics = []
    ; diagnostic_waiter = None
    }
  ;;

  let dune_ready t = t.dune_ready
  let dune_progress t = t.dune_progress
  let multiple_instances t = t.multiple_instances
  let errors t = t.errors
  let progress t = t.progress

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

  let take_pending t =
    let diagnostics = List.rev t.diagnostics in
    t.diagnostics <- [];
    diagnostics
  ;;

  let on_notification t _ (notification : Lsp.Server_notification.t) =
    match notification with
    | PublishDiagnostics diagnostic -> publish_diagnostics t diagnostic
    | LogMessage { message; _ }
      when Re.execp (Re.compile (Re.str ": connected to dune at ")) message ->
      Signal.notify t.dune_ready
    | LogMessage { message; _ } when String.is_substring message ~substring:" ignores " ->
      Signal.notify t.multiple_instances
    | LogMessage ({ type_ = Error; _ } as params) -> Mailbox.push t.errors params
    | LogTrace { message; _ } when String.is_prefix message ~prefix:"Dune build " ->
      Signal.notify t.dune_progress
    | WorkDoneProgress progress -> Mailbox.push t.progress progress
    | _ -> Fiber.return ()
  ;;
end

module Lifecycle_events = struct
  type t =
    { dune : Events.t
    ; progress_creations : WorkDoneProgressCreateParams.t Mailbox.t
    ; registrations : RegistrationParams.t Mailbox.t
    ; unregistrations : UnregistrationParams.t Mailbox.t
    }

  let create () =
    { dune = Events.create ()
    ; progress_creations = Mailbox.create ()
    ; registrations = Mailbox.create ()
    ; unregistrations = Mailbox.create ()
    }
  ;;

  let handler t =
    let on_request
          (type response state)
          (client : state Client.t)
          (request : response Lsp.Server_request.t)
      : (response Lsp_fiber.Rpc.Reply.t * state) Fiber.t
      =
      match request with
      | WorkDoneProgressCreate params ->
        let+ () = Mailbox.push t.progress_creations params in
        Lsp_fiber.Rpc.Reply.now (), Client.state client
      | ClientRegisterCapability params ->
        let+ () = Mailbox.push t.registrations params in
        Lsp_fiber.Rpc.Reply.now (), Client.state client
      | ClientUnregisterCapability params ->
        let+ () = Mailbox.push t.unregistrations params in
        Lsp_fiber.Rpc.Reply.now (), Client.state client
      | _ -> assert false
    in
    Client.Handler.make
      ~on_request:{ Client.Handler.on_request }
      ~on_notification:(Events.on_notification t.dune)
      ()
  ;;
end

let for_uri uri (params : PublishDiagnosticsParams.t) = Uri.equal uri params.uri

let is_dune_diagnostic (diagnostic : Diagnostic.t) =
  Option.equal String.equal diagnostic.source (Some "dune")
;;

let has_dune_diagnostic (params : PublishDiagnosticsParams.t) =
  List.exists params.diagnostics ~f:is_dune_diagnostic
;;

let no_dune_diagnostic params = not (has_dune_diagnostic params)

let only_dune_diagnostics (params : PublishDiagnosticsParams.t) =
  { params with diagnostics = List.filter params.diagnostics ~f:is_dune_diagnostic }
;;

let terminate_process pid =
  match Unix.kill pid Sys.sigterm with
  | () -> ()
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ()
;;

let stop_process pid =
  terminate_process pid;
  ignore (Test.waitpid pid : Unix.process_status)
;;

let wait_for_rpc_registration runtime_dir pid =
  let rpc_dir = Filename.concat runtime_dir "dune/rpc" in
  let registration = Filename.concat rpc_dir (Printf.sprintf "%d.csexp" pid) in
  let rec loop retries =
    let registered =
      match Fs_io.read_file registration with
      | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false
      | Error _ -> false
      | Ok contents ->
        let file : Dune_rpc.Private.Registry.File.t = { path = registration; contents } in
        Result.is_ok (Dune_rpc.Private.Registry.Dune.of_file file)
    in
    if registered
    then ()
    else if retries = 0
    then failwith (Printf.sprintf "Dune %d did not register its RPC endpoint" pid)
    else (
      Unix.sleepf 0.01;
      loop (retries - 1))
  in
  loop 500
;;

let start_dune ?build_dir ?jobs root runtime_dir =
  let prog = Bin.which "dune" |> Option.value_exn in
  let output = Unix.openfile Test.null_device [ Unix.O_WRONLY ] 0o666 in
  let env =
    let is_runtime_dir value = String.is_prefix value ~prefix:"XDG_RUNTIME_DIR=" in
    Unix.environment ()
    |> Array.to_list
    |> List.filter ~f:(fun value -> not (is_runtime_dir value))
    |> List.cons ("XDG_RUNTIME_DIR=" ^ runtime_dir)
    |> Spawn.Env.of_list
  in
  let argv =
    [ prog; "build"; "--root"; root ]
    @ (match build_dir with
       | None -> []
       | Some build_dir -> [ "--build-dir"; build_dir ])
    @ (match jobs with
       | None -> []
       | Some jobs -> [ "-j"; Int.to_string jobs ])
    @ [ "-w"; "@repro" ]
  in
  let pid =
    Spawn.spawn ~env ~cwd:(Path root) ~prog ~argv ~stdout:output ~stderr:output ()
  in
  Unix.close output;
  pid
;;

let stop_abruptly client pid =
  let* () = Client.stop client in
  (* Close the write side before killing the server so pending replies cannot hit EPIPE. *)
  let+ () = Client.close client in
  terminate_process pid
;;

let open_document client ~uri ~text =
  let textDocument =
    TextDocumentItem.create ~uri ~languageId:(LanguageKind.Other "ocaml") ~version:0 ~text
  in
  Client.notification
    client
    (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
;;

type project =
  { temp : string
  ; root : string
  ; runtime_dir : string
  ; expected : string
  ; trigger : string
  ; gate : string
  ; old_source : string
  ; mutable dune_pid : int option
  }

let create_project name =
  let temp = Test.temp_dir ("lsp-" ^ name ^ "-") in
  let root = Filename.concat temp "r" in
  let runtime_dir = Filename.concat temp "x" in
  Unix.mkdir root 0o700;
  Unix.mkdir runtime_dir 0o700;
  Test.write_file (Filename.concat root "dune-project") "(lang dune 3.24)\n";
  let expected = Filename.concat root "expected.ml" in
  let trigger = Filename.concat root "trigger" in
  let gate = Filename.concat temp "gate" in
  let old_source = "let answer = 0\n" in
  Test.write_file expected old_source;
  Test.write_file trigger "0\n";
  let wait_script = Filename.concat root "wait.sh" in
  Test.write_file wait_script "#!/bin/sh\nwhile [ ! -e \"$1\" ]; do sleep 0.01; done\n";
  Unix.chmod wait_script 0o755;
  Test.write_file
    (Filename.concat root "dune")
    (Printf.sprintf
       {jbuild|
(rule
 (alias repro)
 (deps expected.ml trigger wait.sh)
 (target actual.ml)
 (action
  (progn
   (run %%{dep:wait.sh} %s)
   (with-stdout-to %%{target} (echo "let answer = 42"))
   (diff expected.ml %%{target}))))
|jbuild}
       gate);
  let dune_pid = start_dune root runtime_dir in
  (* These tests exercise connected lifecycle transitions, so make Dune
     discoverable before starting ocamllsp. *)
  match wait_for_rpc_registration runtime_dir dune_pid with
  | () ->
    { temp
    ; root
    ; runtime_dir
    ; expected
    ; trigger
    ; gate
    ; old_source
    ; dune_pid = Some dune_pid
    }
  | exception exn ->
    stop_process dune_pid;
    ignore (Sys.command ("rm -rf -- " ^ Filename.quote temp) : int);
    raise exn
;;

let sanitize_string project string =
  let replacements =
    [ Uri.to_string (Uri.of_path project.expected), "<document-uri>"
    ; project.expected, "<document-path>"
    ; Uri.to_string (Uri.of_path project.root), "<workspace-uri>"
    ; project.root, "<workspace-path>"
    ; Uri.to_string (Uri.of_path project.temp), "<test-uri>"
    ; project.temp, "<test-dir>"
    ]
  in
  List.fold_left replacements ~init:string ~f:(fun string (pattern, with_) ->
    String.substr_replace_all string ~pattern ~with_)
;;

let rec sanitize_json project : Yojson.Safe.t -> Yojson.Safe.t = function
  | `String string -> `String (sanitize_string project string)
  | `Assoc fields ->
    `Assoc (List.map fields ~f:(fun (name, value) -> name, sanitize_json project value))
  | `List values -> `List (List.map values ~f:(sanitize_json project))
  | (`Bool _ | `Float _ | `Int _ | `Intlit _ | `Null) as json -> json
;;

let print_payload project label json =
  print_endline label;
  sanitize_json project json |> Test.print_result
;;

let print_payloads project label yojson_of_t values =
  values
  |> List.map ~f:yojson_of_t
  |> fun values -> print_payload project label (`List values)
;;

let stop_dune project =
  let pid = Option.value_exn project.dune_pid in
  project.dune_pid <- None;
  stop_process pid
;;

let restart_dune project =
  assert (Option.is_none project.dune_pid);
  let dune_pid = start_dune project.root project.runtime_dir in
  match wait_for_rpc_registration project.runtime_dir dune_pid with
  | () -> project.dune_pid <- Some dune_pid
  | exception exn ->
    stop_process dune_pid;
    raise exn
;;

let destroy_project project =
  Option.iter project.dune_pid ~f:stop_process;
  ignore (Sys.command ("rm -rf -- " ^ Filename.quote project.temp) : int)
;;

let run_with_workspace ?capabilities ?trace ~root ~runtime_dir events ~f =
  let ocamllsp_stderr = Unix.openfile Test.null_device [ Unix.O_WRONLY ] 0o666 in
  Fun.protect
    ~finally:(fun () -> Unix.close ocamllsp_stderr)
    (fun () ->
       let capabilities =
         match capabilities with
         | Some capabilities -> capabilities
         | None ->
           let window = WindowClientCapabilities.create ~workDoneProgress:true () in
           ClientCapabilities.create ~window ()
       in
       let workspace = WorkspaceFolder.create ~uri:(Uri.of_path root) ~name:"dune-rpc" in
       let server_pid = ref None in
       Test.run_initialized
         ~extra_env:[ "OCAMLLSP_TEST=false"; "XDG_RUNTIME_DIR=" ^ runtime_dir ]
         ~timeout:30.0
         ~handler:(Lifecycle_events.handler events)
         ~stderr:ocamllsp_stderr
         ~capabilities
         ~workspaceFolders:(Some [ workspace ])
         ?trace
         ~on_spawn:(fun pid -> server_pid := Some pid)
       @@ fun client ->
       Fiber.finalize
         (fun () -> f client workspace)
         ~finally:(fun () -> stop_abruptly client (Option.value_exn !server_pid)))
;;

let run ?workspace_root ?capabilities ?trace project events ~f =
  let root = Option.value workspace_root ~default:project.root in
  run_with_workspace ~root ~runtime_dir:project.runtime_dir ?capabilities ?trace events ~f
;;

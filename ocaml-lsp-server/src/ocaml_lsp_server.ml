open Import
module Version = Version
open Fiber.O

let make_error = Jsonrpc.Response.Error.make

let not_supported () =
  Jsonrpc.Response.Error.raise
    (make_error ~code:InternalError ~message:"Request not supported yet!" ())

let initialize_info : InitializeResult.t =
  let codeActionProvider =
    let codeActionKinds =
      Action_inferred_intf.kind :: Action_destruct.kind
      :: List.map
           ~f:(fun (c : Code_action.t) -> c.kind)
           [ Action_type_annotate.t
           ; Action_construct.t
           ; Action_refactor_open.unqualify
           ; Action_refactor_open.qualify
           ; Action_add_rec.t
           ]
      |> List.sort_uniq ~compare:Poly.compare
    in
    `CodeActionOptions (CodeActionOptions.create ~codeActionKinds ())
  in
  let textDocumentSync =
    `TextDocumentSyncOptions
      (TextDocumentSyncOptions.create ~openClose:true
         ~change:TextDocumentSyncKind.Incremental ~willSave:false
         ~save:(`Bool true) ~willSaveWaitUntil:false ())
  in
  let codeLensProvider = CodeLensOptions.create ~resolveProvider:false () in
  let completionProvider =
    (* TODO even if this re-enabled in general, it should stay disabled for
       emacs. It makes completion too slow *)
    CompletionOptions.create ~triggerCharacters:[ "."; "#" ]
      ~resolveProvider:true ()
  in
  let signatureHelpProvider =
    SignatureHelpOptions.create
      ~triggerCharacters:[ " "; "~"; "?"; ":"; "(" ]
      ()
  in
  let renameProvider =
    `RenameOptions (RenameOptions.create ~prepareProvider:true ())
  in
  let workspace =
    let workspaceFolders =
      WorkspaceFoldersServerCapabilities.create ~supported:true
        ~changeNotifications:(`Bool true) ()
    in
    ServerCapabilities.create_workspace ~workspaceFolders ()
  in
  let capabilities =
    let experimental =
      `Assoc
        [ ( "ocamllsp"
          , `Assoc
              [ ("interfaceSpecificLangId", `Bool true)
              ; Req_switch_impl_intf.capability
              ; Req_infer_intf.capability
              ; Req_typed_holes.capability
              ; Req_wrapping_ast_node.capability
              ; Dune.view_promotion_capability
              ] )
        ]
    in
    let executeCommandProvider =
      ExecuteCommandOptions.create ~commands:Dune.commands ()
    in
    ServerCapabilities.create ~textDocumentSync ~hoverProvider:(`Bool true)
      ~declarationProvider:(`Bool true) ~definitionProvider:(`Bool true)
      ~typeDefinitionProvider:(`Bool true) ~completionProvider
      ~signatureHelpProvider ~codeActionProvider ~codeLensProvider
      ~referencesProvider:(`Bool true) ~documentHighlightProvider:(`Bool true)
      ~documentFormattingProvider:(`Bool true)
      ~selectionRangeProvider:(`Bool true) ~documentSymbolProvider:(`Bool true)
      ~workspaceSymbolProvider:(`Bool true) ~foldingRangeProvider:(`Bool true)
      ~experimental ~renameProvider ~workspace ~executeCommandProvider ()
  in
  let serverInfo =
    let version = Version.get () in
    InitializeResult.create_serverInfo ~name:"ocamllsp" ~version ()
  in
  InitializeResult.create ~capabilities ~serverInfo ()

let ocamlmerlin_reason = "ocamlmerlin-reason"

let extract_related_errors uri raw_message =
  match Ocamlc_loc.parse_raw raw_message with
  | `Message message :: related ->
    let string_of_message message =
      String.trim
        (match (message : Ocamlc_loc.message) with
        | Raw s -> s
        | Structured { message; severity; file_excerpt = _ } ->
          let severity =
            match severity with
            | Error -> "Error"
            | Warning { code; name } ->
              sprintf "Warning %s"
                (match (code, name) with
                | None, Some name -> sprintf "[%s]" name
                | Some code, None -> sprintf "%d" code
                | Some code, Some name -> sprintf "%d [%s]" code name
                | None, None -> assert false)
          in
          sprintf "%s: %s" severity message)
    in
    let related =
      let rec loop acc = function
        | `Loc (_, loc) :: `Message m :: xs -> loop ((loc, m) :: acc) xs
        | [] -> List.rev acc
        | _ ->
          (* give up when we see something unexpected *)
          Log.log ~section:"debug" (fun () ->
              Log.msg "unable to parse error" [ ("error", `String raw_message) ]);
          []
      in
      loop [] related
    in
    let related =
      match related with
      | [] -> None
      | related ->
        let make_related ({ Ocamlc_loc.path = _; line; chars }, message) =
          let location =
            let start, end_ =
              let line_start, line_end =
                match line with
                | `Single i -> (i, i)
                | `Range (i, j) -> (i, j)
              in
              let char_start, char_end =
                match chars with
                | None -> (1, 1)
                | Some (x, y) -> (x, y)
              in
              ( Position.create ~line:line_start ~character:char_start
              , Position.create ~line:line_end ~character:char_end )
            in
            let range = Range.create ~start ~end_ in
            Location.create ~range ~uri
          in
          let message = string_of_message message in
          DiagnosticRelatedInformation.create ~location ~message
        in
        Some (List.map related ~f:make_related)
    in
    (string_of_message message, related)
  | _ -> (raw_message, None)

let set_diagnostics rpc doc =
  let state : State.t = Server.state rpc in
  let uri = Document.uri doc in
  let create_diagnostic = Diagnostic.create ~source:"ocamllsp" in
  let async send =
    let+ () =
      task_if_running state.detached ~f:(fun () ->
          let timer = Document.timer doc in
          let* () = Lev_fiber.Timer.Wheel.cancel timer in
          let* () = Lev_fiber.Timer.Wheel.reset timer in
          let* res = Lev_fiber.Timer.Wheel.await timer in
          match res with
          | `Cancelled -> Fiber.return ()
          | `Ok -> send ())
    in
    ()
  in
  match Document.syntax doc with
  | Dune
  | Cram
  | Menhir
  | Ocamllex ->
    Fiber.return ()
  | Reason when Option.is_none (Bin.which ocamlmerlin_reason) ->
    let no_reason_merlin =
      let message =
        sprintf "Could not detect %s. Please install reason" ocamlmerlin_reason
      in
      create_diagnostic ~range:Range.first_line ~message ()
    in
    Diagnostics.set state.diagnostics (`Merlin (uri, [ no_reason_merlin ]));
    async (fun () -> Diagnostics.send state.diagnostics (`One uri))
  | Reason
  | Ocaml ->
    let send () =
      let* diagnostics =
        let command =
          Query_protocol.Errors { lexing = true; parsing = true; typing = true }
        in
        Document.with_pipeline_exn doc (fun pipeline ->
            match Query_commands.dispatch pipeline command with
            | exception Extend_main.Handshake.Error error ->
              let message =
                sprintf
                  "%s.\n\
                   Hint: install the following packages: merlin-extend, reason"
                  error
              in
              [ create_diagnostic ~range:Range.first_line ~message () ]
            | errors ->
              let merlin_diagnostics =
                List.rev_map errors ~f:(fun (error : Loc.error) ->
                    let loc = Loc.loc_of_report error in
                    let range = Range.of_loc loc in
                    let severity =
                      match error.source with
                      | Warning -> DiagnosticSeverity.Warning
                      | _ -> DiagnosticSeverity.Error
                    in
                    let make_message ppf m =
                      String.trim (Format.asprintf "%a@." ppf m)
                    in
                    let message = make_message Loc.print_main error in
                    let message, relatedInformation =
                      match error.sub with
                      | [] -> extract_related_errors uri message
                      | _ :: _ ->
                        ( message
                        , Some
                            (List.map error.sub ~f:(fun (sub : Loc.msg) ->
                                 let location =
                                   let range = Range.of_loc sub.loc in
                                   Location.create ~range ~uri
                                 in
                                 let message =
                                   make_message Loc.print_sub_msg sub
                                 in
                                 DiagnosticRelatedInformation.create ~location
                                   ~message)) )
                    in
                    create_diagnostic ?relatedInformation ~range ~message
                      ~severity ())
              in
              let holes_as_err_diags =
                Query_commands.dispatch pipeline Holes
                |> List.rev_map ~f:(fun (loc, typ) ->
                       let range = Range.of_loc loc in
                       let severity = DiagnosticSeverity.Error in
                       let message =
                         "This typed hole should be replaced with an \
                          expression of type " ^ typ
                       in
                       (* we set specific diagnostic code = "hole" to be able to
                          filter through diagnostics easily *)
                       create_diagnostic ~code:(`String "hole") ~range ~message
                         ~severity ())
              in
              (* Can we use [List.merge] instead? *)
              List.rev_append holes_as_err_diags merlin_diagnostics
              |> List.sort
                   ~compare:(fun (d1 : Diagnostic.t) (d2 : Diagnostic.t) ->
                     Range.compare d1.range d2.range))
      in
      Diagnostics.set state.diagnostics (`Merlin (uri, diagnostics));
      Diagnostics.send state.diagnostics (`One uri)
    in
    async send

let on_initialize server (ip : InitializeParams.t) =
  let state : State.t = Server.state server in
  let workspaces = Workspaces.create ip in
  let+ dune =
    let progress =
      Progress.create ip.capabilities
        ~report_progress:(fun progress ->
          Server.notification server
            (Server_notification.WorkDoneProgress progress))
        ~create_task:(fun task ->
          Server.request server (Server_request.WorkDoneProgressCreate task))
    in
    let dune =
      Dune.create workspaces ip.capabilities state.diagnostics progress
        state.store ~log:(State.log_msg server)
    in
    let+ () = Fiber.Pool.task state.detached ~f:(fun () -> Dune.run dune) in
    dune
  in
  let state = State.initialize state ip workspaces dune in
  let state =
    match ip.trace with
    | None -> state
    | Some trace -> { state with trace }
  in
  let resp =
    match ip.capabilities.textDocument with
    | Some
        { TextDocumentClientCapabilities.synchronization =
            Some
              { TextDocumentSyncClientCapabilities.dynamicRegistration =
                  Some true
              ; _
              }
        ; _
        } ->
      Reply.later (fun send ->
          let* () = send initialize_info in
          let register =
            RegistrationParams.create
              ~registrations:
                (let make method_ =
                   let id = "ocamllsp-cram-dune-files/" ^ method_ in
                   (* TODO not nice to copy paste *)
                   let registerOptions =
                     let documentSelector =
                       [ "cram"; "dune"; "dune-project"; "dune-workspace" ]
                       |> List.map ~f:(fun language ->
                              DocumentFilter.create ~language ())
                     in
                     TextDocumentRegistrationOptions.create ~documentSelector ()
                     |> TextDocumentRegistrationOptions.yojson_of_t
                   in
                   Registration.create ~id ~method_ ~registerOptions ()
                 in
                 [ make "textDocument/didOpen"; make "textDocument/didClose" ])
          in
          Server.request server
            (Server_request.ClientRegisterCapability register))
    | _ -> Reply.now initialize_info
  in
  (resp, state)

module Code_action_error = struct
  type t =
    | Initial
    | Need_merlin_extend of string
    | Exn of Exn_with_backtrace.t

  let empty = Initial

  let combine x y =
    match (x, y) with
    | Initial, _ -> y (* [Initial] cedes to any *)
    | _, Initial -> x
    | Exn _, _ -> x (* [Exn] takes over any *)
    | _, Exn _ -> y
    | Need_merlin_extend _, Need_merlin_extend _ -> y
end

module Code_action_error_monoid = struct
  type t = Code_action_error.t

  include Stdune.Monoid.Make (Code_action_error)
end

let code_action server (params : CodeActionParams.t) =
  let state : State.t = Server.state server in
  let doc =
    let uri = params.textDocument.uri in
    let store = state.store in
    Document_store.get_opt store uri
  in
  let dune_actions =
    Dune.code_actions (State.dune state) params.textDocument.uri
  in
  let actions = function
    | [] -> None
    | xs -> Some (List.map ~f:(fun a -> `CodeAction a) xs)
  in
  match doc with
  | None -> Fiber.return (Reply.now (actions dune_actions), state)
  | Some doc -> (
    match Document.syntax doc with
    | Ocamllex
    | Menhir
    | Cram
    | Dune ->
      let state : State.t = Server.state server in
      Fiber.return (Reply.now (actions dune_actions), state)
    | Ocaml
    | Reason ->
      let reply () =
        let code_action (ca : Code_action.t) =
          match params.context.only with
          | Some set when not (List.mem set ca.kind ~equal:Poly.equal) ->
            Fiber.return None
          | Some _
          | None -> (
            let+ res =
              Fiber.map_reduce_errors
                ~on_error:(fun (exn : Exn_with_backtrace.t) ->
                  match exn.exn with
                  | Extend_main.Handshake.Error error ->
                    Fiber.return (Code_action_error.Need_merlin_extend error)
                  | _ -> Fiber.return (Code_action_error.Exn exn))
                (module Code_action_error_monoid)
                (fun () -> ca.run doc params)
            in
            match res with
            | Ok res -> res
            | Error Initial -> assert false
            | Error (Need_merlin_extend _) -> None
            | Error (Exn exn) -> Exn_with_backtrace.reraise exn)
        in
        let+ code_action_results =
          (* XXX this is a really bad use of resources. we should be batching
             all the merlin related work *)
          Fiber.parallel_map ~f:code_action
            [ Action_destruct.t state
            ; Action_inferred_intf.t state
            ; Action_type_annotate.t
            ; Action_construct.t
            ; Action_refactor_open.unqualify
            ; Action_refactor_open.qualify
            ; Action_add_rec.t
            ; Action_mark_remove_unused.mark
            ; Action_mark_remove_unused.remove
            ]
        in
        List.filter_opt code_action_results
        |> List.append dune_actions |> actions
      in
      let later f =
        Fiber.return
          ( Reply.later (fun k ->
                let* resp = f () in
                k resp)
          , state )
      in
      later reply)

module Formatter = struct
  let jsonrpc_error (e : Ocamlformat.error) =
    let message = Ocamlformat.message e in
    let code : Jsonrpc.Response.Error.Code.t =
      match e with
      | Unsupported_syntax _
      | Unknown_extension _
      | Missing_binary _ ->
        InvalidRequest
      | Unexpected_result _ -> InternalError
    in
    make_error ~code ~message ()

  let run rpc doc =
    let state : State.t = Server.state rpc in
    if Document.is_merlin doc then
      let* res =
        let* res = Ocamlformat_rpc.format_doc state.ocamlformat_rpc doc in
        match res with
        | Ok res -> Fiber.return @@ Ok res
        | Error _ -> Ocamlformat.run doc
      in
      match res with
      | Ok result -> Fiber.return (Some result)
      | Error e ->
        let message = Ocamlformat.message e in
        let error = jsonrpc_error e in
        let msg = ShowMessageParams.create ~message ~type_:Warning in
        let+ () =
          let state : State.t = Server.state rpc in
          task_if_running state.detached ~f:(fun () ->
              Server.notification rpc (ShowMessage msg))
        in
        Jsonrpc.Response.Error.raise error
    else
      match Dune.for_doc (State.dune state) doc with
      | [] ->
        let message =
          sprintf "No dune instance found. Please run dune in watch mode for %s"
            (Uri.to_path (Document.uri doc))
        in
        Jsonrpc.Response.Error.raise
          (make_error ~code:InvalidRequest ~message ())
      | dune :: rest ->
        let* () =
          match rest with
          | [] -> Fiber.return ()
          | _ :: _ ->
            let message =
              sprintf
                "More than one dune instance detected for %s. Selecting one at \
                 random"
                (Uri.to_path (Document.uri doc))
            in
            State.log_msg rpc ~type_:MessageType.Warning ~message
        in
        let+ to_ = Dune.Instance.format_dune_file dune doc in
        Some (Diff.edit ~from:(Document.text doc) ~to_)
end

let location_of_merlin_loc uri : _ -> (_, string) result = function
  | `At_origin -> Ok None
  | `Builtin _ -> Ok None
  | `File_not_found s -> Error (sprintf "File_not_found: %s" s)
  | `Invalid_context -> Ok None
  | `Not_found (ident, where) ->
    let msg =
      let msg = sprintf "%s not found." ident in
      match where with
      | None -> msg
      | Some w -> sprintf "%s last looked in %s" msg w
    in
    Error msg
  | `Not_in_env m -> Error (sprintf "not in environment: %s" m)
  | `Found (path, lex_position) ->
    Ok
      (Position.of_lexical_position lex_position
      |> Option.map ~f:(fun position ->
             let range = { Range.start = position; end_ = position } in
             let uri =
               match path with
               | None -> uri
               | Some path -> Uri.of_path path
             in
             let locs = [ { Location.uri; range } ] in
             `Location locs))

let format_doc ~markdown ~doc =
  `MarkupContent
    (if markdown then
      let value =
        match Doc_to_md.translate doc with
        | Raw d -> sprintf "(** %s *)" d
        | Markdown d -> d
      in
      { MarkupContent.value; kind = MarkupKind.Markdown }
    else
      { MarkupContent.value = doc; kind = MarkupKind.PlainText })

let signature_help (state : State.t)
    { SignatureHelpParams.textDocument = { uri }; position; _ } =
  let doc =
    let store = state.store in
    Document_store.get store uri
  in
  let pos = Position.logical position in
  let prefix =
    (* The value of [short_path] doesn't make a difference to the final result
       because labels cannot include dots. However, a true value is slightly
       faster for getting the prefix. *)
    Compl.prefix_of_position (Document.source doc) pos ~short_path:true
  in
  (* TODO use merlin resources efficiently and do everything in 1 thread *)
  let* application_signature =
    Document.with_pipeline_exn doc (fun pipeline ->
        let typer = Mpipeline.typer_result pipeline in
        let pos = Mpipeline.get_lexing_pos pipeline pos in
        let node = Mtyper.node_at typer pos in
        Merlin_analysis.Signature_help.application_signature node ~prefix)
  in
  match application_signature with
  | None ->
    let help = SignatureHelp.create ~signatures:[] () in
    Fiber.return help
  | Some application_signature ->
    let prefix =
      let fun_name =
        Option.value ~default:"_" application_signature.function_name
      in
      sprintf "%s : " fun_name
    in
    let offset = String.length prefix in
    let+ doc =
      Document.doc_comment doc application_signature.function_position
    in
    let info =
      let parameters =
        List.map application_signature.parameters
          ~f:(fun (p : Merlin_analysis.Signature_help.parameter_info) ->
            let label =
              `Offset (offset + p.param_start, offset + p.param_end)
            in
            ParameterInformation.create ~label ())
      in
      let documentation =
        let open Option.O in
        let+ doc = doc in
        let markdown =
          ClientCapabilities.markdown_support (State.client_capabilities state)
            ~field:(fun td ->
              let* sh = td.signatureHelp in
              let+ si = sh.signatureInformation in
              si.documentationFormat)
        in
        format_doc ~markdown ~doc
      in
      let label = prefix ^ application_signature.signature in
      SignatureInformation.create ~label ?documentation ~parameters ()
    in
    SignatureHelp.create ~signatures:[ info ] ~activeSignature:0
      ?activeParameter:application_signature.active_param ()

let text_document_lens (state : State.t)
    { CodeLensParams.textDocument = { uri }; _ } =
  let store = state.store in
  let doc = Document_store.get store uri in
  match Document.kind doc with
  | Intf -> Fiber.return []
  | Impl ->
    let+ outline =
      let command = Query_protocol.Outline in
      Document.dispatch_exn doc command
    in
    let rec symbol_info_of_outline_item item =
      let children =
        List.concat_map item.Query_protocol.children
          ~f:symbol_info_of_outline_item
      in
      match item.Query_protocol.outline_type with
      | None -> children
      | Some typ ->
        let loc = item.Query_protocol.location in
        let info =
          let range = Range.of_loc loc in
          let command = Command.create ~title:typ ~command:"" () in
          CodeLens.create ~range ~command ()
        in
        info :: children
    in
    List.concat_map ~f:symbol_info_of_outline_item outline

let rename (state : State.t)
    { RenameParams.textDocument = { uri }; position; newName; _ } =
  let doc = Document_store.get state.store uri in
  let command =
    Query_protocol.Occurrences (`Ident_at (Position.logical position))
  in
  let+ locs = Document.dispatch_exn doc command in
  let version = Document.version doc in
  let source = Document.source doc in
  let edits =
    List.map locs ~f:(fun (loc : Warnings.loc) ->
        let range = Range.of_loc loc in
        let make_edit () = TextEdit.create ~range ~newText:newName in
        match
          let occur_start_pos =
            Position.of_lexical_position loc.loc_start |> Option.value_exn
          in
          occur_start_pos
        with
        | { character = 0; _ } -> make_edit ()
        | pos -> (
          let mpos = Position.logical pos in
          let (`Offset index) = Msource.get_offset source mpos in
          assert (index > 0)
          (* [index = 0] if we pass [`Logical (1, 0)], but we handle the case
             when [character = 0] in a separate matching branch *);
          let source_txt = Msource.text source in
          match source_txt.[index - 1] with
          | '~' (* the occurrence is a named argument *)
          | '?' (* is an optional argument *) ->
            let empty_range_at_occur_end =
              let occur_end_pos = range.Range.end_ in
              { range with start = occur_end_pos }
            in
            TextEdit.create ~range:empty_range_at_occur_end
              ~newText:(":" ^ newName)
          | _ -> make_edit ()))
  in
  let workspace_edits =
    let documentChanges =
      let open Option.O in
      Option.value ~default:false
        (let client_capabilities = State.client_capabilities state in
         let* workspace = client_capabilities.workspace in
         let* edit = workspace.workspaceEdit in
         edit.documentChanges)
    in
    if documentChanges then
      let textDocument =
        OptionalVersionedTextDocumentIdentifier.create ~uri ~version ()
      in
      let edits = List.map edits ~f:(fun e -> `TextEdit e) in
      WorkspaceEdit.create
        ~documentChanges:
          [ `TextDocumentEdit (TextDocumentEdit.create ~textDocument ~edits) ]
        ()
    else
      WorkspaceEdit.create ~changes:[ (uri, edits) ] ()
  in
  workspace_edits

let selection_range (state : State.t)
    { SelectionRangeParams.textDocument = { uri }; positions; _ } =
  let selection_range_of_shapes (cursor_position : Position.t)
      (shapes : Query_protocol.shape list) : SelectionRange.t option =
    let rec ranges_of_shape parent s =
      let selectionRange =
        let range = Range.of_loc s.Query_protocol.shape_loc in
        { SelectionRange.range; parent }
      in
      match s.Query_protocol.shape_sub with
      | [] -> [ selectionRange ]
      | xs -> List.concat_map xs ~f:(ranges_of_shape (Some selectionRange))
    in
    (* try to find the nearest range inside first, then outside *)
    let nearest_range =
      let ranges = List.concat_map ~f:(ranges_of_shape None) shapes in
      List.min ranges ~f:(fun r1 r2 ->
          let inc (r : SelectionRange.t) =
            Position.compare_inclusion cursor_position r.range
          in
          match (inc r1, inc r2) with
          | `Outside x, `Outside y -> Position.compare x y
          | `Outside _, `Inside -> Gt
          | `Inside, `Outside _ -> Lt
          | `Inside, `Inside -> Range.compare_size r1.range r2.range)
    in
    nearest_range
  in
  let doc = Document_store.get state.store uri in
  let+ ranges =
    Fiber.sequential_map positions ~f:(fun x ->
        let+ shapes =
          let command = Query_protocol.Shape (Position.logical x) in
          Document.dispatch_exn doc command
        in
        selection_range_of_shapes x shapes)
  in
  List.filter_opt ranges

let references (state : State.t)
    { ReferenceParams.textDocument = { uri }; position; _ } =
  let doc = Document_store.get state.store uri in
  let command =
    Query_protocol.Occurrences (`Ident_at (Position.logical position))
  in
  let+ locs = Document.dispatch_exn doc command in
  Some
    (List.map locs ~f:(fun loc ->
         let range = Range.of_loc loc in
         (* using original uri because merlin is looking only in local file *)
         { Location.uri; range }))

let definition_query server (state : State.t) uri position merlin_request =
  let doc = Document_store.get state.store uri in
  let position = Position.logical position in
  let command = merlin_request position in
  let* result = Document.dispatch_exn doc command in
  match location_of_merlin_loc uri result with
  | Ok s -> Fiber.return s
  | Error message ->
    let+ () =
      let message = sprintf "Locate failed. %s" message in
      State.log_msg server ~type_:Error ~message
    in
    None

let workspace_symbol server (state : State.t) (params : WorkspaceSymbolParams.t)
    =
  let* symbols, errors =
    let workspaces = Workspaces.workspace_folders (State.workspaces state) in
    let* thread = Lazy_fiber.force state.symbols_thread in
    let+ symbols_results =
      let* task =
        Lev_fiber.Thread.task thread ~f:(fun () ->
            Workspace_symbol.run params workspaces)
      in
      let+ res = Lev_fiber.Thread.await task in
      match res with
      | Ok s -> s
      | Error `Cancelled -> assert false
      | Error (`Exn exn) -> Exn_with_backtrace.reraise exn
    in
    List.partition_map symbols_results ~f:(function
      | Ok r -> Left r
      | Error e -> Right e)
  in
  let+ () =
    match errors with
    | [] -> Fiber.return ()
    | _ :: _ ->
      let msg =
        let message =
          List.map errors ~f:(function
              | Workspace_symbol.Build_dir_not_found workspace_name ->
              workspace_name)
          |> String.concat ~sep:", "
          |> sprintf "No build directory found in workspace(s): %s"
        in
        ShowMessageParams.create ~message ~type_:Warning
      in
      task_if_running state.detached ~f:(fun () ->
          Server.notification server (ShowMessage msg))
  in
  Some (List.concat symbols)

let highlight (state : State.t)
    { DocumentHighlightParams.textDocument = { uri }; position; _ } =
  let store = state.store in
  let doc = Document_store.get store uri in
  let command =
    Query_protocol.Occurrences (`Ident_at (Position.logical position))
  in
  let+ locs = Document.dispatch_exn doc command in
  let lsp_locs =
    List.map locs ~f:(fun loc ->
        let range = Range.of_loc loc in
        (* using the default kind as we are lacking info to make a difference
           between assignment and usage. *)
        DocumentHighlight.create ~range ~kind:DocumentHighlightKind.Text ())
  in
  Some lsp_locs

let document_symbol (state : State.t) uri =
  let+ symbols =
    let doc =
      let store = state.store in
      Document_store.get store uri
    in
    let client_capabilities = State.client_capabilities state in
    Document_symbol.run client_capabilities doc uri
  in
  Some symbols

let on_request :
    type resp.
       State.t Server.t
    -> resp Client_request.t
    -> (resp Reply.t * State.t) Fiber.t =
 fun server req ->
  let rpc = server in
  let state : State.t = Server.state server in
  let store = state.store in
  let now res = Fiber.return (Reply.now res, state) in
  let later f req =
    Fiber.return
      ( Reply.later (fun k ->
            let* resp = f state req in
            k resp)
      , state )
  in
  match req with
  | Client_request.UnknownRequest { meth; params } -> (
    match
      [ ( Req_switch_impl_intf.meth
        , fun ~params _ ->
            Fiber.of_thunk (fun () ->
                Fiber.return (Req_switch_impl_intf.on_request ~params)) )
      ; (Req_infer_intf.meth, Req_infer_intf.on_request)
      ; (Req_typed_holes.meth, Req_typed_holes.on_request)
      ; (Req_wrapping_ast_node.meth, Req_wrapping_ast_node.on_request)
      ]
      |> List.assoc_opt meth
    with
    | None ->
      Jsonrpc.Response.Error.raise
        (make_error ~code:InternalError ~message:"Unknown method"
           ~data:(`Assoc [ ("method", `String meth) ])
           ())
    | Some handler ->
      Fiber.return
        ( Reply.later (fun send ->
              let* res = handler ~params state in
              send res)
        , state ))
  | Initialize ip ->
    let+ res, state = on_initialize server ip in
    (res, state)
  | DebugTextDocumentGet { textDocument = { uri }; position = _ } -> (
    match Document_store.get_opt store uri with
    | None -> now None
    | Some doc -> now (Some (Msource.text (Document.source doc))))
  | DebugEcho params -> now params
  | Shutdown -> Fiber.return (Reply.now (), state)
  | WorkspaceSymbol req ->
    later (fun state () -> workspace_symbol server state req) ()
  | CodeActionResolve ca -> now ca
  | ExecuteCommand command ->
    later
      (fun state () ->
        let dune = State.dune state in
        (* all of our commands are handled by dune for now *)
        Dune.on_command dune command)
      ()
  | CompletionItemResolve ci ->
    later
      (fun state () ->
        let markdown =
          ClientCapabilities.markdown_support (State.client_capabilities state)
            ~field:(fun d ->
              let open Option.O in
              let+ completion = d.completion in
              let* completion_item = completion.completionItem in
              completion_item.documentationFormat)
        in
        let resolve = Compl.Resolve.of_completion_item ci in
        match resolve with
        | None -> Fiber.return ci
        | Some resolve ->
          let doc =
            let uri = Compl.Resolve.uri resolve in
            Document_store.get state.store uri
          in
          Compl.resolve doc ci resolve Document.doc_comment ~markdown)
      ()
  | CodeAction params -> code_action server params
  | TextDocumentColor _ -> now []
  | TextDocumentColorPresentation _ -> now []
  | TextDocumentHover req ->
    later (fun (_ : State.t) () -> Hover_req.handle rpc req) ()
  | TextDocumentReferences req -> later references req
  | TextDocumentCodeLensResolve codeLens -> now codeLens
  | TextDocumentCodeLens req -> later text_document_lens req
  | TextDocumentHighlight req -> later highlight req
  | DocumentSymbol { textDocument = { uri }; _ } -> later document_symbol uri
  | TextDocumentDeclaration { textDocument = { uri }; position } ->
    later
      (fun state () ->
        definition_query rpc state uri position (fun pos ->
            Query_protocol.Locate (None, `MLI, pos)))
      ()
  | TextDocumentDefinition { textDocument = { uri }; position; _ } ->
    later
      (fun state () ->
        definition_query rpc state uri position (fun pos ->
            Query_protocol.Locate (None, `ML, pos)))
      ()
  | TextDocumentTypeDefinition { textDocument = { uri }; position; _ } ->
    later
      (fun state () ->
        definition_query rpc state uri position (fun pos ->
            Query_protocol.Locate_type pos))
      ()
  | TextDocumentCompletion params ->
    later (fun _ () -> Compl.complete state params) ()
  | TextDocumentPrepareRename { textDocument = { uri }; position } ->
    later
      (fun _ () ->
        let doc = Document_store.get store uri in
        let command =
          Query_protocol.Occurrences (`Ident_at (Position.logical position))
        in
        let+ locs = Document.dispatch_exn doc command in
        let loc =
          List.find_opt locs ~f:(fun loc ->
              let range = Range.of_loc loc in
              Position.compare_inclusion position range = `Inside)
        in
        Option.map loc ~f:Range.of_loc)
      ()
  | TextDocumentRename req -> later rename req
  | TextDocumentFoldingRange req -> later Folding_range.compute req
  | SignatureHelp req -> later signature_help req
  | TextDocumentLinkResolve l -> now l
  | TextDocumentLink _ -> now None
  | WillSaveWaitUntilTextDocument _ -> now None
  | TextDocumentFormatting { textDocument = { uri }; options = _; _ } ->
    later
      (fun _ () ->
        let doc = Document_store.get store uri in
        Formatter.run rpc doc)
      ()
  | TextDocumentOnTypeFormatting _ -> now None
  | SelectionRange req -> later selection_range req
  | TextDocumentMoniker _ -> not_supported ()
  | SemanticTokensFull _ -> not_supported ()
  | SemanticTokensDelta _ -> not_supported ()
  | SemanticTokensRange _ -> not_supported ()
  | LinkedEditingRange _ -> not_supported ()

let on_notification server (notification : Client_notification.t) :
    State.t Fiber.t =
  let state : State.t = Server.state server in
  let store = state.store in
  match notification with
  | TextDocumentDidOpen params ->
    let* doc =
      Document.make (State.wheel state) state.merlin_config params
        ~merlin_thread:state.merlin
    in
    assert (Document_store.get_opt store params.textDocument.uri = None);
    let* () = Document_store.open_document store doc in
    let+ () = set_diagnostics server doc in
    state
  | TextDocumentDidClose { textDocument = { uri } } ->
    let+ () =
      Diagnostics.remove state.diagnostics (`Merlin uri);
      let* () = Document_store.close_document store uri in
      task_if_running state.detached ~f:(fun () ->
          Diagnostics.send state.diagnostics (`One uri))
    in
    state
  | TextDocumentDidChange { textDocument = { uri; version }; contentChanges } ->
    let doc =
      Document_store.change_document store uri ~f:(fun prev_doc ->
          Document.update_text ~version prev_doc contentChanges)
    in
    let+ () = set_diagnostics server doc in
    state
  | CancelRequest _ ->
    Log.log ~section:"debug" (fun () -> Log.msg "ignoring cancellation" []);
    Fiber.return state
  | ChangeConfiguration req ->
    (* TODO this is wrong and we should just fetch the config from the client
       after receiving this notification *)
    let configuration = Configuration.update state.configuration req in
    Fiber.return { state with configuration }
  | DidSaveTextDocument { textDocument = { uri }; _ } -> (
    let state = Server.state server in
    match Document_store.get_opt state.store uri with
    | None ->
      ( Log.log ~section:"on receive DidSaveTextDocument" @@ fun () ->
        Log.msg "saved document is not in the store" [] );
      Fiber.return state
    | Some _ ->
      let doc =
        Document_store.change_document store uri ~f:(fun doc ->
            (* we need [update_text] with no changes to get a new merlin
               pipeline; otherwise the diagnostics don't get updated *)
            Document.update_text doc [])
      in
      let+ () = set_diagnostics server doc in
      state)
  | ChangeWorkspaceFolders change ->
    let state =
      State.modify_workspaces state ~f:(fun ws ->
          Workspaces.on_change ws change)
    in
    Dune.update_workspaces (State.dune state) (State.workspaces state);
    Fiber.return state
  | WillSaveTextDocument _
  | Initialized
  | WorkDoneProgressCancel _
  | Exit ->
    Fiber.return state
  | SetTrace { value } -> Fiber.return { state with trace = value }
  | Unknown_notification req ->
    let+ () =
      State.log_msg server ~type_:Error
        ~message:("Unknown notication " ^ req.method_)
    in
    state

let start () =
  let detached = Fiber.Pool.create () in
  let server = Fdecl.create Dyn.opaque in
  let store = Document_store.make server detached in
  let handler =
    let on_request = { Server.Handler.on_request } in
    Server.Handler.make ~on_request ~on_notification ()
  in
  let* stream =
    let* stdin = Lev_fiber.Io.stdin in
    let+ stdout = Lev_fiber.Io.stdout in
    Lsp_fiber.Fiber_io.make stdin stdout
  in
  let configuration = Configuration.default in
  let diagnostics =
    let workspace_root =
      lazy
        (let server = Fdecl.get server in
         let state = Server.state server in
         State.workspace_root state)
    in
    Diagnostics.create ~workspace_root (function
      | [] -> Fiber.return ()
      | diagnostics ->
        let server = Fdecl.get server in
        let state = Server.state server in
        task_if_running state.detached ~f:(fun () ->
            let batch = Server.Batch.create server in
            List.iter diagnostics ~f:(fun d ->
                Server.Batch.notification batch (PublishDiagnostics d));
            Server.Batch.submit batch))
  in
  let ocamlformat_rpc = Ocamlformat_rpc.create () in
  let* wheel =
    let delay = Configuration.diagnostics_delay configuration in
    Lev_fiber.Timer.Wheel.create ~delay
  in
  let* server =
    let+ merlin = Lev_fiber.Thread.create () in
    let symbols_thread = Lazy_fiber.create Lev_fiber.Thread.create in
    Fdecl.set server
      (Server.make handler stream
         (State.create ~store ~merlin ~ocamlformat_rpc ~configuration ~detached
            ~diagnostics ~symbols_thread ~wheel));
    Fdecl.get server
  in
  Fiber.all_concurrently_unit
    [ Fiber.Pool.run detached
    ; Lev_fiber.Timer.Wheel.run wheel
    ; (let* () = Server.start server in
       let finalize =
         [ Document_store.close_all store
         ; Fiber.Pool.stop detached
         ; Ocamlformat_rpc.stop ocamlformat_rpc
         ; Lev_fiber.Timer.Wheel.stop wheel
         ]
       in
       let finalize =
         match (Server.state server).init with
         | Uninitialized -> finalize
         | Initialized init -> Dune.stop init.dune :: finalize
       in
       Fiber.all_concurrently_unit finalize)
    ; (let* state =
         Ocamlformat_rpc.run ~logger:(State.log_msg server) ocamlformat_rpc
       in
       let message =
         match state with
         | Error `Binary_not_found ->
           Some
             "ocamlformat-rpc is missing, displayed types might not be \
              properly formatted. Hint: $ opam install ocamlformat-rpc and \
              restart the lsp server"
         | Error `Disabled
         | Ok () ->
           None
       in
       match message with
       | None -> Fiber.return ()
       | Some message ->
         let* (_ : InitializeParams.t) = Server.initialized server in
         let state = Server.state server in
         task_if_running state.detached ~f:(fun () ->
             let log = ShowMessageParams.create ~type_:Info ~message in
             Server.notification server (Server_notification.ShowMessage log)))
    ]

let run () =
  Unix.putenv "__MERLIN_MASTER_PID" (string_of_int (Unix.getpid ()));
  Lev_fiber.run (Lev.Loop.default ()) ~f:start

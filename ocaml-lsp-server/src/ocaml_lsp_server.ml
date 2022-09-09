open Import
module Version = Version
module Diagnostics = Diagnostics
open Fiber.O

let make_error = Jsonrpc.Response.Error.make

let not_supported () =
  Jsonrpc.Response.Error.raise
    (make_error ~code:InternalError ~message:"Request not supported yet!" ())

let view_metrics_command_name = "ocamllsp/view-metrics"

let view_metrics server =
  let* json = Metrics.dump () in
  let uri, chan =
    Filename.open_temp_file (sprintf "lsp-metrics.%d" (Unix.getpid ())) ".json"
  in
  output_string chan json;
  close_out_noerr chan;
  let req =
    let uri = Uri.of_path uri in
    Server_request.ShowDocumentRequest
      (ShowDocumentParams.create ~uri ~takeFocus:true ())
  in
  let+ { ShowDocumentResult.success = _ } = Server.request server req in
  `Null

let initialize_info (client_capabilities : ClientCapabilities.t) :
    InitializeResult.t =
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
      (TextDocumentSyncOptions.create
         ~openClose:true
         ~change:TextDocumentSyncKind.Incremental
         ~willSave:false
         ~save:(`Bool true)
         ~willSaveWaitUntil:false
         ())
  in
  let codeLensProvider = CodeLensOptions.create ~resolveProvider:false () in
  let completionProvider =
    CompletionOptions.create
      ~triggerCharacters:[ "."; "#" ]
      ~resolveProvider:true
      ()
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
      WorkspaceFoldersServerCapabilities.create
        ~supported:true
        ~changeNotifications:(`Bool true)
        ()
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
      let commands =
        if
          Action_open_related.available
            (let open Option.O in
            let* window = client_capabilities.window in
            window.showDocument)
        then
          view_metrics_command_name :: Action_open_related.command_name
          :: Dune.commands
        else Dune.commands
      in
      ExecuteCommandOptions.create ~commands ()
    in
    let semanticTokensProvider =
      Option.map (Sys.getenv_opt "OCAMLLSP_SEMANTIC_HIGHLIGHTING") ~f:(fun v ->
          let delta = String.equal v "full/delta" in
          let full = `Full (SemanticTokensOptions.create_full ~delta ()) in
          `SemanticTokensOptions
            (SemanticTokensOptions.create
               ~legend:Semantic_highlighting.legend
               ~full
               ()))
    in
    ServerCapabilities.create
      ~textDocumentSync
      ~hoverProvider:(`Bool true)
      ~declarationProvider:(`Bool true)
      ~definitionProvider:(`Bool true)
      ~typeDefinitionProvider:(`Bool true)
      ~completionProvider
      ~signatureHelpProvider
      ~codeActionProvider
      ~codeLensProvider
      ~referencesProvider:(`Bool true)
      ~documentHighlightProvider:(`Bool true)
      ~documentFormattingProvider:(`Bool true)
      ~selectionRangeProvider:(`Bool true)
      ~documentSymbolProvider:(`Bool true)
      ~workspaceSymbolProvider:(`Bool true)
      ~foldingRangeProvider:(`Bool true)
      ?semanticTokensProvider
      ~experimental
      ~renameProvider
      ~workspace
      ~executeCommandProvider
      ()
  in
  let serverInfo =
    let version = Version.get () in
    InitializeResult.create_serverInfo ~name:"ocamllsp" ~version ()
  in
  InitializeResult.create ~capabilities ~serverInfo ()

let ocamlmerlin_reason = "ocamlmerlin-reason"

let set_diagnostics detached diagnostics doc =
  let uri = Document.uri doc in
  let async send =
    let+ () =
      task_if_running detached ~f:(fun () ->
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
  | Dune | Cram | Menhir | Ocamllex -> Fiber.return ()
  | Reason when Option.is_none (Bin.which ocamlmerlin_reason) ->
    let no_reason_merlin =
      let message =
        sprintf "Could not detect %s. Please install reason" ocamlmerlin_reason
      in
      Diagnostic.create
        ~source:Diagnostics.ocamllsp_source
        ~range:Range.first_line
        ~message
        ()
    in
    Diagnostics.set diagnostics (`Merlin (uri, [ no_reason_merlin ]));
    async (fun () -> Diagnostics.send diagnostics (`One uri))
  | Reason | Ocaml ->
    async (fun () ->
        let* () = Diagnostics.merlin_diagnostics diagnostics doc in
        Diagnostics.send diagnostics (`One uri))

let on_initialize server (ip : InitializeParams.t) =
  let state : State.t = Server.state server in
  let workspaces = Workspaces.create ip in
  let+ dune =
    let progress =
      Progress.create
        ip.capabilities
        ~report_progress:(fun progress ->
          Server.notification
            server
            (Server_notification.WorkDoneProgress progress))
        ~create_task:(fun task ->
          Server.request server (Server_request.WorkDoneProgressCreate task))
    in
    let dune =
      Dune.create
        workspaces
        ip.capabilities
        state.diagnostics
        progress
        state.store
        ~log:(State.log_msg server)
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
          let* () = send (initialize_info ip.capabilities) in
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
                              `DocumentFilter
                                (`TextDocumentFilter
                                  (TextDocumentFilter.create ~language ())))
                     in
                     TextDocumentRegistrationOptions.create ~documentSelector ()
                     |> TextDocumentRegistrationOptions.yojson_of_t
                   in
                   Registration.create ~id ~method_ ~registerOptions ()
                 in
                 [ make "textDocument/didOpen"; make "textDocument/didClose" ])
          in
          Server.request
            server
            (Server_request.ClientRegisterCapability register))
    | _ -> Reply.now (initialize_info ip.capabilities)
  in
  (resp, state)

module Formatter = struct
  let jsonrpc_error (e : Ocamlformat.error) =
    let message = Ocamlformat.message e in
    let code : Jsonrpc.Response.Error.Code.t =
      match e with
      | Unsupported_syntax _ | Unknown_extension _ | Missing_binary _ ->
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
        | Error _ ->
          let* cancel = Server.cancel_token () in
          Ocamlformat.run doc cancel
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
          sprintf
            "No dune instance found. Please run dune in watch mode for %s"
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
    else { MarkupContent.value = doc; kind = MarkupKind.PlainText })

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
    if Document.is_merlin doc then
      Document.with_pipeline_exn doc (fun pipeline ->
          let typer = Mpipeline.typer_result pipeline in
          let pos = Mpipeline.get_lexing_pos pipeline pos in
          let node = Mtyper.node_at typer pos in
          Merlin_analysis.Signature_help.application_signature node ~prefix)
    else Fiber.return None
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
        List.map
          application_signature.parameters
          ~f:(fun (p : Merlin_analysis.Signature_help.parameter_info) ->
            let label =
              `Offset (offset + p.param_start, offset + p.param_end)
            in
            ParameterInformation.create ~label ())
      in
      let documentation =
        let open Option.O in
        let+ doc in
        let markdown =
          ClientCapabilities.markdown_support
            (State.client_capabilities state)
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
    SignatureHelp.create
      ~signatures:[ info ]
      ~activeSignature:0
      ?activeParameter:application_signature.active_param
      ()

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
        List.concat_map
          item.Query_protocol.children
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
            TextEdit.create
              ~range:empty_range_at_occur_end
              ~newText:(":" ^ newName)
          | _ -> make_edit ()))
  in
  let workspace_edits =
    let documentChanges =
      let open Option.O in
      Option.value
        ~default:false
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
    else WorkspaceEdit.create ~changes:[ (uri, edits) ] ()
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
      let* cancel = Server.cancel_token () in
      let task =
        Lev_fiber.Thread.task thread ~f:(fun () ->
            Workspace_symbol.run params workspaces cancel)
      in
      let* res, cancel =
        match task with
        | Error `Stopped -> Fiber.never
        | Ok task ->
          let maybe_cancel =
            match cancel with
            | None ->
              fun f ->
                let+ res = f () in
                (res, Fiber.Cancel.Not_cancelled)
            | Some token ->
              let on_cancel () = Lev_fiber.Thread.cancel task in
              fun f -> Fiber.Cancel.with_handler token ~on_cancel f
          in
          maybe_cancel @@ fun () -> Lev_fiber.Thread.await task
      in
      match cancel with
      | Cancelled () ->
        let e =
          Jsonrpc.Response.Error.make
            ~code:RequestCancelled
            ~message:"cancelled"
            ()
        in
        raise (Jsonrpc.Response.Error.E e)
      | Fiber.Cancel.Not_cancelled -> (
        match res with
        | Ok (Ok s) -> Fiber.return s
        | Ok (Error `Cancelled) -> assert false
        | Error `Cancelled -> assert false
        | Error (`Exn exn) -> Exn_with_backtrace.reraise exn)
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
      ; ( Semantic_highlighting.Debug.meth_request_full
        , Semantic_highlighting.Debug.on_request_full )
      ]
      |> List.assoc_opt meth
    with
    | None ->
      Jsonrpc.Response.Error.raise
        (make_error
           ~code:MethodNotFound
           ~message:"Unknown method"
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
    if String.equal command.command view_metrics_command_name then
      later (fun _state server -> view_metrics server) server
    else if String.equal command.command Action_open_related.command_name then
      later
        (fun _state server -> Action_open_related.command_run server command)
        server
    else
      later
        (fun state () ->
          let dune = State.dune state in
          Dune.on_command dune command)
        ()
  | CompletionItemResolve ci ->
    later
      (fun state () ->
        let markdown =
          ClientCapabilities.markdown_support
            (State.client_capabilities state)
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
  | CodeAction params -> Code_actions.compute server params
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
  | TextDocumentPrepareRename
      { textDocument = { uri }; position; workDoneToken = _ } ->
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
  | TextDocumentImplementation _ -> not_supported ()
  | SemanticTokensFull p -> later Semantic_highlighting.on_request_full p
  | SemanticTokensDelta p -> later Semantic_highlighting.on_request_full_delta p
  | TextDocumentMoniker _ -> not_supported ()
  | TextDocumentPrepareCallHierarchy _ -> not_supported ()
  | TextDocumentRangeFormatting _ -> not_supported ()
  | CallHierarchyIncomingCalls _ -> not_supported ()
  | CallHierarchyOutgoingCalls _ -> not_supported ()
  | SemanticTokensRange _ -> not_supported ()
  | LinkedEditingRange _ -> not_supported ()
  | WillCreateFiles _ -> not_supported ()
  | WillRenameFiles _ -> not_supported ()
  | WillDeleteFiles _ -> not_supported ()

let on_notification server (notification : Client_notification.t) :
    State.t Fiber.t =
  let state : State.t = Server.state server in
  let store = state.store in
  match notification with
  | TextDocumentDidOpen params ->
    let* doc =
      Document.make
        (State.wheel state)
        state.merlin_config
        params
        ~merlin_thread:state.merlin
    in
    assert (Document_store.get_opt store params.textDocument.uri = None);
    let* () = Document_store.open_document store doc in
    let+ () = set_diagnostics state.detached state.diagnostics doc in
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
    let+ () = set_diagnostics state.detached state.diagnostics doc in
    state
  | CancelRequest _ ->
    Log.log ~section:"debug" (fun () -> Log.msg "ignoring cancellation" []);
    Fiber.return state
  | ChangeConfiguration req ->
    (* TODO this is wrong and we should just fetch the config from the client
       after receiving this notification *)
    let+ configuration = Configuration.update state.configuration req in
    { state with configuration }
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
      let+ () = set_diagnostics state.detached state.diagnostics doc in
      state)
  | ChangeWorkspaceFolders change ->
    let state =
      State.modify_workspaces state ~f:(fun ws ->
          Workspaces.on_change ws change)
    in
    Dune.update_workspaces (State.dune state) (State.workspaces state);
    Fiber.return state
  | DidChangeWatchedFiles _
  | DidCreateFiles _
  | DidDeleteFiles _
  | DidRenameFiles _
  | LogTrace _
  | WillSaveTextDocument _
  | Initialized
  | WorkDoneProgressCancel _
  | Exit -> Fiber.return state
  | SetTrace { value } -> Fiber.return { state with trace = value }
  | UnknownNotification req ->
    let+ () =
      State.log_msg
        server
        ~type_:Error
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
  let* configuration = Configuration.default () in
  let wheel = Configuration.wheel configuration in
  let* server =
    let+ merlin = Lev_fiber.Thread.create () in
    let symbols_thread = Lazy_fiber.create Lev_fiber.Thread.create in
    Fdecl.set
      server
      (Server.make
         handler
         stream
         (State.create
            ~store
            ~merlin
            ~ocamlformat_rpc
            ~configuration
            ~detached
            ~diagnostics
            ~symbols_thread
            ~wheel));
    Fdecl.get server
  in
  let state = Server.state server in
  let with_log_errors what f =
    let+ (_ : (unit, unit) result) =
      Fiber.map_reduce_errors
        (module Monoid.Unit)
        f
        ~on_error:(fun exn ->
          Format.eprintf "%s: %a@." what Exn_with_backtrace.pp_uncaught exn;
          Fiber.return ())
    in
    ()
  in
  let run_ocamlformat_rpc () =
    let* state =
      Ocamlformat_rpc.run ~logger:(State.log_msg server) ocamlformat_rpc
    in
    let message =
      match state with
      | Error `Binary_not_found ->
        Some
          "ocamlformat-rpc is missing, displayed types might not be properly \
           formatted. Hint: $ opam install ocamlformat-rpc and restart the lsp \
           server"
      | Error `Disabled | Ok () -> None
    in
    match message with
    | None -> Fiber.return ()
    | Some message ->
      let* (_ : InitializeParams.t) = Server.initialized server in
      let state = Server.state server in
      task_if_running state.detached ~f:(fun () ->
          let log = ShowMessageParams.create ~type_:Info ~message in
          Server.notification server (Server_notification.ShowMessage log))
  in
  let run () =
    Fiber.all_concurrently_unit
      [ with_log_errors "detached" (fun () -> Fiber.Pool.run detached)
      ; Lev_fiber.Timer.Wheel.run wheel
      ; with_log_errors "merlin" (fun () ->
            Merlin_config.DB.run state.merlin_config)
      ; (let* () = Server.start server in
         let finalize =
           [ Document_store.close_all store
           ; Fiber.Pool.stop detached
           ; Ocamlformat_rpc.stop ocamlformat_rpc
           ; Lev_fiber.Timer.Wheel.stop wheel
           ; Merlin_config.DB.stop state.merlin_config
           ]
         in
         let finalize =
           match (Server.state server).init with
           | Uninitialized -> finalize
           | Initialized init -> Dune.stop init.dune :: finalize
         in
         Fiber.all_concurrently_unit finalize)
      ; with_log_errors "ocamlformat-rpc" run_ocamlformat_rpc
      ]
  in
  let metrics = Metrics.create () in
  Metrics.with_metrics metrics run

let run ~read_dot_merlin () =
  Merlin_config.should_read_dot_merlin := read_dot_merlin;
  Unix.putenv "__MERLIN_MASTER_PID" (string_of_int (Unix.getpid ()));
  Lev_fiber.run ~sigpipe:`Ignore start |> Lev_fiber.Error.ok_exn

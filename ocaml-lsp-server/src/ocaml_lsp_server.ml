open Import
module Version = Version
module Diagnostics = Diagnostics
module Position = Position
module Doc_to_md = Doc_to_md
module Diff = Diff
module Testing = Testing
open Fiber.O

let make_error = Jsonrpc.Response.Error.make

let not_supported () =
  Jsonrpc.Response.Error.raise
    (make_error ~code:InternalError ~message:"Request not supported yet!" ())
;;

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
    Server_request.ShowDocumentRequest (ShowDocumentParams.create ~uri ~takeFocus:true ())
  in
  let+ { ShowDocumentResult.success = _ } = Server.request server req in
  `Null
;;

let initialize_info (client_capabilities : ClientCapabilities.t) : InitializeResult.t =
  let codeActionProvider =
    match client_capabilities.textDocument with
    | Some { codeAction = Some { codeActionLiteralSupport = Some _; _ }; _ } ->
      let codeActionKinds =
        Action_inferred_intf.kind
        :: Action_destruct.kind
        :: List.map
             ~f:(fun (c : Code_action.t) -> c.kind)
             [ Action_type_annotate.t
             ; Action_remove_type_annotation.t
             ; Action_construct.t
             ; Action_refactor_open.unqualify
             ; Action_refactor_open.qualify
             ; Action_add_rec.t
             ; Action_inline.t
             ]
        |> List.sort_uniq ~compare:Poly.compare
      in
      `CodeActionOptions (CodeActionOptions.create ~codeActionKinds ())
    | _ -> `Bool true
  in
  let textDocumentSync =
    `TextDocumentSyncOptions
      (TextDocumentSyncOptions.create
         ~openClose:true
         ~change:TextDocumentSyncKind.Incremental
         ~willSave:false
         ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
         ~willSaveWaitUntil:false
         ())
  in
  let codeLensProvider = CodeLensOptions.create ~resolveProvider:false () in
  let completionProvider =
    CompletionOptions.create ~triggerCharacters:[ "."; "#" ] ~resolveProvider:true ()
  in
  let signatureHelpProvider =
    SignatureHelpOptions.create ~triggerCharacters:[ " "; "~"; "?"; ":"; "(" ] ()
  in
  let renameProvider = `RenameOptions (RenameOptions.create ~prepareProvider:true ()) in
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
              [ "interfaceSpecificLangId", `Bool true
              ; Req_switch_impl_intf.capability
              ; Req_infer_intf.capability
              ; Req_typed_holes.capability
              ; Req_wrapping_ast_node.capability
              ; Dune.view_promotion_capability
              ; Req_hover_extended.capability
              ; Req_merlin_call_compatible.capability
              ; Req_type_enclosing.capability
              ; Req_get_documentation.capability
              ] )
        ]
    in
    let executeCommandProvider =
      let commands =
        if Action_open_related.available
             (let open Option.O in
              let* window = client_capabilities.window in
              window.showDocument)
        then
          view_metrics_command_name
          :: Action_open_related.command_name
          :: Document_text_command.command_name
          :: Merlin_config_command.command_name
          :: Dune.commands
        else Dune.commands
      in
      ExecuteCommandOptions.create ~commands ()
    in
    let semanticTokensProvider =
      let full = `Full (SemanticTokensOptions.create_full ~delta:true ()) in
      `SemanticTokensOptions
        (SemanticTokensOptions.create ~legend:Semantic_highlighting.legend ~full ())
    in
    let positionEncoding =
      let open Option.O in
      let* general = client_capabilities.general in
      let* options = general.positionEncodings in
      List.find_map
        ([ UTF8; UTF16 ] : PositionEncodingKind.t list)
        ~f:(fun encoding ->
          Option.some_if (List.mem options ~equal:Poly.equal encoding) encoding)
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
      ~semanticTokensProvider
      ~experimental
      ~renameProvider
      ~inlayHintProvider:(`Bool true)
      ~workspace
      ~executeCommandProvider
      ?positionEncoding
      ()
  in
  let serverInfo =
    let version = Version.get () in
    InitializeResult.create_serverInfo ~name:"ocamllsp" ~version ()
  in
  InitializeResult.create ~capabilities ~serverInfo ()
;;

let ocamlmerlin_reason = "ocamlmerlin-reason"

let set_diagnostics detached diagnostics doc =
  let uri = Document.uri doc in
  match Document.kind doc with
  | `Other -> Fiber.return ()
  | `Merlin merlin ->
    let async send =
      let+ () =
        task_if_running detached ~f:(fun () ->
          let timer = Document.Merlin.timer merlin in
          let* () = Lev_fiber.Timer.Wheel.cancel timer in
          let* () = Lev_fiber.Timer.Wheel.reset timer in
          let* res = Lev_fiber.Timer.Wheel.await timer in
          match res with
          | `Cancelled -> Fiber.return ()
          | `Ok -> send ())
      in
      ()
    in
    (match Document.syntax doc with
     | Dune | Cram | Menhir | Ocamllex -> Fiber.return ()
     | Reason when Option.is_none (Bin.which ocamlmerlin_reason) ->
       let no_reason_merlin =
         let message =
           `String
             (sprintf "Could not detect %s. Please install reason" ocamlmerlin_reason)
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
         let* () = Diagnostics.merlin_diagnostics diagnostics merlin in
         Diagnostics.send diagnostics (`One uri)))
;;

let on_initialize server (ip : InitializeParams.t) =
  let state : State.t = Server.state server in
  let workspaces = Workspaces.create ip in
  let diagnostics =
    let report_dune_diagnostics =
      Configuration.report_dune_diagnostics state.configuration
    in
    Diagnostics.create
      ~report_dune_diagnostics
      (let open Option.O in
       let* td = ip.capabilities.textDocument in
       td.publishDiagnostics)
      (function
        | [] -> Fiber.return ()
        | diagnostics ->
          let state = Server.state server in
          task_if_running state.detached ~f:(fun () ->
            let batch = Server.Batch.create server in
            List.iter diagnostics ~f:(fun d ->
              Server.Batch.notification batch (PublishDiagnostics d));
            Server.Batch.submit batch))
  in
  let+ dune =
    let progress =
      Progress.create
        ip.capabilities
        ~report_progress:(fun progress ->
          Server.notification server (Server_notification.WorkDoneProgress progress))
        ~create_task:(fun task ->
          Server.request server (Server_request.WorkDoneProgressCreate task))
    in
    let dune =
      Dune.create
        workspaces
        ip.capabilities
        diagnostics
        progress
        state.store
        ~log:(State.log_msg server)
    in
    let+ () = Fiber.Pool.task state.detached ~f:(fun () -> Dune.run dune) in
    dune
  in
  let initialize_info = initialize_info ip.capabilities in
  let state =
    let position_encoding =
      match initialize_info.capabilities.positionEncoding with
      | None | Some UTF16 -> `UTF16
      | Some UTF8 -> `UTF8
      | Some UTF32 | Some (Other _) -> assert false
    in
    State.initialize state ~position_encoding ip workspaces dune diagnostics
  in
  let state =
    match ip.trace with
    | None -> state
    | Some trace -> { state with trace }
  in
  let resp =
    match ip.capabilities.textDocument with
    | Some
        { TextDocumentClientCapabilities.synchronization =
            Some { TextDocumentSyncClientCapabilities.dynamicRegistration = Some true; _ }
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
                       `TextDocumentFilter (TextDocumentFilter.create ~language ()))
                   in
                   TextDocumentRegistrationOptions.create ~documentSelector ()
                   |> TextDocumentRegistrationOptions.yojson_of_t
                 in
                 Registration.create ~id ~method_ ~registerOptions ()
               in
               [ make "textDocument/didOpen"; make "textDocument/didClose" ])
        in
        Server.request server (Server_request.ClientRegisterCapability register))
    | _ -> Reply.now initialize_info
  in
  resp, state
;;

module Formatter = struct
  let jsonrpc_error (e : Ocamlformat.error) =
    let message = Ocamlformat.message e in
    let code : Jsonrpc.Response.Error.Code.t =
      match e with
      | Unsupported_syntax _ | Unknown_extension _ | Missing_binary _ -> InvalidRequest
      | Unexpected_result _ -> InternalError
    in
    make_error ~code ~message ()
  ;;

  let run rpc doc =
    let state : State.t = Server.state rpc in
    match Document.kind doc with
    | `Merlin _ ->
      let* res =
        let* cancel = Server.cancel_token () in
        Ocamlformat.run doc cancel
      in
      (match res with
       | Ok result -> Fiber.return (Some result)
       | Error e ->
         let+ () =
           let state : State.t = Server.state rpc in
           let msg =
             let message = Ocamlformat.message e in
             ShowMessageParams.create ~message ~type_:Warning
           in
           task_if_running state.detached ~f:(fun () ->
             Server.notification rpc (ShowMessage msg))
         in
         Jsonrpc.Response.Error.raise (jsonrpc_error e))
    | `Other ->
      (match Dune.for_doc (State.dune state) doc with
       | [] ->
         let message =
           sprintf
             "No dune instance found. Please run dune in watch mode for %s"
             (Uri.to_path (Document.uri doc))
         in
         Jsonrpc.Response.Error.raise (make_error ~code:InvalidRequest ~message ())
       | dune :: rest ->
         let* () =
           match rest with
           | [] -> Fiber.return ()
           | _ :: _ ->
             let message =
               sprintf
                 "More than one dune instance detected for %s. Selecting one at random"
                 (Uri.to_path (Document.uri doc))
             in
             State.log_msg rpc ~type_:MessageType.Warning ~message
         in
         let+ to_ = Dune.Instance.format_dune_file dune doc in
         Some (Diff.edit ~from:(Document.text doc) ~to_))
  ;;
end

let text_document_lens (state : State.t) { CodeLensParams.textDocument = { uri }; _ } =
  let store = state.store in
  let doc = Document_store.get store uri in
  match Document.kind doc with
  | `Other -> Fiber.return []
  | `Merlin m when Document.Merlin.kind m = Intf -> Fiber.return []
  | `Merlin doc ->
    let+ outline = Document.Merlin.dispatch_exn ~name:"outline" doc Outline in
    let rec symbol_info_of_outline_item (item : Query_protocol.item) =
      let children = List.concat_map item.children ~f:symbol_info_of_outline_item in
      match item.outline_type with
      | None -> children
      | Some typ ->
        let loc = item.location in
        let info =
          let range = Range.of_loc loc in
          let command = Command.create ~title:typ ~command:"" () in
          CodeLens.create ~range ~command ()
        in
        info :: children
    in
    List.concat_map ~f:symbol_info_of_outline_item outline
;;

let selection_range
  (state : State.t)
  { SelectionRangeParams.textDocument = { uri }; positions; _ }
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return []
  | `Merlin merlin ->
    let selection_range_of_shapes
      (cursor_position : Position.t)
      (shapes : Query_protocol.shape list)
      : SelectionRange.t option
      =
      let rec ranges_of_shape parent (s : Query_protocol.shape) =
        let selectionRange =
          let range = Range.of_loc s.shape_loc in
          { SelectionRange.range; parent }
        in
        match s.shape_sub with
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
          match inc r1, inc r2 with
          | `Outside x, `Outside y -> Position.compare x y
          | `Outside _, `Inside -> Gt
          | `Inside, `Outside _ -> Lt
          | `Inside, `Inside -> Range.compare_size r1.range r2.range)
      in
      nearest_range
    in
    let+ ranges =
      Fiber.sequential_map positions ~f:(fun x ->
        let+ shapes =
          Document.Merlin.dispatch_exn ~name:"shape" merlin (Shape (Position.logical x))
        in
        selection_range_of_shapes x shapes)
    in
    List.filter_opt ranges
;;

let references
  rpc
  (state : State.t)
  { ReferenceParams.textDocument = { uri }; position; _ }
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin doc ->
    let* locs, synced =
      Document.Merlin.dispatch_exn
        ~name:"occurrences"
        doc
        (Occurrences (`Ident_at (Position.logical position), `Project))
    in
    let+ () =
      match synced with
      | `Out_of_sync _ ->
        let msg =
          let message =
            "The index might be out-of-sync.  If you use Dune you can build the target \
             `@ocaml-index` to refresh the index."
          in
          ShowMessageParams.create ~message ~type_:Warning
        in
        task_if_running state.detached ~f:(fun () ->
          Server.notification rpc (ShowMessage msg))
      | _ -> Fiber.return ()
    in
    Some
      (List.map locs ~f:(fun loc ->
         let range = Range.of_loc loc in
         let uri =
           match loc.loc_start.pos_fname with
           | "" -> uri
           | path -> Uri.of_path path
         in
         Log.log ~section:"debug" (fun () ->
           Log.msg
             "merlin returned fname %a"
             [ "pos_fname", `String loc.loc_start.pos_fname
             ; "uri", `String (Uri.to_string uri)
             ]);
         { Location.uri; range }))
;;

let highlight
  (state : State.t)
  { DocumentHighlightParams.textDocument = { uri }; position; _ }
  =
  let store = state.store in
  let doc = Document_store.get store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin m ->
    let+ locs, _synced =
      Document.Merlin.dispatch_exn
        ~name:"occurrences"
        m
        (Occurrences (`Ident_at (Position.logical position), `Buffer))
    in
    let lsp_locs =
      List.filter_map locs ~f:(fun loc ->
        let range = Range.of_loc loc in
        (* filter out multi-line ranges, since those are very noisy and happen
           a lot with certain PPXs *)
        match range.start.line = range.end_.line with
        | true ->
          (* using the default kind as we are lacking info to make a
             difference between assignment and usage. *)
          Some (DocumentHighlight.create ~range ~kind:DocumentHighlightKind.Text ())
        | false -> None)
    in
    Some lsp_locs
;;

let document_symbol (state : State.t) uri =
  let doc =
    let store = state.store in
    Document_store.get store uri
  in
  let client_capabilities = State.client_capabilities state in
  Document_symbol.run client_capabilities doc uri
;;

let on_request
  : type resp.
    State.t Server.t -> resp Client_request.t -> (resp Reply.t * State.t) Fiber.t
  =
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
  | Client_request.UnknownRequest { meth; params } ->
    (match
       [ ( Req_switch_impl_intf.meth
         , fun ~params state ->
             Fiber.of_thunk (fun () ->
               Fiber.return (Req_switch_impl_intf.on_request ~params state)) )
       ; Req_infer_intf.meth, Req_infer_intf.on_request
       ; Req_typed_holes.meth, Req_typed_holes.on_request
       ; Req_merlin_call_compatible.meth, Req_merlin_call_compatible.on_request
       ; Req_type_enclosing.meth, Req_type_enclosing.on_request
       ; Req_get_documentation.meth, Req_get_documentation.on_request
       ; Req_wrapping_ast_node.meth, Req_wrapping_ast_node.on_request
       ; ( Semantic_highlighting.Debug.meth_request_full
         , Semantic_highlighting.Debug.on_request_full )
       ; ( Req_hover_extended.meth
         , fun ~params _ -> Req_hover_extended.on_request ~params rpc )
       ]
       |> List.assoc_opt meth
     with
     | None ->
       Jsonrpc.Response.Error.raise
         (make_error
            ~code:MethodNotFound
            ~message:"Unknown method"
            ~data:(`Assoc [ "method", `String meth ])
            ())
     | Some handler ->
       Fiber.return
         ( Reply.later (fun send ->
             let* res = handler ~params state in
             send res)
         , state ))
  | Initialize ip ->
    let+ res, state = on_initialize server ip in
    res, state
  | DebugTextDocumentGet { textDocument = { uri }; position = _ } ->
    (match Document_store.get_opt store uri with
     | None -> now None
     | Some doc -> now (Some (Msource.text (Document.source doc))))
  | DebugEcho params -> now params
  | Shutdown -> Fiber.return (Reply.now (), state)
  | WorkspaceSymbol req ->
    later (fun state () -> Workspace_symbol.run server state req) ()
  | CodeActionResolve ca -> now ca
  | ExecuteCommand command ->
    if String.equal command.command Merlin_config_command.command_name
    then
      later
        (fun state server ->
          let store = state.store in
          let+ () = Merlin_config_command.command_run server store in
          `Null)
        server
    else if String.equal command.command Document_text_command.command_name
    then
      later
        (fun state server ->
          let store = state.store in
          let+ () = Document_text_command.command_run server store command.arguments in
          `Null)
        server
    else if String.equal command.command view_metrics_command_name
    then later (fun _state server -> view_metrics server) server
    else if String.equal command.command Action_open_related.command_name
    then
      later (fun _state server -> Action_open_related.command_run server command) server
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
          (match Document.kind doc with
           | `Other -> Fiber.return ci
           | `Merlin doc ->
             Compl.resolve
               doc
               ci
               resolve
               (Document.Merlin.doc_comment ~name:"completion-resolve")
               ~markdown))
      ()
  | CodeAction params -> Code_actions.compute server params
  | InlayHint params -> later (fun state () -> Inlay_hints.compute state params) ()
  | TextDocumentColor _ -> now []
  | TextDocumentColorPresentation _ -> now []
  | TextDocumentHover req ->
    let mode =
      match state.configuration.data.extended_hover with
      | Some { enable = true } -> Hover_req.Extended_variable
      | Some _ | None -> Hover_req.Default
    in
    later (fun (_ : State.t) () -> Hover_req.handle rpc req mode) ()
  | TextDocumentReferences req -> later (references rpc) req
  | TextDocumentCodeLensResolve codeLens -> now codeLens
  | TextDocumentCodeLens req ->
    (match state.configuration.data.codelens with
     | Some { enable = true } -> later text_document_lens req
     | _ -> now [])
  | TextDocumentHighlight req -> later highlight req
  | DocumentSymbol { textDocument = { uri }; _ } -> later document_symbol uri
  | TextDocumentDeclaration { textDocument = { uri }; position } ->
    later (fun state () -> Definition_query.run `Declaration state uri position) ()
  | TextDocumentDefinition { textDocument = { uri }; position; _ } ->
    later (fun state () -> Definition_query.run `Definition state uri position) ()
  | TextDocumentTypeDefinition { textDocument = { uri }; position; _ } ->
    later (fun state () -> Definition_query.run `Type_definition state uri position) ()
  | TextDocumentCompletion params -> later (fun _ () -> Compl.complete state params) ()
  | TextDocumentPrepareRename { textDocument = { uri }; position; workDoneToken = _ } ->
    later
      (fun _ () ->
        let doc = Document_store.get store uri in
        match Document.kind doc with
        | `Other -> Fiber.return None
        | `Merlin doc ->
          let+ locs, _synced =
            Document.Merlin.dispatch_exn
              ~name:"occurrences"
              doc
              (Occurrences (`Ident_at (Position.logical position), `Buffer))
          in
          let loc =
            List.find_opt locs ~f:(fun loc ->
              let range = Range.of_loc loc in
              Position.compare_inclusion position range = `Inside)
          in
          Option.map loc ~f:Range.of_loc)
      ()
  | TextDocumentRename req -> later Rename.rename req
  | TextDocumentFoldingRange req -> later Folding_range.compute req
  | SignatureHelp req -> later Signature_help.run req
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
  | InlayHintResolve _ -> not_supported ()
  | TextDocumentDiagnostic _ -> not_supported ()
  | TextDocumentInlineCompletion _ -> not_supported ()
  | TextDocumentInlineValue _ -> not_supported ()
  | WorkspaceSymbolResolve _ -> not_supported ()
  | WorkspaceDiagnostic _ -> not_supported ()
  | TextDocumentRangesFormatting _ -> not_supported ()
  | TextDocumentPrepareTypeHierarchy _ -> not_supported ()
  | TypeHierarchySupertypes _ -> not_supported ()
  | TypeHierarchySubtypes _ -> not_supported ()
;;

let on_notification server (notification : Client_notification.t) : State.t Fiber.t =
  let state : State.t = Server.state server in
  let store = state.store in
  match notification with
  | TextDocumentDidOpen params ->
    let* doc =
      let position_encoding = State.position_encoding state in
      Document.make
        ~position_encoding
        (State.wheel state)
        state.merlin_config
        state.merlin
        params
    in
    let* () = Document_store.open_document store doc in
    let+ () = set_diagnostics state.detached (State.diagnostics state) doc in
    state
  | TextDocumentDidClose { textDocument = { uri } } ->
    let+ () =
      Diagnostics.remove (State.diagnostics state) (`Merlin uri);
      let* () = Document_store.close_document store uri in
      task_if_running state.detached ~f:(fun () ->
        Diagnostics.send (State.diagnostics state) (`One uri))
    in
    state
  | TextDocumentDidChange { textDocument = { uri; version }; contentChanges } ->
    let doc =
      Document_store.change_document store uri ~f:(fun prev_doc ->
        Document.update_text ~version prev_doc contentChanges)
    in
    let+ () = set_diagnostics state.detached (State.diagnostics state) doc in
    state
  | CancelRequest _ -> Fiber.return state
  | ChangeConfiguration req ->
    let* configuration = Configuration.update state.configuration req in
    let+ () =
      let report_dune_diagnostics = Configuration.report_dune_diagnostics configuration in
      Diagnostics.set_report_dune_diagnostics
        ~report_dune_diagnostics
        (State.diagnostics state)
    in
    { state with configuration }
  | DidSaveTextDocument { textDocument = { uri }; _ } ->
    let state = Server.state server in
    (match Document_store.get_opt state.store uri with
     | None ->
       (Log.log ~section:"on receive DidSaveTextDocument"
        @@ fun () -> Log.msg "saved document is not in the store" []);
       Fiber.return state
     | Some doc ->
       let+ () = set_diagnostics state.detached (State.diagnostics state) doc in
       state)
  | ChangeWorkspaceFolders change ->
    let state =
      State.modify_workspaces state ~f:(fun ws -> Workspaces.on_change ws change)
    in
    Dune.update_workspaces (State.dune state) (State.workspaces state);
    Fiber.return state
  | DidChangeWatchedFiles _
  | DidCreateFiles _
  | DidDeleteFiles _
  | DidRenameFiles _
  | WillSaveTextDocument _
  | Initialized
  | WorkDoneProgressCancel _
  | WorkDoneProgress _
  | NotebookDocumentDidOpen _
  | NotebookDocumentDidChange _
  | NotebookDocumentDidSave _
  | NotebookDocumentDidClose _
  | Exit -> Fiber.return state
  | SetTrace { value } -> Fiber.return { state with trace = value }
  | UnknownNotification req ->
    let+ () =
      State.log_msg server ~type_:Error ~message:("Unknown notication " ^ req.method_)
    in
    state
;;

let start stream =
  let detached = Fiber.Pool.create () in
  let server = Fdecl.create Dyn.opaque in
  let store = Document_store.make server detached in
  let handler =
    let on_request = { Server.Handler.on_request } in
    Server.Handler.make ~on_request ~on_notification ()
  in
  let ocamlformat_rpc = Ocamlformat_rpc.create () in
  let* configuration = Configuration.default () in
  let wheel = Configuration.wheel configuration in
  let* merlin = Lev_fiber.Thread.create () in
  let server =
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
    let* state = Ocamlformat_rpc.run ~logger:(State.log_msg server) ocamlformat_rpc in
    let message =
      match state with
      | Error `Binary_not_found ->
        Some
          "Unable to find 'ocamlformat-rpc' binary. Types on hover may not be \
           well-formatted. You need to install either 'ocamlformat' of version > 0.21.0 \
           or, otherwise, 'ocamlformat-rpc' package."
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
      ; with_log_errors "merlin" (fun () -> Merlin_config.DB.run state.merlin_config)
      ; (let* () = Server.start server in
         let finalize =
           [ Document_store.close_all store
           ; Fiber.Pool.stop detached
           ; Ocamlformat_rpc.stop ocamlformat_rpc
           ; Lev_fiber.Timer.Wheel.stop wheel
           ; Merlin_config.DB.stop state.merlin_config
           ; Fiber.of_thunk (fun () ->
               Lev_fiber.Thread.close merlin;
               Fiber.return ())
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
;;

let socket sockaddr =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let fd =
    Lev_fiber.Fd.create
      (Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0)
      (`Non_blocking false)
  in
  let* () = Lev_fiber.Socket.connect fd sockaddr in
  Lev_fiber.Io.create_rw fd
;;

let stream_of_channel : Lsp.Cli.Channel.t -> _ = function
  | Stdio ->
    let* stdin = Lev_fiber.Io.stdin in
    let+ stdout = Lev_fiber.Io.stdout in
    stdin, stdout
  | Pipe path ->
    if Sys.win32
    then (
      Format.eprintf "windows pipes are not supported";
      exit 1)
    else (
      let sockaddr = Unix.ADDR_UNIX path in
      socket sockaddr)
  | Socket port ->
    let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    socket sockaddr
;;

(* Merlin uses [Sys.command] to run preprocessors and ppxes. We provide an
   alternative version using the Spawn library for unixes.

   TODO: Currently PPX config is passed to Merlin in the form of a quoted shell
   command. The [prog_is_quoted] argument in Merlin's API is meant to allow
   supporting a way to launch ppx executables without using the shell.

   This will require additionnal changes of the API so there is no need to deal
   with the [prog_is_quoted] argument until this happen. *)
let run_in_directory ~prog ~prog_is_quoted:_ ~args ~cwd ?stdin ?stdout ?stderr () =
  (* Currently we assume that [prog] is always quoted and might contain
     arguments such as [-as-ppx]. This is due to the way Merlin gets its
     configuration. Thus we cannot rely on [Filename.quote_command]. *)
  let args = String.concat ~sep:" " @@ List.map ~f:Filename.quote args in
  let cmd = Format.sprintf "%s %s" prog args in
  let prog = "/bin/sh" in
  let argv = [ "sh"; "-c"; cmd ] in
  let stdin =
    match stdin with
    | Some file -> Unix.openfile file [ Unix.O_RDONLY ] 0o664
    | None -> Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0o777
  in
  let stdout, should_close_stdout =
    match stdout with
    | Some file -> Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT ] 0o664, true
    | None ->
      (* Runned programs should never output to stdout since it is the channel
         used by LSP to communicate with the editor *)
      Unix.stderr, false
  in
  let stderr =
    Option.map stderr ~f:(fun file ->
      Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT ] 0o664)
  in
  let pid =
    let cwd : Spawn.Working_dir.t = Path cwd in
    Spawn.spawn ~cwd ~prog ~argv ~stdin ~stdout ?stderr ()
  in
  let _, status = Unix.waitpid [] pid in
  let res =
    match (status : Unix.process_status) with
    | WEXITED n -> n
    | WSIGNALED _ -> -1
    | WSTOPPED _ -> -1
  in
  Unix.close stdin;
  if should_close_stdout then Unix.close stdout;
  `Finished res
;;

let run_in_directory =
  (* Merlin has specific stubs for Windows, we reuse them *)
  let for_windows = !Merlin_utils.Std.System.run_in_directory in
  fun () -> if Sys.win32 then for_windows else run_in_directory
;;

let run channel ~read_dot_merlin () =
  Merlin_utils.Lib_config.set_program_name "ocamllsp";
  Merlin_utils.Lib_config.System.set_run_in_directory (run_in_directory ());
  Merlin_config.should_read_dot_merlin := read_dot_merlin;
  Unix.putenv "__MERLIN_MASTER_PID" (string_of_int (Unix.getpid ()));
  Lev_fiber.run ~sigpipe:`Ignore (fun () ->
    let* input, output = stream_of_channel channel in
    start (Lsp_fiber.Fiber_io.make input output))
  |> Lev_fiber.Error.ok_exn
;;

module Custom_request = Custom_request

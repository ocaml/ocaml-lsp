open Import

let make_error = Lsp.Jsonrpc.Response.Error.make

let not_supported () =
  Error
    (make_error ~code:InternalError ~message:"Request not supported yet!" ())

module Action = struct
  let destruct = "destruct"
end

let initializeInfo : InitializeResult.t =
  let codeActionProvider =
    `CodeActionOptions
      (CodeActionOptions.create ~codeActionKinds:[ Other Action.destruct ] ())
  in
  let textDocumentSync =
    `TextDocumentSyncOptions
      (TextDocumentSyncOptions.create ~openClose:true
         ~change:TextDocumentSyncKind.Incremental ~willSave:false
         ~willSaveWaitUntil:false ())
  in
  let codeLensProvider = CodeLensOptions.create ~resolveProvider:false () in
  let completionProvider =
    (* TODO even if this re-enabled in general, it should stay disabled for
       emacs. It makes completion too slow *)
    CompletionOptions.create ~triggerCharacters:[ "."; "#" ]
      ~resolveProvider:false ()
  in
  let renameProvider =
    `RenameOptions (Lsp.Types.RenameOptions.create ~prepareProvider:true ())
  in
  let capabilities =
    let experimental =
      `Assoc
        [ ("ocamllsp", `Assoc [ ("interfaceSpecificLangId", `Bool true) ]) ]
    in
    ServerCapabilities.create ~textDocumentSync ~hoverProvider:(`Bool true)
      ~definitionProvider:(`Bool true) ~typeDefinitionProvider:(`Bool true)
      ~completionProvider ~codeActionProvider ~codeLensProvider
      ~referencesProvider:(`Bool true) ~documentHighlightProvider:(`Bool true)
      ~documentFormattingProvider:(`Bool true)
      ~selectionRangeProvider:(`Bool true) ~documentSymbolProvider:(`Bool true)
      ~foldingRangeProvider:(`Bool true) ~experimental ~renameProvider ()
  in
  let serverInfo =
    let version = Version.get () in
    InitializeResult.create_serverInfo ~name:"ocamllsp" ~version ()
  in
  InitializeResult.create ~capabilities ~serverInfo ()

let ocamlmerlin_reason = "ocamlmerlin-reason"

let send_diagnostics rpc doc =
  let diagnostic_create = Diagnostic.create ~source:"ocamllsp" in
  let reason_merlin_available =
    match Document.syntax doc with
    | Ocaml -> true
    | Reason -> Option.is_some (Bin.which ocamlmerlin_reason)
  in
  let uri = Document.uri doc |> Lsp.Uri.to_string in
  match reason_merlin_available with
  | false ->
    let notif =
      let diagnostics =
        let message =
          sprintf "Could not detect %s. Please install reason"
            ocamlmerlin_reason
        in
        let range =
          let pos = Position.create ~line:1 ~character:1 in
          Range.create ~start:pos ~end_:pos
        in
        [ diagnostic_create ~range ~message () ]
      in
      Lsp.Server_notification.PublishDiagnostics
        (PublishDiagnosticsParams.create ~uri ~diagnostics ())
    in
    Lsp.Server.send_notification rpc notif
  | true ->
    let command =
      Query_protocol.Errors { lexing = true; parsing = true; typing = true }
    in
    Document.with_pipeline doc @@ fun pipeline ->
    let errors = Query_commands.dispatch pipeline command in
    let diagnostics =
      List.map errors ~f:(fun (error : Loc.error) ->
          let loc = Loc.loc_of_report error in
          let range = Range.of_loc loc in
          let severity =
            match error.source with
            | Warning -> DiagnosticSeverity.Warning
            | _ -> DiagnosticSeverity.Error
          in
          let message =
            Loc.print_main Format.str_formatter error;
            String.trim (Format.flush_str_formatter ())
          in
          diagnostic_create ~range ~message ~severity ())
    in

    let notif =
      Lsp.Server_notification.PublishDiagnostics
        (PublishDiagnosticsParams.create ~uri ~diagnostics ())
    in

    Lsp.Server.send_notification rpc notif

let on_initialize rpc state _params =
  let log_consumer (section, title, text) =
    if title <> Logger.Title.LocalDebug then
      let type_, text =
        match title with
        | Error -> (MessageType.Error, text)
        | Warning -> (Warning, text)
        | Info -> (Info, text)
        | Debug -> (Log, Printf.sprintf "debug: %s" text)
        | Notify -> (Log, Printf.sprintf "notify: %s" text)
        | Custom s -> (Log, Printf.sprintf "%s: %s" s text)
        | LocalDebug -> failwith "impossible"
      in
      let message = Printf.sprintf "[%s] %s" section text in
      let notif = Lsp.Server_notification.LogMessage { message; type_ } in
      Lsp.Server.send_notification rpc notif
  in
  Logger.register_consumer log_consumer;
  Ok (state, initializeInfo)

let code_action_of_case_analysis uri (loc, newText) =
  let edit : WorkspaceEdit.t =
    let textedit : TextEdit.t = { range = Range.of_loc loc; newText } in
    let uri = Lsp.Uri.to_string uri in
    WorkspaceEdit.create ~changes:[ (uri, [ textedit ]) ] ()
  in
  let title = String.capitalize_ascii Action.destruct in
  CodeAction.create ~title ~kind:(CodeActionKind.Other Action.destruct) ~edit
    ~isPreferred:false ()

let code_action store (params : CodeActionParams.t) =
  let open Lsp.Import.Result.O in
  match params.context.only with
  | Some set when not (List.mem (CodeActionKind.Other Action.destruct) ~set) ->
    Ok (store, None)
  | Some _
  | None ->
    let uri = Lsp.Uri.t_of_yojson (`String params.textDocument.uri) in
    let* doc = Document_store.get store uri in
    let command =
      let start = Position.logical params.range.start in
      let finish = Position.logical params.range.end_ in
      Query_protocol.Case_analysis (start, finish)
    in
    let result : CodeActionResult.t =
      try
        let res = Document.dispatch doc command in
        Some [ `CodeAction (code_action_of_case_analysis uri res) ]
      with
      | Destruct.Wrong_parent _
      | Query_commands.No_nodes
      | Destruct.Not_allowed _
      | Destruct.Useless_refine
      | Destruct.Nothing_to_do ->
        Some []
    in
    Ok (store, result)

module Formatter = struct
  let jsonrpc_error (e : Fmt.error) =
    let message = Fmt.message e in
    let code : Lsp.Jsonrpc.Response.Error.Code.t =
      match e with
      | Missing_binary _ -> InvalidRequest
      | Unexpected_result _ -> InternalError
      | Unknown_extension _ -> InvalidRequest
    in
    make_error ~code ~message ()

  let run rpc store doc =
    match Fmt.run doc with
    | Result.Error e ->
      let message = Fmt.message e in
      let error = jsonrpc_error e in
      let msg = ShowMessageParams.create ~message ~type_:Error in
      Lsp.Server.send_notification rpc (ShowMessage msg);
      Error error
    | Result.Ok result ->
      let pos line col = { Position.character = col; line } in
      let range =
        let start_pos = pos 0 0 in
        match Msource.get_logical (Document.source doc) `End with
        | `Logical (l, c) ->
          let end_pos = pos l c in
          { Range.start = start_pos; end_ = end_pos }
      in
      let change = { TextEdit.newText = result; range } in
      Ok (store, Some [ change ])
end

let on_request :
    type resp.
       Lsp.Server.t
    -> Document_store.t
    -> ClientCapabilities.t
    -> resp Lsp.Client_request.t
    -> (Document_store.t * resp, Lsp.Jsonrpc.Response.Error.t) result =
 fun rpc store client_capabilities req ->
  let open Lsp.Import.Result.O in
  match req with
  | Lsp.Client_request.Initialize _ -> assert false
  | Lsp.Client_request.Shutdown -> Ok (store, ())
  | Lsp.Client_request.DebugTextDocumentGet
      { textDocument = { uri }; position = _ } -> (
    let uri = Lsp.Uri.t_of_yojson (`String uri) in
    match Document_store.get_opt store uri with
    | None -> Ok (store, None)
    | Some doc -> Ok (store, Some (Msource.text (Document.source doc))) )
  | Lsp.Client_request.DebugEcho params -> Ok (store, params)
  | Lsp.Client_request.TextDocumentColor _ -> Ok (store, [])
  | Lsp.Client_request.TextDocumentColorPresentation _ -> Ok (store, [])
  | Lsp.Client_request.TextDocumentHover { textDocument = { uri }; position }
    -> (
    let query_type doc pos =
      let command = Query_protocol.Type_enclosing (None, pos, None) in
      match Document.dispatch doc command with
      | []
      | (_, `Index _, _) :: _ ->
        None
      | (location, `String value, _) :: _ -> Some (location, value)
    in

    let query_doc doc pos =
      let command = Query_protocol.Document (None, pos) in
      match Document.dispatch doc command with
      | `Found s
      | `Builtin s ->
        Some s
      | _ -> None
    in

    let format_contents ~as_markdown ~typ ~doc =
      let doc =
        match doc with
        | None -> ""
        | Some s -> Printf.sprintf "\n(** %s *)" s
      in
      `MarkupContent
        ( if as_markdown then
          { MarkupContent.value = Printf.sprintf "```ocaml\n%s%s\n```" typ doc
          ; kind = MarkupKind.Markdown
          }
        else
          { MarkupContent.value = Printf.sprintf "%s%s" typ doc
          ; kind = MarkupKind.PlainText
          } )
    in

    let uri = Lsp.Uri.t_of_yojson (`String uri) in
    let* doc = Document_store.get store uri in
    let pos = Position.logical position in
    match query_type doc pos with
    | None -> Ok (store, None)
    | Some (loc, typ) ->
      let doc = query_doc doc pos in
      let as_markdown =
        match client_capabilities.textDocument with
        | None -> false
        | Some { hover = Some { contentFormat; _ }; _ } ->
          List.mem MarkupKind.Markdown
            ~set:(Option.value contentFormat ~default:[ Markdown ])
        | _ -> false
      in
      let contents = format_contents ~as_markdown ~typ ~doc in
      let range = Range.of_loc loc in
      let resp = Hover.create ~contents ~range () in
      Ok (store, Some resp) )
  | Lsp.Client_request.TextDocumentReferences
      { textDocument = { uri }; position; context = _ } ->
    let uri = Lsp.Uri.t_of_yojson (`String uri) in
    let* doc = Document_store.get store uri in
    let command =
      Query_protocol.Occurrences (`Ident_at (Position.logical position))
    in
    let locs : Loc.t list = Document.dispatch doc command in
    let lsp_locs =
      List.map locs ~f:(fun loc ->
          let range = Range.of_loc loc in
          (* using original uri because merlin is looking only in local file *)
          let uri = Lsp.Uri.to_string uri in
          { Location.uri; range })
    in
    Ok (store, Some lsp_locs)
  | Lsp.Client_request.TextDocumentCodeLensResolve codeLens ->
    Ok (store, codeLens)
  | Lsp.Client_request.TextDocumentCodeLens { textDocument = { uri } } -> (
    let uri = Lsp.Uri.t_of_yojson (`String uri) in
    let* doc = Document_store.get store uri in
    match Document.kind doc with
    | Intf -> Ok (store, [])
    | Impl ->
      let command = Query_protocol.Outline in
      let outline = Document.dispatch doc command in
      let symbol_infos =
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
      in
      Ok (store, symbol_infos) )
  | Lsp.Client_request.TextDocumentHighlight
      { textDocument = { uri }; position } ->
    let uri = Lsp.Uri.t_of_yojson (`String uri) in
    let* doc = Document_store.get store uri in
    let command =
      Query_protocol.Occurrences (`Ident_at (Position.logical position))
    in
    let locs : Loc.t list = Document.dispatch doc command in
    let lsp_locs =
      List.map locs ~f:(fun loc ->
          let range = Range.of_loc loc in
          (* using the default kind as we are lacking info to make a difference
             between assignment and usage. *)
          DocumentHighlight.create ~range ~kind:DocumentHighlightKind.Text ())
    in
    Ok (store, Some lsp_locs)
  | Lsp.Client_request.WorkspaceSymbol _ -> Ok (store, None)
  | Lsp.Client_request.DocumentSymbol { textDocument = { uri } } ->
    let uri = Lsp.Uri.t_of_yojson (`String uri) in
    let* doc = Document_store.get store uri in
    let symbols = Document_symbol.run client_capabilities doc uri in
    Ok (store, Some symbols)
  | Lsp.Client_request.TextDocumentDeclaration _ -> Ok (store, None)
  | Lsp.Client_request.TextDocumentDefinition
      { textDocument = { uri }; position } -> (
    let uri = Lsp.Uri.t_of_yojson (`String uri) in
    let* doc = Document_store.get store uri in
    let position = Position.logical position in
    let command = Query_protocol.Locate (None, `ML, position) in
    match Document.dispatch doc command with
    | `Found (path, lex_position) ->
      let position = Position.of_lexical_position lex_position in
      let range = { Range.start = position; end_ = position } in
      let uri =
        match path with
        | None -> uri
        | Some path -> Lsp.Uri.of_path path
      in
      let locs = [ { Location.uri = Lsp.Uri.to_string uri; range } ] in
      Ok (store, Some (`Location locs))
    | `At_origin
    | `Builtin _
    | `File_not_found _
    | `Invalid_context
    | `Not_found _
    | `Not_in_env _ ->
      Ok (store, None) )
  | Lsp.Client_request.TextDocumentTypeDefinition
      { textDocument = { uri }; position } -> (
    let uri = Lsp.Uri.t_of_yojson (`String uri) in
    let* doc = Document_store.get store uri in
    let position = Position.logical position in
    let command = Query_protocol.Locate_type position in
    match Document.dispatch doc command with
    | `Found (path, lex_position) ->
      let position = Position.of_lexical_position lex_position in
      let range = { Range.start = position; end_ = position } in
      let uri =
        match path with
        | None -> uri
        | Some path -> Lsp.Uri.of_path path
      in
      let locs = [ { Location.uri = Lsp.Uri.to_string uri; range } ] in
      Ok (store, Some (`Location locs))
    | `At_origin
    | `Builtin _
    | `File_not_found _
    | `Invalid_context
    | `Not_found _
    | `Not_in_env _ ->
      Ok (store, None) )
  | Lsp.Client_request.TextDocumentCompletion
      { textDocument = { uri }; position; context = _ } ->
    let uri = Lsp.Uri.t_of_yojson (`String uri) in
    let* doc = Document_store.get store uri in
    let+ resp = Compl.complete doc position in
    (store, Some resp)
  | Lsp.Client_request.TextDocumentPrepareRename
      { textDocument = { uri }; position } ->
    let uri = Lsp.Uri.t_of_yojson (`String uri) in
    let* doc = Document_store.get store uri in
    let command =
      Query_protocol.Occurrences (`Ident_at (Position.logical position))
    in
    let locs : Loc.t list = Document.dispatch doc command in
    let loc =
      List.find_opt locs ~f:(fun loc ->
          let range = Range.of_loc loc in
          Position.compare_inclusion position range = `Inside)
    in
    Ok (store, Option.map loc ~f:Range.of_loc)
  | Lsp.Client_request.TextDocumentRename
      { textDocument = { uri }; position; newName } ->
    let uri = Lsp.Uri.t_of_yojson (`String uri) in
    let* doc = Document_store.get store uri in
    let command =
      Query_protocol.Occurrences (`Ident_at (Position.logical position))
    in
    let locs : Loc.t list = Document.dispatch doc command in
    let version = Document.version doc in
    let edits =
      List.map locs ~f:(fun loc ->
          let range = Range.of_loc loc in
          { TextEdit.newText = newName; range })
    in
    let workspace_edits =
      let documentChanges =
        let open Option.O in
        Option.value ~default:false
          (let* workspace = client_capabilities.workspace in
           let* edit = workspace.workspaceEdit in
           edit.documentChanges)
      in
      let uri = Lsp.Uri.to_string uri in
      if documentChanges then
        let textDocument =
          VersionedTextDocumentIdentifier.create ~uri ~version ()
        in
        WorkspaceEdit.create
          ~documentChanges:
            [ `TextDocumentEdit (TextDocumentEdit.create ~textDocument ~edits) ]
          ()
      else
        WorkspaceEdit.create ~changes:[ (uri, edits) ] ()
    in
    Ok (store, workspace_edits)
  | Lsp.Client_request.TextDocumentFoldingRange { textDocument = { uri } } ->
    let uri = Lsp.Uri.t_of_yojson (`String uri) in
    let* doc = Document_store.get store uri in
    let command = Query_protocol.Outline in
    let outline = Document.dispatch doc command in
    let folds : FoldingRange.t list =
      let folding_range (range : Range.t) =
        FoldingRange.create ~startLine:range.start.line ~endLine:range.end_.line
          ~startCharacter:range.start.character
          ~endCharacter:range.end_.character ~kind:Region ()
      in
      let rec loop acc (items : Query_protocol.item list) =
        match items with
        | [] -> acc
        | item :: items ->
          let range = Range.of_loc item.location in
          if range.end_.line - range.start.line < 2 then
            loop acc items
          else
            let items = item.children @ items in
            let range = folding_range range in
            loop (range :: acc) items
      in
      loop [] outline
      |> List.sort ~compare:(fun x y -> Ordering.of_int (compare x y))
    in
    Ok (store, Some folds)
  | Lsp.Client_request.SignatureHelp _ -> not_supported ()
  | Lsp.Client_request.ExecuteCommand _ -> not_supported ()
  | Lsp.Client_request.TextDocumentLinkResolve l -> Ok (store, l)
  | Lsp.Client_request.TextDocumentLink _ -> Ok (store, None)
  | Lsp.Client_request.WillSaveWaitUntilTextDocument _ -> Ok (store, None)
  | Lsp.Client_request.CodeAction params -> code_action store params
  | Lsp.Client_request.CompletionItemResolve compl -> Ok (store, compl)
  | Lsp.Client_request.TextDocumentFormatting
      { textDocument = { uri }; options = _ } ->
    let uri = Lsp.Uri.t_of_yojson (`String uri) in
    let* doc = Document_store.get store uri in
    Formatter.run rpc store doc
  | Lsp.Client_request.TextDocumentOnTypeFormatting _ -> Ok (store, None)
  | Lsp.Client_request.SelectionRange { textDocument = { uri }; positions } ->
    let selection_range_of_shapes (cursor_position : Position.t)
        (shapes : Query_protocol.shape list) : SelectionRange.t option =
      let rec ranges_of_shape parent s =
        let range = Range.of_loc s.Query_protocol.shape_loc in
        let selectionRange = { SelectionRange.range; parent } in
        match s.Query_protocol.shape_sub with
        | [] -> [ selectionRange ]
        | xs -> List.concat_map xs ~f:(ranges_of_shape (Some selectionRange))
      in
      let ranges = List.concat_map ~f:(ranges_of_shape None) shapes in
      (* try to find the nearest range inside first, then outside *)
      let nearest_range =
        let min_by_opt xs ~f =
          List.fold_left xs ~init:None ~f:(fun state x ->
              match state with
              | None -> Some x
              | Some y -> (
                match f x y with
                | Ordering.Lt -> Some x
                | _ -> Some y ))
        in
        min_by_opt ranges ~f:(fun r1 r2 ->
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
    let uri = Lsp.Uri.t_of_yojson (`String uri) in
    let* doc = Document_store.get store uri in
    let results =
      List.filter_map positions ~f:(fun x ->
          let command = Query_protocol.Shape (Position.logical x) in
          let shapes = Document.dispatch doc command in
          selection_range_of_shapes x shapes)
    in
    Ok (store, results)
  | Lsp.Client_request.UnknownRequest _ ->
    Error (make_error ~code:InvalidRequest ~message:"Got unknown request" ())

let on_notification rpc store (notification : Lsp.Client_notification.t) :
    (Document_store.t, string) result =
  match notification with
  | TextDocumentDidOpen params ->
    let doc = Document.make params in
    Document_store.put store doc;
    send_diagnostics rpc doc;
    Ok store
  | TextDocumentDidClose { textDocument = { uri } } ->
    let uri = Lsp.Uri.t_of_yojson (`String uri) in
    Document_store.remove_document store uri;
    Ok store
  | TextDocumentDidChange { textDocument = { uri; version }; contentChanges }
    -> (
    let uri = Lsp.Uri.t_of_yojson (`String uri) in
    match Document_store.get store uri with
    | Ok prev_doc ->
      let doc =
        let f doc change = Document.update_text ?version change doc in
        List.fold_left ~f ~init:prev_doc contentChanges
      in
      Document_store.put store doc;
      send_diagnostics rpc doc;
      Ok store
    | Error e -> Error e.message )
  | DidSaveTextDocument _
  | WillSaveTextDocument _
  | ChangeConfiguration _
  | ChangeWorkspaceFolders _
  | Initialized
  | Exit ->
    Ok store
  | Unknown_notification req -> (
    match req.method_ with
    | "$/setTraceNotification" -> Ok store
    | "$/cancelRequest" -> Ok store
    | _ ->
      ( match req.params with
      | None ->
        log ~title:Logger.Title.Warning "unknown notification: %s" req.method_
      | Some json ->
        log ~title:Logger.Title.Warning "unknown notification: %s %a"
          req.method_
          (fun () -> Yojson.Safe.pretty_to_string ~std:false)
          json );
      Ok store )

let start () =
  let docs = Document_store.make () in
  let prepare_and_run prep_exn f =
    let f () =
      match f () with
      | Ok s -> Ok s
      | Error e -> Error e
      | exception exn -> Error (prep_exn exn)
    in
    (* TODO: what to do with merlin notifications? *)
    let _notifications = ref [] in
    Logger.with_notifications (ref []) @@ fun () -> File_id.with_cache @@ f
  in
  let on_initialize rpc state params =
    Fiber.return
      ( prepare_and_run Printexc.to_string @@ fun () ->
        on_initialize rpc state params )
  in
  let on_notification rpc state notif =
    Fiber.return
      ( prepare_and_run Printexc.to_string @@ fun () ->
        on_notification rpc state notif )
  in
  let on_request rpc state caps req =
    Fiber.return
      ( prepare_and_run Lsp.Jsonrpc.Response.Error.of_exn @@ fun () ->
        on_request rpc state caps req )
  in
  ( match
      Lsp.Server.start docs
        { on_initialize; on_request; on_notification }
        stdin stdout
      |> Fiber.run
    with
  | None ->
    Format.eprintf "Scheduler got stuck@.";
    exit 1
  | Some () -> () );
  log ~title:Logger.Title.Info "exiting"

let run ~log_file =
  Unix.putenv "__MERLIN_MASTER_PID" (string_of_int (Unix.getpid ()));
  Lsp.Logger.with_log_file ~sections:[ "ocamllsp"; "lsp" ] log_file start

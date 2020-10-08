open Import

let client_capabilities (state : State.t) =
  match state.init with
  | Uninitialized -> assert false
  | Initialized c -> c

let make_error = Jsonrpc.Response.Error.make

let not_supported () =
  Fiber.return
  @@ Error
       (make_error ~code:InternalError ~message:"Request not supported yet!" ())

let initialize_info : InitializeResult.t =
  let codeActionProvider =
    let codeActionKinds = [ CodeActionKind.Other Destruct_lsp.action_kind ] in
    `CodeActionOptions (CodeActionOptions.create ~codeActionKinds ())
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
      ~resolveProvider:true ()
  in
  let renameProvider =
    `RenameOptions (RenameOptions.create ~prepareProvider:true ())
  in
  let capabilities =
    let experimental =
      `Assoc
        [ ( "ocamllsp"
          , `Assoc
              [ ("interfaceSpecificLangId", `Bool true)
              ; Switch_impl_intf.capability
              ] )
        ]
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
  let state : State.t = Server.state rpc in
  let send () =
    let diagnostic_create = Diagnostic.create ~source:"ocamllsp" in
    let available =
      match Document.syntax doc with
      | Menhir
      | Ocamllex ->
        `Unsupported
      | Ocaml -> `Available true
      | Reason -> `Available (Option.is_some (Bin.which ocamlmerlin_reason))
    in
    let uri = Document.uri doc |> Lsp.Uri.to_string in
    match available with
    | `Unsupported -> Fiber.return ()
    | `Available false ->
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
        Server_notification.PublishDiagnostics
          (PublishDiagnosticsParams.create ~uri ~diagnostics ())
      in
      Server.notification rpc notif
    | `Available true ->
      let open Fiber.O in
      let* diagnostics =
        Document.with_pipeline_exn doc @@ fun pipeline ->
        let command =
          Query_protocol.Errors { lexing = true; parsing = true; typing = true }
        in
        let errors = Query_commands.dispatch pipeline command in
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
        Server_notification.PublishDiagnostics
          (PublishDiagnosticsParams.create ~uri ~diagnostics ())
      in
      Server.notification rpc notif
  in
  Scheduler.detach state.scheduler (fun () ->
      let open Fiber.O in
      let timer = Document.timer doc in
      let+ res = Scheduler.schedule timer send in
      match res with
      | Error `Cancelled
      | Ok () ->
        ())

let on_initialize rpc =
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
      let notif = Server_notification.LogMessage { message; type_ } in
      let (_ : unit Fiber.t) =
        let state : State.t = Server.state rpc in
        Scheduler.detach state.scheduler (fun () ->
            Server.notification rpc notif)
      in
      ()
  in
  Logger.register_consumer log_consumer;
  initialize_info

let code_action server (params : CodeActionParams.t) =
  let state : State.t = Server.state server in
  let store = state.store in
  match params.context.only with
  | Some set
    when not (List.mem (CodeActionKind.Other Destruct_lsp.action_kind) ~set) ->
    Fiber.return (Ok (None, state))
  | Some _
  | None ->
    let open Fiber.Result.O in
    let uri = Uri.t_of_yojson (`String params.textDocument.uri) in
    let* doc = Fiber.return (Document_store.get store uri) in
    let+ action = Destruct_lsp.code_action doc params in
    let action =
      Option.map action ~f:(fun destruct -> [ `CodeAction destruct ])
    in
    (action, state)

module Formatter = struct
  let jsonrpc_error (e : Fmt.error) =
    let message = Fmt.message e in
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
    match Fmt.run doc with
    | Result.Error e ->
      let message = Fmt.message e in
      let error = jsonrpc_error e in
      let msg = ShowMessageParams.create ~message ~type_:Error in
      let (_ : unit Fiber.t) =
        let state : State.t = Server.state rpc in
        Scheduler.detach state.scheduler (fun () ->
            Server.notification rpc (ShowMessage msg))
      in
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
      let state = Server.state rpc in
      Ok (Some [ change ], state)
end

let markdown_support (client_capabilities : ClientCapabilities.t) ~field =
  match client_capabilities.textDocument with
  | None -> false
  | Some td -> (
    match field td with
    | None -> false
    | Some format ->
      let set = Option.value format ~default:[ MarkupKind.Markdown ] in
      List.mem MarkupKind.Markdown ~set )

let location_of_merlin_loc uri = function
  | `At_origin
  | `Builtin _
  | `File_not_found _
  | `Invalid_context
  | `Not_found _
  | `Not_in_env _ ->
    None
  | `Found (path, lex_position) ->
    Position.of_lexical_position lex_position
    |> Option.map ~f:(fun position ->
           let range = { Range.start = position; end_ = position } in
           let uri =
             match path with
             | None -> uri
             | Some path -> Uri.of_path path
           in
           let locs = [ { Location.uri = Uri.to_string uri; range } ] in
           `Location locs)

let format_contents ~syntax ~markdown ~typ ~doc =
  (* TODO for vscode, we should just use the language id. But that will not work
     for all editors *)
  let markdown_name = Document.Syntax.markdown_name syntax in
  `MarkupContent
    ( if markdown then
      let value =
        match doc with
        | None -> sprintf "```%s\n%s\n```" markdown_name typ
        | Some s ->
          let doc =
            match Doc_to_md.translate s with
            | Raw d -> sprintf "(** %s *)" d
            | Markdown d -> d
          in
          sprintf "```%s\n%s\n```\n---\n%s" markdown_name typ doc
      in
      { MarkupContent.value; kind = MarkupKind.Markdown }
    else
      let value =
        match doc with
        | None -> sprintf "%s" typ
        | Some d -> sprintf "%s\n%s" typ d
      in
      { MarkupContent.value; kind = MarkupKind.PlainText } )

let query_doc doc pos =
  let command = Query_protocol.Document (None, pos) in
  let open Fiber.O in
  let+ res = Document.dispatch_exn doc command in
  match res with
  | `Found s
  | `Builtin s ->
    Some s
  | _ -> None

let query_type doc pos =
  let command = Query_protocol.Type_enclosing (None, pos, None) in
  let open Fiber.O in
  let+ res = Document.dispatch_exn doc command in
  match res with
  | []
  | (_, `Index _, _) :: _ ->
    None
  | (location, `String value, _) :: _ -> Some (location, value)

let hover (state : State.t) { HoverParams.textDocument = { uri }; position } =
  let store = state.store in
  let uri = Uri.t_of_yojson (`String uri) in
  let open Fiber.Result.O in
  let* doc = Fiber.return (Document_store.get store uri) in
  let pos = Position.logical position in
  let client_capabilities = client_capabilities state in
  let open Fiber.O in
  let* query_type = query_type doc pos in
  match query_type with
  | None -> Fiber.return @@ Ok (None, state)
  | Some (loc, typ) ->
    let syntax = Document.syntax doc in
    let+ doc = query_doc doc pos in
    let contents =
      let markdown =
        markdown_support client_capabilities ~field:(fun td ->
            Option.map td.hover ~f:(fun h -> h.contentFormat))
      in
      format_contents ~syntax ~markdown ~typ ~doc
    in
    let range = Range.of_loc loc in
    let resp = Hover.create ~contents ~range () in
    Ok (Some resp, state)

let text_document_lens (state : State.t)
    { CodeLensParams.textDocument = { uri } } =
  let uri = Uri.t_of_yojson (`String uri) in
  let store = state.store in
  let open Fiber.Result.O in
  let* doc = Fiber.return @@ Document_store.get store uri in
  match Document.kind doc with
  | Intf -> Fiber.return @@ Ok ([], state)
  | Impl ->
    let open Fiber.O in
    let command = Query_protocol.Outline in
    let+ outline = Document.dispatch_exn doc command in
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
    Ok (symbol_infos, state)

let folding_range (state : State.t)
    { FoldingRangeParams.textDocument = { uri } } =
  let uri = Uri.t_of_yojson (`String uri) in
  let open Fiber.Result.O in
  let* doc = Fiber.return (Document_store.get state.store uri) in
  let command = Query_protocol.Outline in
  let open Fiber.O in
  let+ outline = Document.dispatch_exn doc command in
  let folds : FoldingRange.t list =
    let folding_range (range : Range.t) =
      FoldingRange.create ~startLine:range.start.line ~endLine:range.end_.line
        ~startCharacter:range.start.character ~endCharacter:range.end_.character
        ~kind:Region ()
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
  Ok (Some folds, state)

let rename (state : State.t)
    { RenameParams.textDocument = { uri }; position; newName } =
  let open Fiber.Result.O in
  let uri = Uri.t_of_yojson (`String uri) in
  let* doc = Fiber.return (Document_store.get state.store uri) in
  let command =
    Query_protocol.Occurrences (`Ident_at (Position.logical position))
  in
  let open Fiber.O in
  let+ locs = Document.dispatch_exn doc command in
  let version = Document.version doc in
  let edits =
    List.map locs ~f:(fun loc ->
        let range = Range.of_loc loc in
        { TextEdit.newText = newName; range })
  in
  let workspace_edits =
    let client_capabilities = client_capabilities state in
    let documentChanges =
      let open Option.O in
      Option.value ~default:false
        (let* workspace = client_capabilities.workspace in
         let* edit = workspace.workspaceEdit in
         edit.documentChanges)
    in
    let uri = Uri.to_string uri in
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
  Ok (workspace_edits, state)

let selection_range (state : State.t)
    { SelectionRangeParams.textDocument = { uri }; positions } =
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
  let uri = Uri.t_of_yojson (`String uri) in
  let open Fiber.Result.O in
  let* doc = Fiber.return (Document_store.get state.store uri) in
  let+ results =
    let open Fiber.O in
    let+ ranges =
      Fiber.sequential_map positions ~f:(fun x ->
          let command = Query_protocol.Shape (Position.logical x) in
          let open Fiber.O in
          let+ shapes = Document.dispatch_exn doc command in
          selection_range_of_shapes x shapes)
    in
    Ok (List.filter_opt ranges)
  in
  (results, state)

let references (state : State.t)
    { ReferenceParams.textDocument = { uri }; position; context = _ } =
  let open Fiber.Result.O in
  let uri = Uri.t_of_yojson (`String uri) in
  let* doc = Fiber.return (Document_store.get state.store uri) in
  let command =
    Query_protocol.Occurrences (`Ident_at (Position.logical position))
  in
  let open Fiber.O in
  let+ locs = Document.dispatch_exn doc command in
  let lsp_locs =
    List.map locs ~f:(fun loc ->
        let range = Range.of_loc loc in
        (* using original uri because merlin is looking only in local file *)
        let uri = Uri.to_string uri in
        { Location.uri; range })
  in
  Ok (Some lsp_locs, state)

let definition_query (state : State.t) uri position merlin_request =
  let open Fiber.Result.O in
  let uri = Uri.t_of_yojson (`String uri) in
  let* doc = Fiber.return (Document_store.get state.store uri) in
  let position = Position.logical position in
  let command = merlin_request position in
  let open Fiber.O in
  let+ result = Document.dispatch_exn doc command in
  let result = location_of_merlin_loc uri result in
  Ok (result, state)

(** handles requests for OCaml (syntax) documents *)
let ocaml_on_request :
    type resp.
       State.t Server.t
    -> resp Client_request.t
    -> (resp * State.t, Jsonrpc.Response.Error.t) result Fiber.t =
 fun rpc req ->
  let state = Server.state rpc in
  let store = state.store in
  let open Import.Result.O in
  match req with
  | Client_request.Initialize ip ->
    let initialize_result = on_initialize rpc in
    let state = { state with init = Initialized ip.capabilities } in
    Fiber.return @@ Ok (initialize_result, state)
  | Client_request.Shutdown -> Fiber.return @@ Ok ((), state)
  | Client_request.DebugTextDocumentGet { textDocument = { uri }; position = _ }
    ->
    Fiber.return
      (let uri = Uri.t_of_yojson (`String uri) in
       match Document_store.get_opt store uri with
       | None -> Ok (None, state)
       | Some doc -> Ok (Some (Msource.text (Document.source doc)), state))
  | Client_request.DebugEcho params -> Fiber.return @@ Ok (params, state)
  | Client_request.TextDocumentColor _ -> Fiber.return @@ Ok ([], state)
  | Client_request.TextDocumentColorPresentation _ ->
    Fiber.return @@ Ok ([], state)
  | Client_request.TextDocumentHover req -> hover state req
  | Client_request.TextDocumentReferences req -> references state req
  | Client_request.TextDocumentCodeLensResolve codeLens ->
    Fiber.return @@ Ok (codeLens, state)
  | Client_request.TextDocumentCodeLens req -> text_document_lens state req
  | Client_request.TextDocumentHighlight { textDocument = { uri }; position } ->
    let open Fiber.Result.O in
    let uri = Uri.t_of_yojson (`String uri) in
    let* doc = Fiber.return (Document_store.get store uri) in
    let command =
      Query_protocol.Occurrences (`Ident_at (Position.logical position))
    in
    let open Fiber.O in
    let+ locs = Document.dispatch_exn doc command in
    let lsp_locs =
      List.map locs ~f:(fun loc ->
          let range = Range.of_loc loc in
          (* using the default kind as we are lacking info to make a difference
             between assignment and usage. *)
          DocumentHighlight.create ~range ~kind:DocumentHighlightKind.Text ())
    in
    Ok (Some lsp_locs, state)
  | Client_request.WorkspaceSymbol _ -> Fiber.return @@ Ok (None, state)
  | Client_request.DocumentSymbol { textDocument = { uri } } ->
    let uri = Uri.t_of_yojson (`String uri) in
    let open Fiber.Result.O in
    let* doc = Fiber.return (Document_store.get store uri) in
    let client_capabilities = client_capabilities state in
    let open Fiber.O in
    let+ symbols = Document_symbol.run client_capabilities doc uri in
    Ok (Some symbols, state)
  | Client_request.TextDocumentDeclaration _ -> Fiber.return @@ Ok (None, state)
  | Client_request.TextDocumentDefinition { textDocument = { uri }; position }
    ->
    definition_query state uri position (fun pos ->
        Query_protocol.Locate (None, `ML, pos))
  | Client_request.TextDocumentTypeDefinition
      { textDocument = { uri }; position } ->
    definition_query state uri position (fun pos ->
        Query_protocol.Locate_type pos)
  | Client_request.TextDocumentCompletion
      { textDocument = { uri }; position; context = _ } ->
    let uri = Uri.t_of_yojson (`String uri) in
    let open Fiber.Result.O in
    let* doc = Fiber.return (Document_store.get store uri) in
    let+ resp = Compl.complete doc position in
    (Some resp, state)
  | Client_request.TextDocumentPrepareRename
      { textDocument = { uri }; position } ->
    let uri = Uri.t_of_yojson (`String uri) in
    let open Fiber.Result.O in
    let* doc = Fiber.return (Document_store.get store uri) in
    let command =
      Query_protocol.Occurrences (`Ident_at (Position.logical position))
    in
    let open Fiber.O in
    let+ locs = Document.dispatch_exn doc command in
    let loc =
      List.find_opt locs ~f:(fun loc ->
          let range = Range.of_loc loc in
          Position.compare_inclusion position range = `Inside)
    in
    Ok (Option.map loc ~f:Range.of_loc, state)
  | Client_request.TextDocumentRename req -> rename state req
  | Client_request.TextDocumentFoldingRange req -> folding_range state req
  | Client_request.SignatureHelp _ -> not_supported ()
  | Client_request.ExecuteCommand _ -> not_supported ()
  | Client_request.TextDocumentLinkResolve l -> Fiber.return @@ Ok (l, state)
  | Client_request.TextDocumentLink _ -> Fiber.return @@ Ok (None, state)
  | Client_request.WillSaveWaitUntilTextDocument _ ->
    Fiber.return @@ Ok (None, state)
  | Client_request.CodeAction params -> code_action rpc params
  | Client_request.CompletionItemResolve ci ->
    let markdown =
      markdown_support (client_capabilities state) ~field:(fun d ->
          let open Option.O in
          let+ completion = d.completion in
          let* completion_item = completion.completionItem in
          completion_item.documentationFormat)
    in
    let open Fiber.Result.O in
    let resolve = Compl.Resolve.of_completion_item ci in
    let* doc =
      let uri = Compl.Resolve.uri resolve in
      Fiber.return (Document_store.get state.store uri)
    in
    let* compl = Compl.resolve doc ci resolve query_doc ~markdown in
    Fiber.return @@ Ok (compl, state)
  | Client_request.TextDocumentFormatting
      { textDocument = { uri }; options = _ } ->
    let uri = Uri.t_of_yojson (`String uri) in
    Fiber.return
      (let* doc = Document_store.get store uri in
       Formatter.run rpc doc)
  | Client_request.TextDocumentOnTypeFormatting _ ->
    Fiber.return @@ Ok (None, state)
  | Client_request.SelectionRange req -> selection_range state req
  | Client_request.UnknownRequest _ ->
    Fiber.return
    @@ Error (make_error ~code:InvalidRequest ~message:"Got unknown request" ())

let on_request :
    type resp.
       State.t Server.t
    -> resp Client_request.t
    -> (resp * State.t, Jsonrpc.Response.Error.t) result Fiber.t =
 fun server req ->
  let state : State.t = Server.state server in
  let store = state.store in
  let syntax : Document.Syntax.t option =
    let open Option.O in
    let* td =
      Client_request.text_document req (fun ~meth:_ ~params:_ -> None)
    in
    let uri = Uri.t_of_yojson (`String td.uri) in
    let+ doc = Document_store.get_opt store uri in
    Document.syntax doc
  in
  match req with
  | Client_request.UnknownRequest { meth; params } -> (
    match
      [ (Switch_impl_intf.meth, Switch_impl_intf.on_request) ]
      |> List.assoc_opt meth
    with
    | None ->
      Fiber.return
        (Error
           (make_error ~code:InternalError ~message:"Unknown method"
              ~data:(`Assoc [ ("method", `String meth) ])
              ()))
    | Some handler -> handler ~params state )
  | _ -> (
    match syntax with
    | Some (Ocamllex | Menhir) -> not_supported ()
    | _ -> ocaml_on_request server req )

let on_notification server (notification : Client_notification.t) :
    State.t Fiber.t =
  let state : State.t = Server.state server in
  let store = state.store in
  match notification with
  | TextDocumentDidOpen params ->
    let doc =
      let delay = Configuration.diagnostics_delay state.configuration in
      let timer = Scheduler.create_timer state.scheduler ~delay in
      Document.make timer state.merlin params
    in
    Document_store.put store doc;
    let (_ : unit Fiber.t) = send_diagnostics server doc in
    Fiber.return state
  | TextDocumentDidClose { textDocument = { uri } } ->
    let uri = Uri.t_of_yojson (`String uri) in
    Document_store.remove_document store uri;
    Fiber.return state
  | TextDocumentDidChange { textDocument = { uri; version }; contentChanges }
    -> (
    let uri = Uri.t_of_yojson (`String uri) in
    match Document_store.get store uri with
    | Error e ->
      Format.eprintf "uri doesn't exist %s@.%!" e.message;
      Fiber.return state
    | Ok prev_doc ->
      let doc =
        let f doc change = Document.update_text ?version change doc in
        List.fold_left ~f ~init:prev_doc contentChanges
      in
      Document_store.put store doc;
      let (_ : unit Fiber.t) = send_diagnostics server doc in
      Fiber.return state )
  | ChangeConfiguration req ->
    (* TODO this is wrong and we should just fetch the config from the client
       after receiving this notification *)
    let configuration = Configuration.update state.configuration req in
    Fiber.return { state with configuration }
  | DidSaveTextDocument _
  | WillSaveTextDocument _
  | ChangeWorkspaceFolders _
  | Initialized
  | Exit ->
    Fiber.return state
  | Unknown_notification req -> (
    match req.method_ with
    | "$/setTraceNotification"
    | "$/cancelRequest" ->
      Fiber.return state
    | _ ->
      ( match req.params with
      | None ->
        log ~title:Logger.Title.Warning "unknown notification: %s" req.method_
      | Some json ->
        log ~title:Logger.Title.Warning "unknown notification: %s %a"
          req.method_
          (fun () -> Json.to_pretty_string)
          json );
      Fiber.return state )

let start () =
  let docs = Document_store.make () in
  let prepare_and_run prep_exn f =
    let f () =
      let open Fiber.O in
      let+ result = Fiber.collect_errors f in
      match result with
      | Ok (Ok s) -> Ok s
      | Ok (Error e) -> Error e
      | Error [ e ] -> Error (prep_exn e.exn)
      | Error _exns ->
        Error
          (Jsonrpc.Response.Error.make
             ~code:Jsonrpc.Response.Error.Code.InternalError
             ~message:"Multiple exceptions" ())
    in
    (* TODO: what to do with merlin notifications? *)
    let _notifications = ref [] in
    Logger.with_notifications (ref []) @@ fun () -> File_id.with_cache @@ f
  in
  let on_request server req =
    prepare_and_run Jsonrpc.Response.Error.of_exn @@ fun () ->
    on_request server req
  in
  let scheduler = Scheduler.create () in
  let handler =
    let on_request = { Server.Handler.on_request } in
    Server.Handler.make ~on_request ~on_notification ()
  in
  let stream =
    let io = Lsp.Io.make stdin stdout in
    Lsp.Rpc.Stream_io.make scheduler io
  in
  let configuration = Configuration.default in
  let server =
    let merlin = Scheduler.create_thread scheduler in
    Server.make handler stream
      { store = docs; init = Uninitialized; merlin; scheduler; configuration }
  in
  Scheduler.run scheduler (Server.start server);
  log ~title:Logger.Title.Info "exiting"

let run ~log_file =
  Unix.putenv "__MERLIN_MASTER_PID" (string_of_int (Unix.getpid ()));
  Logger.with_log_file ~sections:[ "ocamllsp"; "lsp" ] log_file start

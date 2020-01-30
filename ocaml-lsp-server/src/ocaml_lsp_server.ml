open Import

let { Logger.log } = Logger.for_section "ocaml-lsp-server"

let not_supported () = Error "request not supported yet"

module Action = struct
  let destruct = "destruct"
end

let completion_kind kind : Lsp.Completion.completionItemKind option =
  match kind with
  | `Value -> Some Value
  | `Constructor -> Some Constructor
  | `Variant -> None
  | `Label -> Some Property
  | `Module
  | `Modtype ->
    Some Module
  | `Type -> Some TypeParameter
  | `MethodCall -> Some Method

let outline_kind kind : Lsp.Protocol.SymbolKind.t =
  match kind with
  | `Value -> Function
  | `Constructor -> Constructor
  | `Label -> Property
  | `Module -> Module
  | `Modtype -> Module
  | `Type -> String
  | `Exn -> Constructor
  | `Class -> Class
  | `Method -> Method

let initializeInfo : Lsp.Initialize.Result.t =
  let codeActionProvider : Lsp.Initialize.CodeActionOptions.t =
    { codeActionsKinds = [ Other Action.destruct ] }
  in
  { capabilities =
      { textDocumentSync =
          { openClose = true
          ; change = IncrementalSync
          ; willSave = false
          ; willSaveWaitUntil = false
          ; didSave = None
          }
      ; hoverProvider = true
      ; definitionProvider = true
      ; typeDefinitionProvider = true
      ; completionProvider =
          Some { resolveProvider = true; triggerCharacters = [ "." ] }
      ; referencesProvider = true
      ; documentHighlightProvider = true
      ; documentSymbolProvider = true
      ; workspaceSymbolProvider = false
      ; codeActionProvider = Value codeActionProvider
      ; codeLensProvider = Some { resolveProvider = false }
      ; documentFormattingProvider = false
      ; documentRangeFormattingProvider = false
      ; documentOnTypeFormattingProvider = None
      ; renameProvider = true
      ; documentLinkProvider = None
      ; executeCommandProvider = None
      ; typeCoverageProvider = false
      ; foldingRangeProvider = Bool true
      ; signatureHelpProvider = None
      }
  }

let dispatch_in_doc doc command =
  Document.with_pipeline doc (fun pipeline ->
      Query_commands.dispatch pipeline command)

let logical_of_position (position : Lsp.Protocol.Position.t) =
  let line = position.line + 1 in
  let col = position.character in
  `Logical (line, col)

let position_of_lexical_position (lex_position : Lexing.position) =
  let line = lex_position.pos_lnum - 1 in
  let character = lex_position.pos_cnum - lex_position.pos_bol in
  { Lsp.Protocol.Position.line; character }

let range_of_loc (loc : Location.t) : Lsp.Protocol.Range.t =
  { start_ = position_of_lexical_position loc.loc_start
  ; end_ = position_of_lexical_position loc.loc_end
  }

let send_diagnostics rpc doc =
  let command =
    Query_protocol.Errors { lexing = true; parsing = true; typing = true }
  in
  Document.with_pipeline doc @@ fun pipeline ->
  let errors = Query_commands.dispatch pipeline command in
  let diagnostics =
    List.map
      ~f:(fun (error : Location.error) ->
        let loc = Location.loc_of_report error in
        let range = range_of_loc loc in
        let severity =
          match error.source with
          | Warning -> Some Lsp.Protocol.PublishDiagnostics.Warning
          | _ -> Some Lsp.Protocol.PublishDiagnostics.Error
        in
        let message =
          Location.print_main Format.str_formatter error;
          String.trim (Format.flush_str_formatter ())
        in
        let diagnostic : Lsp.Protocol.PublishDiagnostics.diagnostic =
          { Lsp.Protocol.PublishDiagnostics.message
          ; severity
          ; range
          ; relatedInformation = []
          ; relatedLocations = []
          ; code = NoCode
          ; source = None
          }
        in
        diagnostic)
      errors
  in

  let notif =
    Lsp.Server_notification.PublishDiagnostics
      { uri = Document.uri doc; diagnostics }
  in

  Lsp.Rpc.send_notification rpc notif

let on_initialize _rpc state _params = Ok (state, initializeInfo)

let code_action_of_case_analysis uri (loc, newText) =
  let edit : Lsp.Protocol.WorkspaceEdit.t =
    let textedit : Lsp.Protocol.TextEdit.t =
      { range = range_of_loc loc; newText }
    in
    { changes = [ (uri, [ textedit ]) ]; documentChanges = [] }
  in
  let title = String.capitalize_ascii Action.destruct in
  { Lsp.CodeAction.title
  ; kind = Some (Lsp.CodeAction.Kind.Other Action.destruct)
  ; diagnostics = []
  ; edit = Some edit
  ; command = None
  }

let code_action store (params : Lsp.CodeAction.Params.t) =
  let open Lsp.Import.Result.O in
  match params.context.only with
  | Only set
    when not (List.mem (Lsp.CodeAction.Kind.Other Action.destruct) ~set) ->
    Ok (store, [])
  | Only _
  | All ->
    Document_store.get store params.textDocument.uri >>= fun doc ->
    let command =
      let start = logical_of_position params.range.start_ in
      let finish = logical_of_position params.range.end_ in
      Query_protocol.Case_analysis (start, finish)
    in
    let result : Lsp.CodeAction.result =
      try
        let res = dispatch_in_doc doc command in
        [ Either.Right
            (code_action_of_case_analysis params.textDocument.uri res)
        ]
      with
      | Destruct.Not_allowed _
      | Destruct.Useless_refine
      | Destruct.Nothing_to_do ->
        []
    in
    Ok (store, result)

let on_request :
    type resp.
       Lsp.Rpc.t
    -> Document_store.t
    -> Lsp.Initialize.ClientCapabilities.t
    -> resp Lsp.Client_request.t
    -> (Document_store.t * resp, string) result =
 fun _rpc store client_capabilities req ->
  let open Lsp.Import.Result.O in
  match req with
  | Lsp.Client_request.Initialize _ -> assert false
  | Lsp.Client_request.Shutdown -> Ok (store, ())
  | Lsp.Client_request.DebugTextDocumentGet
      { textDocument = { uri }; position = _ } -> (
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
      match dispatch_in_doc doc command with
      | []
      | (_, `Index _, _) :: _ ->
        None
      | (location, `String value, _) :: _ -> Some (location, value)
    in

    let query_doc doc pos =
      let command = Query_protocol.Document (None, pos) in
      match dispatch_in_doc doc command with
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
      if as_markdown then
        { Lsp.Protocol.MarkupContent.value =
            Printf.sprintf "```ocaml\n%s%s\n```" typ doc
        ; kind = Lsp.Protocol.MarkupKind.Markdown
        }
      else
        { Lsp.Protocol.MarkupContent.value = Printf.sprintf "%s%s" typ doc
        ; kind = Lsp.Protocol.MarkupKind.Plaintext
        }
    in

    Document_store.get store uri >>= fun doc ->
    let pos = logical_of_position position in
    match query_type doc pos with
    | None -> Ok (store, None)
    | Some (loc, typ) ->
      let doc = query_doc doc pos in
      let as_markdown =
        List.mem Lsp.Protocol.MarkupKind.Markdown
          ~set:client_capabilities.textDocument.hover.contentFormat
      in
      let contents = format_contents ~as_markdown ~typ ~doc in
      let range = Some (range_of_loc loc) in
      let resp = { Lsp.Protocol.Hover.contents; range } in
      Ok (store, Some resp) )
  | Lsp.Client_request.TextDocumentReferences
      { textDocument = { uri }; position; context = _ } ->
    Document_store.get store uri >>= fun doc ->
    let command =
      Query_protocol.Occurrences (`Ident_at (logical_of_position position))
    in
    let locs : Location.t list = dispatch_in_doc doc command in
    let lsp_locs =
      List.map
        ~f:(fun loc ->
          let range = range_of_loc loc in
          (* using original uri because merlin is looking only in local file *)
          { Lsp.Protocol.Location.uri; range })
        locs
    in
    Ok (store, lsp_locs)
  | Lsp.Client_request.TextDocumentCodeLensResolve codeLens ->
    Ok (store, codeLens)
  | Lsp.Client_request.TextDocumentCodeLens { textDocument = { uri } } ->
    Document_store.get store uri >>= fun doc ->
    let command = Query_protocol.Outline in
    let outline = dispatch_in_doc doc command in
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
            { Lsp.Protocol.CodeLens.range = range_of_loc loc
            ; data = None
            ; command =
                Some
                  { Lsp.Protocol.Command.title = typ
                  ; command = ""
                  ; arguments = None
                  }
            }
          in
          info :: children
      in
      List.concat_map ~f:symbol_info_of_outline_item outline
    in
    Ok (store, symbol_infos)
  | Lsp.Client_request.TextDocumentHighlight
      { textDocument = { uri }; position } ->
    Document_store.get store uri >>= fun doc ->
    let command =
      Query_protocol.Occurrences (`Ident_at (logical_of_position position))
    in
    let locs : Location.t list = dispatch_in_doc doc command in
    let lsp_locs =
      List.map
        ~f:(fun loc ->
          let range = range_of_loc loc in
          (* using the default kind as we are lacking info to make a difference
             between assignment and usage. *)
          { Lsp.Protocol.DocumentHighlight.kind = Some Text; range })
        locs
    in
    Ok (store, lsp_locs)
  | Lsp.Client_request.WorkspaceSymbol _ -> Ok (store, [])
  | Lsp.Client_request.DocumentSymbol { textDocument = { uri } } ->
    let range item = range_of_loc item.Query_protocol.location in

    let rec symbol item =
      let children = List.map item.Query_protocol.children ~f:symbol in
      let range : Lsp.Protocol.Range.t = range item in
      { Lsp.Protocol.DocumentSymbol.name = item.Query_protocol.outline_name
      ; detail = item.Query_protocol.outline_type
      ; kind = outline_kind item.outline_kind
      ; deprecated = false
      ; range
      ; selectionRange = range
      ; children
      }
    in

    let rec symbol_info ?containerName item =
      let location = { Lsp.Protocol.Location.uri; range = range item } in
      let info =
        { Lsp.Protocol.SymbolInformation.name = item.Query_protocol.outline_name
        ; kind = outline_kind item.outline_kind
        ; deprecated = Some false
        ; location
        ; containerName
        }
      in
      let children =
        List.concat_map item.children ~f:(symbol_info ~containerName:info.name)
      in
      info :: children
    in

    Document_store.get store uri >>= fun doc ->
    let command = Query_protocol.Outline in
    let outline = dispatch_in_doc doc command in
    let symbols =
      let caps = client_capabilities.textDocument.documentSymbol in
      match caps.hierarchicalDocumentSymbolSupport with
      | true ->
        let symbols = List.map outline ~f:symbol in
        Lsp.Protocol.TextDocumentDocumentSymbol.DocumentSymbol symbols
      | false ->
        let symbols = List.concat_map ~f:symbol_info outline in
        Lsp.Protocol.TextDocumentDocumentSymbol.SymbolInformation symbols
    in
    Ok (store, symbols)
  | Lsp.Client_request.TextDocumentDeclaration _ -> Ok (store, None)
  | Lsp.Client_request.TextDocumentDefinition
      { textDocument = { uri }; position } -> (
    Document_store.get store uri >>= fun doc ->
    let position = logical_of_position position in
    let command = Query_protocol.Locate (None, `ML, position) in
    match dispatch_in_doc doc command with
    | `Found (path, lex_position) ->
      let position = position_of_lexical_position lex_position in
      let range = { Lsp.Protocol.Range.start_ = position; end_ = position } in
      let uri =
        match path with
        | None -> uri
        | Some path -> Lsp.Uri.of_path path
      in
      let locs =
        Lsp.Protocol.Locations.Location { Lsp.Protocol.Location.uri; range }
      in
      Ok (store, Some locs)
    | `At_origin
    | `Builtin _
    | `File_not_found _
    | `Invalid_context
    | `Not_found _
    | `Not_in_env _ ->
      Ok (store, None) )
  | Lsp.Client_request.TextDocumentTypeDefinition
      { textDocument = { uri }; position } ->
    Document_store.get store uri >>= fun doc ->
    let position = logical_of_position position in
    Document.with_pipeline doc @@ fun pipeline ->
    let typer = Mpipeline.typer_result pipeline in
    let structures = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    let pos = Mpipeline.get_lexing_pos pipeline position in
    let path = Mbrowse.enclosing pos [ structures ] in
    let path =
      let rec resolve_tlink env ty =
        match ty.Types.desc with
        | Tconstr (path, _, _) -> Some (env, path)
        | Tlink ty -> resolve_tlink env ty
        | _ -> None
      in
      List.filter_map path ~f:(fun (env, node) ->
          log ~title:"debug" "inspecting node: %s"
            (Browse_raw.string_of_node node);
          match node with
          | Browse_raw.Expression { exp_type = ty; _ }
          | Pattern { pat_type = ty; _ }
          | Core_type { ctyp_type = ty; _ }
          | Value_description { val_desc = { ctyp_type = ty; _ }; _ } ->
            resolve_tlink env ty
          | _ -> None)
    in
    let locs =
      List.filter_map path ~f:(fun (env, path) ->
          log ~title:"debug" "found type: %s" (Path.name path);
          let local_defs = Mtyper.get_typedtree typer in
          match
            Locate.from_string
              ~config:(Mpipeline.final_config pipeline)
              ~env ~local_defs ~pos ~namespaces:[ `Type ] `MLI
              (* FIXME: instead of converting to a string, pass it directly. *)
              (Path.name path)
          with
          | exception Env.Error _ -> None
          | `Found (path, lex_position) ->
            let position = position_of_lexical_position lex_position in
            let range =
              { Lsp.Protocol.Range.start_ = position; end_ = position }
            in
            let uri =
              match path with
              | None -> uri
              | Some path -> Lsp.Uri.of_path path
            in
            let loc = { Lsp.Protocol.Location.uri; range } in
            Some loc
          | `At_origin
          | `Builtin _
          | `File_not_found _
          | `Invalid_context
          | `Missing_labels_namespace
          | `Not_found _
          | `Not_in_env _ ->
            None)
    in
    Ok (store, locs)
  | Lsp.Client_request.TextDocumentCompletion
      { textDocument = { uri }; position; context = _ } ->
    let lsp_position = position in
    let position = logical_of_position position in

    let make_string chars =
      let chars = Array.of_list chars in
      String.init (Array.length chars) ~f:(Array.get chars)
    in

    let prefix_of_position source position =
      match Msource.text source with
      | "" -> ""
      | text ->
        let len = String.length text in

        let rec find prefix i =
          if i < 0 then
            make_string prefix
          else if i >= len then
            find prefix (i - 1)
          else
            let ch = text.[i] in
            (* The characters for an infix function are missing *)
            match ch with
            | 'a' .. 'z'
            | 'A' .. 'Z'
            | '0' .. '9'
            | '.'
            | '\''
            | '_' ->
              find (ch :: prefix) (i - 1)
            | _ -> make_string prefix
        in

        let (`Offset index) = Msource.get_offset source position in
        find [] (index - 1)
    in

    let range_prefix prefix =
      let start_ =
        let len = String.length prefix in
        let character = lsp_position.character - len in
        { lsp_position with character }
      in
      { Lsp.Protocol.Range.start_; end_ = lsp_position }
    in

    let item index entry =
      let prefix, (entry : Query_protocol.Compl.entry) =
        match entry with
        | `Keep entry -> (`Keep, entry)
        | `Replace (range, entry) -> (`Replace range, entry)
      in
      let kind = completion_kind entry.kind in
      let textEdit =
        match prefix with
        | `Keep -> None
        | `Replace range ->
          Some { Lsp.Protocol.TextEdit.range; newText = entry.name }
      in
      { Lsp.Completion.label = entry.name
      ; kind
      ; detail = Some entry.desc
      ; documentation = Some entry.info
      ; deprecated = entry.deprecated
      ; preselect = None
      ; (* Without this field the client is not forced to respect the order
           provided by merlin. *)
        sortText = Some (Printf.sprintf "%04d" index)
      ; filterText = None
      ; insertText = None
      ; insertTextFormat = None
      ; textEdit
      ; additionalTextEdits = []
      ; commitCharacters = []
      ; data = None
      }
    in

    let completion_kinds =
      [ `Constructor
      ; `Labels
      ; `Modules
      ; `Modules_type
      ; `Types
      ; `Values
      ; `Variants
      ]
    in

    Document_store.get store uri >>= fun doc ->
    let prefix = prefix_of_position (Document.source doc) position in
    log ~title:"debug" "completion prefix: |%s|" prefix;
    let complete =
      Query_protocol.Complete_prefix
        (prefix, position, completion_kinds, true, true)
    in
    let expand =
      Query_protocol.Expand_prefix (prefix, position, completion_kinds, true)
    in

    Document.with_pipeline doc @@ fun pipeline ->
    let completions = Query_commands.dispatch pipeline complete in
    let labels =
      match completions with
      | { Query_protocol.Compl.entries = _; context = `Unknown } -> []
      | { Query_protocol.Compl.entries = _; context = `Application context } ->
        let { Query_protocol.Compl.labels; argument_type = _ } = context in
        List.map
          ~f:(fun (name, typ) ->
            `Keep
              { Query_protocol.Compl.name
              ; kind = `Label
              ; desc = typ
              ; info = ""
              ; deprecated = false (* TODO this is wrong *)
              })
          labels
    in
    let items =
      match (completions, labels) with
      | { Query_protocol.Compl.entries = []; context = _ }, [] ->
        let { Query_protocol.Compl.entries; context = _ } =
          Query_commands.dispatch pipeline expand
        in
        let range = range_prefix prefix in
        List.map ~f:(fun entry -> `Replace (range, entry)) entries
      | { entries; context = _ }, _labels ->
        List.map ~f:(fun entry -> `Keep entry) entries
    in
    let all = List.concat [ labels; items ] in
    let items = List.mapi ~f:item all in
    let resp = { Lsp.Completion.isIncomplete = false; items } in
    Ok (store, resp)
  | Lsp.Client_request.TextDocumentPrepareRename _ -> Ok (store, None)
  | Lsp.Client_request.TextDocumentRename
      { textDocument = { uri }; position; newName } ->
    Document_store.get store uri >>= fun doc ->
    let command =
      Query_protocol.Occurrences (`Ident_at (logical_of_position position))
    in
    let locs : Location.t list = dispatch_in_doc doc command in
    let version = Document.version doc in
    let edits =
      List.map
        ~f:(fun loc ->
          let range = range_of_loc loc in
          { Lsp.Protocol.TextEdit.newText = newName; range })
        locs
    in
    let workspace_edits =
      let documentChanges =
        client_capabilities.workspace.workspaceEdit.documentChanges
      in
      Lsp.Protocol.WorkspaceEdit.make ~documentChanges ~uri ~version ~edits
    in
    Ok (store, workspace_edits)
  | Lsp.Client_request.TextDocumentFoldingRange { textDocument = { uri } } ->
    Document_store.get store uri >>= fun doc ->
    let command = Query_protocol.Outline in
    let outline = dispatch_in_doc doc command in
    let folds : Lsp.Protocol.FoldingRange.result =
      let folding_range (range : Lsp.Protocol.Range.t) =
        { Lsp.Protocol.FoldingRange.startLine = range.start_.line
        ; endLine = range.end_.line
        ; startCharacter = Some range.start_.character
        ; endCharacter = Some range.end_.character
        ; kind = Some Region
        }
      in
      let rec loop acc (items : Query_protocol.item list) =
        match items with
        | [] -> acc
        | item :: items ->
          let range = range_of_loc item.location in
          if range.end_.line - range.start_.line < 2 then
            loop acc items
          else
            let items = item.children @ items in
            let range = folding_range range in
            loop (range :: acc) items
      in
      loop [] outline
      |> List.sort ~compare:(fun x y -> Ordering.of_int (compare x y))
    in
    Ok (store, folds)
  | Lsp.Client_request.SignatureHelp _ -> not_supported ()
  | Lsp.Client_request.TextDocumentLinkResolve l -> Ok (store, l)
  | Lsp.Client_request.TextDocumentLink _ -> Ok (store, [])
  | Lsp.Client_request.WillSaveWaitUntilTextDocument _ -> Ok (store, [])
  | Lsp.Client_request.CodeAction params -> code_action store params
  | Lsp.Client_request.CompletionItemResolve compl -> Ok (store, compl)
  | Lsp.Client_request.TextDocumentFormatting _ -> Ok (store, [])
  | Lsp.Client_request.TextDocumentOnTypeFormatting _ -> Ok (store, [])
  | Lsp.Client_request.SelectionRange _ -> Ok (store, [])
  | Lsp.Client_request.UnknownRequest _ -> Error "got unknown request"

let on_notification rpc store (notification : Lsp.Client_notification.t) =
  let open Lsp.Import.Result.O in
  match notification with
  | TextDocumentDidOpen params ->
    let doc =
      Document.make ~uri:params.textDocument.uri ~text:params.textDocument.text
        ()
    in
    Document_store.put store doc;
    send_diagnostics rpc doc;
    Ok store
  | TextDocumentDidChange { textDocument = { uri; version }; contentChanges } ->
    Document_store.get store uri >>= fun prev_doc ->
    let doc =
      let f doc change = Document.update_text ~version change doc in
      List.fold_left ~f ~init:prev_doc contentChanges
    in
    Document_store.put store doc;
    send_diagnostics rpc doc;
    Ok store
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
      | None -> log ~title:"warn" "unknown notification: %s" req.method_
      | Some json ->
        log ~title:"warn" "unknown notification: %s %a" req.method_
          (fun () -> Yojson.Safe.pretty_to_string ~std:false)
          json );
      Ok store )

let start () =
  let docs = Document_store.make () in
  let prepare_and_run f =
    let f () =
      match f () with
      | Ok s -> Ok s
      | Error e -> Error e
      | exception exn -> Error (Printexc.to_string exn)
    in
    (* TODO: what to do with merlin notifications? *)
    let _notifications = ref [] in
    Logger.with_notifications (ref []) @@ fun () -> File_id.with_cache @@ f
  in
  let on_initialize rpc state params =
    prepare_and_run @@ fun () -> on_initialize rpc state params
  in
  let on_notification rpc state notif =
    prepare_and_run @@ fun () -> on_notification rpc state notif
  in
  let on_request rpc state caps req =
    prepare_and_run @@ fun () -> on_request rpc state caps req
  in
  Lsp.Rpc.start docs { on_initialize; on_request; on_notification } stdin stdout;
  log ~title:"info" "exiting"

let main () =
  (* Setup env for extensions *)
  Unix.putenv "__MERLIN_MASTER_PID" (string_of_int (Unix.getpid ()));
  start ()

let () =
  let open Cmdliner in
  Printexc.record_backtrace true;

  let lsp_server log_file =
    Lsp.Logger.with_log_file ~sections:[ "ocamllsp"; "lsp" ] log_file main
  in

  let log_file =
    let open Arg in
    let doc = "Enable logging to file (pass `-' for logging to stderr)" in
    let env = env_var "OCAML_LSP_SERVER_LOG" in
    value & opt (some string) None & info [ "log-file" ] ~docv:"FILE" ~doc ~env
  in

  let cmd =
    let doc = "Start OCaml LSP server (only stdio transport is supported)" in
    ( Term.(const lsp_server $ log_file)
    , Term.info "ocamllsp" ~doc ~exits:Term.default_exits )
  in

  Term.(exit @@ eval cmd)

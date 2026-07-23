open Import
open Fiber.O

module Resolve = struct
  type t = CompletionParams.t

  let uri (t : t) = t.textDocument.uri
  let yojson_of_t = CompletionParams.yojson_of_t
  let t_of_yojson = CompletionParams.t_of_yojson
  let of_completion_item (ci : CompletionItem.t) = Option.map ci.data ~f:t_of_yojson
end

let completion_kind ~supports_enum_member kind : CompletionItemKind.t option =
  match kind with
  | `Value -> Some Value
  | `Variant -> Some (if supports_enum_member then EnumMember else Constructor)
  | `Label -> Some Field
  | `Module -> Some Module
  | `Modtype -> Some Interface
  | `MethodCall -> Some Method
  | `Keyword -> Some Keyword
  | `Constructor -> Some Constructor
  | `Type -> Some TypeParameter
;;

let prefix_of_position ~short_path source position =
  match Msource.text source with
  | "" -> ""
  | text ->
    let end_of_prefix =
      let (`Offset index) = Msource.get_offset source position in
      min (String.length text - 1) (index - 1)
    in
    let pos =
      (*clamp the length of a line to process at 500 chars, this is just a
        reasonable limit for regex performance*)
      max 0 (end_of_prefix - 500)
    in
    let reconstructed_prefix =
      Prefix_parser.parse ~pos ~len:(end_of_prefix + 1 - pos) text
      |> Option.value ~default:""
      (* We remove the whitespace because merlin expects no whitespace and it's
         semantically meaningless *)
      |> String.filter ~f:(function
        | ' ' | '\n' | '\r' | '\t' | '\012' -> false
        | _ -> true)
    in
    let starts_like_a_path =
      match reconstructed_prefix.[0] with
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '\128' .. '\255'
      | '_' | '\'' | '~' | '?' | '`' -> true
      | _ -> false
      | exception Invalid_argument _ -> false
    in
    if short_path && starts_like_a_path
    then (
      match String.split reconstructed_prefix ~on:'.' |> List.last with
      | Some s -> s
      | None -> reconstructed_prefix)
    else reconstructed_prefix
;;

let ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\128' .. '\255' | '\'' | '_' -> true
  | _ -> false
;;

let operator_char = function
  | '$'
  | '&'
  | '*'
  | '+'
  | '-'
  | '/'
  | '='
  | '>'
  | '@'
  | '^'
  | '|'
  | '~'
  | '!'
  | '?'
  | '%'
  | '<'
  | ':'
  | '.'
  | '#' -> true
  | _ -> false
;;

let suffix_of_position ~is_char source position =
  match Msource.text source with
  | "" -> ""
  | text ->
    let (`Offset index) = Msource.get_offset source position in
    let len = String.length text in
    if index >= len
    then ""
    else (
      let from = index in
      let len =
        let until =
          String.lfindi ~pos:from text ~f:(fun _ c -> not (is_char c))
          |> Option.value ~default:len
        in
        until - from
      in
      String.sub text ~pos:from ~len)
;;

let reconstruct_ident source position =
  let prefix = prefix_of_position ~short_path:false source position in
  let suffix = suffix_of_position ~is_char:ident_char source position in
  let ident = prefix ^ suffix in
  Option.some_if (ident <> "") ident
;;

let text_width ~position_encoding text =
  let start = Position.create ~line:0 ~character:0 in
  let end_ = Position.advance_text ~position_encoding start text in
  if end_.line <> 0 then invalid_arg "Compl.text_width: multiline text";
  end_.character
;;

let range_prefix ~position_encoding (lsp_position : Position.t) prefix : Range.t =
  let character = lsp_position.character - text_width ~position_encoding prefix in
  let start = { lsp_position with character } in
  { Range.start; end_ = lsp_position }
;;

let identifier_range ~position_encoding (position : Position.t) ~prefix ~suffix =
  let prefix_width = text_width ~position_encoding prefix in
  let suffix_width = text_width ~position_encoding suffix in
  let start = { position with character = position.character - prefix_width } in
  let end_ = { position with character = position.character + suffix_width } in
  Range.create ~start ~end_
;;

let merlin_position doc position =
  let offset = Text_document.absolute_position (Document.text_document doc) position in
  Msource.get_logical (Document.source doc) (`Offset offset)
;;

let byte_position (`Logical (line, character)) =
  Position.create ~line:(line - 1) ~character
;;

let position_of_lexical_position ~position_encoding source lex_position =
  let open Option.O in
  let+ byte_position = Position.of_lexical_position lex_position in
  let text = Msource.text source in
  let line_start = lex_position.pos_bol in
  let position = lex_position.pos_cnum in
  if line_start < 0 || position < line_start || position > String.length text
  then byte_position
  else (
    let line_prefix = String.sub text ~pos:line_start ~len:(position - line_start) in
    let character = text_width ~position_encoding line_prefix in
    { byte_position with character })
;;

let range_of_loc ~position_encoding source (loc : Loc.t) =
  match
    ( position_of_lexical_position ~position_encoding source loc.loc_start
    , position_of_lexical_position ~position_encoding source loc.loc_end )
  with
  | Some start, Some end_ -> Range.create ~start ~end_
  | _ -> Range.of_loc loc
;;

let resize_for_edit ~position_encoding { TextEdit.range; newText } =
  let end_ = Position.advance_text ~position_encoding range.start newText in
  { range with end_ }
;;

let sortText_width item_count =
  max 4 (String.length (Int.to_string (max 0 (item_count - 1))))
;;

let sortText_of_index ~width idx = Printf.sprintf "%0*d" width idx

module For_tests = struct
  let sortText_of_index ~item_count idx =
    sortText_of_index ~width:(sortText_width item_count) idx
  ;;
end

module Complete_by_prefix = struct
  let completionItem_of_completion_entry
        idx
        (entry : Query_protocol.Compl.entry)
        ~compl_params
        ~range
        ~supports_deprecated_field
        ~supports_deprecated_tag
        ~supports_enum_member
        ~sort_text_width
    =
    let kind = completion_kind ~supports_enum_member entry.kind in
    let deprecated, tags =
      if not entry.deprecated
      then None, None
      else if supports_deprecated_tag
      then None, Some [ CompletionItemTag.Deprecated ]
      else if supports_deprecated_field
      then Some true, None
      else None, None
    in
    let textEdit = `TextEdit { TextEdit.range; newText = entry.name } in
    CompletionItem.create
      ~label:entry.name
      ?kind
      ~detail:entry.desc
      ?deprecated
      ?tags
        (* Without this field the client is not forced to respect the order
           provided by merlin. *)
      ~sortText:(sortText_of_index ~width:sort_text_width idx)
      ?data:compl_params
      ~textEdit
      ()
  ;;

  let dispatch_cmd ~prefix position pipeline =
    let complete = Query_protocol.Complete_prefix (prefix, position, [], false, true) in
    Query_commands.dispatch pipeline complete
  ;;

  let process_dispatch_resp
        ~position_encoding
        ~supports_deprecated_field
        ~supports_deprecated_tag
        ~supports_enum_member
        ~resolve
        ~prefix
        ~position
        doc
        pos
        (completion : Query_protocol.completions)
    =
    let range =
      range_prefix
        ~position_encoding
        pos
        (prefix_of_position ~short_path:true (Document.Merlin.source doc) position)
    in
    let completion_entries =
      match completion.context with
      | `Unknown -> completion.entries
      | `Application { Query_protocol.Compl.labels; argument_type = _ } ->
        completion.entries
        @ List.map labels ~f:(fun (name, typ) ->
          let name =
            if String.is_prefix prefix ~prefix:"~" && String.is_prefix name ~prefix:"?"
            then "~" ^ String.chop_prefix_if_exists name ~prefix:"?"
            else name
          in
          { Query_protocol.Compl.name
          ; kind = `Label
          ; desc = typ
          ; info = ""
          ; deprecated = false (* TODO this is wrong *)
          })
    in
    (* we need to json-ify completion params to put them in completion item's
       [data] field to keep it across [textDocument/completion] and the
       following [completionItem/resolve] requests *)
    let compl_params =
      match resolve with
      | false -> None
      | true ->
        Some
          (let textDocument =
             TextDocumentIdentifier.create
               ~uri:(Document.uri (Document.Merlin.to_doc doc))
           in
           CompletionParams.create ~textDocument ~position:pos ()
           |> CompletionParams.yojson_of_t)
    in
    let sort_text_width = sortText_width (List.length completion_entries) in
    List.mapi
      completion_entries
      ~f:
        (completionItem_of_completion_entry
           ~supports_deprecated_field
           ~supports_deprecated_tag
           ~supports_enum_member
           ~range
           ~compl_params
           ~sort_text_width)
  ;;

  let can_complete_in position pipeline =
    let typer = Mpipeline.typer_result pipeline in
    let browse = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    let position = Mpipeline.get_lexing_pos pipeline position in
    Mbrowse.deepest_before position [ browse ]
    |> List.exists ~f:(function
      | _, Browse_raw.Expression { exp_desc = Texp_let (_, _, body); _ } ->
        body.exp_loc.loc_ghost
      | _ -> false)
  ;;

  let complete_keywords ~position_encoding ~can_complete_in completion_position prefix =
    match prefix, can_complete_in with
    | ("" | "i" | "in"), true ->
      let ci_for_in =
        CompletionItem.create
          ~label:"in"
          ~textEdit:
            (`TextEdit
                (TextEdit.create
                   ~newText:"in"
                   ~range:(range_prefix ~position_encoding completion_position prefix)))
          ~kind:CompletionItemKind.Keyword
          ()
      in
      [ ci_for_in ]
    | _, _ -> []
  ;;

  let complete
        doc
        prefix
        pos
        ~position
        ~position_encoding
        ~supports_deprecated_field
        ~supports_deprecated_tag
        ~supports_enum_member
        ~resolve
    =
    let+ (completion : Query_protocol.completions), can_complete_in =
      Document.Merlin.with_pipeline_exn ~name:"completion-prefix" doc (fun pipeline ->
        let can_complete_in =
          match prefix with
          | "" | "i" | "in" -> can_complete_in position pipeline
          | _ -> false
        in
        dispatch_cmd ~prefix position pipeline, can_complete_in)
    in
    let keyword_completionItems =
      (* we complete only keyword 'in' for now *)
      match Document.Merlin.kind doc with
      | Intf -> []
      | Impl -> complete_keywords ~position_encoding ~can_complete_in pos prefix
    in
    keyword_completionItems
    @ process_dispatch_resp
        ~position_encoding
        ~supports_deprecated_field
        ~supports_deprecated_tag
        ~supports_enum_member
        ~resolve
        ~prefix
        ~position
        doc
        pos
        completion
  ;;
end

module Complete_with_construct = struct
  let dispatch_cmd position pipeline =
    match
      Exn_with_backtrace.try_with (fun () ->
        let command = Query_protocol.Construct (position, None, None) in
        Query_commands.dispatch pipeline command)
    with
    | Ok (loc, exprs) -> Some (loc, exprs)
    | Error { Exn_with_backtrace.exn = Merlin_analysis.Construct.Not_a_hole; _ } -> None
    | Error exn -> Exn_with_backtrace.reraise exn
  ;;

  let process_dispatch_resp ~position_encoding ~source ~supportsJumpToNextHole = function
    | None -> []
    | Some (loc, constructed_exprs) ->
      let range = range_of_loc ~position_encoding source loc in
      let sort_text_width = sortText_width (List.length constructed_exprs) in
      let deparen_constr_expr expr =
        if
          (not (String.equal expr "()"))
          && String.is_prefix expr ~prefix:"("
          && String.is_suffix expr ~suffix:")"
        then
          expr
          |> String.chop_prefix_if_exists ~prefix:"("
          |> String.chop_suffix_if_exists ~suffix:")"
        else expr
      in
      let completionItem_of_constructed_expr idx expr =
        let expr_wo_parens = deparen_constr_expr expr in
        let edit = { TextEdit.range; newText = expr } in
        let command =
          if supportsJumpToNextHole
          then
            Some
              (Client.Custom_commands.next_hole
                 ~in_range:(resize_for_edit ~position_encoding edit)
                 ~notify_if_no_hole:false
                 ())
          else None
        in
        CompletionItem.create
          ~label:expr_wo_parens
          ~textEdit:(`TextEdit edit)
          ~filterText:("_" ^ expr)
          ~kind:CompletionItemKind.Text
          ~sortText:(sortText_of_index ~width:sort_text_width idx)
          ?command
          ()
      in
      List.mapi constructed_exprs ~f:completionItem_of_constructed_expr
  ;;
end

let complete
      (state : State.t)
      ({ textDocument = { uri }; position = pos; context; _ } : CompletionParams.t)
  =
  Fiber.of_thunk (fun () ->
    let doc = Document_store.get state.store uri in
    match Document.kind doc with
    | `Other -> Fiber.return None
    | `Merlin merlin ->
      let position_encoding = State.position_encoding state in
      let position = merlin_position doc pos in
      let completion_capability =
        let open Option.O in
        let capabilities = State.client_capabilities state in
        let* td = capabilities.textDocument in
        td.completion
      in
      let completion_item_capability =
        let open Option.O in
        let* completion = completion_capability in
        completion.completionItem
      in
      let resolve =
        match
          let open Option.O in
          let* item = completion_item_capability in
          item.resolveSupport
        with
        | None -> false
        | Some { properties } -> List.mem properties ~equal:String.equal "documentation"
      in
      let supports_deprecated_tag =
        match
          let open Option.O in
          let* item = completion_item_capability in
          item.tagSupport
        with
        | None -> false
        | Some { valueSet } ->
          List.mem valueSet CompletionItemTag.Deprecated ~equal:Poly.equal
      in
      let supports_deprecated_field =
        (not supports_deprecated_tag)
        && Option.value
             ~default:false
             (let open Option.O in
              let* item = completion_item_capability in
              item.deprecatedSupport)
      in
      let supports_enum_member =
        Option.value
          ~default:false
          (let open Option.O in
           let* completion = completion_capability in
           let* kinds = completion.completionItemKind in
           let* valueSet = kinds.valueSet in
           Some (List.mem valueSet CompletionItemKind.EnumMember ~equal:Poly.equal))
      in
      let* should_provide_completions =
        match context with
        | Some context ->
          (match context.triggerKind with
           | TriggerCharacter ->
             let+ inside_comment =
               Check_for_comments.position_in_comment
                 ~position:(byte_position position)
                 ~merlin
             in
             (match inside_comment with
              | true -> `Ignore
              | false -> `Provide_completions)
           | Invoked | TriggerForIncompleteCompletions ->
             Fiber.return `Provide_completions)
        | None -> Fiber.return `Provide_completions
      in
      (match should_provide_completions with
       | `Ignore -> Fiber.return None
       | `Provide_completions ->
         let+ items =
           let prefix =
             prefix_of_position ~short_path:false (Document.source doc) position
           in
           if not (Merlin_analysis.Typed_hole.can_be_hole prefix)
           then
             Complete_by_prefix.complete
               merlin
               prefix
               pos
               ~position
               ~position_encoding
               ~supports_deprecated_field
               ~supports_deprecated_tag
               ~supports_enum_member
               ~resolve
           else (
             let reindex_sortText completion_items =
               let width = sortText_width (List.length completion_items) in
               List.mapi completion_items ~f:(fun idx (ci : CompletionItem.t) ->
                 let sortText = Some (sortText_of_index ~width idx) in
                 { ci with sortText })
             in
             let preselect_first =
               match
                 let open Option.O in
                 let* item = completion_item_capability in
                 item.preselectSupport
               with
               | None | Some false -> fun x -> x
               | Some true ->
                 (function
                   | [] -> []
                   | ci :: rest ->
                     { ci with CompletionItem.preselect = Some true } :: rest)
             in
             let+ construct_cmd_resp, compl_by_prefix_resp =
               Document.Merlin.with_pipeline_exn
                 ~name:"completion"
                 merlin
                 (fun pipeline ->
                    let construct_cmd_resp =
                      Complete_with_construct.dispatch_cmd position pipeline
                    in
                    let compl_by_prefix_resp =
                      Complete_by_prefix.dispatch_cmd ~prefix position pipeline
                    in
                    construct_cmd_resp, compl_by_prefix_resp)
             in
             let construct_completionItems =
               let supportsJumpToNextHole =
                 State.experimental_client_capabilities state
                 |> Client.Experimental_capabilities.supportsJumpToNextHole
               in
               Complete_with_construct.process_dispatch_resp
                 ~position_encoding
                 ~source:(Document.source doc)
                 ~supportsJumpToNextHole
                 construct_cmd_resp
             in
             let compl_by_prefix_completionItems =
               Complete_by_prefix.process_dispatch_resp
                 ~position_encoding
                 ~supports_deprecated_field
                 ~supports_deprecated_tag
                 ~resolve
                 ~supports_enum_member
                 ~prefix
                 ~position
                 merlin
                 pos
                 compl_by_prefix_resp
             in
             construct_completionItems @ compl_by_prefix_completionItems
             |> reindex_sortText
             |> preselect_first)
         in
         Some (`CompletionList (CompletionList.create ~isIncomplete:false ~items ()))))
;;

let format_doc ~markdown doc =
  match markdown with
  | false -> `String doc
  | true ->
    `MarkupContent
      (match Doc_to_md.translate doc with
       | Markdown value -> { kind = MarkupKind.Markdown; MarkupContent.value }
       | Raw value -> { kind = MarkupKind.PlainText; MarkupContent.value })
;;

let resolve
      ~position_encoding
      doc
      (compl : CompletionItem.t)
      (resolve : Resolve.t)
      query_doc
      ~markdown
  =
  Fiber.of_thunk (fun () ->
    (* Due to merlin's API, we create a version of the given document with the
       applied completion item and pass it to merlin to get the docs for the
       [compl.label] *)
    let position : Position.t = resolve.position in
    let original_doc = Document.Merlin.to_doc doc in
    let logical_position = merlin_position original_doc position in
    let doc =
      let prefix =
        prefix_of_position ~short_path:true (Document.Merlin.source doc) logical_position
      in
      let suffix =
        let is_operator =
          let rec loop index =
            index = String.length prefix
            || (operator_char prefix.[index] && loop (index + 1))
          in
          (not (String.is_empty prefix)) && loop 0
        in
        let is_char = if is_operator then operator_char else ident_char in
        suffix_of_position ~is_char (Document.Merlin.source doc) logical_position
      in
      let range = identifier_range ~position_encoding position ~prefix ~suffix in
      let complete =
        `TextDocumentContentChangePartial
          (TextDocumentContentChangePartial.create ~range ~text:compl.label ())
      in
      Document.update_text original_doc [ complete ]
    in
    let+ documentation =
      let+ documentation = query_doc (Document.merlin_exn doc) logical_position in
      Option.map ~f:(format_doc ~markdown) documentation
    in
    { compl with documentation; data = None })
;;

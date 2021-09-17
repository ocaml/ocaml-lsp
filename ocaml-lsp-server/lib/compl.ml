open Import
open Fiber.O

module Resolve = struct
  type t = CompletionParams.t

  let uri (t : t) = t.textDocument.uri

  let yojson_of_t = CompletionParams.yojson_of_t

  let t_of_yojson = CompletionParams.t_of_yojson

  let of_completion_item (ci : CompletionItem.t) =
    Option.map ci.data ~f:t_of_yojson
end

let completion_kind kind : CompletionItemKind.t option =
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
  | `Keyword -> Some Keyword

let prefix_of_position ~short_path source position =
  match Msource.text source with
  | "" -> ""
  | text ->
    let from =
      let (`Offset index) = Msource.get_offset source position in
      min (String.length text - 1) (index - 1)
    in
    let pos =
      let should_terminate = ref false in
      let has_seen_dot = ref false in
      let is_prefix_char c =
        if !should_terminate then
          false
        else
          match c with
          | 'a' .. 'z'
          | 'A' .. 'Z'
          | '0' .. '9'
          | '\''
          | '_'
          (* Infix function characters *)
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
          | '!'
          | '?'
          | '%'
          | '<'
          | ':'
          | '~'
          | '#' ->
            true
          | '`' ->
            if !has_seen_dot then
              false
            else (
              should_terminate := true;
              true
            )
          | '.' ->
            has_seen_dot := true;
            not short_path
          | _ -> false
      in
      String.rfindi text ~from ~f:(fun c -> not (is_prefix_char c))
    in
    let pos =
      match pos with
      | None -> 0
      | Some pos -> pos + 1
    in
    let len = from - pos + 1 in
    String.sub text ~pos ~len

let suffix_of_position source position =
  match Msource.text source with
  | "" -> ""
  | text ->
    let (`Offset index) = Msource.get_offset source position in
    let len = String.length text in
    if index >= len then
      ""
    else
      let from = index in
      let len =
        let ident_char = function
          | 'a' .. 'z'
          | 'A' .. 'Z'
          | '0' .. '9'
          | '\''
          | '_' ->
            true
          | _ -> false
        in
        let until =
          String.findi ~from text ~f:(fun c -> not (ident_char c))
          |> Option.value ~default:len
        in
        until - from
      in
      String.sub text ~pos:from ~len

let reconstruct_ident source position =
  let prefix = prefix_of_position ~short_path:false source position in
  let suffix = suffix_of_position source position in
  let ident = prefix ^ suffix in
  Option.some_if (ident <> "") ident

let range_prefix (lsp_position : Position.t) prefix : Range.t =
  let start =
    let len = String.length prefix in
    let character = lsp_position.character - len in
    { lsp_position with character }
  in
  { Range.start; end_ = lsp_position }

let sortText_of_index idx = Printf.sprintf "%04d" idx

module Complete_by_prefix = struct
  let completionItem_of_completion_entry idx
      (entry : Query_protocol.Compl.entry) ~compl_params ~range =
    let kind = completion_kind entry.kind in
    let textEdit = `TextEdit { TextEdit.range; newText = entry.name } in
    CompletionItem.create ~label:entry.name ?kind ~detail:entry.desc
      ~deprecated:
        entry.deprecated
        (* Without this field the client is not forced to respect the order
           provided by merlin. *)
      ~sortText:(sortText_of_index idx) ~data:compl_params ~textEdit ()

  let dispatch_cmd ~prefix position pipeline =
    let complete =
      Query_protocol.Complete_prefix (prefix, position, [], false, true)
    in
    Query_commands.dispatch pipeline complete

  let process_dispatch_resp doc pos (completion : Query_protocol.completions) =
    let range =
      let logical_pos = Position.logical pos in
      range_prefix pos
        (prefix_of_position ~short_path:true (Document.source doc) logical_pos)
    in
    let completion_entries =
      match completion.context with
      | `Unknown -> completion.entries
      | `Application { Query_protocol.Compl.labels; argument_type = _ } ->
        completion.entries
        @ List.map labels ~f:(fun (name, typ) ->
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
      let textDocument =
        TextDocumentIdentifier.create ~uri:(Document.uri doc)
      in
      CompletionParams.create ~textDocument ~position:pos ()
      |> CompletionParams.yojson_of_t
    in
    List.mapi completion_entries
      ~f:(completionItem_of_completion_entry ~range ~compl_params)

  let complete doc prefix pos =
    let+ (completion : Query_protocol.completions) =
      let logical_pos = Position.logical pos in
      Document.with_pipeline_exn doc (dispatch_cmd ~prefix logical_pos)
    in
    process_dispatch_resp doc pos completion
end

module Complete_with_construct = struct
  let dispatch_cmd position pipeline =
    match
      Exn_with_backtrace.try_with (fun () ->
          let command = Query_protocol.Construct (position, None, None) in
          Query_commands.dispatch pipeline command)
    with
    | Ok (loc, exprs) -> Some (loc, exprs)
    | Error { Exn_with_backtrace.exn = Merlin_analysis.Construct.Not_a_hole; _ }
      ->
      None
    | Error exn -> Exn_with_backtrace.reraise exn

  let process_dispatch_resp = function
    | None -> []
    | Some (loc, constructed_exprs) ->
      let range = Range.of_loc loc in
      let deparen_constr_expr expr =
        if
          (not (String.equal expr "()"))
          && String.is_prefix expr ~prefix:"("
          && String.is_suffix expr ~suffix:")"
        then
          String.sub expr ~pos:1 ~len:(String.length expr - 2)
        else
          expr
      in
      let completionItem_of_constructed_expr idx expr =
        let expr_wo_parens = deparen_constr_expr expr in
        let textEdit = `TextEdit { TextEdit.range; newText = expr } in
        let command =
          Client.Vscode.Commands.Custom.next_hole ~start_position:range.start
            ~notify_if_no_hole:false ()
        in
        CompletionItem.create ~label:expr_wo_parens ~textEdit
          ~filterText:("_" ^ expr) ~kind:CompletionItemKind.Text
          ~sortText:(sortText_of_index idx) ~command ()
      in
      List.mapi constructed_exprs ~f:completionItem_of_constructed_expr
end

let complete doc pos =
  let+ items =
    let position = Position.logical pos in
    let prefix =
      prefix_of_position ~short_path:false (Document.source doc) position
    in
    if not (Typed_hole.can_be_hole prefix) then
      Complete_by_prefix.complete doc prefix pos
    else
      let reindex_sortText completion_items =
        List.mapi completion_items ~f:(fun idx (ci : CompletionItem.t) ->
            let sortText = Some (sortText_of_index idx) in
            { ci with sortText })
      in
      let preselect_first = function
        | [] -> []
        | ci :: rest -> { ci with CompletionItem.preselect = Some true } :: rest
      in
      let+ construct_cmd_resp, compl_by_prefix_resp =
        Document.with_pipeline_exn doc (fun pipeline ->
            let construct_cmd_resp =
              Complete_with_construct.dispatch_cmd position pipeline
            in
            let compl_by_prefix_resp =
              Complete_by_prefix.dispatch_cmd ~prefix position pipeline
            in
            (construct_cmd_resp, compl_by_prefix_resp))
      in
      let construct_completionItems =
        Complete_with_construct.process_dispatch_resp construct_cmd_resp
      in
      let compl_by_prefix_completionItems =
        Complete_by_prefix.process_dispatch_resp doc pos compl_by_prefix_resp
      in
      construct_completionItems @ compl_by_prefix_completionItems
      |> reindex_sortText |> preselect_first
  in
  `CompletionList (CompletionList.create ~isIncomplete:false ~items)

let format_doc ~markdown doc =
  match markdown with
  | false -> `String doc
  | true ->
    `MarkupContent
      (match Doc_to_md.translate doc with
      | Markdown value -> { kind = MarkupKind.Markdown; MarkupContent.value }
      | Raw value -> { kind = MarkupKind.PlainText; MarkupContent.value })

let resolve doc (compl : CompletionItem.t) (resolve : Resolve.t) query_doc
    ~markdown =
  (* Due to merlin's API, we create a version of the given document with the
     applied completion item and pass it to merlin to get the docs for the
     [compl.label] *)
  let position : Position.t = resolve.position in
  let logical_position = Position.logical position in
  let* doc =
    let complete =
      let start =
        let prefix =
          prefix_of_position ~short_path:true (Document.source doc)
            logical_position
        in
        { position with character = position.character - String.length prefix }
      in
      let end_ =
        let suffix =
          suffix_of_position (Document.source doc) logical_position
        in
        { position with character = position.character + String.length suffix }
      in
      let range = Range.create ~start ~end_ in
      TextDocumentContentChangeEvent.create ~range ~text:compl.label ()
    in
    Document.update_text doc [ complete ]
  in
  let+ documentation =
    let+ documentation = query_doc doc logical_position in
    Option.map ~f:(format_doc ~markdown) documentation
  in
  { compl with documentation; data = None }

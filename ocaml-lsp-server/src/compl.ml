open Import

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
  | `Snippet -> Some Snippet

let make_string chars =
  let chars = Array.of_list chars in
  String.init (Array.length chars) ~f:(Array.get chars)

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

let range_prefix (lsp_position : Position.t) prefix : Range.t =
  let start =
    let len = String.length prefix in
    let character = lsp_position.character - len in
    { lsp_position with character }
  in
  { Range.start; end_ = lsp_position }

let item index entry =
  let prefix, (entry : Query_protocol.Compl.entry), insertText, insertTextFormat
      =
    match entry with
    | `Keep entry -> (`Keep, entry, None, None)
    | `Replace (range, entry) -> (`Replace range, entry, None, None)
    | `Insert (entry, `PlainText text) ->
      (`Insert, entry, Some text, Some Lsp.Types.InsertTextFormat.PlainText)
    | `Insert (entry, `Snippet snippet) ->
      ( `Insert
      , entry
      , Some (Lsp.Snippet.to_string snippet)
      , Some Lsp.Types.InsertTextFormat.Snippet )
  in
  let kind = completion_kind entry.kind in
  let textEdit =
    match prefix with
    | `Keep
    | `Insert ->
      None
    | `Replace range -> Some { TextEdit.range; newText = entry.name }
  in
  CompletionItem.create ~label:entry.name ?kind ~detail:entry.desc
    ~deprecated:
      entry.deprecated
      (* Without this field the client is not forced to respect the order
         provided by merlin. *)
    ~sortText:(Printf.sprintf "%04d" index)
    ?insertText ?insertTextFormat ?textEdit ()

let completion_kinds =
  [ `Constructor; `Labels; `Modules; `Modules_type; `Types; `Values; `Variants ]

let complete doc position =
  let lsp_position = position in
  let position = Position.logical position in

  let prefix = prefix_of_position (Document.source doc) position in
  log ~title:Logger.Title.Debug "completion prefix: |%s|" prefix;

  Document.with_pipeline doc @@ fun pipeline ->
  let completion =
    let complete =
      Query_protocol.Complete_prefix
        (prefix, position, completion_kinds, true, true)
    in
    (* TODO use Document.dispatch *)
    Query_commands.dispatch pipeline complete
  in
  let items = completion.entries |> List.map ~f:(fun entry -> `Keep entry) in
  let items =
    match completion.context with
    | `Unknown -> items
    | `Application { Query_protocol.Compl.labels; argument_type } ->
      log ~title:Logger.Title.Debug "app. labels=[ %s ], argtype='%s'"
        ( labels
        |> List.map ~f:(fun (a, b) -> sprintf "%s=%s" a b)
        |> String.concat ~sep:", " )
        argument_type;
      let all_labels_item =
        match labels with
        | [] -> []
        | labels ->
          let entry =
            { Query_protocol.Compl.name = "complete all labels"
            ; kind = `Snippet
            ; desc = labels |> List.map ~f:fst |> String.concat ~sep:", "
            ; info = ""
            ; deprecated = false
            }
          in
          let open Lsp.Snippet.Grammar in
          let conv (label, _) =
            let f, name =
              match
                ( String.drop_prefix ~prefix:"~" label
                , String.drop_prefix ~prefix:"?" label )
              with
              | Some name, None -> (text label, name)
              | None, Some name -> (choice [ "~" ^ name; "?" ^ name ], name)
              | _ -> failwith (Printf.sprintf "invalid label '%s" label)
            in
            f +@ ":" ^^ placeholder (text (Printf.sprintf "%s" name))
          in
          let snippet =
            labels |> List.map ~f:conv |> Lsp.Snippet.concat ~sep:(text " ")
          in
          [ `Insert (entry, `Snippet snippet) ]
      in
      let label_items =
        List.map labels ~f:(fun (name, typ) ->
            `Keep
              { Query_protocol.Compl.name
              ; kind = `Label
              ; desc = typ
              ; info = ""
              ; deprecated = false (* TODO this is wrong *)
              })
      in
      all_labels_item @ label_items @ items
  in
  let items =
    match items with
    | _ :: _ -> items
    | [] ->
      let expand =
        Query_protocol.Expand_prefix (prefix, position, completion_kinds, true)
      in
      let { Query_protocol.Compl.entries; context = _ } =
        Query_commands.dispatch pipeline expand
      in
      let range = range_prefix lsp_position prefix in
      List.map ~f:(fun entry -> `Replace (range, entry)) entries
  in
  let items = List.mapi ~f:item items in
  Ok (`CompletionList { CompletionList.isIncomplete = false; items })

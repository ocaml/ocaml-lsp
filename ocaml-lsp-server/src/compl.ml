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
  let prefix, (entry : Query_protocol.Compl.entry) =
    match entry with
    | `Keep entry -> (`Keep, entry)
    | `Replace (range, entry) -> (`Replace range, entry)
  in
  let kind = completion_kind entry.kind in
  let textEdit =
    match prefix with
    | `Keep -> None
    | `Replace range -> Some { TextEdit.range; newText = entry.name }
  in
  CompletionItem.create ~label:entry.name ?kind ~detail:entry.desc
    ~deprecated:
      entry.deprecated
      (* Without this field the client is not forced to respect the order
         provided by merlin. *)
    ~sortText:(Printf.sprintf "%04d" index)
    ?textEdit ()

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
    Query_commands.dispatch pipeline complete
  in
  let items = completion.entries |> List.map ~f:(fun entry -> `Keep entry) in
  let items =
    match completion.context with
    | `Unknown -> items
    | `Application { Query_protocol.Compl.labels; argument_type = _ } ->
      items
      @ List.map labels ~f:(fun (name, typ) ->
            `Keep
              { Query_protocol.Compl.name
              ; kind = `Label
              ; desc = typ
              ; info = ""
              ; deprecated = false (* TODO this is wrong *)
              })
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

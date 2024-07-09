let document = "let mem = ListLabels.mem\n\nlet _ = mem ~se" |> Merlin_kernel.Msource.make

let long_document_text =
  {|let prefix_of_position ~short_path source position =
  let open Prefix_parser in
  match Msource.text source with
  | "" -> ""
  | text ->
    let end_of_prefix =
      let (`Offset index) = Msource.get_offset source position in
      min (String.length text - 1) (index - 1)
    in
    let prefix_text =
      let pos =
        (*clamp the length of a line to process at 500 chars, this is just a
          reasonable limit for regex performance*)
        max 0 (end_of_prefix - 500)
      in
      String.sub text ~pos ~len:(end_of_prefix + 1 - pos)
      (*because all whitespace is semantically the same we convert it all to
        spaces for easier regex matching*)
      |> String.rev_map ~f:(fun x -> if x = '\n' || x = '\t' then ' ' else x)
    in

    let reconstructed_prefix =
      try_parse_with_regex prefix_text
      |> Option.value ~default:""
      |> String.rev_filter ~f:(fun x -> x <> ' ')
    in

    if short_path then
      match String.split_on_char reconstructed_prefix ~sep:'.' |> List.last with
      | Some s -> s
      | None -> reconstructed_prefix
    else reconstructed_prefix

let suffix_of_position source position =
  match Msource.text source with
  | "" -> ""
  | text ->
    let (`Offset index) = Msource.get_offset source position in
    let len = String.length text in
    if index >= len then ""
    else
      let from = index in
      let len =
        let ident_char = function
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\'' | '_' -> true
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
      (entry : Query_protocol.Compl.entry) ~compl_params ~range ~deprecated =
    let kind = completion_kind entry.kind in
    let textEdit = `TextEdit { TextEdit.range; newText = entry.name } in
    CompletionItem.create
      ~label:entry.name
      ?kind
      ~detail:entry.desc
      ?deprecated:(Option.some_if deprecated entry.deprecated)
        (* Without this field the client is not forced to respect the order
           provided by merlin. *)
      ~sortText:(sortText_of_index idx)
      ?data:compl_params
      ~textEdit
      ()

  let dispatch_cmd ~prefix position pipeline =
    let complete =
      Query_protocol.Complete_prefix (prefix, position, [], false, true)
    in
    Query_commands.dispatch pipeline comp
    |}
;;

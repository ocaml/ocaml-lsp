open Types
module String = StringLabels

exception Invalid_utf8

let find_offset ~utf8 ~utf16_range:range =
  let dec =
    Uutf.decoder
      ~nln:(`ASCII (Uchar.of_char '\n'))
      ~encoding:`UTF_8 (`String utf8)
  in
  let utf16_codepoint_size = 4 in
  let utf16_codepoints_buf = Bytes.create utf16_codepoint_size in
  let enc = Uutf.encoder `UTF_16LE `Manual in
  let rec find_char line char =
    if char = 0 || Uutf.decoder_line dec = line then
      Uutf.decoder_byte_count dec
    else
      match Uutf.decode dec with
      | `Await -> raise Invalid_utf8
      | `End -> Uutf.decoder_byte_count dec
      | `Malformed _ ->
        invalid_arg "Text_document.find_offset: utf8 string is malformed"
      | `Uchar _ as u ->
        Uutf.Manual.dst enc utf16_codepoints_buf 0 utf16_codepoint_size;
        (match Uutf.encode enc u with
        | `Partial ->
          (* we always have space for one character *)
          assert false
        | `Ok -> ());
        let char =
          let bytes_read = utf16_codepoint_size - Uutf.Manual.dst_rem enc in
          char - (bytes_read / 2)
        in
        find_char line char
  in
  let rec find_pos (pos : Position.t) =
    if Uutf.decoder_line dec - 1 = pos.line then
      find_char pos.line pos.character
    else
      match Uutf.decode dec with
      | `Uchar _ -> find_pos pos
      | `Malformed _
      | `Await ->
        raise Invalid_utf8
      | `End -> Uutf.decoder_byte_count dec
  in
  let { Range.start; end_ } = range in
  let start_offset = find_pos start in
  let end_offset =
    if start = end_ then
      start_offset
    else if start.line = end_.line then
      find_char start.line (end_.character - start.character)
    else
      find_pos end_
  in
  (start_offset, end_offset)

(* Text is received as UTF-8. However, the protocol specifies offsets should be
   computed based on UTF-16. Therefore we reencode every file into utf16 for
   analysis. *)

type t = TextDocumentItem.t

let text (t : TextDocumentItem.t) = t.text

let make (t : DidOpenTextDocumentParams.t) = t.textDocument

let documentUri (t : TextDocumentItem.t) = t.uri

let version (t : TextDocumentItem.t) = t.version

let languageId (t : TextDocumentItem.t) = t.languageId

let apply_content_change ?version (t : TextDocumentItem.t)
    (change : TextDocumentContentChangeEvent.t) =
  (* Changes can only be applied using utf16 offsets *)
  let version =
    match version with
    | None -> t.version + 1
    | Some version -> version
  in
  match change.range with
  | None -> { t with version; text = change.text }
  | Some utf16_range ->
    let start_offset, end_offset = find_offset ~utf8:t.text ~utf16_range in
    let text =
      String.concat ~sep:""
        [ String.sub t.text ~pos:0 ~len:start_offset
        ; change.text
        ; String.sub t.text ~pos:end_offset
            ~len:(String.length t.text - end_offset)
        ]
    in
    { t with text; version }

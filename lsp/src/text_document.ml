open Types
module String = StringLabels
module List = ListLabels
module Map = MoreLabels.Map

exception Invalid_utf8

exception Outside

let find_nth_nl =
  let rec find_nth_nl str nth pos len =
    if nth = 0 then pos
    else if pos >= len then raise Outside
    else if str.[pos] = '\n' then find_nth_nl str (nth - 1) (pos + 1) len
    else find_nth_nl str nth (pos + 1) len
  in
  fun s ~nth ~start ->
    let len = String.length s in
    match find_nth_nl s nth start len with
    | n -> n
    | exception Outside -> len

let newline = Uchar.of_char '\n'

let find_utf8_pos =
  let rec find_pos newline char dec =
    if char = 0 then Uutf.decoder_byte_count dec
    else
      match Uutf.decode dec with
      | `Malformed _ | `Await -> raise Invalid_utf8
      | `End -> assert false
      | `Uchar u ->
        if Uchar.equal u newline then Uutf.decoder_byte_count dec - 1
        else find_pos newline (char - 1) dec
  in
  fun s ~start ~character ->
    let dec = Uutf.decoder ~nln:(`ASCII newline) ~encoding:`UTF_8 `Manual in
    Uutf.Manual.src
      dec
      (Bytes.unsafe_of_string s)
      start
      (String.length s - start);
    assert (Uutf.decoder_line dec = 1);
    find_pos newline character dec + start

let find_offset_8 ~utf8 range =
  let { Range.start; end_ } = range in
  let start_line_offset = find_nth_nl utf8 ~nth:start.line ~start:0 in
  let end_line_offset =
    if end_.line = start.line then start_line_offset
    else if end_.line > start.line then
      find_nth_nl utf8 ~nth:(end_.line - start.line) ~start:start_line_offset
    else invalid_arg "inverted range"
  in
  let make_offset ~start ~character =
    if start = String.length utf8 then start
    else find_utf8_pos utf8 ~start ~character
  in
  let start_offset =
    make_offset ~start:start_line_offset ~character:start.character
  in
  let end_offset =
    make_offset ~start:end_line_offset ~character:end_.character
  in
  (start_offset, end_offset)

let find_offset_16 ~utf8 range =
  let dec =
    Uutf.decoder ~nln:(`ASCII newline) ~encoding:`UTF_8 (`String utf8)
  in
  let utf16_codepoint_size = 4 in
  let utf16_codepoints_buf = Bytes.create utf16_codepoint_size in
  let enc = Uutf.encoder `UTF_16LE `Manual in
  let rec find_char line char =
    if char = 0 then Uutf.decoder_byte_count dec
    else
      match Uutf.decode dec with
      | `Await -> raise Invalid_utf8
      | `End -> Uutf.decoder_byte_count dec
      | `Malformed _ ->
        invalid_arg "Text_document.find_offset: utf8 string is malformed"
      | `Uchar c as u ->
        if Uchar.equal newline c then Uutf.decoder_byte_count dec - 1
        else (
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
          find_char line char)
  in
  let rec find_pos (pos : Position.t) =
    if Uutf.decoder_line dec = pos.line + 1 then
      find_char pos.line pos.character
    else
      match Uutf.decode dec with
      | `Uchar _ -> find_pos pos
      | `Malformed _ | `Await -> raise Invalid_utf8
      | `End -> Uutf.decoder_byte_count dec
  in
  let { Range.start; end_ } = range in
  let start_offset = find_pos start in
  let end_offset =
    if start = end_ then start_offset
    else if start.line = end_.line then
      find_char start.line (end_.character - start.character)
    else find_pos end_
  in
  (start_offset, end_offset)

(* Text is received as UTF-8. However, the protocol specifies offsets should be
   computed based on UTF-16. Therefore we reencode every file into utf16 for
   analysis. *)

type t =
  { document : TextDocumentItem.t
  ; position_encoding : [ `UTF8 | `UTF16 ]
  }

let text (t : t) = t.document.text

let make ~position_encoding (t : DidOpenTextDocumentParams.t) =
  { document = t.textDocument; position_encoding }

let documentUri (t : t) = t.document.uri

let version (t : t) = t.document.version

let languageId (t : t) = t.document.languageId

let apply_change encoding text (change : TextDocumentContentChangeEvent.t) =
  match change.range with
  | None -> change.text
  | Some range ->
    let start_offset, end_offset =
      let utf8 = text in
      match encoding with
      | `UTF16 -> find_offset_16 ~utf8 range
      | `UTF8 -> find_offset_8 ~utf8 range
    in
    [| Substring.of_slice text ~pos:0 ~len:start_offset
     ; Substring.of_slice change.text ~pos:0 ~len:(String.length change.text)
     ; Substring.of_slice
         text
         ~pos:end_offset
         ~len:(String.length text - end_offset)
    |]
    |> Array_view.make ~pos:0 |> Substring.concat

let apply_content_changes ?version t changes =
  let text =
    List.fold_left
      ~f:(apply_change t.position_encoding)
      ~init:t.document.text
      changes
  in
  let document = { t.document with text } in
  let document =
    match version with
    | None -> document
    | Some version -> { document with version }
  in
  { t with document }

type change =
  { start : int
  ; stop : int
  ; replacement : string
  }

module Edit_map = Map.Make (struct
  type t = int

  let compare = Int.compare
end)

let add_edit map change =
  (* TODO check non overlapping property *)
  Edit_map.update map ~key:change.start ~f:(function
      | None -> Some [ change ]
      | Some changes -> Some (change :: changes))

let simplify_changes text find_position (changes : TextEdit.t list) =
  List.fold_left
    changes
    ~init:Edit_map.empty
    ~f:(fun acc { TextEdit.range; newText = replacement } ->
      let start, stop = find_position ~utf8:text range in
      let change = { start; stop; replacement } in
      add_edit acc change)

let apply_changes text encoding changes =
  let find_position =
    match encoding with
    | `UTF8 -> find_offset_8
    | `UTF16 -> find_offset_16
  in
  let simplified = simplify_changes text find_position changes in
  let pos = ref 0 in
  let b = Buffer.create (String.length text) in
  Edit_map.iter simplified ~f:(fun ~key:start ~data ->
      (* guaranteed by the non overlapping property we aren't yet checking *)
      assert (start >= !pos);
      Buffer.add_substring b text !pos (start - !pos);
      List.rev data
      |> List.iter ~f:(fun { start = start'; stop; replacement } ->
             assert (start = start');
             Buffer.add_string b replacement;
             (* if this is an insert, it's allowed to increase the position *)
             pos := max !pos stop));
  Buffer.add_substring b text !pos (String.length text - !pos);
  Buffer.contents b

let set_version t ~version = { t with document = { t.document with version } }

let apply_text_document_edits t (edits : TextEdit.t list) =
  let text = apply_changes t.document.text t.position_encoding edits in
  let document = { t.document with text } in
  { t with document }

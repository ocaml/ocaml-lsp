include struct
  open Types
  module DidOpenTextDocumentParams = DidOpenTextDocumentParams
  module Range = Range
  module TextDocumentItem = TextDocumentItem
  module TextDocumentContentChangeEvent = TextDocumentContentChangeEvent
  module TextEdit = TextEdit
end

module String = StringLabels
module List = ListLabels
module Map = MoreLabels.Map

exception Outside

exception Invalid_utf8

let newline = Uchar.of_char '\n'

module Rope = struct
  type t = Substring.t list

  let empty = []

  let of_string sub =
    if String.length sub = 0 then empty else [ Substring.of_string sub ]

  let length t =
    List.fold_left ~init:0 t ~f:(fun acc (a : Substring.t) ->
        acc + Substring.length a)

  let to_string t =
    let dst = Bytes.make (length t) '\000' in
    let dst_pos = ref 0 in
    List.iter t ~f:(fun sub ->
        Substring.blit sub ~dst ~dst_pos:!dst_pos;
        dst_pos := !dst_pos + Substring.length sub);
    Bytes.unsafe_to_string dst

  type 's step =
    | Continue of 's
    | Done of int * 's

  type 's iter =
    { step : 's -> Substring.t -> 's step
    ; final : 's -> 's * [ `Success | `Failure ]
    }

  type 's split =
    { state : 's
    ; result : t * t
    }

  let rec consume_lines line sub pos =
    if line = 0 then (line, pos)
    else
      match Substring.index_from sub ~pos '\n' with
      | None -> (line, Substring.length sub)
      | Some j -> consume_lines (line - 1) sub (j + 1)

  let consume_chars chars sub =
    if chars = 0 then 0
    else
      let { Substring.newline; consumed } =
        Substring.count_upto_chars_or_newline sub chars
      in
      if newline then 0 else chars - consumed

  let iter_sub { Position.line; character } sub =
    let line, pos = consume_lines line sub 0 in
    let sub = Substring.drop sub pos in
    let new_character = consume_chars character sub in
    let pos = pos + (character - new_character) in
    ({ Position.line; character = new_character }, pos)

  let iter_position =
    let final state =
      (state, if Position.is_zero state then `Success else `Failure)
    in
    let step position sub =
      let position, consumed = iter_sub position sub in
      if Position.is_zero position then Done (consumed, position)
      else Continue position
    in
    { final; step }

  let split_at : t -> 's iter -> 's -> 's split =
    let rec loop acc state iter = function
      | [] -> (
        let state, status = iter.final state in
        match status with
        | `Success -> { state; result = (acc, []) }
        | `Failure -> { state; result = (acc, []) })
      | x :: xs -> (
        match iter.step state x with
        | Continue state -> loop (x :: acc) state iter xs
        | Done (i, state) ->
          let l, r = Substring.split_at x i in
          let l = List.rev (if Substring.length l = 0 then acc else l :: acc) in
          let r = if Substring.length r = 0 then xs else r :: xs in
          { state; result = (l, r) })
    in
    fun t iter s -> loop [] s iter t

  let drop_while t iter s =
    let { state = _; result = _, r } = split_at t iter s in
    r

  module Cursor = struct
    (* Essentially a zipper on [Rope.t] that also maintains the current line
       based position.

       The goal is to allow for cheap edits near [position] itself. Non adjacent
       edits are allowed to be slow. *)
    type t =
      { left : Substring.t list
      ; right : Substring.t list
      ; position : Position.t
      }

    let cons_sub sub xs = if Substring.length sub = 0 then xs else sub :: xs

    let move_left left lefts rights chars =
      if chars = 0 then (left :: lefts, rights)
      else if chars = Substring.length left then (lefts, left :: rights)
      else
        let l, r = Substring.rsplit_at left chars in
        (l :: lefts, r :: rights)

    let of_rope t = { left = []; right = t; position = Position.zero }

    let to_rope { left; right; position = _ } = List.rev_append left right

    let rec rewind_char t chars =
      if chars = 0 then t
      else
        match t.left with
        | [] -> t
        | x :: xs ->
          let { Substring.newline; consumed } =
            Substring.count_upto_chars_or_newline_backwards x chars
          in
          let position =
            let character =
              if newline then 0 else t.position.character - consumed
            in
            { t.position with character }
          in
          let left, right = move_left x xs t.right consumed in
          let t = { position; left; right } in
          if newline then t else rewind_char t (chars - consumed)

    let jump_char t character =
      if t.position.character = character then t
      else if t.position.character < character then
        let { state; result = left, right } =
          let character = character - t.position.character in
          split_at t.right iter_position (Position.create ~line:0 ~character)
        in
        let position =
          { t.position with character = character - state.character }
        in
        { position; left = List.rev_append left t.left; right }
      else rewind_char t (t.position.character - character)

    let rec rewind_line t lines =
      if lines = 0 then t
      else
        match t.left with
        | [] -> t
        | x :: xs -> (
          match Substring.rindex x '\n' with
          | None ->
            let position =
              let character = t.position.character - Substring.length x in
              { t.position with character }
            in
            { left = xs; right = x :: t.right; position }
          | Some consumed ->
            let left, right = move_left x xs t.right consumed in
            let position =
              Position.create ~character:0 ~line:(t.position.line - 1)
            in
            let t = { position; left; right } in
            rewind_line t (lines - 1))

    let jump_line t line =
      if t.position.line = line then t
      else if t.position.line < line then
        let { state; result = left, right } =
          let line = line - t.position.line in
          split_at t.right iter_position (Position.create ~line ~character:0)
        in
        let position = { t.position with line = line - state.line } in
        { position; left = List.rev_append left t.left; right }
      else rewind_line t (t.position.line - line)

    let jump t { Position.line; character } =
      let t = jump_line t line in
      assert (t.position.line = line);
      jump_char t character

    let drop_until (t : t) (position : Position.t) =
      let delta = Position.subtract position t.position in
      let right = drop_while t.right iter_position delta in
      { t with right }

    let cons t s = { t with right = cons_sub s t.right }

    let update_range (t : t) { Range.start; end_ } replacement : t =
      let t = jump t start in
      let post = drop_until t end_ in
      cons post (Substring.of_string replacement)
  end
end

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

let apply_change _encoding rope (change : TextDocumentContentChangeEvent.t) =
  match change.range with
  | None -> Rope.Cursor.of_rope @@ Rope.of_string change.text
  | Some range -> Rope.Cursor.update_range rope range change.text

let apply_content_changes ?version t changes =
  let text =
    List.fold_left
      ~f:(apply_change t.position_encoding)
      ~init:(Rope.of_string t.document.text |> Rope.Cursor.of_rope)
      changes
    |> Rope.Cursor.to_rope |> Rope.to_string
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

let rec simplify_changes acc text find_position (changes : TextEdit.t list) =
  match changes with
  | [] -> acc
  | { range; newText = replacement } :: changes ->
    let start, stop = find_position ~utf8:text range in
    let change = { start; stop; replacement } in
    let acc = add_edit acc change in
    simplify_changes acc text find_position changes

let apply_changes text encoding changes =
  let find_position =
    match encoding with
    | `UTF8 -> find_offset_8
    | `UTF16 -> find_offset_16
  in
  let simplified = simplify_changes Edit_map.empty text find_position changes in
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

module Private = struct
  module Rope = Rope
end

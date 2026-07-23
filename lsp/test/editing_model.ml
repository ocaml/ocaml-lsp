open Base
module Position = Lsp.Types.Position
module Range = Lsp.Types.Range

type atom =
  | A
  | B
  | Space
  | Tab
  | Nul
  | Newline
  | Two_byte
  | Three_byte
  | Four_byte
[@@deriving quickcheck, sexp_of]

type encoding =
  | UTF8
  | UTF16
[@@deriving quickcheck, sexp_of]

let string_of_atom = function
  | A -> "a"
  | B -> "b"
  | Space -> " "
  | Tab -> "\t"
  | Nul -> "\000"
  | Newline -> "\n"
  | Two_byte -> "é"
  | Three_byte -> "€"
  | Four_byte -> "😀"
;;

let string_of_atoms atoms = List.map atoms ~f:string_of_atom |> String.concat ~sep:""

let lsp_encoding = function
  | UTF8 -> `UTF8
  | UTF16 -> `UTF16
;;

type cursor =
  { byte_offset : int
  ; line : int
  ; character : int
  }
[@@deriving sexp_of]

let position { line; character; byte_offset = _ } = Position.create ~line ~character

let cursors text encoding =
  let text_length = String.length text in
  let rec loop byte_offset line character acc =
    let acc = { byte_offset; line; character } :: acc in
    if byte_offset = text_length
    then List.rev acc
    else (
      let decoded = Stdlib.String.get_utf_8_uchar text byte_offset in
      assert (Stdlib.Uchar.utf_decode_is_valid decoded);
      let uchar = Stdlib.Uchar.utf_decode_uchar decoded in
      let byte_length = Stdlib.Uchar.utf_decode_length decoded in
      if Stdlib.Uchar.equal uchar (Stdlib.Uchar.of_char '\n')
      then loop (byte_offset + byte_length) (line + 1) 0 acc
      else (
        let code_units =
          match encoding with
          | UTF8 -> byte_length
          | UTF16 -> Stdlib.Uchar.utf_16_byte_length uchar / 2
        in
        loop (byte_offset + byte_length) line (character + code_units) acc))
  in
  loop 0 0 0 []
;;

(* Mapping arbitrary selectors to the current text keeps generated operation
   sequences valid even though earlier operations may have changed the document. *)
let index selector ~length = Stdlib.Int.rem (selector land Stdlib.max_int) length

let select_cursor text encoding selector =
  let cursors = cursors text encoding |> Array.of_list in
  cursors.(index selector ~length:(Array.length cursors))
;;

let cursor_at_byte_offset text encoding byte_offset =
  List.find_exn (cursors text encoding) ~f:(fun cursor ->
    cursor.byte_offset = byte_offset)
;;

let ordered_cursors text encoding first second =
  let first = select_cursor text encoding first in
  let second = select_cursor text encoding second in
  if first.byte_offset <= second.byte_offset then first, second else second, first
;;

let replace text ~start ~stop replacement =
  let length = String.length text in
  Stdlib.String.sub text 0 start
  ^ replacement
  ^ Stdlib.String.sub text stop (length - stop)
;;

let slice text ~start ~stop = Stdlib.String.sub text start (stop - start)

let line_starts text =
  let rec loop offset acc =
    if offset = String.length text
    then List.rev acc
    else if Char.equal text.[offset] '\n'
    then loop (offset + 1) ((offset + 1) :: acc)
    else loop (offset + 1) acc
  in
  loop 0 [ 0 ]
;;

let count_newlines_before text stop =
  let rec loop offset count =
    if offset = stop
    then count
    else loop (offset + 1) (if Char.equal text.[offset] '\n' then count + 1 else count)
  in
  loop 0 0
;;

let position_after_text (start : Position.t) text encoding =
  let final = List.last_exn (cursors text encoding) in
  Position.create
    ~line:(start.Position.line + final.line)
    ~character:
      (if final.line = 0 then start.character + final.character else final.character)
;;

let failf fmt = Printf.ksprintf failwith fmt

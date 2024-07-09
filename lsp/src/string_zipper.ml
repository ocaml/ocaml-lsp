open! Import
module Range = Types.Range

type invalid_utf =
  | Malformed of string
  | Insufficient_input

exception Invalid_utf of invalid_utf

let () =
  Printexc.register_printer (function
    | Invalid_utf (Malformed s) -> Some (sprintf "malformed %S" s)
    | Invalid_utf Insufficient_input -> Some "insufficient input"
    | _ -> None)
;;

module T = struct
  type t =
    { left : Substring.t list
    ; rel_pos : int (** the cursor's position inside [current] *)
    ; abs_pos : int (** the total length of strings in [left] *)
    ; current : Substring.t
    (** [current] needed to prevent fragmentation of the substring. E.g.
        so that moving inside the substring doesn't create unnecessary
        splits *)
    ; line : int (** the number of '\n' characters traversed past the current position *)
    ; right : Substring.t list
    }
end

include T

let of_string s =
  { left = []
  ; rel_pos = 0
  ; abs_pos = 0
  ; current = Substring.of_string s
  ; right = []
  ; line = 0
  }
;;

let offset t = t.abs_pos + t.rel_pos

let length =
  let f acc sub = acc + Substring.length sub in
  fun { abs_pos = _; current; left; right; rel_pos = _; line = _ } ->
    let init = Substring.length current in
    let init = List.fold_left ~init ~f left in
    List.fold_left ~init ~f right
;;

let to_string_and_pos t =
  let dst = Bytes.make (length t) '\000' in
  let dst_pos = ref 0 in
  let f sub =
    Substring.blit sub ~dst ~dst_pos:!dst_pos;
    dst_pos := !dst_pos + Substring.length sub
  in
  List.iter (List.rev t.left) ~f;
  let final_pos = t.rel_pos + !dst_pos in
  f t.current;
  List.iter t.right ~f;
  Bytes.unsafe_to_string dst, final_pos, t.line
;;

let to_string t =
  let s, _, _ = to_string_and_pos t in
  s
;;

let squash t =
  let str, rel_pos, line = to_string_and_pos t in
  let current = Substring.of_string str in
  { abs_pos = 0; left = []; right = []; rel_pos; line; current }, str
;;

let empty = of_string ""

let to_string_debug t =
  let left, right = Substring.split_at t.current t.rel_pos in
  List.rev_append t.left (left :: Substring.of_string "|" :: right :: t.right)
  |> List.map ~f:Substring.to_string
  |> String.concat ~sep:""
;;

let cons sub list = if Substring.length sub = 0 then list else sub :: list

let is_end t =
  let res = Substring.length t.current = t.rel_pos in
  if res
  then (
    match t.right with
    | [] -> ()
    | _ :: _ ->
      invalid_arg (sprintf "invalid state: current = %S" (Substring.to_string t.current)));
  res
;;

let is_begin t =
  match t.left with
  | [] -> t.rel_pos = 0
  | _ :: _ -> false
;;

let insert t (x : string) =
  if String.length x = 0
  then t
  else (
    let current = Substring.of_string x in
    if t.rel_pos = 0
    then { t with current; right = cons t.current t.right }
    else if t.rel_pos = Substring.length t.current
    then (
      let abs_pos = t.rel_pos + Substring.length t.current in
      { t with current; rel_pos = 0; left = cons t.current t.left; abs_pos })
    else (
      let l, r = Substring.split_at t.current t.rel_pos in
      let abs_pos = t.abs_pos + Substring.length l in
      { t with current; rel_pos = 0; left = l :: t.left; right = r :: t.right; abs_pos }))
;;

let advance_char t =
  if is_end t
  then t
  else (
    let line =
      match Substring.get_exn t.current t.rel_pos with
      | '\n' -> t.line + 1
      | _ -> t.line
    in
    let rel_pos = t.rel_pos + 1 in
    if rel_pos < Substring.length t.current
    then { t with rel_pos; line }
    else (
      match t.right with
      | [] -> { t with rel_pos; line }
      | current :: right ->
        { abs_pos = t.abs_pos + Substring.length t.current
        ; left = t.current :: t.left
        ; current
        ; line
        ; right
        ; rel_pos = 0
        }))
;;

let rec find_next_nl t =
  if is_end t
  then t
  else (
    match Substring.index_from t.current ~pos:t.rel_pos '\n' with
    | Some rel_pos -> { t with rel_pos }
    | None ->
      (match t.right with
       | [] -> { t with rel_pos = Substring.length t.current }
       | current :: right ->
         let abs_pos = t.abs_pos + Substring.length t.current in
         { t with current; left = t.current :: t.left; right; rel_pos = 0; abs_pos }
         |> find_next_nl))
;;

let rec goto_line_forward t n =
  if n = 0
  then t
  else if is_end t
  then t
  else (
    let t = find_next_nl t in
    let t = advance_char t in
    goto_line_forward t (n - 1))
;;

(* put the cursor left of the previous newline *)
let rec prev_newline t =
  if is_begin t
  then t
  else (
    match Substring.rindex_from t.current ~pos:t.rel_pos '\n' with
    | Some rel_pos -> { t with rel_pos; line = t.line - 1 }
    | None ->
      (match t.left with
       | [] -> { t with rel_pos = 0 }
       | current :: left ->
         prev_newline
           { t with
             current
           ; left
           ; rel_pos = Substring.length current
           ; abs_pos = t.abs_pos + Substring.length t.current
           ; right = t.current :: t.right
           }))
;;

let beginning_of_line t =
  let line = t.line in
  let t = prev_newline t in
  if is_begin t && t.line = line then t else advance_char t
;;

let rec goto_line_backward t = function
  | 0 -> beginning_of_line t
  | n -> goto_line_backward (prev_newline t) (n - 1)
;;

let goto_line t n =
  if t.line = n
  then beginning_of_line t
  else if t.line > n
  then goto_line_backward t (t.line - n)
  else goto_line_forward t (n - t.line)
;;

let newline = Uchar.of_char '\n'
let nln = `ASCII newline

module Advance (Char : sig
    val units_of_char : Uchar.t -> int
  end) : sig
  val advance : t -> code_units:int -> t
end = struct
  let feed_current_chunk dec t = Substring.Uutf.src t.current ~pos:t.rel_pos dec

  let finish_chunk (t : t) consumed =
    let rel_pos = t.rel_pos + consumed in
    if rel_pos < Substring.length t.current
    then { t with rel_pos }
    else (
      assert (rel_pos = Substring.length t.current);
      match t.right with
      | [] -> { t with rel_pos }
      | current :: right ->
        let abs_pos = t.abs_pos + Substring.length t.current in
        { t with current; left = t.current :: t.left; right; abs_pos; rel_pos = 0 })
  ;;

  let rec loop dec (t : t) byte_count_ex_this_chunk (remaining : int) : t =
    if remaining = 0
    then finish_chunk t (Uutf.decoder_byte_count dec - byte_count_ex_this_chunk)
    else (
      match Uutf.decode dec with
      | `Malformed e -> raise (Invalid_utf (Malformed e))
      | `End | `Await -> next_chunk dec t remaining
      | `Uchar u ->
        if Uchar.equal u newline
        then finish_chunk t (Uutf.decoder_byte_count dec - byte_count_ex_this_chunk - 1)
        else (
          let remaining = remaining - Char.units_of_char u in
          loop dec t byte_count_ex_this_chunk remaining))

  and next_chunk dec (t : t) remaining =
    match t.right with
    | [] -> { t with rel_pos = Substring.length t.current }
    | current :: right ->
      let t =
        let abs_pos = t.abs_pos + Substring.length t.current in
        { t with left = t.current :: t.left; current; abs_pos; right; rel_pos = 0 }
      in
      feed_current_chunk dec t;
      loop dec t (Uutf.decoder_byte_count dec) remaining
  ;;

  let advance t ~code_units =
    if code_units = 0
    then t
    else (
      let dec = Uutf.decoder ~nln ~encoding:`UTF_8 `Manual in
      feed_current_chunk dec t;
      loop dec t 0 code_units)
  ;;
end

let advance_utf16 =
  let module Char = struct
    let units_of_char u = Uchar.utf_16_byte_length u / 2
  end
  in
  let module F = Advance (Char) in
  F.advance
;;

let advance_utf8 =
  let module Char = struct
    let units_of_char = Uchar.utf_8_byte_length
  end
  in
  let module F = Advance (Char) in
  F.advance
;;

let drop_until from until =
  if is_end from
  then from
  else (
    let right = cons (Substring.drop until.current until.rel_pos) until.right in
    let left = cons (Substring.take from.current from.rel_pos) from.left in
    match right with
    | current :: right ->
      let abs_pos = from.abs_pos + Substring.length from.current in
      { from with right; left; abs_pos; current; rel_pos = 0 }
    | [] ->
      (match left with
       | [] -> empty
       | current :: left ->
         let rel_pos = Substring.length current in
         let abs_pos = from.abs_pos + rel_pos in
         { from with right; left; current; rel_pos; abs_pos }))
;;

let add_buffer_between b start stop =
  let rec loop bufs = function
    | 0 -> ()
    | remaining ->
      (match bufs with
       | [] ->
         invalid_arg (sprintf "add_buffer_between: not enough remaining (%d)" remaining)
       | current :: bufs ->
         let to_read = min remaining (Substring.length current) in
         let current = Substring.take current to_read in
         Substring.add_buffer current b;
         loop bufs (remaining - Substring.length current))
  in
  let remaining = stop.abs_pos + stop.rel_pos - (start.abs_pos + start.rel_pos) in
  let bufs = cons (Substring.drop start.current start.rel_pos) start.right in
  loop bufs remaining
;;

let rec goto_end t = if is_end t then t else find_next_nl t |> advance_char |> goto_end

let goto_position t (position : Position.t) encoding =
  let advance =
    match encoding with
    | `UTF8 -> advance_utf8
    | `UTF16 -> advance_utf16
  in
  let t = goto_line t position.line in
  advance t ~code_units:position.character
;;

let apply_change t (range : Range.t) encoding ~replacement =
  let advance =
    match encoding with
    | `UTF8 -> advance_utf8
    | `UTF16 -> advance_utf16
  in
  let t = goto_line t range.start.line in
  let t = advance t ~code_units:range.start.character in
  let t' =
    let delta_line = range.end_.line - range.start.line in
    let delta_character =
      if delta_line = 0
      then range.end_.character - range.start.character
      else range.end_.character
    in
    let t = if delta_line = 0 then t else goto_line t range.end_.line in
    advance t ~code_units:delta_character
  in
  insert (drop_until t t') replacement
;;

module Private = struct
  include T

  let reflect x = x
end

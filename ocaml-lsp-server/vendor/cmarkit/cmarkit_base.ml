(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* N.B. The doc strings of the .mli can help understanding these internal
   functions. *)

let sub_includes ~affix s ~first ~last =
  let get = String.get in
  let len_a = String.length affix in
  let len_s = last - first + 1 in
  if len_a > len_s then false else
  let max_idx_a = len_a - 1 in
  let max_idx_s = first + (len_s - len_a) in
  let rec loop i k =
    if i > max_idx_s then false else
    if k > max_idx_a then true else
    if k > 0
    then if get affix k = get s (i + k) then loop i (k + 1) else loop (i + 1) 0
    else if get affix 0 = get s i then loop i 1 else loop (i + 1) 0
  in
  loop first 0

let unsafe_get = String.unsafe_get

module String_set = Set.Make (String)

(* Heterogeneous dictionaries *)

module Dict = struct
  (* Type identifiers, can be deleted once we require 5.1 *)
  module Type = struct
    type (_, _) eq = Equal : ('a, 'a) eq
    module Id = struct
      type _ id = ..
      module type ID = sig type t type _ id += Id : t id end
      type 'a t = (module ID with type t = 'a)

      let make (type a) () : a t =
        (module struct type t = a type _ id += Id : t id end)

      let provably_equal
          (type a b) ((module A) : a t) ((module B) : b t) : (a, b) eq option
        =
        match A.Id with B.Id -> Some Equal | _ -> None

      let uid (type a) ((module A) : a t) =
        Obj.Extension_constructor.id (Obj.Extension_constructor.of_val A.Id)
    end
  end

  module M = Map.Make (Int)
  type 'a key = 'a Type.Id.t
  type binding = B : 'a key * 'a -> binding
  type t = binding M.t

  let key = Type.Id.make
  let empty = M.empty
  let mem k m = M.mem (Type.Id.uid k) m
  let add k v m = M.add (Type.Id.uid k) (B (k, v)) m
  let tag k m = add k () m
  let remove k m = M.remove (Type.Id.uid k) m
  let find : type a. a key -> t -> a option =
  fun k m -> match M.find_opt (Type.Id.uid k) m with
  | None -> None
  | Some B (k', v) ->
      match Type.Id.provably_equal k k' with
      | None -> assert false | Some Type.Equal -> Some v
end

(* Text locations *)

module Textloc = struct

  (* File paths *)

  type fpath = string
  let file_none = "-"
  let pp_path = Format.pp_print_string

  (* Byte positions *)

  type byte_pos = int (* zero-based *)
  let byte_pos_none = -1

  (* Lines *)

  type line_num = int (* one-based *)
  let line_num_none = -1

  (* Line positions

     We keep the byte position of the first element on the line. This
     first element may not exist and be equal to the text length if
     the input ends with a newline. Editors expect tools to compute
     visual columns (not a very good idea). By keeping these byte
     positions we can approximate columns by subtracting the line byte
     position data byte location. This will only be correct on
     US-ASCII data. *)

  type line_pos = line_num * byte_pos
  let line_pos_first = 1, 0
  let line_pos_none = line_num_none, byte_pos_none

  (* Text locations *)

  type t =
    { file : fpath;
      first_byte : byte_pos; last_byte : byte_pos;
      first_line : line_pos; last_line : line_pos }

  let v ~file ~first_byte ~last_byte ~first_line ~last_line =
    { file; first_byte; last_byte; first_line; last_line }

  let file l = l.file
  let first_byte l = l.first_byte
  let last_byte l = l.last_byte
  let first_line l = l.first_line
  let last_line l = l.last_line
  let none =
    let first_byte = byte_pos_none and last_byte = byte_pos_none in
    let first_line = line_pos_none and last_line = line_pos_none in
    v ~file:file_none ~first_byte ~last_byte ~first_line ~last_line

  (* Predicates and comparisons *)

  let is_none l = l.first_byte < 0
  let is_empty l = l.first_byte > l.last_byte
  let equal l0 l1 =
    String.equal l0.file l1.file &&
    Int.equal l0.first_byte l1.first_byte &&
    Int.equal l0.last_byte l1.last_byte

  let compare l0 l1 =
    let c = String.compare l0.file l1.file in
    if c <> 0 then c else
    let c = Int.compare l0.first_byte l1.first_byte in
    if c <> 0 then c else
    Int.compare l0.last_byte l1.last_byte

  (* Shrink and stretch *)

  let set_first l ~first_byte ~first_line = { l with first_byte; first_line }
  let set_last l ~last_byte ~last_line = { l with last_byte; last_line }

  [@@@warning "-6"]
  let to_first l = v l.file l.first_byte l.first_byte l.first_line l.first_line
  let to_last l = v l.file l.last_byte l.last_byte l.last_line l.last_line
  let before l = v l.file l.first_byte byte_pos_none l.first_line line_pos_none
  let after l =
    v l.file (l.first_byte + 1) byte_pos_none l.last_line line_pos_none
  [@@@warning "+6"]

  let span l0 l1 =
    let first_byte, first_line =
      if l0.first_byte < l1.first_byte
      then l0.first_byte, l0.first_line
      else l1.first_byte, l1.first_line
    in
    let last_byte, last_line, file =
      if l0.last_byte < l1.last_byte
      then l1.last_byte, l1.last_line, l1.file
      else l0.last_byte, l0.last_line, l0.file
    in
    v ~file ~first_byte ~first_line ~last_byte ~last_line

  [@@@warning "-6"]
  let reloc ~first ~last =
    v last.file first.first_byte last.last_byte first.first_line last.last_line
  [@@@warning "+6"]

  (* Formatters *)

  let pf = Format.fprintf
  let pp_ocaml ppf l = match is_none l with
  | true -> pf ppf "File \"%a\"" pp_path l.file
  | false ->
      let pp_lines ppf l = match fst l.first_line = fst l.last_line with
      | true -> pf ppf "line %d" (fst l.first_line)
      | false -> pf ppf "lines %d-%d" (fst l.first_line) (fst l.last_line)
      in
      (* "characters" represent positions (insertion points) not columns *)
      let pos_s = l.first_byte - snd l.first_line in
      let pos_e = l.last_byte - snd l.last_line + 1 in
      if pos_s = 0 && pos_e = 0
      then pf ppf "File \"%a\", %a" pp_path l.file pp_lines l
      else pf ppf "File \"%a\", %a, characters %d-%d"
          pp_path l.file pp_lines l pos_s pos_e

  let pp_gnu ppf l = match is_none l with
  | true -> pf ppf "%a:" pp_path l.file
  | false ->
      let pp_lines ppf l =
        let col_s = l.first_byte - snd l.first_line + 1 in
        let col_e = l.last_byte - snd l.last_line + 1 in
        match fst l.first_line = fst l.last_line with
        | true ->  pf ppf "%d.%d-%d" (fst l.first_line) col_s col_e
        | false ->
            pf ppf "%d.%d-%d.%d"
              (fst l.first_line) col_s (fst l.last_line) col_e
      in
      pf ppf "%a:%a" pp_path l.file pp_lines l

  let pp = pp_gnu

  let pp_dump ppf l =
    pf ppf "file:%s bytes:%d-%d lines:%d-%d lines-bytes:%d-%d]"
      l.file l.first_byte l.last_byte (fst l.first_line) (fst l.last_line)
      (snd l.first_line) (snd l.last_line)
end

type line_span =
  { line_pos : Textloc.line_pos;
    first : Textloc.byte_pos;
    last : Textloc.byte_pos }

type line_start = Textloc.byte_pos
type rev_spans = (line_start * line_span) list
type 'a next_line = 'a -> ('a * line_span) option

(* Node meta data *)

module Meta = struct
  type id = int
  type t = { textloc : Textloc.t; id : id; dict : Dict.t }

  let new_id = let id = Atomic.make 0 in fun () -> Atomic.fetch_and_add id 1
  let make ?(textloc = Textloc.none) () =
    { textloc; id = new_id (); dict = Dict.empty }

  let none = make ()
  let id m = m.id
  let textloc m = m.textloc
  let with_textloc ~keep_id m textloc = match keep_id with
  | true -> { m with textloc }
  | false -> { m with textloc; id = new_id () }

  let equal m0 m1 = Int.equal m0.id m1.id
  let compare m0 m1 = Int.compare m0.id m1.id
  let is_none m = equal none m

  type 'a key = 'a Dict.key
  let key = Dict.key
  let mem k m = Dict.mem k m.dict
  let add k v m = { m with dict = Dict.add k v m.dict }
  let tag k m = add k () m
  let remove k m = { m with dict = Dict.remove k m.dict }
  let find k m = Dict.find k m.dict
end

(* US-ASCII processing *)

module Ascii = struct
  let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false
  let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
  let is_upper = function 'A' .. 'Z' -> true | _ -> false
  let is_lower = function 'a' .. 'z' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_hex_digit = function
  | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true | _ -> false

  let hex_digit_to_int = function
  | '0' .. '9' as c -> Char.code c - 0x30
  | 'A' .. 'F' as c -> Char.code c - 0x37
  | 'a' .. 'f' as c -> Char.code c - 0x57
  | _ -> assert false

  let is_alphanum = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false

  let is_white = function
  | '\x20' | '\x09' | '\x0A' | '\x0B' | '\x0C' | '\x0D' -> true | _ -> false

  let is_punct = function
  (* https://spec.commonmark.org/current/#ascii-punctuation-character *)
  | '!' | '\"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+'
  | ',' | '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@'
  | '[' | '\\' | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~' -> true
  | _ -> false

  let is_blank = function ' ' | '\t' -> true | _ -> false

  let caseless_starts_with ~prefix s =
    let get = String.get in
    let len_a = String.length prefix in
    let len_s = String.length s in
    if len_a > len_s then false else
    let max_idx_a = len_a - 1 in
    let rec loop s i max =
      if i > max then true else
      let c = match get s i with
      | 'A' .. 'Z' as c -> Char.(unsafe_chr (code c + 32)) | c -> c
      in
      if get prefix i <> c then false else loop s (i + 1) max
    in
    loop s 0 max_idx_a

  let match' ~sub s ~start =
    (* assert (start + String.length sub - 1 < String.length s) *)
    try
      for i = 0 to String.length sub - 1
      do if s.[start + i] <> sub.[i] then raise_notrace Exit done; true
    with
    | Exit -> false

  let caseless_match ~sub s ~start =
    (* assert (start + String.length sub - 1 < String.length s) *)
    try
      for i = 0 to String.length sub - 1 do
        let c = match s.[start + i] with
        | 'A' .. 'Z' as c -> Char.(unsafe_chr (code c + 32)) | c -> c
        in
        if c <> sub.[i] then raise_notrace Exit
      done;
      true
    with
    | Exit -> false

  let lowercase_sub s first len =
    let b = Bytes.create len in
    for i = 0 to len - 1 do
      let c = match s.[first + i] with
      | 'A' .. 'Z' as c -> Char.(unsafe_chr (code c + 32)) | c -> c
      in
      Bytes.set b i c
    done;
    Bytes.unsafe_to_string b
end

module Text = struct
  let _utf_8_clean_unesc_unref ~do_unesc buf s ~first ~last =
    (* This unescapes CommonMark escapes if [do_unesc] is true,
       resolves entity and character references and replaces U+0000 or
       UTF-8 decoding errors by U+FFFD *)
    let get = String.get in
    let flush buf s last start k =
      if start <= last then Buffer.add_substring buf s start (k - start)
    in
    let rec try_entity_hex ~do_unesc buf s last start num_start k u =
      (* https://spec.commonmark.org/current/\
         #hexadecimal-numeric-character-references *)
      if k > last || k > num_start + 6
      then resolve ~do_unesc buf s last start k else
      match get s k with
      | ';' ->
          let next = k + 1 in
          if k = num_start then resolve ~do_unesc buf s last start next else
          let u =
            if Uchar.is_valid u && u <> 0 then Uchar.unsafe_of_int u else
            Uchar.rep
          in
          flush buf s last start (num_start - 3 (* don't include &#(x|X) *));
          Buffer.add_utf_8_uchar buf u;
          resolve ~do_unesc buf s last next next
      | c when Ascii.is_hex_digit c ->
          let u = u * 16 + (Ascii.hex_digit_to_int c) in
          try_entity_hex ~do_unesc buf s last start num_start (k + 1) u
      | _ ->
          resolve ~do_unesc buf s last start k
    and try_entity_dec ~do_unesc buf s last start num_start k u =
      if k > last || k > num_start + 7
      then resolve ~do_unesc buf s last start k else
      match get s k with
      | ';' ->
          let next = k + 1 in
          if k = num_start then resolve ~do_unesc buf s last start next else
          let u =
            if Uchar.is_valid u && u <> 0 then Uchar.unsafe_of_int u else
            Uchar.rep
          in
          flush buf s last start (num_start - 2 (* don't include &# *));
          Buffer.add_utf_8_uchar buf u;
          resolve ~do_unesc buf s last next next
      | c when Ascii.is_digit c ->
          let u = u * 10 + (Char.code c - 0x30) in
          try_entity_dec ~do_unesc buf s last start num_start (k + 1) u
      | _ ->
          resolve ~do_unesc buf s last start k
    and try_entity_named ~do_unesc buf s last start name_start k =
      (* https://spec.commonmark.org/current/\
         #entity-and-numeric-character-references *)
      if k > last then resolve ~do_unesc buf s last start k else
      match get s k with
      | ';' ->
          let name = String.sub s name_start (k - name_start) in
          begin match Cmarkit_data.html_entity name with
          | None -> resolve ~do_unesc buf s last start (k + 1)
          | Some rep ->
              let next = k + 1 in
              flush buf s last start (name_start - 1 (* don't include & *)) ;
              Buffer.add_string buf rep;
              resolve ~do_unesc buf s last next next
          end
      | c when Ascii.is_alphanum c ->
          try_entity_named ~do_unesc buf s last start name_start (k + 1)
      | _ ->
          resolve ~do_unesc buf s last start k
    and resolve ~do_unesc buf s last start k =
      if k > last then (flush buf s last start k; Buffer.contents buf) else
      let next = k + 1 in
      match get s k with
      | '\x00' ->
          flush buf s last start k; Buffer.add_utf_8_uchar buf Uchar.rep;
          resolve ~do_unesc buf s last next next
      | '\\' when do_unesc ->
          if next > last then resolve ~do_unesc buf s last start next else
          let nc = get s next in
          if not (Ascii.is_punct nc)
          then resolve ~do_unesc buf s last start next else
          let next' = next + 1 in
          (flush buf s last start k; Buffer.add_char buf nc;
           resolve ~do_unesc buf s last next' next')
      | '&' ->
          if k + 2 > last then resolve ~do_unesc buf s last start next else
          begin match get s next with
          | c when Ascii.is_letter c ->
              try_entity_named ~do_unesc buf s last start next next
          | '#' ->
              let next = next + 1 in
              begin match get s next with
              | c when Ascii.is_digit c ->
                  try_entity_dec ~do_unesc buf s last start next next 0
              | 'x' | 'X' ->
                  let next = next + 1 in
                  try_entity_hex ~do_unesc buf s last start next next 0
              | _ -> resolve ~do_unesc buf s last start next
              end
          | _ -> resolve ~do_unesc buf s last start next
          end
      | '\x01' .. '\x7F' -> resolve ~do_unesc buf s last start next
      | b ->
          let d = String.get_utf_8_uchar s k in
          let next = k + Uchar.utf_decode_length d in
          match Uchar.utf_decode_is_valid d with
          | true -> resolve ~do_unesc buf s last start next
          | false ->
              flush buf s last start k;
              Buffer.add_utf_8_uchar buf Uchar.rep;
              resolve ~do_unesc buf s last next next
    in
    let rec check ~do_unesc buf s last start k =
      if k > last then String.sub s first (last - start + 1) else
      match unsafe_get s k with
      | '\\' when do_unesc ->
          Buffer.reset buf; resolve ~do_unesc buf s last start k
      | '&' | '\x00' ->
          Buffer.reset buf; resolve ~do_unesc buf s last start k
      | '\x01' .. '\x7F' ->
          check ~do_unesc buf s last start (k + 1)
      | _ ->
          let d = String.get_utf_8_uchar s k in
          if Uchar.utf_decode_is_valid d
          then check ~do_unesc buf s last start (k + Uchar.utf_decode_length d)
          else (Buffer.reset buf; resolve ~do_unesc buf s last start k)
    in
    if first > last then "" else
    let max = String.length s - 1 in
    let last = if last > max then max else last in
    let first = if first < 0 then 0 else first in
    check ~do_unesc buf s last first first

  let utf_8_clean_unesc_unref buf s ~first ~last =
    _utf_8_clean_unesc_unref ~do_unesc:true buf s ~first ~last

  let utf_8_clean_unref buf s ~first ~last =
    _utf_8_clean_unesc_unref ~do_unesc:false buf s ~first ~last

  let utf_8_clean_raw ?(pad = 0) buf s ~first ~last =
    let get = String.get in
    let padit buf pad = for i = 1 to pad do Buffer.add_char buf ' ' done in
    let clean buf s last first dirty =
      let flush buf s last start k =
        if start <= last then Buffer.add_substring buf s start (k - start);
      in
      let rec loop buf s last start k =
        if k > last then (flush buf s last start k; Buffer.contents buf) else
        match get s k with
        | '\x01' .. '\x7F' (* US-ASCII *) -> loop buf s last start (k + 1)
        | '\x00' ->
            let next = k + 1 in
            flush buf s last start k; Buffer.add_utf_8_uchar buf Uchar.rep;
            loop buf s last next next
        | _ ->
            let d = String.get_utf_8_uchar s k in
            let next = k + Uchar.utf_decode_length d in
            match Uchar.utf_decode_is_valid d with
            | true -> loop buf s last start next
            | false ->
                flush buf s last start k; Buffer.add_utf_8_uchar buf Uchar.rep;
                loop buf s last next next
      in
      flush buf s last first dirty; loop buf s last dirty dirty
    in
    let rec check buf s last first k =
      if k > last then String.sub s first (last - first + 1) else
      match get s k with
      | '\x01' .. '\x7F' (* US-ASCII *) -> check buf s last first (k + 1)
      | '\x00' -> (Buffer.reset buf; clean buf s last first k)
      | _ ->
          let d = String.get_utf_8_uchar s k in
          if Uchar.utf_decode_is_valid d
          then check buf s last first (k + Uchar.utf_decode_length d)
          else (Buffer.reset buf; clean buf s last first k)
    in
    if first > last then
      if pad = 0 then "" else
      (Buffer.reset buf; padit buf pad; Buffer.contents buf)
    else
    let max = String.length s - 1 in
    let last = if last > max then max else last in
    let first = if first < 0 then 0 else first in
    if pad = 0 then check buf s last first first else
    (Buffer.reset buf; padit buf pad; clean buf s last first first)
end

(* Unicode matching *)

let prev_uchar s ~first ~before =
  let rec find_utf_8_starter s ~first ~start =
    if start < first then first else match s.[start] with
    | '\x00' .. '\x7F' | '\xC2' .. '\xDF'
    | '\xE0' .. '\xEF' | '\xF0' .. '\xF4' -> start
    | _ -> find_utf_8_starter s ~first ~start:(start - 1)
  in
  if before <= first then Uchar.of_int 0x0020 (* something white *) else
  let k = find_utf_8_starter s ~first ~start:(before - 1) in
  Uchar.utf_decode_uchar (String.get_utf_8_uchar s k)

let next_uchar s ~last ~after =
  if after >= last then Uchar.of_int 0x0020 (* something white *) else
  Uchar.utf_decode_uchar (String.get_utf_8_uchar s (after + 1))

(* Result types *)

type indent = int
type byte_pos = Textloc.byte_pos
type first = Textloc.byte_pos
type last = Textloc.byte_pos
type next = Textloc.byte_pos
type heading_level = int

(* Runs, blanks and indents *)

let rec run_of ~char s ~last ~start =
  if start > last || s.[start] <> char then start - 1 else
  run_of ~char s ~last ~start:(start + 1)

let rec first_non_blank s ~last ~start =
  if start > last then last + 1 else match s.[start] with
  | ' ' | '\t' -> first_non_blank s ~last ~start:(start + 1)
  | _ -> start

let first_non_blank_in_span s sp =
  first_non_blank s ~last:sp.last ~start:sp.first

let rec last_non_blank s ~first ~start =
  if start < first then first - 1 else match s.[start] with
  | ' ' | '\t' -> last_non_blank s ~first ~start:(start - 1)
  | _ -> start

let rec rev_drop_spaces s ~first ~start =
  if start < first then first - 1 else
  if s.[start] = ' ' then rev_drop_spaces s ~first ~start:(start - 1) else start

let push_span ~line first' last' = function
| (line_start, { line_pos; first; last }) :: acc
  when (fst line_pos) = (fst line.line_pos) -> (* Merge if on same line *)
    (line_start, { line with first; last = last' }) :: acc
| acc ->
    (line.first, { line with first = first'; last = last' }) :: acc

let accept_to ~char ~next_line s lines ~line spans ~after =
  (* Includes final [char] in spans *)
  let rec loop ~char ~next_line s lines line start acc k =
    if k > line.last then match next_line lines with
    | None -> None
    | Some (lines, newline) ->
        let acc = push_span ~line start line.last acc in
        let start = first_non_blank_in_span s newline in
        loop ~char ~next_line s lines newline start acc start
    else
    if s.[k] = char
    then Some (lines, line, push_span ~line start k acc, k)
    else loop ~char ~next_line s lines line start acc (k + 1)
  in
  loop ~char ~next_line s lines line after spans (after + 1)

let accept_upto ~char ~next_line s lines ~line acc ~after =
  (* Does not not include final [char] in spans and continues on
     backslashed [char]. *)
  let rec loop ~char ~next_line s lines line ~prev_bslash start acc k =
    if k > line.last then match next_line lines with
    | None -> None
    | Some (lines, newline) ->
        if newline.first > newline.last (* empty *) then None else
        let acc = push_span ~line start line.last acc in
        let start = first_non_blank_in_span s newline in
        let prev_bslash = false in
        loop ~char ~next_line s lines newline ~prev_bslash start acc start
    else
    if s.[k] = char && not prev_bslash
    then Some (lines, line, push_span ~line start (k - 1) acc, k) else
    let prev_bslash = s.[k] = '\\' && not prev_bslash (* \\ is not *) in
    loop ~char ~next_line s lines line ~prev_bslash start acc (k + 1)
  in
  let start = after + 1 in
  loop ~char ~next_line s lines line ~prev_bslash:false start acc start

let first_non_blank_over_nl ~next_line s lines ~line ~start =
  let nb = first_non_blank s ~last:line.last ~start in
  if nb <= line.last then `This_line nb else
  match next_line lines with
  | None -> `None
  | Some (lines, newline) ->
      let nb = first_non_blank_in_span s newline in
      if nb > newline.last then `None else `Next_line (lines, newline, nb)

let first_non_blank_over_nl' ~next_line s lines ~line spans ~start =
  (* Same as [first_non_blank_over_nl] but pushes skipped data on [spans]. *)
  match first_non_blank_over_nl ~next_line s lines ~line ~start with
  | `None -> None
  | `This_line nb ->
      let line = { line with first = start } (* no layout *) in
      let spans = push_span ~line start (nb - 1) spans in
      Some (lines, line, spans, nb - 1)
  | `Next_line (lines, newline, nb) ->
      let line = { line with first = start } (* no layout *) in
      let spans = push_span ~line start line.last spans in
      Some (lines, newline, spans, nb - 1)

let first_non_escaped_char c s ~last ~start =
  let rec loop c s ~last ~start k =
    if k > last then k else
    if s.[k] = c && (k = start || s.[k - 1] <> '\\') then k else
    loop c s ~last ~start (k + 1)
  in
  loop c s ~last ~start start

(* Autolinks *)

let autolink_email s ~last ~start =
  (* https://spec.commonmark.org/current/#email-address
     Via the ABNF "<" email ">" with email defined by:
     https://html.spec.whatwg.org/multipage/input.html#valid-e-mail-address *)
  let is_atext_plus_dot = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '/' | '=' | '?'
  | '^' | '_' | '`' | '{' | '|' | '}' | '~' | '.' -> true
  | _ -> false
  in
  let is_let_dig = Ascii.is_alphanum in
  let is_let_dig_hyp c = Ascii.is_alphanum c || c = '-' in
  let rec label_seq s last k =
    let rec loop s last c k =
      if k > last then None else
      if is_let_dig_hyp s.[k] && c <= 63 then loop s last (c + 1) (k + 1) else
      if c > 63 || not (is_let_dig s.[k - 1]) then None else
      match s.[k] with
      | '>' -> Some k
      | '.' -> label_seq s last (k + 1)
      | c -> None
    in
    if k > last || not (is_let_dig s.[k]) then None else
    loop s last 1 (k + 1)
  in
  let rec atext_seq s last k =
    if k > last then None else
    if is_atext_plus_dot s.[k] then atext_seq s last (k + 1) else
    if s.[k] = '@' && is_atext_plus_dot s.[k - 1]
    then label_seq s last (k + 1)
    else None
  in
  if start > last || s.[start] <> '<' then None else
  atext_seq s last (start + 1)

let autolink_uri s ~last ~start  =
  (* https://spec.commonmark.org/current/#uri-autolink *)
  let is_scheme_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '+' | '.' | '-' -> true | _ -> false
  in
  let is_uri_char = function
  |'\x00' .. '\x1F' | '\x7F' | ' ' | '<' | '>' -> false | _ -> true
  in
  let rec rest s last k =
    if k > last then None else
    if is_uri_char s.[k] then rest s last (k + 1) else
    if s.[k] = '>' then Some k else None
  in
  let rec scheme s last c k =
    if k > last then None else
    if is_scheme_letter s.[k] && c <= 32 then scheme s last (c + 1) (k + 1) else
    if s.[k] = ':' && 2 <= c && c <= 32 then rest s last (k + 1) else None
  in
  let next = start + 1 in
  if next > last || s.[start] <> '<' || not (Ascii.is_letter s.[next])
  then None else scheme s last 1 (next + 1)

(* Raw HTML *)

let tag_name s ~last ~start : last option =
  (* https://spec.commonmark.org/current/#tag-name *)
  let rec loop s last k =
    if k > last || not (Ascii.is_alphanum s.[k] || s.[k] = '-')
    then Some (k - 1) else loop s last (k + 1)
  in
  if start > last || not (Ascii.is_letter s.[start]) then None else
  loop s last (start + 1)

let attribute_name s ~last ~start : next option =
  (* https://spec.commonmark.org/current/#attribute-name *)
  let is_start = function
  | c when Ascii.is_letter c -> true | '_' | ':' -> true | _ -> false
  in
  let is_cont = function
  | c when Ascii.is_alphanum c -> true | '_' | '.' | ':' | '-' -> true
  | _ -> false
  in
  let rec loop s last k =
    if k > last || not (is_cont s.[k])
    then Some (k - 1) else loop s last (k + 1)
  in
  if start > last || not (is_start s.[start]) then None else
  loop s last (start + 1)

let attribute_value ~next_line s lines ~line spans ~start =
  (* https://spec.commonmark.org/current/#attribute-value *)
  if start > line.last then None else match s.[start] with
  | '\'' | '\"' as char ->
      (* https://spec.commonmark.org/current/#double-quoted-attribute-value
         https://spec.commonmark.org/current/#unquoted-attribute-value *)
      accept_to ~char ~next_line s lines ~line spans ~after:start
  | c ->
      (* https://spec.commonmark.org/current/#unquoted-attribute-value *)
      let cont = function
      | ' ' | '\t' | '\"' | '\'' | '=' | '<' | '>' | '`' -> false | _ -> true
      in
      let rec loop s last k =
        if k > last || not (cont s.[k]) then
          let last = k - 1 in
          Some (lines, line, push_span ~line start last spans, last)
        else loop s last (k + 1)
      in
      loop s line.last (start + 1)

let attribute ~next_line s lines ~line spans ~start =
  (* https://spec.commonmark.org/current/#attribute *)
  (* https://spec.commonmark.org/current/#attribute-value-specification *)
  match attribute_name s ~last:line.last ~start with
  | None -> None
  | Some end_name ->
      let spans = push_span ~line start end_name spans in
      let start = end_name + 1 in
      match first_non_blank_over_nl' ~next_line s lines ~line spans ~start with
      | None -> None
      | Some (lines', line', spans', last_blank) ->
          let nb = last_blank + 1 in
          if s.[nb] <> '='
          then Some (lines, line, spans, end_name) (* no value *) else
          let spans' = push_span ~line nb nb spans' in
          let start = nb + 1 in
          match
            first_non_blank_over_nl'
              ~next_line s lines' ~line:line' spans' ~start
          with
          | None -> None
          | Some (lines, line, spans, last_blank) ->
              let start = last_blank + 1 in
              attribute_value ~next_line s lines ~line spans ~start

let open_tag ~next_line s lines ~line ~start:tag_start = (* tag_start has < *)
  (* https://spec.commonmark.org/current/#open-tag *)
  match tag_name s ~last:line.last ~start:(tag_start + 1) with
  | None -> None
  | Some tag_end_name ->
      let rec loop ~next_line s lines ~line spans ~start =
        match
          first_non_blank_over_nl' ~next_line s lines ~line spans ~start
        with
        | None -> None
        | Some (lines, line, spans, last_blank) ->
            let next = last_blank + 1 in
            match s.[next] with
            | '>' ->
                Some (lines, line, push_span ~line next next spans, next)
            | '/' ->
                let last = next + 1 in
                if last > line.last || s.[last] <> '>' then None else
                Some (lines, line, push_span ~line next last spans, last)
            | c ->
                if next = start then None else
                match attribute ~next_line s lines ~line spans ~start:next with
                | None -> None
                | Some (lines, line, spans, last) ->
                    loop ~next_line s lines ~line spans ~start:(last + 1)
      in
      let start = tag_end_name + 1 in
      let span = { line with first = tag_start; last = tag_end_name} in
      let spans = [tag_start, span] in
      loop ~next_line s lines ~line spans ~start

let closing_tag ~next_line s ls ~line ~start:tag_start = (* start is on </ *)
  (* https://spec.commonmark.org/current/#closing-tag *)
  match tag_name s ~last:line.last ~start:(tag_start + 2) with
  | None -> None
  | Some tag_name_end ->
      let span = { line with first = tag_start; last = tag_name_end} in
      let spans = [tag_start, span] in
      let start = tag_name_end + 1 in
      match first_non_blank_over_nl' ~next_line s ls ~line spans ~start with
      | None -> None
      | Some (lines, line, spans, last_blank) ->
          let last = last_blank + 1 in
          if s.[last] <> '>' then None else
          Some (lines, line, push_span ~line last last spans, last)

let declaration ~next_line s lines ~line ~start = (* start is on <!{letter} *)
  (* https://spec.commonmark.org/current/#declaration *)
  accept_to ~char:'>' ~next_line s lines ~line [] ~after:start

let processing_instruction ~next_line s lines ~line ~start = (* start is on <?*)
  (* https://spec.commonmark.org/current/#processing-instruction *)
  let rec loop ~next_line s lines line start acc k =
    if k > line.last then match next_line lines with
    | None -> None
    | Some (lines, newline) ->
        let acc = push_span ~line start line.last acc in
        let start = first_non_blank_in_span s newline in
        loop ~next_line s lines newline start acc start
    else
    if s.[k] <> '?' then loop ~next_line s lines line start acc (k + 1) else
    let last = k + 1 in
    if last <= line.last && s.[last] = '>' (* ?> *)
    then Some (lines, line, push_span ~line start last acc, last)
    else loop ~next_line s lines line start acc last
  in
  loop ~next_line s lines line start [] (start + 2)

let html_comment ~next_line s lines ~line ~start = (* start is on <!- *)
  (* https://spec.commonmark.org/current/#html-comment *)
  let rec loop ~next_line s lines line start acc k =
    if k > line.last then match next_line lines with
    | None -> None
    | Some (lines, newline) ->
        let acc = push_span ~line start line.last acc in
        let start = first_non_blank_in_span s newline in
        loop ~next_line s lines newline start acc start
    else
    if s.[k] = '-' && s.[k - 1] <> '-' then
      let last = k + 2 in
      if last <= line.last && s.[k + 1] = '-' then
        if s.[last] = '>' (* --> and we do not end with - *)
        then Some (lines, line, push_span ~line start last acc, last)
        else None (* -- in the input *)
      else loop ~next_line s lines line start acc (k + 1)
    else loop ~next_line s lines line start acc (k + 1)
  in
  (* Check we have at least <!-- and not <!--> or <!---> *)
  if (start + 3 > line.last) || not (s.[start + 3] = '-') ||
     (start + 4 <= line.last && s.[start + 4] = '>') ||
     (start + 5 <= line.last && s.[start + 4] = '-' && s.[start + 5] = '>')
  then None else loop ~next_line s lines line start [] (start + 4)

let cdata_section ~next_line s lines ~line ~start = (* start is on <![ *)
  (* https://spec.commonmark.org/current/#cdata-section *)
  let rec loop ~next_line s lines line start acc k =
    if k > line.last then match next_line lines with
    | None -> None
    | Some (lines, newline) ->
        let acc = push_span ~line start line.last acc in
        let start = first_non_blank_in_span s newline in
        loop ~next_line s lines newline start acc start
    else
    if s.[k] <> ']' then loop ~next_line s lines line start acc (k + 1) else
    let last = k + 2 in
    if last <= line.last && s.[k + 1] = ']' && s.[last] = '>' (* ]> *)
    then Some (lines, line, push_span ~line start last acc, last)
    else loop ~next_line s lines line start acc (k + 1)
  in
  if start + 8 > line.last || (* not CDATA[ *)
     not (s.[start + 3] = 'C' && s.[start + 4] = 'D' && s.[start + 5] = 'A' &&
          s.[start + 6] = 'T' && s.[start + 7] = 'A' && s.[start + 8] = '[')
  then None else loop ~next_line s lines line start [] (start + 9)

let raw_html ~next_line s lines ~line ~start =
  (* https://spec.commonmark.org/current/#html-tag *)
  let next = start + 1 and last = line.last in
  if next > last || s.[start] <> '<' then None else match s.[next] with
  | '/' -> closing_tag ~next_line s lines ~line ~start
  | '?' -> processing_instruction ~next_line s lines ~line ~start
  | '!' ->
      let next = next + 1 in
      if next > last then None else
      begin match s.[next] with
      | '-' -> html_comment ~next_line s lines ~line ~start
      | '[' -> cdata_section ~next_line s lines ~line ~start
      | c when Ascii.is_letter c -> declaration ~next_line s lines ~line ~start
      | _ -> None
      end
  | c -> open_tag ~next_line s lines ~line ~start

(* Links *)

let link_destination s ~last ~start =
  let delimited s ~last ~start  = (* start has '<' *)
    (* https://spec.commonmark.org/current/#link-destination 1st *)
    let rec loop s start last prev_byte k =
      if k > last then None else match s.[k] with
      | '\n' | '\r' -> None
      | '\\' when prev_byte = '\\' -> loop s start last '\x00' (k + 1)
      | '<' when prev_byte <> '\\' -> None
      | '>' when prev_byte <> '\\' -> Some (true, (start + 1), k - 1)
      | c -> loop s start last c (k + 1)
    in
    loop s start last '\x00' (start + 1)
  in
  let not_delimited s ~last ~start =
    (* https://spec.commonmark.org/current/#link-destination 2nd *)
    let rec loop s start last prev_byte bal k =
      if k > last then (if bal = 0 then Some (false, start, k - 1) else None)
      else match s.[k] with
      | '\\' when prev_byte = '\\' -> loop s start last '\x00' bal (k + 1)
      | '(' as c when prev_byte <> '\\' -> loop s start last c (bal + 1) (k + 1)
      | ')' as c when prev_byte <> '\\' ->
          let bal = bal - 1 in
          if bal < 0
          then Some (false, start, k - 1) (* hit inline link closing ')' *)
          else loop s start last c bal (k + 1)
      | ' ' -> if k <> start && bal = 0 then Some (false, start, k-1) else None
      | c when Ascii.is_control c ->
          if k <> start && bal = 0 then Some (false, start, k - 1) else None
      | c -> loop s start last c bal (k + 1)
    in
    loop s start last '\x00' 0 start
  in
  if start > last then None else
  if s.[start] = '<'
  then delimited s ~last ~start
  else not_delimited s ~last ~start

let link_title ~next_line s lines ~line ~start =
  (* https://spec.commonmark.org/current/#link-title *)
  let rec paren ~next_line s lines ~line ~prev_bslash start acc k =
    if k > line.last then match next_line lines with
    | None -> None
    | Some (lines, newline) ->
        if newline.first > newline.last (* empty *) then None else
        let acc = push_span ~line start line.last acc in
        let start = first_non_blank_in_span s newline in
        let prev_bslash = false in
        paren ~next_line s lines ~line:newline ~prev_bslash start acc start
    else
    if s.[k] = '(' && not prev_bslash then None else
    if s.[k] = ')' && not prev_bslash
    then Some (lines, line, push_span ~line start (k - 1) acc, k) else
    let prev_bslash = s.[k] = '\\' && not prev_bslash in
    paren ~next_line s lines ~line ~prev_bslash start acc (k + 1)
  in
  if start > line.last then None else match s.[start] with
  | '\"' | '\'' as char ->
      accept_upto ~char ~next_line s lines ~line [] ~after:start
  | '(' ->
      let start = start + 1 and prev_bslash = false in
      paren ~next_line s lines ~line ~prev_bslash start [] start
  | _ -> None

let link_label b ~next_line s lines ~line ~start =
  (* https://spec.commonmark.org/current/#link-label *)
  let rec loop b ~next_line s lines ~line ~prev_byte start acc count k =
    if k > line.last then match next_line lines with
    | None -> None
    | Some (lines, newline) ->
        if newline.first > newline.last (* empty *) then None else
        let acc = push_span ~line start line.last acc in
        let start = first_non_blank_in_span s newline in
        let () = if Buffer.length b <> 0 then Buffer.add_char b ' ' in
        let prev_byte = '\x00' in
        loop b ~next_line s lines ~line:newline ~prev_byte start acc count start
    else
    if count > 999 then None else match s.[k] with
    | '\\' when prev_byte = '\\' ->
        Buffer.add_char b '\\';
        let prev_byte = '\x00' in
        loop b ~next_line s lines ~line ~prev_byte start acc (count + 1) (k + 1)
    | ']' when prev_byte <> '\\' ->
        let key = Buffer.contents b in
        if String.for_all Ascii.is_blank key then None else
        let acc  = push_span ~line start (k - 1) acc in
        Some (lines, line, acc, k, key)
    | '[' when prev_byte <> '\\' -> None
    | ' ' | '\t' as prev_byte ->
        loop b ~next_line s lines ~line ~prev_byte start acc (count + 1) (k + 1)
    | c ->
        let () =
          (* Collapses non initial white *)
          if Ascii.is_blank prev_byte && Buffer.length b <> 0
          then Buffer.add_char b ' '
        in
        let d = String.get_utf_8_uchar s k in
        let u = Uchar.utf_decode_uchar d in
        let u = match Uchar.to_int u with 0x0000 -> Uchar.rep | _ -> u in
        let k' = k + Uchar.utf_decode_length d in
        let () = match Cmarkit_data.unicode_case_fold u with
        | None -> Buffer.add_utf_8_uchar b u
        | Some fold -> Buffer.add_string b fold
        in
        let prev_byte = s.[k] in
        loop b ~next_line s lines ~line ~prev_byte start acc (count + 1) k'
  in
  if start > line.last || s.[start] <> '[' then None else
  let start = start + 1 in
  (Buffer.reset b;
   loop b ~next_line s lines ~line ~prev_byte:'\x00' start [] 0 start)

(* Leaf blocks

   The matching functions assume the indentation has been stripped. *)

type html_block_end_cond =
  [ `End_str of string | `End_cond_1 | `End_blank | `End_blank_7 ]

type line_type =
| Atx_heading_line of heading_level * byte_pos * first * last
| Blank_line
| Block_quote_line
| Fenced_code_block_line of first * last * (first * last) option
| Html_block_line of html_block_end_cond
| Indented_code_block_line
| List_marker_line of ([ `Ordered of int * char | `Unordered of char ] * last)
| Paragraph_line
| Setext_underline_line of heading_level * last
| Thematic_break_line of last
| Ext_table_row of last
| Ext_footnote_label of rev_spans * last * string
| Nomatch

let thematic_break s ~last ~start =
  (* https://spec.commonmark.org/current/#thematic-breaks *)
  let rec loop s last count prev k =
    if k > last
    then (if count < 3 then Nomatch else Thematic_break_line prev) else
    if s.[k] = s.[prev] then loop s last (count + 1) k (k + 1) else
    if s.[k] = ' ' || s.[k] = '\t' then loop s last count prev (k + 1) else
    Nomatch
  in
  if start > last then Nomatch else match s.[start] with
  | '-' | '_' | '*' -> loop s last 1 start (start + 1)
  | _ -> Nomatch

let atx_heading s ~last ~start =
  (* https://spec.commonmark.org/current/#atx-headings *)
  let rec skip_hashes s last k =
    if k > last then k else
    if s.[k] = '#' then skip_hashes s last (k + 1) else k
  in
  let find_end s last k = (* blank on k, last + 1 if blank* [#+] blank* *)
    let after_blank = first_non_blank s ~last ~start:(k + 1) in
    if after_blank > last then after_blank else
    if s.[after_blank] <> '#' then after_blank else
    let after_hash = skip_hashes s last (after_blank + 1) in
    let after_blank = first_non_blank s ~last ~start:after_hash in
    if after_blank > last || after_blank = after_hash then after_blank else
    after_blank - 1 (* this could be the beginning of the end, trigger again *)
  in
  let rec content s last k =
    if k > last then k - 1 else
    if not (s.[k] = ' ' || s.[k] = '\t') then content s last (k + 1) else
    let end' = find_end s last k in
    if end' > last then (k - 1) else content s last end'
  in
  let rec level s last acc k =
    if k > last then Atx_heading_line (acc, k, k, last) else
    if s.[k] = '#' then
      if acc < 6 then level s last (acc + 1) (k + 1) else Nomatch
    else
    let first = first_non_blank s ~last ~start:k in
    if first > last
    then Atx_heading_line (acc, k, last + 1, last) (* empty cases *) else
    if first = k then Nomatch (* need a blank *) else
    let last =
      if s.[first] <> '#' then content s last (first + 1) else
      let end' = find_end s last (first - 1 (* start on blank *)) in
      if end' > last then first - 1 else content s last end'
    in
    Atx_heading_line (acc, k, first, last)
  in
  if start > last || s.[start] <> '#' then Nomatch else
  level s last 1 (start + 1)

let setext_heading_underline s ~last ~start =
  (* https://spec.commonmark.org/current/#setext-heading *)
  let level c = if c = '=' then 1 else 2 in
  let rec underline s last start k =
    if k > last then Setext_underline_line (level s.[start], k - 1) else
    if s.[k] = s.[start] then underline s last start (k + 1) else
    if not (s.[k] = ' ' || s.[k] = '\t') then Nomatch else
    let end_blank = first_non_blank s ~last ~start:(k + 1) in
    if end_blank > last
    then Setext_underline_line (level s.[start], k - 1)
    else Nomatch
  in
  if start > last then Nomatch else
  if not (s.[start] = '-' || s.[start] = '=') then Nomatch else
  underline s last start (start + 1)

let fenced_code_block_start s ~last ~start  =
  (* https://spec.commonmark.org/current/#code-fence *)
  let rec info s last nobt info_first k =
    if k > last then Some (info_first, last) else
    if nobt && s.[k] = '`' then raise_notrace Exit else
    if not (s.[k] = ' ' || s.[k] = '\t')
    then info s last nobt info_first (k + 1) else
    let after_blank = first_non_blank s ~last ~start:k in
    if after_blank > last then Some (info_first, k - 1) else
    info s last nobt info_first after_blank
  in
  let rec fence s last fence_first k =
    if k <= last && s.[k] = s.[fence_first]
    then fence s last fence_first (k + 1) else
    let fence_last = k - 1 in
    let fcount = fence_last - fence_first + 1 in
    if fcount < 3 then Nomatch else
    let info =
      let after_blank = first_non_blank s ~last ~start:k in
      if after_blank > last then None else
      info s last (s.[fence_first] = '`') after_blank after_blank
    in
    Fenced_code_block_line (fence_first, fence_last, info)
  in
  let rec loop s first last k =
    if k > last then Nomatch else
    if k - first + 1 < 4 && s.[k] = ' ' then loop s first last (k + 1) else
    if not (s.[k] = '~' || s.[k] = '`') then Nomatch else
    try fence s last k (k + 1) with
    | Exit (* backtick fence and info *) -> Nomatch
  in
  if start > last then Nomatch else loop s start last start

let fenced_code_block_continue ~fence:(fc, fcount) s ~last ~start =
  (* https://spec.commonmark.org/current/#code-fence *)
  let rec fence s last fence_first k =
    if k <= last && s.[k] = fc then fence s last fence_first (k + 1) else
    let fence_last = k - 1 in
    if fence_last - fence_first + 1 < fcount then raise Exit (* not closing *)
    else
    let after_blank = first_non_blank s ~last ~start:k in
    if after_blank > last then `Close (fence_first, fence_last) else
    raise Exit
  in
  let rec loop s first last k =
    if k > last then `Code (* short blank line *) else
    if k - first + 1 < 4 && s.[k] = ' ' then loop s first last (k + 1) else
    if s.[k] <> fc then `Code else
    try fence s last k (k + 1) with Exit -> `Code
  in
  if start > last then `Code else loop s start last start

let html_start_cond_1_set =
  String_set.of_list ["pre"; "script"; "style"; "textarea"]

let html_start_cond_6_set =
  String_set.of_list
    [ "address"; "article"; "aside"; "base"; "basefont"; "blockquote"; "body";
      "caption"; "center"; "col"; "colgroup"; "dd"; "details"; "dialog"; "dir";
      "div"; "dl"; "dt"; "fieldset"; "figcaption"; "figure"; "footer"; "form";
      "frame"; "frameset"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "head"; "header";
      "hr"; "html"; "iframe"; "legend"; "li"; "link"; "main"; "menu";
      "menuitem"; "nav"; "noframes"; "ol"; "optgroup"; "option"; "p"; "param";
      "section"; "source"; "summary"; "table"; "tbody"; "td"; "tfoot"; "th";
      "thead"; "title"; "tr"; "track"; "ul" ]

let html_block_start_5 s ~last ~start = (* 3 first chars checked *)
  let next = start + 3 and sub = "CDATA[" in
  if start + 8 > last || not (Ascii.match' ~sub s ~start:next) then Nomatch else
  Html_block_line (`End_str "]]>") (* 5 *)

let html_block_start_2 s ~last ~start  = (* 3 first chars checked *)
  let next = start + 3 in
  if next > last || s.[next] <> '-' then Nomatch else
  Html_block_line (`End_str "-->") (* 2 *)

let html_block_start_7_open_tag s ~last ~start =
  (* Has to be on the same line we fake one and use the inline parser *)
  let line = { line_pos = Textloc.line_pos_none; first = start; last } in
  let next_line () = None in
  match open_tag ~next_line s () ~line ~start with
  | None -> Nomatch
  | Some (_, _, _, tag_end) ->
      let next = first_non_blank s ~last ~start:(tag_end + 1) in
      if next > last then Html_block_line `End_blank_7 else Nomatch

let html_block_start_7_close_tag s ~last ~start =
  (* Has to be on the same line we fake one and use the inline parser *)
  let line = { line_pos = Textloc.line_pos_none; first = start; last } in
  let next_line () = None in
  match closing_tag ~next_line s () ~line ~start with
  | None -> Nomatch
  | Some (_, _, _, tag_end) ->
      let next = first_non_blank s ~last ~start:(tag_end + 1) in
      if next > last then Html_block_line `End_blank_7 else Nomatch

let html_block_start s ~last ~start  =
  (* https://spec.commonmark.org/current/#html-blocks *)
  let next = start + 1 in
  if next > last || s.[start] <> '<' then Nomatch else
  match s.[next] with
  | '?' ->  Html_block_line (`End_str "?>") (* 3 *)
  | '!' ->
      let next = next + 1 in
      if next > last then Nomatch else
      begin match s.[next] with
      | '[' -> html_block_start_5 s ~last ~start
      | '-' -> html_block_start_2 s ~last ~start
      | c when Ascii.is_letter c -> Html_block_line (`End_str ">") (* 4 *)
      | _ -> Nomatch
      end
  | c when Ascii.is_letter c || c = '/' ->
      let tag_first = if c = '/' then next + 1 else next in
      let tag_last =
        let rec find_tag_end s last i =
          if i > last || not (Ascii.is_letter s.[i]) then i - 1 else
          find_tag_end s last (i + 1)
        in
        find_tag_end s last tag_first
      in
      let tag = Ascii.lowercase_sub s tag_first (tag_last - tag_first + 1) in
      let is_open_end =
        let n = tag_last + 1 in
        n > last || s.[n] = ' ' || s.[n] = '\t' || s.[n] = '>'
      in
      let is_open_close_end =
        is_open_end ||
        (tag_last + 2 <= last && s.[tag_last + 1] = '/' &&
         s.[tag_last + 2] = '>')
      in
      if c <> '/' then begin
        if String_set.mem tag html_start_cond_1_set && is_open_end
        then Html_block_line `End_cond_1 (* 1 *) else
        if String_set.mem tag html_start_cond_6_set && is_open_close_end
        then Html_block_line `End_blank (* 6 *) else
        html_block_start_7_open_tag s ~last ~start
      end else begin
        if String_set.mem tag html_start_cond_6_set && is_open_close_end
        then Html_block_line `End_blank (* 6 *) else
        html_block_start_7_close_tag s ~last ~start
      end
  | _ -> Nomatch

let html_block_end_cond_1 s ~last ~start =
  (* https://spec.commonmark.org/current/#html-blocks end condition 1. *)
  let rec loop s last k =
    if k + 3 > last then false else
    if s.[k] <> '<' || s.[k + 1] <> '/' then loop s last (k + 1) else
    let next = k + 2 in
    let is_end_tag = match s.[next] with
    | 'p' -> Ascii.caseless_match ~sub:"pre>" s ~start:next
    | 's' ->
        if s.[k + 3] = 't'
        then Ascii.caseless_match ~sub:"style>" s ~start:next
        else Ascii.caseless_match ~sub:"script>" s ~start:next
    | 't' -> Ascii.caseless_match ~sub:"textarea>" s ~start:next
    | _ -> false
    in
    if is_end_tag then true else loop s last (k + 1)
  in
  loop s last start

let html_block_end ~end_cond s ~last ~start = match end_cond with
| `End_str str -> sub_includes ~affix:str s ~first:start ~last
| `End_cond_1 -> html_block_end_cond_1 s ~last ~start
| `End_blank | `End_blank_7 -> first_non_blank s ~last ~start = last + 1

let ext_table_row s ~last ~start =
  if start > last || s.[start] <> '|' then Nomatch else
  let first = start + 1 in
  let last_nb = last_non_blank s ~first ~start:last in
  let before = last_nb - 1 in
  if last_nb < first || s.[last_nb] <> '|' ||
     (before >= first && s.[before] = '\\')
  then Nomatch else Ext_table_row last_nb

let ext_footnote_label buf s ~line_pos ~last ~start =
  if start + 1 > last || s.[start] <> '[' || s.[start + 1] <> '^'
  then Nomatch else
  let rbrack = first_non_escaped_char ']' s ~last ~start:(start + 2) in
  let colon = rbrack + 1 in
  if colon > last || s.[colon] <> ':' || colon - start + 1 < 5 then Nomatch else
  (* Get the normalized label *)
  let line = { line_pos; first = start; last } in
  let next_line () = None in
  match link_label buf ~next_line s () ~line ~start with
  | None -> (* should not happen *) Nomatch
  | Some (_, _, rev_spans, _, key) ->
      Ext_footnote_label (rev_spans, colon, key)

let could_be_link_reference_definition s ~last ~start =
  (* https://spec.commonmark.org/current/#link-reference-definition *)
  let rec loop s first last k =
    if k > last then false else
    if k - first + 1 < 4 && s.[k] = ' ' then loop s first last (k + 1) else
    s.[k] = '['
  in
  if start > last then false else loop s start last start

(* Container blocks *)

let list_marker s ~last ~start =
  (* https://spec.commonmark.org/current/#list-marker *)
  if start > last then Nomatch else match s.[start] with
  | '-' | '+' | '*' as c ->
      let next = start + 1 in
      if next > last || Ascii.is_blank s.[next]
      then List_marker_line (`Unordered c, start)
      else Nomatch
  | '0' .. '9' as c ->
      let[@inline] digit c = Char.code c - 0x30 in
      let rec loop s last count acc k =
        if k > last || count > 9 then Nomatch else
        match s.[k] with
        | '0' .. '9' as c ->
            loop s last (count + 1) (acc * 10 + digit c) (k + 1)
        | '.' | ')' as c ->
            let next = k + 1 in
            if next > last || Ascii.is_blank s.[next]
            then List_marker_line (`Ordered (acc, c), k) else Nomatch
        | _ -> Nomatch
      in
      loop s last 1 (digit c) (start + 1)
  | _ -> Nomatch

let ext_task_marker s ~last ~start =
  if start + 1 > last then None else
  if s.[start] <> '[' then None else
  let next = start + 1 in
  let u = String.get_utf_8_uchar s next in
  if not (Uchar.utf_decode_is_valid u) then None else
  let next = next + Uchar.utf_decode_length u in
  if next > last then None else
  if s.[next] <> ']' then None else
  let next = next + 1 in
  if next > last
  then Some (Uchar.utf_decode_uchar u, last)
  else if s.[next] <> ' ' then None else
  Some (Uchar.utf_decode_uchar u, next)

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

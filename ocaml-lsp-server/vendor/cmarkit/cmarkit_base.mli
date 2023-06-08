(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Low-level internal tools. *)

(** Heterogeneous dictionaries.

    Used by {!Cmarkit.Meta}. *)
module Dict : sig
  type 'a key
  val key : unit -> 'a key
  type t
  val empty : t
  val mem : 'a key -> t -> bool
  val add : 'a key -> 'a -> t -> t
  val tag : unit key -> t -> t
  val remove : 'a key -> t -> t
  val find : 'a key -> t -> 'a option
end

(** Text locations.

   See {!Cmarkit.Textloc} for documentation. *)
module Textloc : sig
  type fpath = string
  val file_none : fpath

  type byte_pos = int
  val byte_pos_none : byte_pos

  type line_num = int
  val line_num_none : line_num

  type line_pos = line_num * byte_pos
  val line_pos_first : line_pos
  val line_pos_none : line_pos

  type t
  val none : t
  val v :
    file:fpath -> first_byte:byte_pos -> last_byte:byte_pos ->
    first_line:line_pos -> last_line:line_pos -> t

  val file : t -> fpath
  val first_byte : t -> byte_pos
  val last_byte : t -> byte_pos
  val first_line : t -> line_pos
  val last_line : t -> line_pos
  val is_none : t -> bool
  val is_empty : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val set_first : t -> first_byte:byte_pos -> first_line:line_pos -> t
  val set_last : t -> last_byte:byte_pos -> last_line:line_pos -> t
  val to_first : t -> t
  val to_last : t -> t
  val before : t -> t
  val after : t -> t
  val span : t -> t -> t
  val reloc : first:t -> last:t -> t
  val pp_ocaml : Format.formatter -> t -> unit
  val pp_gnu : Format.formatter -> t -> unit
  val pp : Format.formatter -> t -> unit
  val pp_dump : Format.formatter -> t -> unit
end

(** Node metadata.

   See {!Cmarkit.Meta} for documentation. *)
module Meta : sig
  type id = int
  type t

  val none : t
  val make : ?textloc:Textloc.t -> unit -> t
  val id : t -> id

  val textloc : t -> Textloc.t
  val with_textloc : keep_id:bool -> t -> Textloc.t -> t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_none : t -> bool

  type 'a key
  val key : unit -> 'a key
  val mem : 'a key -> t -> bool
  val add : 'a key -> 'a -> t -> t
  val tag : unit key -> t -> t
  val remove : 'a key -> t -> t
  val find : 'a key -> t -> 'a option
end

type line_span =
  { line_pos : Textloc.line_pos;
    first : Textloc.byte_pos;
    last : Textloc.byte_pos }
(** The type for line spans. A line position, the first and last
    bytes of the span. If the former is greater than the latter,
    the span is empty. *)

type line_start = Textloc.byte_pos
(** The type for denoting a line start inside
    a CommonMark container (i.e. may not match the text line's first
    character). *)

type rev_spans = (line_start * line_span) list
(** A reversed list of line spans, tupled with the byte position on
    where the line starts (inside a CommonMark container).  The
    [line_start] is the start of line in the container, the
    [line_span] has the actual data. The characters in the
    \[[line_start];[line_span.first - 1]\] are blanks. *)

type 'a next_line = 'a -> ('a * line_span) option
(** The type for getting a new line of input. This is used by certain
    multi-line matchers (e.g. raw HTML).  *)

(** {1:ascii US-ASCII matching} *)

(** US-ASCII matching. *)
module Ascii : sig
  val is_control : char -> bool
  val is_letter : char -> bool
  val is_upper : char -> bool
  val is_lower : char -> bool
  val is_digit : char -> bool
  val is_hex_digit : char -> bool
  val hex_digit_to_int : char -> int
  val is_alphanum : char -> bool
  val is_white : char -> bool
  val is_punct : char -> bool
  val is_blank : char -> bool
  val caseless_starts_with : prefix:string -> string -> bool
end

(** {1:uchar Unicode matching} *)

val prev_uchar : string -> first:int -> before:int -> Uchar.t
(** [prev_uchar s ~first ~before] is the first Unicode character before
    byte position [before] in the range \[[first];[before-1]\]. If
    [before <= first], U+0020 is returned (a Unicode whitespace character). *)

val next_uchar : string -> last:int -> after:int -> Uchar.t
(** [next_uchar s ~last ~after] is the next Unicode character after
    byte position [after] in the range \[[after+1];[last]\]. If [after
    >= last], U+0020 is returned (a Unicode whitespace character). *)

(** {1:content Textual content} *)

(** Textual content.

    Ensures UTF-8 validity, unescapes, resolves numeric and named character
    references. *)
module Text : sig
  val utf_8_clean_unesc_unref :
    Buffer.t -> string -> first:int -> last:int -> string
  (** [utf_8_clean_unesc_unref b s ~first ~last] unescapes CommonMark
      escapes, resolves HTML entity and character references in the
      given span and replaces U+0000 and UTF-8 decoding errors by
      {!Uchar.rep}. [b] is used as scratch space.  If [last > first]
      or [first] and [last] are not valid indices of [s] is [""].  *)

  val utf_8_clean_unref :
    Buffer.t -> string -> first:int -> last:int -> string
  (** [utf_8_clean_unref b s ~first ~last] is like
      {!utf_8_clean_unesc_unref} but does not unsescape. *)

  val utf_8_clean_raw :
    ?pad:int -> Buffer.t -> string -> first:int -> last:int -> string
  (** [utf_8_clean_raw b s ~first ~last] replaces U+0000 and UTF-8
      decoding errors by {!Uchar.rep}. [b] is used as scratch space.
      [pad] (defaults to [0]) specifies a number of U+0020 spaces to prepend.
      If [last > first] or [first] and [last] are not valid indices of
      [s] is either [""] or the padded string. *)
end

(** {1:result Result types} *)

type indent = int
(** The type for indentation magnitude. *)

type byte_pos = Textloc.byte_pos
(** The type for positions. *)

type first = Textloc.byte_pos
(** The type for the first first byte position of a parsed construct. *)

type last = Textloc.byte_pos
(** The type for the last byte position of a parsed construct. *)

type next = Textloc.byte_pos
(** The type for a byte position after a parsed construct. The byte position
    may be invalid (end of input range) or on the newline. *)

type heading_level = int
(** The type for heading levels. *)

(** {1:blanks Newlines, runs, blanks and indents} *)

val run_of : char:char -> string -> last:byte_pos -> start:byte_pos -> last
(** [run_of ~char s ~last ~start] is the last byte position of a
    consecutive run of [char] in the range \[[start];[last]\] or
    [start - 1] if [start] is not [char]. *)

val first_non_blank : string -> last:byte_pos -> start:byte_pos -> byte_pos
(** [first_non_blank s ~last ~start] is the first byte position in the
    range \[[start];[last]\] that is not blank and [last + 1] if there
    is none. *)

val first_non_blank_in_span : string -> line_span -> byte_pos
(** [first_non_blank_in_span s span] is
    [first_non_blank s ~last:span.last ~start:span.first]. *)

val last_non_blank : string -> first:byte_pos -> start:byte_pos -> byte_pos
(** [last_non_blank s ~first ~start] is the last position in the
    range \[[first];[start]\] that is non blank and [first - 1] if
    there is none. *)

val rev_drop_spaces : string -> first:byte_pos -> start:byte_pos -> byte_pos
(** [rev_drop_spaces s ~first ~start] is the last position in the
    range \[[first];[start]\] that is not U+0020 and [first - 1] if
    there is none. *)

val first_non_blank_over_nl :
  next_line: 'a next_line -> string -> 'a -> line:line_span -> start:int ->
  [ `None
  | `This_line of byte_pos
  | `Next_line of 'a * line_span * byte_pos ]
(** [first_non_blank_over_nl ~next_line s ~line ~start] is the first
    byte position starting with [start] that is not blank in [line] or
    on the next line as determined by [next_line]. Returns [`None] if
    there is no such position. *)

val first_non_escaped_char :
  char -> string -> last:byte_pos -> start:byte_pos -> byte_pos
(** [first_non_escaped_char c s ~last ~start] is the first byte position
    in the range \[[start];[last]\] that has [c] unescaped and [last + 1]
    if there is none. *)

(** {1:autolinks Autolinks} *)

val autolink_email : string -> last:byte_pos -> start:byte_pos -> last option
(** [autolink_email s ~last ~start] matches an email autolink starting at
    [start] in the range \[[start];[last]\] (assumed on the same line). *)

val autolink_uri : string -> last:byte_pos -> start:byte_pos -> last option
(** [autolink_uri s ~last ~start] matches an URI autolink starting at
    [start] in the range \[[start];[last]\] (assumed on the same line). *)

(** {1:raw_html Raw HTML} *)

val raw_html :
  next_line:'a next_line -> string -> 'a -> line:line_span -> start:byte_pos ->
  ('a * line_span * rev_spans * last) option
(** [raw_html ~next_line s lines ~line ~start] matches raw HTML on
    line [line] starting at [start]. [next_line] is used to get new
    lines on [lines]. Returns [Some (lines, last_line, spans,
    last_byte)] with [lines] the lines after consuming the raw HTML,
    [last_line] the line where it stops [spans] the byte ranges of [s]
    that make up the raw HTML in reverse order and [last_byte] the
    last byte included in it (guaranteed to be on [last_line]). *)

(** {1:link Links} *)

val link_destination :
  string -> last:byte_pos -> start:byte_pos -> (bool * first * last) option
(** [link_destination s ~last ~start] matches a link destination
    starting at [start] in the range \[[start];[last]\] (assumed on
    the same line). This is [Some (delimited, first, last)] with the
    data in \[[first];[last]\] the destination data. [delimited] is
    [true] if [first-1] is '<' and [last + 1] is '>'. *)

val link_title :
  next_line:'a next_line -> string -> 'a -> line:line_span -> start:byte_pos ->
  ('a * line_span * rev_spans * last) option
(** [link_title ~next_line s lines ~line ~last] is a link title on line [line]
    starting at [start]. Returns [Some (lines, last_line, spans, last)] with
    [lines] the lines after consuming the title, [last_line] the line where
    it stops, [spans] the byte ranges of [s] that make up the title in reverse
    order, [last] is on the closing delimiter and guaranteed to be on
    [last_line]. *)

val link_label :
  Buffer.t -> next_line:'a next_line -> string -> 'a -> line:line_span ->
  start:byte_pos -> ('a * line_span * rev_spans * last * string) option
(** [link_label buf ~next_line s lines ~line ~start] matches a link label
    on line [line] starting at [start]. The byte ranges have the label's
    content, the string is the normalized label. [buf] is used as scratch
    space. *)

(** {1:leaf_block Leaf blocks}

    Unless otherwise noted [start] is always after leading blanks. *)

type html_block_end_cond =
  [ `End_str of string | `End_cond_1 | `End_blank | `End_blank_7 ]
(** The type for HTML block end conditions. *)

type line_type =
| Atx_heading_line of heading_level * byte_pos (* after # *) * first * last
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
| Nomatch (* built-in [None] to avoid option allocs *)

val thematic_break : string -> last:byte_pos -> start:byte_pos -> line_type
(** [thematic_break s ~last ~start] matches a thematic break in the range
    in the range \[[start];[last]\]. The returned position is the last
    non-blank. *)

val atx_heading :
  string -> last:byte_pos -> start:byte_pos -> line_type
(** [atx_heading s ~first ~last] is an ATX heading in the range
    \[[start];[last]\]. *)

val setext_heading_underline :
  string -> last:byte_pos -> start:byte_pos -> line_type
(** [setext_heading_underline s ~last ~start] is a setext heading
    underline in the range \[[start];[last]\]. The returned position
    is the last underline char. *)

val fenced_code_block_start :
  string -> last:byte_pos -> start:byte_pos -> line_type
(** [fenced_code_block_start s ~last ~start] is the start of a fenced
    code block line in the range \[[start];[last]\]. The first span is
    the fence and the second one is the info string (if any). *)

val fenced_code_block_continue :
  fence:char * int -> string -> last:byte_pos -> start:byte_pos ->
  [ `Close of first * last | `Code ]
(** [fenced_code_block_continue ~fence s ~last ~start] indicates
    whether the fence code continues or closes in the the range
    \[[start];[last]\] given the opening [open] which indicates the
    indent, fence char and number of fence chars. *)

val html_block_start :
  string -> last:byte_pos -> start:byte_pos -> line_type
(** [html_block_start s ~last ~start] matches the start of an HTML
    block starting at [start] in the range \[[start];[last]\] and on
    success returns the condition to end it. *)

val html_block_end :
  end_cond:html_block_end_cond -> string -> last:byte_pos -> start:byte_pos ->
  bool
(** [html_block ~end_code s ~last ~start] is [true] if the HTML block
    end with [end_code] in the the range \[[start];[last]\] *)

val ext_table_row : string -> last:byte_pos -> start:byte_pos -> line_type
(** [ext_table s ~last ~start] matches a table row in the range
    \[[start];[last]\]. The returned position is the rightmost [|]. *)

val ext_footnote_label :
  Buffer.t -> string -> line_pos:Textloc.line_pos -> last:byte_pos ->
  start:byte_pos -> line_type
(** [ext_footnote_label s ~last ~start] matches a footnote label the range
    \[[start];[last]\]. The returned position is the rightmost [:].
    This remains on the same line. *)

val could_be_link_reference_definition :
  string -> last:byte_pos -> start:byte_pos -> bool
(** [could_be_link_reference_definition s ~last ~start] is [true] if
    in the range \[[start];[last]\] could hold a link reference definition. *)

(** {1:container Container blocks} *)

val list_marker :
  string -> last:byte_pos -> start:byte_pos -> line_type
(** [list_marker s ~last ~start] is a list marker in the range
    \[[start];[last]\]. This checks there's at least one space
    following unless the item is empty. *)

val ext_task_marker :
  string -> last:byte_pos -> start:byte_pos -> (Uchar.t * last) option
(** [ext_task_marker s ~last ~start] is a list task item marker in the
    range \[[start];[last]\]. *)

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

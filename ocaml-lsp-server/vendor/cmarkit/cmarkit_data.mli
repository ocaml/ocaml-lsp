(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Data needed for CommonMark parsing. *)

(** {1:unicode Unicode data} *)

val unicode_version : string
(** [unicode_version] is the supported Unicode version. *)

val is_unicode_whitespace : Uchar.t -> bool
(** [is_unicode_whitespace u] is [true] iff
    [u] is a CommonMark
    {{:https://spec.commonmark.org/current/#unicode-whitespace-character}
    Unicode whitespace character}. *)

val is_unicode_punctuation : Uchar.t -> bool
(** [is_unicode_punctuation u] is [true] iff
    [u] is a CommonMark
    {{:https://spec.commonmark.org/current/#unicode-punctuation-character}
    Unicode punctuation character}. *)

val unicode_case_fold : Uchar.t -> string option
(** [unicode_case_fold u] is the UTF-8 encoding of [u]'s Unicode
    {{:http://www.unicode.org/reports/tr44/#Case_Folding}case fold} or
    [None] if [u] case folds to itself. *)

(** {1:html HTML data} *)

val html_entity : string -> string option
(** [html_entity e] is the UTF-8 data for of the HTML entity {e name}
    (without [&] and [;]) [e]. *)

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

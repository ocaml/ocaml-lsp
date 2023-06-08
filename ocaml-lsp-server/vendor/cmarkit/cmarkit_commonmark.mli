(*---------------------------------------------------------------------------
   Copyright (c) 2023 The cmarkit programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Rendering CommonMark to CommonMark.

    Generates CommonMark. If your document was parsed with
    [layout:true], it preserves most of the source layout on output.
    This won't be perfect, make sure you understand the
    {{!layout}details} before reporting issues.

    See {{!page-index.quick}an example}.

    {b Warning.} Rendering outputs are unstable. They may be tweaked
    even between minor versions of the library. *)

(** {1:rendering Rendering} *)

val of_doc : Cmarkit.Doc.t -> string
(** [of_doc d] is a CommonMark document for [d]. See {!val-renderer} for
    more details. *)

(** {1:renderer Renderer} *)

val renderer : unit -> Cmarkit_renderer.t
(** [renderer ()] is the default CommonMark renderer. This renders
    the strict CommonMark abstract syntax tree and the supported
    Cmarkit {{!Cmarkit.extensions}extensions}.

    The inline, block and document renderers always return
    [true]. Unknown block and inline values are rendered by an HTML
    comment (as permitted by the CommonMark specification).

    See {{!Cmarkit_renderer.example}this example} to extend or
    selectively override the renderer. *)

(** {1:render Render functions}

    Only useful if you extend the renderer. *)

(** {2:indents Newlines and indentation} *)

val newline : Cmarkit_renderer.context -> unit
(** [newline c] starts a new line, except on the first call on [c] which is
    a nop. *)

type indent =
[ `I of int (** Identation by given amount. *)
| `L of int * string * int * Uchar.t option
   (** Indent before, list marker, indent after, list item task extension *)
| `Q of int (** Identation followed by a block quote marker and a space *)
| `Fn of int * Cmarkit.Label.t (** Indent before, label (footnote extension)*)]
(** The type for specifying block indentation. *)

val push_indent : Cmarkit_renderer.context -> indent -> unit
(** [push_indent c i] pushes [i] on the current indentation of [c]. This
    does not render anything. *)

val pop_indent : Cmarkit_renderer.context -> unit
(** [pop_indent c] pops the last indentation pushed on [c]. This
    does not render anything. *)

val indent : Cmarkit_renderer.context -> unit
(** [indent i c] outputs current indentation on [c]. Note that [`L]
    and [`Fn] get replaced by an [`I] indent on subsequent lines, that
    is the list or foonote marker is output only once. *)

(** {2:bslash Backslash escaping} *)

module Char_set : Set.S with type elt = char
(** Sets of US-ASCII characters. *)

val escaped_string :
  ?esc_ctrl:bool -> Cmarkit_renderer.context -> Char_set.t -> string -> unit
(** [escaped_string ?esc_ctrl c cs s] renders [s] on [c] with
    characters in [cs] backslash escaped. If [esc_ctrl] is [true]
    (default) {{:https://spec.commonmark.org/0.30/#ascii-control-character}
    ASCII control characters} are escaped to decimal escapes. *)

val buffer_add_escaped_string :
  ?esc_ctrl:bool -> Buffer.t -> Char_set.t -> string -> unit
(** [buffer_add_escaped_string b cs s] is {!escaped_string} but
    appends to a buffer value. *)

val escaped_text : Cmarkit_renderer.context -> string -> unit
(** [escaped_text c s] renders [s] on [c] trying to be smart about escaping
    Commonmark structural symbols for {!Cmarkit.Inline.extension-Text} inlines.
    We assume text can be anywhere in a sequence of inlines and in particular
    that it can start a line. This function also takes into account
    the existence of the {{!Cmarkit.extensions}extensions}.

    As such we escape:

    {ul
    {- These block markers: [-] [+] [_] [=] only if present at [s.[0]].}
    {- Only the first of runs of them: [#] [`]}
    {- Only the first of a run longer than 1: [~]
       ({{!Cmarkit.ext_strikethrough}strikethrough extension}).}
    {- [&] if followed by an US-ASCII letter or [#].}
    {- [!] if it is the last character of [s].}
    {- [.] or [)] only if preceeded by a single [1] and zero or more [0] to
      the start of text.}
    {- Everywhere, [*] [_] [\ ] [<] [>] [\[] [\]],
      {{:https://spec.commonmark.org/0.30/#ascii-control-character}
       ASCII control characters}, [$] ({{!Cmarkit.ext_math_inline}inline math
       extension}), [|] ({{!Cmarkit.ext_tables}table extension}) }} *)

val buffer_add_escaped_text : Buffer.t -> string -> unit
(** [buffer_add_escaped_text b s] is {!escaped_text} but appends to
    a buffer value. *)

(** {1:layout Source layout preservation}

    The abstract syntax tree has a few block cases and data fields to
    represent the source document layout. This allows to update
    CommonMark documents without normalizing them too much when they
    are {{!Cmarkit.Doc.of_string}parsed} with [layout:true].

    To keep things reasonably simple a few things are {b not} attempted like:

    {ol
    {- Preserving entities and character references.}
    {- Preserving the exact line by line indentation layout of container
       blocks.}
    {- Preserving lazy continuation lines.}
    {- Keeping track of used newlines except for the first one.}
    {- Preserving layout source location information when it can be
       reconstructed from the document data source location.}}

    In general we try to keep the following desirable properties
    for the abstract syntax tree definition:

    {ol
    {- Layout information should not interfere with document data or
       be affected by it. Otherwise data updates also needs to update
       the layout data, which is error prone and unconvenient.}
    {- Abstract syntax trees resulting from the source document, from
       renders of the source document parsed with or without
       [layout:tree] should all render to the same HTML.}}

    In practice CommonMark being not context free point 1. is not
    always achieved. In particular in {!Cmarkit.Inline.extension-Code_span} the
    number of delimiting backticks depends on the code content
    ({!Cmarkit.Inline.Code_span.of_string}, computes that for you).

    The renderer performs almost no checks on the layout data. You
    should be careful if you fill these yourself since you could
    generate CommonMark that will be misinterpreted. Layout
    data of pristine nodes coming out of {!Cmarkit.Doc.of_string}, created
    with the {!Cmarkit.Inline} and {!Cmarkit.Block} constructors
    should not need your attention (respect their input constraints
    though).  *)

(** {2:rendering_class Classifying renderings}

    We say that a CommonMark render:
    {ul
    {- is {e correct}, if the result renders the same HTML
       as the source document. This can be checked with the
       [cmarkit] tool included in the distribution:
       {[
       cmarkit commonmark --html-diff mydoc.md
       ]}
       If a difference shows up, the rendering is said to be {e incorrect}.}
    {- {e round trips}, if the result is byte-for-byte equal to the
       source document. This can be checked with the [cmarkit] tool
       included in the distribution:
       {[
       cmarkit commonmark --diff mydoc.md
       ]}
       If a difference shows up, the rendering does not round trip but
       it may still be correct.}} *)

(** {2:known_diffs Known correct differences}

    In general lack of round trip is due to:

    {ul
    {- Loss of layout on input (see above).}
    {- Eager escaping of CommonMark delimiters (the escape strategy
       is {{!escaped_text}here}).}
    {- Churn around blank lines which can be part of blocks without
       adhering to their structural convention.}}

    Please do not report issues for differences that are due to the
    following:

    {ol
    {- Source US-ASCII control characters in textual data render as decimal
       character references in the output.}
    {- Source entity and character references are lost during parsing and
       thus replaced by their definition in the output.}
    {- Source tab stop advances may be replaced by spaces in the output.}
    {- Source escaped characters may end up unescaped in the output.}
    {- Source unescaped characters may end up escaped in the output.}
    {- Source lazy continuation lines are made part of blocks in the output.}
    {- Source indented blank lines following indented code blocks
       lose four spaces of indentation (as per specification these are not
       part of the block).}
    {- Source unindented blank lines in indented code blocks are indented
       in the output.}
    {- Source fenced code block indentation is retained from the opening
       fence and used for the following lines in the output.}
    {- Source block quote indentation is retained from the first line
       and used for the following lines in the output. The optional space
       following the quotation mark ['>'] is made mandatory. }
    {- Source list item indentation is regularized, in particular blank lines
       will indent.}
    {- Source list item that start with an empty line get a space after
       their marker.}
    {- The newline used in the output is the one found in the rendered
      {!Cmarkit.Doc.t} value.}}

    {e Simple} and {e implemented} round trip improvements to the
    renderer are welcome.

    {2:known_incorrect Known incorrect renderings}

    Please do not report issues incorrect renderings that are due to the
    following (and unlikely to be fixed):

    {ol
    {- Use of entities and character references around structural
       CommonMark symbols can make things go wrong. These get resolved
       after inline parsing because they can't be used to stand for
       structural CommonMark symbols, however once they have been resolved they
       can interact with parsing. Here is an example:
       {[
         *emph&nbsp;*
       ]}
       It parses as emphasis. But if we render it to CommonMark
       non-breaking space renders as is and we get:
       {[
         *emph *
       ]}
       which no longer parses as emphasis.

       Note in this particular case it is possible to do something
       about it by being smarter about the context when escaping. However
       there's a trade-off between renderer complexity and the (conjectured)
       paucity of these cases.}
    }

    Otherwise, if you spot an incorrect rendering please report a minimal
    reproduction case.

    {e Simple} and {e implemented} round trip improvements to the
    renderer are welcome.
 *)

(*---------------------------------------------------------------------------
   Copyright (c) 2023 The cmarkit programmers

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

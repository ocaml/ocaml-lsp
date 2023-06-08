(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Rendering CommonMark to HTML.

    Generates HTML fragments, consult the
    {{!integration}integration notes} for requirements on the webpage.

    See {{!page-index.quick}a quick example} and
    {{!page_frame}another one}.

    {b Warning.} Rendering outputs are unstable, they may be tweaked
    even between minor versions of the library. *)

(** {1:rendering Rendering} *)

val of_doc : ?backend_blocks:bool -> safe:bool -> Cmarkit.Doc.t -> string
(** [of_doc ~safe d] is an HTML fragment for [d]. See {!renderer}
    for more details and documentation about rendering options. *)

(** {1:renderers Renderers} *)

val renderer : ?backend_blocks:bool -> safe:bool -> unit -> Cmarkit_renderer.t
(** [renderer ~safe ()] is the default HTML renderer. This renders the
    strict CommonMark abstract syntax tree and the supported Cmarkit
    {{!Cmarkit.extensions}extensions}.

    The inline, block and document renderers always return
    [true]. Unknown block and inline values are rendered by an HTML
    comment.

    The following options are available:

    {ul
    {- [safe], if [true] {{!Cmarkit.Block.extension-Html_block}HTML blocks} and
       {{!Cmarkit.Inline.extension-Raw_html}raw HTML inlines} are discarded and
       replaced by an HTML comment in the output. Besides the URLs of
       autolinks, links and images that satisfy
       {!Cmarkit.Inline.Link.is_unsafe} are replaced by the empty string.

       Using safe renderings is a good first step at preventing
       {{:https://en.wikipedia.org/wiki/Cross-site_scripting}XSS} from
       untrusted user inputs but you should rather post-process rendering
       outputs with a dedicated HTML sanitizer.}
    {- [backend_blocks], if [true], code blocks with language [=html]
       are written verbatim in the output (iff [safe] is [true]) and
       any other code block whose langage starts with [=] is
       dropped. Defaults to [false].}}

    See {{!Cmarkit_renderer.example}this example} to extend or
    selectively override the renderer. *)

val xhtml_renderer :
  ?backend_blocks:bool -> safe:bool -> unit -> Cmarkit_renderer.t
(** [xhtml_renderer] is like {!val-renderer} but explicitely closes
    empty tags to possibly make the output valid XML. Note that it
    still renders HTML blocks and inline raw HTML unless {!safe} is
    [true] (which also suppresses some URLs).

    See {{!Cmarkit_renderer.example}this example} to extend or
    selectively override the renderer. *)

(** {1:render Render functions}

    Only useful if you extend the renderer. *)

val safe : Cmarkit_renderer.context -> bool
(** [safe c] is [true] if a safe rendering is requested.
    See {!renderer} for more information. *)

val html_escaped_uchar : Cmarkit_renderer.context -> Uchar.t -> unit
(** [html_escaped_uchar c u] renders the UTF-8 encoding of [u] on [c]
    with HTML markup delimiters [<] [>] [&] and ["] escaped
    to HTML entities (Single quotes ['] are not escaped use ["] to delimit your
    attributes). This also renders U+0000 to {!Uchar.rep}. *)

val buffer_add_html_escaped_uchar : Buffer.t -> Uchar.t -> unit
(** [buffer_add_html_escaped_uchar] is {!html_escaped_uchar} but appends
    to a buffer value. *)

val html_escaped_string : Cmarkit_renderer.context -> string -> unit
(** [html_escaped_string c s] renders string [s] on [c] with HTML
    markup delimiters [<], [>], [&], and ["] escaped to HTML
    entities (Single quotes ['] are not escaped, use ["] to delimit your
    attributes). *)

val buffer_add_html_escaped_string : Buffer.t -> string -> unit
(** [buffer_add_html_escaped_string] is {!html_escaped_string} but appends
    to a buffer value. *)

val pct_encoded_string : Cmarkit_renderer.context -> string -> unit
(** [pct_encoded_string c s] renders string [s] on [c] with everything
    percent encoded except [%] and the
    {{:https://datatracker.ietf.org/doc/html/rfc3986#section-2.3}
    [unreserved]},
    {{:https://datatracker.ietf.org/doc/html/rfc3986#section-2.2}
    [sub-delims]}
    and the {{:https://datatracker.ietf.org/doc/html/rfc3986#section-2.2}
    [gen-delims]}
    URI characters except brackets [\[] and [\]] (to match the [cmark] tool).

    In other words only characters [%] [a-z] [A-Z] [0-9] [-] [.] [_] [~] [!]
    [$] [&] ['] [(] [)] [*] [+] [,] [;] [=] [:] [/] [?] [#] [@]
    are not percent-encoded.

    {b Warning.} The function also replaces both [&] and ['] by their
    corresponding HTML entities, so you can't use this in a context
    that doesn't allow entities. Besides this assumes [s] may already
    have percent encoded bits so it doesn't percent encode [%], as such you
    can't use this as a general percent encode function. *)

val buffer_add_pct_encoded_string : Buffer.t -> string -> unit
(** [buffer_add_pct_encoded_string b s] is {!pct_encoded_string} but
    appends to a buffer value. *)

(** {1:integration HTML integration notes}

    {2:code_blocks Code blocks}

    If a language [lang] can be extracted from the info string of a
    code block with
    {!Cmarkit.Block.Code_block.language_of_info_string}, a
    [language-lang] class is added to the corresponding [code]
    element. If you want to highlight the syntax, adding
    {{:https://highlightjs.org/}highlight.js} to your page is an
    option.

    {2:ids Heading identifiers}

    Headings identifiers and anchors are added to the output whenever
    {!Cmarkit.Block.Heading.val-id} holds a value. If the identifier
    already exists it is made unique by appending ["-"] and the first
    number starting from 1 that makes it unique.

    {2:math Maths}

    If your document has {!Cmarkit.Inline.extension-Ext_math_span}
    inlines or {!Cmarkit.Block.extension-Ext_math_block} blocks, the
    default renderer outputs them in [\(], [\)] and
    [\\[], [\\]] delimiters. You should add
    {{:https://katex.org/}K{^A}T{_E}X} or
    {{:https://www.mathjax.org/}MathJax} in your page to let these
    bits be rendered by the typography they deserve.

    {2:page_frame Page frame}

    The default renderers only generate HTML fragments. You may
    want to add a page frame. For example:
{[
let html_doc_of_md ?(lang = "en") ~title ~safe md =
  let doc = Cmarkit.Doc.of_string md in
  let r = Cmarkit_html.renderer ~safe () in
  let buffer_add_doc = Cmarkit_renderer.buffer_add_doc r in
  let buffer_add_title = Cmarkit_html.buffer_add_html_escaped_string in
  Printf.kbprintf Buffer.contents (Buffer.create 1024)
{|<html lang="%s">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>%a</title>
</head>
<body>
%a</body>
</html>|}
    lang buffer_add_title title buffer_add_doc doc
]}
*)

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

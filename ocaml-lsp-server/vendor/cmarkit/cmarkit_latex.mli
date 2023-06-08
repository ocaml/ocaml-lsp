(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Rendering CommonMark to L{^A}T{_E}X.

    Generates L{^A}T{_E}X fragments, consult the {{!integration}
    integration notes} for requirements on the document.

    See {{!page-index.quick}a quick example} and {{!doc_frame}another one}.

    {b Warning.} Rendering outputs are unstable, they may be tweaked even
    between minor versions of the library. *)

(** {1:rendering Rendering} *)

val of_doc : ?backend_blocks:bool -> Cmarkit.Doc.t -> string
(** [of_doc d] is a L{^A}T{_E}X fragment for [d]. See {!val-renderer}
    for more details and documentation about rendering options. *)

(** {1:renderer Renderer} *)

val renderer : ?backend_blocks:bool -> unit -> Cmarkit_renderer.t
(** [renderer] is a default L{^A}T{_E}X renderer. This renders
    the strict CommonMark abstract syntax tree and the supported
    Cmarkit {{!Cmarkit.extensions}extensions}.

    The inline, block and document renderers always return
    [true]. Unknown block and inline values are rendered by a
    L{^A}T{_E}X comment.

    The following options are available:

    {ul
    {- [backend_blocks], if [true], code blocks with language [=latex]
       are written verbatim in the output and any other code block whose
       langage starts with [=] is dropped. Defaults to [false].}}

    See {{!Cmarkit_renderer.example}this example} to extend or
    selectively override the renderer. *)

(** {1:render Render functions}

    Only useful if you extend the renderer. *)

val newline : Cmarkit_renderer.context -> unit
(** [newline c] starts a new line. Except on the first call on [c] which is
    a nop. *)

val latex_escaped_uchar : Cmarkit_renderer.context -> Uchar.t -> unit
(** [latex_escaped_uchar c u] renders the UTF-8 encoding of [u] on [c]
    propertly escaped for L{^A}T{_E}X. That is the characters
    [&] [%] [$] [#] [_] [{] [}] [~] [^] [\ ]
    are escaped. This also renders U+0000 to {!Uchar.rep}. *)

val buffer_add_latex_escaped_uchar : Buffer.t -> Uchar.t -> unit
(** [buffer_add_latex_escaped_uchar] is {!latex_escaped_uchar} but appends
    to a buffer value. *)

val latex_escaped_string : Cmarkit_renderer.context -> string -> unit
(** [latex_escaped_string c s] renders string [s] on [c] with
    characters [&] [%] [$] [#] [_] [{] [}] [~] [^] [\ ] escaped. This
    also escapes U+0000 to {!Uchar.rep}. *)

val buffer_add_latex_escaped_string : Buffer.t -> string -> unit
(** [buffer_add_latex_escaped_string] is {!latex_escaped_string}
    but acts on a buffer value. *)

(** {1:integration L{^A}T{_E}X integration notes}

    Along with the built-in [graphicx] package, the following
    L{^A}T{_E}X packages are needed to use the outputs of the default
    renderer:
{v
tlmgr install enumitem listings hyperref  # Required
tlmgr install ulem                        # Strikethrough extension
tlmgr install bera fontspec               # Optional
v}
    This means you should have at least the following in your
    document preamble:
{v
% Required
\usepackage{graphicx}
\usepackage{enumitem}
\usepackage{listings}
\usepackage{hyperref}
\usepackage[normalem]{ulem} % Strikethrough extension

% Optional
\usepackage[scaled=0.8]{beramono} % A font for code blocks
\usepackage{fontspec}             % Supports more Unicode characters
v}

    See the sections below for more details.

    {2:char_encoding Character encoding}

    The output is UTF-8 encoded.
    {{:https://tug.org/TUGboat/tb39-1/tb121ltnews28.pdf}It became} the
    the default encoding for L{^A}T{_E}X in 2018. But if you are using
    an older version a [\usepackage[utf8]{inputenc}] may be needed.

    Using [xelatex] rather than [pdflatex] will not get stuck on missing
    glyphs.

    {2:links Autolinks and links}

    The {{:https://www.ctan.org/pkg/hyperref}[hyperref]} package is
    used to render links ([\href]) and autolink ([\url]). Link
    destination starting with a [#] are assumed to refer to
    {{!labels}section labels} and are rendered using the [\hyperref]
    macro, with the [#] chopped.

    {2:images Images}

    Images are inserted using the
    {{:https://ctan.org/pkg/graphicx}graphicx}'s package. Only
    images with relative URLs are supported, those that point
    to external ressources on the www are turned into links.

    {2:labels Section labels}

    Section labels are added to the output whenever
    {!Cmarkit.Block.Heading.val-id} holds a value. If the identifier
    already exists it is made unique by appending ["-"] and the first
    number starting from 1 that makes it unique. Also the character
    [_] seems problematic in labels even when escaped, we map it to [-]
    (if you know any better get in touch).

    {2:lists Lists}

    To support the starting point of ordereded lists without having to
    fiddle with [enumi] counters, the
    {{:https://www.ctan.org/pkg/enumitem}[enumitem]} package is used.

    {2:code_blocks Code blocks}

    If a language [lang] can be
    {{!Cmarkit.Block.Code_block.language_of_info_string}extracted}
    from a code block info string, the
    {{:https://www.ctan.org/pkg/listings}[listings]} package is used
    with the corresponding language in a [lstlisting] environment.
    Otherwise the built-in [verbatim] environment is used.

    Note that the [listings] package has no definition for the [ocaml]
    language, the default renderings are a bit subpar and
    break on character literals with double quotes. This improves things:
{v
\lstset{
  columns=[c]fixed,
  basicstyle=\small\ttfamily,
  keywordstyle=\bfseries,
  upquote=true,
  commentstyle=\slshape,
  breaklines=true,
  showstringspaces=false}

\lstdefinelanguage{ocaml}{language=[objective]caml,
   % Fixes double quotes in char literals
   literate={'"'}{\textquotesingle "\textquotesingle}3
            {'\\"'}{\textquotesingle \textbackslash"\textquotesingle}4,
}
v}

   {2:doc_frame Document frame}

   The default renderer only generates L{^A}T{_E}X fragments. You
   may want to add a document frame. For example:
{[
let latex_doc_of_md ?(title = "") md =
  let doc = Cmarkit.Doc.of_string md in
  let r = Cmarkit_latex.renderer () in
  let buffer_add_doc = Cmarkit_renderer.buffer_add_doc r in
  let buffer_add_title = Cmarkit_latex.buffer_add_latex_escaped_string in
  let maketitle = if title = "" then "" else {|\maketitle|} in
  Printf.kbprintf Buffer.contents (Buffer.create 1024)
{|\documentclass{article}

\usepackage{graphicx}
\usepackage{enumitem}
\usepackage{listings}
\usepackage{hyperref}
\usepackage[normalem]{ulem}
\usepackage[scaled=0.8]{beramono}
\usepackage{fontspec}

\lstset{
  columns=[c]fixed,
  basicstyle=\small\ttfamily,
  keywordstyle=\bfseries,
  upquote=true,
  commentstyle=\slshape,
  breaklines=true,
  showstringspaces=false}

\lstdefinelanguage{ocaml}{language=[objective]caml,
  literate={'"'}{\textquotesingle "\textquotesingle}3
            {'\\"'}{\textquotesingle \textbackslash"\textquotesingle}4,
}

\title{%a}
\begin{document}
%s
%a
\end{document}|} buffer_add_title title maketitle buffer_add_doc doc
]}

Ignore this: ".
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

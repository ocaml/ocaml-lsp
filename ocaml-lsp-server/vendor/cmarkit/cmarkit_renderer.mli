(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Renderer abstraction.

    Stateful renderer abstraction to render documents in {!Stdlib.Buffer.t}
    values.

    {b Note.} This is a low-level interface. For quick and standard
    renderings see {!Cmarkit_html.of_doc}, {!Cmarkit_latex.of_doc} and
    {!Cmarkit_commonmark.of_doc}. If you want to extend them,
    see {{!example}this example}. *)

(** {1:rendering Rendering} *)

type t
(** The type for renderers. *)

val doc_to_string : t -> Cmarkit.Doc.t -> string
(** [doc_to_string r d] renders document [d] to a string using renderer [r]. *)

val buffer_add_doc : t -> Buffer.t -> Cmarkit.Doc.t -> unit
(** [buffer_add_doc r b d] renders document [d] on buffer [b] using
    renderer [r]. *)

(** {1:renderers Renderers} *)

type context
(** The type for rendering contexts, holds a renderer, a
    {!Stdlib.Buffer.t} value to act on and rendering state. *)

type inline = context -> Cmarkit.Inline.t -> bool
(** The type for inline renderers.

    Return [false] if you are not interested in rendering the given
    inline. Use {!Context.inline} and {!Context.block} on the given
    context if you need to invoke the renderer recursively. *)

type block = context -> Cmarkit.Block.t -> bool
(** The type for block renderers.

    Return [false] if you are not interested in rendering the given
    block. Use {!Context.inline} and {!Context.block} with the given
    context if you need to invoke the renderer recursively. *)

type doc = context -> Cmarkit.Doc.t -> bool
(** The type for document renderers.

    Return [false] if you are not interested in rendering the given
    document. Use {!Context.inline}, {!Context.block} and {!Context.doc}
    with the given context if you need to invoke the renderer recursively. *)

val make :
  ?init_context:(context -> Cmarkit.Doc.t -> unit) ->
  ?inline:inline -> ?block:block -> ?doc:doc -> unit -> t
(** [make ?init_context ?inline ?block ?doc ()] is a renderer using
    [inline], [block], [doc] to render documents. They all default to
    [(fun _ _ -> false)], which means that by default they defer to
    next renderer (see {!compose}).

    [init_context] is used to initialize the context for the renderer
    before a document render. It defaults to [fun _ _ -> ()]. *)

val compose : t -> t -> t
(** [compose g f] renders first with [f] and if a renderer returns [false],
    falls back on its counterpart in [g].

    The {!init_context} of the result calls [g]'s initialization
    context function first, followed by the one of [f]. This means
    [f]'s initialization function can assume the context is already
    setup for [g]. *)

(** {2:accessors Accessors}

    Normally you should not need these but you may want to peek
    into other renderers. *)

val init_context : t -> (context -> Cmarkit.Doc.t -> unit)
(** [init_context r] is the context initalization function for [r]. *)

val inline : t -> inline
(** [inline r] is the inline renderer of [r]. *)

val block : t -> block
(** [block_renderer r] is the block renderer of [r]. *)

val doc : t -> doc
(** [doc_renderer r] is the documentation renderer of [r]. *)

(** {1:context Rendering contexts} *)

(** Rendering contexts. *)
module Context : sig

  (** {1:contexts Contexts} *)

  type renderer := t

  type t = context
  (** The type for rendering contexts. *)

  val make : renderer -> Buffer.t -> t
  (** [make r b] is a context using renderer [r] to render documents
      on buffer [b].

      The renderer [r] must be able to handle any inline, block and
      document values (i.e. its renderers should always return [true])
      otherwise [Invalid_argument] may raise on renders.

      This means the last renderer you {{!compose}compose with} should
      always have catch all cases returning [true]; after possibly
      indicating in the output that something was missed. The built-in
      renderers {!Cmarkit_commonmark.val-renderer},
      {!Cmarkit_html.val-renderer} and {!Cmarkit_latex.val-renderer}
      do have these catch all cases. *)

  val renderer : t -> renderer
  (** [renderer c] is the renderer of [c]. *)

  val buffer : t -> Buffer.t
  (** [buffer c] is the buffer of [c]. *)

  val get_doc : t -> Cmarkit.Doc.t
  (** [get_doc c] is the document being rendered. *)

  val get_defs : t -> Cmarkit.Label.defs
  (** [get_defs c] is [Doc.defs (get_doc c)]. *)

  (** Custom context state. *)
  module State : sig

    type 'a t
    (** The type for custom state of type ['a]. *)

    val make : unit -> 'a t
    (** [make ()] is a new bit of context state. *)

    val find : context -> 'a t -> 'a option
    (** [find c state] is the state [state] of context [c], if any. *)

    val get : context -> 'a t -> 'a
    (** [get c state] is the state [state] of context [c], raises
        [Invalid_argument] if there is no state [state] in [c]. *)

    val set : context -> 'a t -> 'a option -> unit
    (** [set c state s] sets the state [state] of [c] to [s]. [state] is
        cleared in [c] if [s] is [None]. *)
  end

  val init : t -> Cmarkit.Doc.t -> unit
  (** [init c] calls the initialisation function of [c]'s
      {!val-renderer}. Note, this is done automatically by {!val-doc}. *)

  (** {1:render Rendering functions}

      These function append data to the {!buffer} of the context. For more
      specialized rendering functions, see the corresponding rendering
      backends. *)

  val byte : t -> char -> unit
  (** [byte c b] renders byte [b] verbatim on [c]. *)

  val utf_8_uchar : t -> Uchar.t -> unit
  (** [utf_8_uchar c u] renders the UTF-8 encoding of [u] on [c]. *)

  val string : t -> string -> unit
  (** [string c s] renders string [s] verbatim on [c]. *)

  val inline : t -> Cmarkit.Inline.t -> unit
  (** [inline c i] renders inline [i] on [c]. This invokes the
      {{!compose}composition} of inline renderers of [c]. *)

  val block : t -> Cmarkit.Block.t -> unit
  (** [block c b] renders block [b] on [c]. This invokes the
      {{!compose}composition} of block renderers of [c]. *)

  val doc : t -> Cmarkit.Doc.t -> unit
  (** [doc c d] initializes [c] with {!init} and renders document [d] on [c].
      This invokes the {{!compose}composition} of document renderers of [c]. *)
end

(** {1:example Extending renderers}

    This example extends the {!Cmarkit_html.val-renderer} but it
    applies {e mutatis mutandis} to the other backend document
    renderers.

    Let's assume you want to:

    {ul
    {- Extend the abstract syntax tree with a [Doc] block which
       allows to splice documents in another one (note that
       splicing is already built-in via the {!Cmarkit.Block.extension-Blocks}
       block case).}
    {- Change the rendering of {!Cmarkit.Inline.extension-Image} inlines to
       render HTML [video] or [audio] elements depending on the link's
       destination suffix.}
    {- For the rest use the built-in {!Cmarkit_html.renderer} renderer
       as it exists.}}

    This boils down to:

    {ol
    {- Add a new case to the abstract syntax tree.}
    {- Define a [custom_html] renderer which treats
       {!Cmarkit.Inline.Image} and the new [Doc] case the way we
       see it fit and return [false] otherwise to use the built-in renderer. }
    {- {!compose} [custom_html] with {!Cmarkit_html.val-renderer}}}

{[
type Cmarkit.Block.t += Doc of Cmarkit.Doc.t (* 1 *)

let media_link c l =
  let has_ext s ext = String.ends_with ~suffix:ext s in
  let is_video s = List.exists (has_ext s) [".mp4"; ".webm"] in
  let is_audio s = List.exists (has_ext s) [".mp3"; ".flac"] in
  let defs = Cmarkit_renderer.Context.get_defs c in
  match Cmarkit.Inline.Link.reference_definition defs l with
  | Some Cmarkit.Link_definition.Def (ld, _) ->
      let start_tag = match Cmarkit.Link_definition.dest ld with
      | Some (src, _) when is_video src -> Some ("<video", src)
      | Some (src, _) when is_audio src -> Some ("<audio", src)
      | None | Some _ -> None
      in
      begin match start_tag with
      | None -> false (* let the default HTML renderer handle that *)
      | Some (start_tag, src) ->
          (* More could be done with the reference title and link text *)
          Cmarkit_renderer.Context.string c start_tag;
          Cmarkit_renderer.Context.string c {| src="|};
          Cmarkit_html.pct_encoded_string c src;
          Cmarkit_renderer.Context.string c {|" />|};
          true
      end
  | None | Some _ -> false (* let the default HTML renderer that *)

let custom_html =
  let inline c = function
  | Cmarkit.Inline.Image (l, _) -> media_link c l
  | _ -> false (* let the default HTML renderer handle that *)
  in
  let block c = function
  | Doc d ->
      (* It's important to recurse via Cmarkit_renderer.Context.block *)
      Cmarkit_renderer.Context.block c (Cmarkit.Doc.block d); true
  | _ -> false (* let the default HTML renderer handle that *)
  in
  Cmarkit_renderer.make ~inline ~block () (* 2 *)

let custom_html_of_doc ~safe doc =
  let default = Cmarkit_html.renderer ~safe () in
  let r = Cmarkit_renderer.compose default custom_html in (* 3 *)
  Cmarkit_renderer.doc_to_string r doc
]}

The [custom_html_of_doc] function performs your extended
renderings. *)

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

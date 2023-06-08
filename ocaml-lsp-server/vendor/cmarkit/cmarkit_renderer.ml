(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* Renderers *)

module Dict = Cmarkit_base.Dict

type t =
  { init_context : context -> Cmarkit.Doc.t -> unit;
    inline : inline;
    block : block;
    doc : doc; }

and context =
  { renderer : t;
    mutable state : Dict.t;
    b : Buffer.t;
    mutable doc : Cmarkit.Doc.t }

and inline = context -> Cmarkit.Inline.t -> bool
and block = context -> Cmarkit.Block.t -> bool
and doc = context -> Cmarkit.Doc.t -> bool

let nop _ _ = ()
let none _ _ = false

let make
    ?(init_context = nop) ?(inline = none) ?(block = none) ?(doc = none) ()
  =
  { init_context; inline; block; doc }

let compose g f =
  let init_context c d = g.init_context c d; f.init_context c d in
  let block c b = f.block c b || g.block c b in
  let inline c i = f.inline c i || g.inline c i in
  let doc c d = f.doc c d || g.doc c d in
  { init_context; inline; block; doc }

let init_context r = r.init_context
let inline r = r.inline
let block r = r.block
let doc r = r.doc

module Context = struct
  type t = context
  let make renderer b =
    { renderer; b; state = Dict.empty; doc = Cmarkit.Doc.empty }

  let buffer c = c.b
  let renderer c = c.renderer
  let get_doc (c : context) = c.doc
  let get_defs (c : context) = Cmarkit.Doc.defs c.doc

  module State = struct
    type 'a t = 'a Dict.key
    let make = Dict.key
    let find c st = Dict.find st c.state
    let get c st = Option.get (Dict.find st c.state)
    let set c st = function
    | None -> c.state <- Dict.remove st c.state
    | Some s -> c.state <- Dict.add st s c.state
  end

  let init c d = c.renderer.init_context c d

  let invalid_inline _ = invalid_arg "Unknown Cmarkit.Inline.t case"
  let invalid_block _ = invalid_arg "Unknown Cmarkit.Block.t case"
  let unhandled_doc _ = invalid_arg "Unhandled Cmarkit.Doc.t"

  let byte r c = Buffer.add_char r.b c
  let utf_8_uchar r u = Buffer.add_utf_8_uchar r.b u
  let string c s = Buffer.add_string c.b s
  let inline c i = ignore (c.renderer.inline c i || invalid_inline i)
  let block c b = ignore (c.renderer.block c b || invalid_block b)
  let doc (c : context) d =
    c.doc <- d; init c d;
    ignore (c.renderer.doc c d || unhandled_doc d);
    c.doc <- Cmarkit.Doc.empty
end

let doc_to_string r d =
  let b = Buffer.create 1024 in
  let c = Context.make r b in
  Context.doc c d; Buffer.contents b

let buffer_add_doc r b d = Context.doc (Context.make r b) d

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

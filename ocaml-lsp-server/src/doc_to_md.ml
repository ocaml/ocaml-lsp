open Import
open Cmarkit

(** TODO:

    - Add support for references
    - Broken lists
    - Labels in headers
    - Align text with HTML
    - Verbatim is indented in tests

    Unsupported (next):

    - Support meta from odoc-parser locations *)

type t =
  | Raw of string
  | Markdown of string

let loc_to_meta _loc = Meta.none

let style_inline ~meta (style : Odoc_parser.Ast.style) inline =
  match style with
  | `Bold -> Inline.Strong_emphasis (Inline.Emphasis.make inline, meta)
  | `Italic -> Inline.Emphasis (Inline.Emphasis.make inline, meta)
  | `Emphasis -> Inline.Emphasis (Inline.Emphasis.make inline, meta)
  | `Superscript -> inline
  | `Subscript -> inline

let rec inline_element_to_inline
    (inline : Odoc_parser.Ast.inline_element Odoc_parser.Loc.with_location) :
    Inline.t =
  match inline with
  | Odoc_parser.Loc.{ value = `Space _; location } ->
    let meta = loc_to_meta location in
    Inline.Text (" ", meta)
  | Odoc_parser.Loc.{ value = `Word w; location } ->
    let meta = loc_to_meta location in
    Inline.Text (w, meta)
  | Odoc_parser.Loc.{ value = `Code_span c; location } ->
    let meta = loc_to_meta location in
    Inline.Code_span (Inline.Code_span.of_string c, meta)
  | Odoc_parser.Loc.{ value = `Raw_markup (Some "html", text); location } ->
    let meta = loc_to_meta location in
    Inline.Raw_html (Block_line.tight_list_of_string text, meta)
  | Odoc_parser.Loc.{ value = `Raw_markup (_, text); location } ->
    (* Cmarkit doesn't have constructors for backend other than HTML for inline
       raw markups, only for blocks. *)
    let meta = loc_to_meta location in
    Inline.Text (text, meta)
  | Odoc_parser.Loc.{ value = `Styled (style, inlines); location } ->
    let text = inline_element_list_to_inlines inlines in
    let meta = loc_to_meta location in
    style_inline ~meta style text
  | Odoc_parser.Loc.
      { value = `Reference (_kind, _ref, _inlines); location = _location } ->
    (* TODO: add support for references *)
    Inline.Break (Inline.Break.make `Hard, Meta.none)
  | Odoc_parser.Loc.{ value = `Link (link, inlines); location } ->
    let text = inline_element_list_to_inlines inlines in
    let ref =
      `Inline (Link_definition.make ~dest:(link, Meta.none) (), Meta.none)
    in
    let link = Inline.Link.make text ref in
    let meta = loc_to_meta location in
    Inline.Link (link, meta)
  | Odoc_parser.Loc.{ value = `Math_span text; location } ->
    let meta = loc_to_meta location in
    Inline.Ext_math_span
      ( Inline.Math_span.make
          ~display:false
          (Block_line.tight_list_of_string text)
      , meta )

and inline_element_list_to_inlines inlines =
  let inlines = List.map ~f:inline_element_to_inline inlines in
  Inline.Inlines (inlines, Meta.none)

let rec nestable_block_element_to_block
    (nestable :
      Odoc_parser.Ast.nestable_block_element Odoc_parser.Loc.with_location) =
  match nestable with
  | Odoc_parser.Loc.{ value = `Paragraph text; location } ->
    let inline = inline_element_list_to_inlines text in
    let paragraph = Block.Paragraph.make inline in
    let meta = loc_to_meta location in
    Block.Paragraph (paragraph, meta)
  | Odoc_parser.Loc.{ value = `List (kind, style, xs); location } ->
    let type' =
      match kind with
      | `Unordered -> `Unordered '*'
      | `Ordered -> `Ordered (1, '*')
    in
    let tight =
      match style with
      | `Heavy -> false
      | `Light -> true
    in
    let list_items =
      List.map
        ~f:(fun n ->
          let block = nestable_block_element_list_to_block n in
          (Block.List_item.make block, Meta.none))
        xs
    in
    let l = Block.List'.make ~tight type' list_items in
    let meta = loc_to_meta location in
    Block.List (l, meta)
  | Odoc_parser.Loc.{ value = `Modules modules; location } ->
    let type' = `Unordered '*' in
    let tight = false in
    let list_items =
      List.map
        ~f:(fun Odoc_parser.Loc.{ value = m; location } ->
          let inline = Inline.Text (m, Meta.none) in
          let paragraph = Block.Paragraph.make inline in
          let block = Block.Paragraph (paragraph, Meta.none) in
          let meta = loc_to_meta location in
          let marker = Layout.string "!modules:" in
          (Block.List_item.make ~marker block, meta))
        modules
    in
    let l = Block.List'.make ~tight type' list_items in
    let meta = loc_to_meta location in
    Block.List (l, meta)
  | Odoc_parser.Loc.
      { value = `Code_block (metadata, { value = code; location = _code_loc })
      ; location
      } ->
    let info_string =
      match metadata with
      | None -> None
      | Some ({ value = lang; location = lang_log }, _env) ->
        Some (lang, loc_to_meta lang_log)
    in
    let block_line = Block_line.list_of_string code in
    let code_block = Block.Code_block.make ?info_string block_line in
    let meta = loc_to_meta location in
    Block.Code_block (code_block, meta)
  | Odoc_parser.Loc.{ value = `Verbatim code; location } ->
    let info_string = Some ("verb", Meta.none) in
    let block_line = Block_line.list_of_string code in
    let code_block = Block.Code_block.make ?info_string block_line in
    let meta = loc_to_meta location in
    Block.Code_block (code_block, meta)
  | Odoc_parser.Loc.{ value = `Math_block code; location } ->
    let block_line = Block_line.list_of_string code in
    let code_block = Block.Code_block.make block_line in
    let meta = loc_to_meta location in
    Block.Ext_math_block (code_block, meta)

and nestable_block_element_list_to_block nestables =
  let blocks = List.map ~f:nestable_block_element_to_block nestables in
  Block.Blocks (blocks, Meta.none)

let strong_and_emphasis s =
  Inline.Emphasis
    ( Inline.Emphasis.make
        (Inline.Strong_emphasis
           (Inline.Emphasis.make (Inline.Text (s, Meta.none)), Meta.none))
    , Meta.none )

let inline_code_span_of_string s =
  Inline.Code_span
    ( Inline.Code_span.make ~backtick_count:1 (Block_line.tight_list_of_string s)
    , Meta.none )

let inline_link_of_string ~text uri =
  let ref =
    `Inline (Link_definition.make ~dest:(uri, Meta.none) (), Meta.none)
  in
  Inline.Link (Inline.Link.make (Inline.Text (text, Meta.none)) ref, Meta.none)

let tag_to_block ~meta (tag : Odoc_parser.Ast.tag) =
  let format_tag_empty tag =
    Block.Paragraph (Block.Paragraph.make (strong_and_emphasis tag), Meta.none)
  in
  let format_tag_string tag text =
    let inline =
      Inline.Inlines
        ( [ strong_and_emphasis tag; Inline.Text (" ", Meta.none); text ]
        , Meta.none )
    in
    Block.Paragraph (Block.Paragraph.make inline, meta)
  in
  let format_tag_block tag block =
    let prefix =
      Block.Paragraph (Block.Paragraph.make (strong_and_emphasis tag), Meta.none)
    in
    Block.Blocks ([ prefix; block ], meta)
  in
  let format_tag_string_with_block tag text block =
    let prefix =
      let inline =
        Inline.Inlines
          ( [ strong_and_emphasis tag; Inline.Text (" ", Meta.none); text ]
          , Meta.none )
      in
      Block.Paragraph (Block.Paragraph.make inline, Meta.none)
    in
    Block.Blocks ([ prefix; block ], meta)
  in
  match tag with
  | `Author s ->
    let s = Inline.Text (s, Meta.none) in
    format_tag_string "@author" s
  | `Deprecated text ->
    let block = nestable_block_element_list_to_block text in
    format_tag_block "@deprecated" block
  | `Param (id, []) ->
    let id = Inline.Text (id, Meta.none) in
    format_tag_string "@param" id
  | `Param (id, text) ->
    let block = nestable_block_element_list_to_block text in
    let id = inline_code_span_of_string id in
    format_tag_string_with_block "@param" id block
  | `Raise (exc, text) ->
    let block = nestable_block_element_list_to_block text in
    let exc = inline_code_span_of_string exc in
    format_tag_string_with_block "@raise" exc block
  | `Return text ->
    let block = nestable_block_element_list_to_block text in
    format_tag_block "@return" block
  | `See (`Url, uri, text) ->
    let block = nestable_block_element_list_to_block text in
    let uri = inline_link_of_string ~text:"link" uri in
    format_tag_string_with_block "@see" uri block
  | `See ((`File | `Document), uri, text) ->
    let block = nestable_block_element_list_to_block text in
    let uri = inline_code_span_of_string uri in
    format_tag_string_with_block "@see" uri block
  | `Since version ->
    let version = inline_code_span_of_string version in
    format_tag_string "@since" version
  | `Before (version, text) ->
    let block = nestable_block_element_list_to_block text in
    let version = inline_code_span_of_string version in
    format_tag_string_with_block "@before" version block
  | `Version version ->
    let version = inline_code_span_of_string version in
    format_tag_string "@version" version
  | `Canonical { value = s; location = _ } ->
    let s = Inline.Text (s, Meta.none) in
    format_tag_string "@canonical" s
  | `Inline -> format_tag_empty "@inline"
  | `Open -> format_tag_empty "@open"
  | `Closed -> format_tag_empty "@closed"

let rec block_element_to_block
    (block_element :
      Odoc_parser.Ast.block_element Odoc_parser.Loc.with_location) =
  match block_element with
  | Odoc_parser.Loc.{ value = `Heading (level, _, content); location } ->
    let text = inline_element_list_to_inlines content in
    let heading = Block.Heading.make ~level text in
    let meta = loc_to_meta location in
    Block.Heading (heading, meta)
  | Odoc_parser.Loc.{ value = `Tag t; location } ->
    let meta = loc_to_meta location in
    tag_to_block ~meta t
  | Odoc_parser.Loc.
      { value =
          ( `Paragraph _
          | `List _
          | `Modules _
          | `Code_block _
          | `Verbatim _
          | `Math_block _ )
      ; location = _
      } as nestable -> nestable_block_element_to_block nestable

and block_element_list_to_block l =
  let rec aux acc rest =
    match rest with
    | [] -> List.rev acc
    | el :: [] -> List.rev (block_element_to_block el :: acc)
    | el :: rest ->
      aux
        (Block.Blank_line ("", Meta.none) :: block_element_to_block el :: acc)
        rest
  in
  let blocks = aux [] l in
  Block.Blocks (blocks, Meta.none)

let translate doc : t =
  let location = Lexing.dummy_pos in
  let v = Odoc_parser.parse_comment ~location ~text:doc in
  let ast = Odoc_parser.ast v in
  let block = block_element_list_to_block ast in
  let doc = Doc.make block in
  let cmark = Cmarkit_commonmark.of_doc doc in
  Markdown cmark

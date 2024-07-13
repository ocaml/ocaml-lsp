open Import

open struct
  open Cmarkit
  module Inline = Inline
  module Meta = Meta
  module Block_line = Block_line
  module Link_definition = Link_definition
  module Block = Block
  module Layout = Layout
  module Doc = Doc
end

(** TODO:

    - Add support for references
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
;;

let rec inline_element_to_inline
  (inline : Odoc_parser.Ast.inline_element Odoc_parser.Loc.with_location)
  : Inline.t
  =
  match inline with
  | { value = `Space _; location } ->
    let meta = loc_to_meta location in
    Inline.Text (" ", meta)
  | { value = `Word w; location } ->
    let meta = loc_to_meta location in
    Inline.Text (w, meta)
  | { value = `Code_span c; location } ->
    let meta = loc_to_meta location in
    Inline.Code_span (Inline.Code_span.of_string c, meta)
  | { value = `Raw_markup (Some "html", text); location } ->
    let meta = loc_to_meta location in
    Inline.Raw_html (Block_line.tight_list_of_string text, meta)
  | { value = `Raw_markup (_, text); location } ->
    (* Cmarkit doesn't have constructors for backend other than HTML for inline
       raw markups, only for blocks. *)
    let meta = loc_to_meta location in
    Inline.Text (text, meta)
  | { value = `Styled (`Superscript, inlines); location } ->
    let text = inline_element_list_to_inlines inlines in
    let meta = loc_to_meta location in
    Inline.Inlines ([ Inline.Text ("^{", meta); text; Inline.Text ("}", meta) ], meta)
  | { value = `Styled (`Subscript, inlines); location } ->
    let text = inline_element_list_to_inlines inlines in
    let meta = loc_to_meta location in
    Inline.Inlines ([ Inline.Text ("_{", meta); text; Inline.Text ("}", meta) ], meta)
  | { value = `Styled (style, inlines); location } ->
    let text = inline_element_list_to_inlines inlines in
    let meta = loc_to_meta location in
    style_inline ~meta style text
  | { value = `Reference (kind, ref, inlines); location } ->
    (* TODO: add support for references *)
    let meta = loc_to_meta location in
    (match kind with
     | `Simple -> Inline.Code_span (Inline.Code_span.of_string ref.value, meta)
     | `With_text -> inline_element_list_to_inlines inlines)
  | { value = `Link (link, inlines); location } ->
    let link =
      let text = inline_element_list_to_inlines inlines in
      let ref = `Inline (Link_definition.make ~dest:(link, Meta.none) (), Meta.none) in
      Inline.Link.make text ref
    in
    let meta = loc_to_meta location in
    Inline.Link (link, meta)
  | { value = `Math_span text; location } ->
    let meta = loc_to_meta location in
    Inline.Ext_math_span
      (Inline.Math_span.make ~display:false (Block_line.tight_list_of_string text), meta)

and inline_element_list_to_inlines inlines =
  let inlines = List.map ~f:inline_element_to_inline inlines in
  Inline.Inlines (inlines, Meta.none)
;;

let rec nestable_block_element_to_block
  (nestable : Odoc_parser.Ast.nestable_block_element Odoc_parser.Loc.with_location)
  =
  match nestable with
  | { value = `Paragraph text; location } ->
    let paragraph =
      let inline = inline_element_list_to_inlines text in
      Block.Paragraph.make inline
    in
    let meta = loc_to_meta location in
    Block.Paragraph (paragraph, meta)
  | { value = `Table ((grid, alignment), _); location } ->
    let meta = loc_to_meta location in
    let tbl =
      let rows =
        let alignment_row =
          match alignment with
          | None -> []
          | Some alignment ->
            let alignment =
              List.map ~f:(fun x -> (x, 1 (* nb of separator *)), Meta.none) alignment
            in
            [ (`Sep alignment, Meta.none), "" ]
        in
        let cell ((c, _) : Odoc_parser.Ast.nestable_block_element Odoc_parser.Ast.cell) =
          let c = nestable_block_element_list_to_inlines c in
          c, (" ", " ")
          (* Initial and trailing blanks *)
        in
        let data_row (row : Odoc_parser.Ast.nestable_block_element Odoc_parser.Ast.row) =
          let row = List.map ~f:cell row in
          (`Data row, Meta.none), ""
        in
        let header_row (row : Odoc_parser.Ast.nestable_block_element Odoc_parser.Ast.row) =
          let row = List.map ~f:cell row in
          (`Header row, Meta.none), ""
        in
        match grid with
        | [] -> assert false
        | h :: t -> (header_row h :: alignment_row) @ List.map ~f:data_row t
      in
      Block.Table.make rows
    in
    Block.Ext_table (tbl, meta)
  | { value = `List (kind, style, xs); location } ->
    let l =
      let list_items =
        List.map xs ~f:(fun n ->
          let block = nestable_block_element_list_to_block n in
          Block.List_item.make ~after_marker:1 block, Meta.none)
      in
      let tight =
        match style with
        | `Heavy -> false
        | `Light -> true
      in
      let type' =
        match kind with
        | `Unordered -> `Unordered '-'
        | `Ordered -> `Ordered (1, '*')
      in
      Block.List'.make ~tight type' list_items
    in
    let meta = loc_to_meta location in
    Block.List (l, meta)
  | { value = `Modules modules; location } ->
    let tight = false in
    let list_items =
      List.map modules ~f:(fun Odoc_parser.Loc.{ value = m; location } ->
        let block =
          let paragraph =
            let inline = Inline.Text (m, Meta.none) in
            Block.Paragraph.make inline
          in
          Block.Paragraph (paragraph, Meta.none)
        in
        let meta = loc_to_meta location in
        let marker = Layout.string "!modules:" in
        Block.List_item.make ~after_marker:1 ~marker block, meta)
    in
    let l =
      let type' = `Unordered '*' in
      Block.List'.make ~tight type' list_items
    in
    let meta = loc_to_meta location in
    Block.List (l, meta)
  | { value =
        `Code_block
          { meta = metadata
          ; delimiter = _
          ; content = { value = code; location = code_loc }
          ; output
          }
    ; location
    } ->
    let meta = loc_to_meta location in
    let main_block =
      let code_block =
        let info_string =
          match metadata with
          | None -> Some ("ocaml", loc_to_meta code_loc)
          | Some { language = { value = lang; location = lang_log }; tags = _ } ->
            Some (lang, loc_to_meta lang_log)
        in
        let block_line = Block_line.list_of_string code in
        Block.Code_block.make ?info_string block_line
      in
      Block.Code_block (code_block, meta)
    in
    let output_block =
      match output with
      | None -> []
      | Some output -> [ nestable_block_element_list_to_block output ]
    in
    Block.Blocks (main_block :: output_block, meta)
  | { value = `Verbatim code; location } ->
    let code_block =
      let info_string = Some ("verb", Meta.none) in
      let block_line = Block_line.list_of_string code in
      Block.Code_block.make ?info_string block_line
    in
    let meta = loc_to_meta location in
    Block.Code_block (code_block, meta)
  | { value = `Math_block code; location } ->
    let code_block =
      let block_line = Block_line.list_of_string code in
      Block.Code_block.make block_line
    in
    let meta = loc_to_meta location in
    Block.Ext_math_block (code_block, meta)

and nestable_block_element_to_inlines
  (nestable : Odoc_parser.Ast.nestable_block_element Odoc_parser.Loc.with_location)
  =
  match nestable with
  | { value = `Paragraph text; location = _ } -> inline_element_list_to_inlines text
  | { value = `Table ((grid, _), _); location } ->
    let meta = loc_to_meta location in
    let cell ((c, _) : Odoc_parser.Ast.nestable_block_element Odoc_parser.Ast.cell) =
      nestable_block_element_list_to_inlines c
    in
    let row (row : Odoc_parser.Ast.nestable_block_element Odoc_parser.Ast.row) =
      let sep = Inline.Text (" | ", Meta.none) in
      sep :: List.concat_map ~f:(fun c -> [ cell c; sep ]) row
    in
    let rows = List.concat_map ~f:row grid in
    Inline.Inlines (rows, meta)
  | { value = `List (_, _, xs); location } ->
    let meta = loc_to_meta location in
    let items =
      let item i = nestable_block_element_list_to_inlines i in
      let sep = Inline.Text (" - ", Meta.none) in
      List.concat_map ~f:(fun i -> [ sep; item i ]) xs
    in
    Inline.Inlines (items, meta)
  | { value = `Modules modules; location } ->
    let meta = loc_to_meta location in
    let s = List.map ~f:(fun x -> x.Odoc_parser.Loc.value) modules in
    Inline.Text ("modules: " ^ String.concat ~sep:" " s, meta)
  | { value =
        `Code_block
          { meta = _
          ; delimiter = _
          ; content = { value = code; location = code_loc }
          ; output = _
          }
    ; location
    } ->
    let meta = loc_to_meta location in
    let code_span =
      let meta_code = loc_to_meta code_loc in
      Inline.Code_span.make ~backtick_count:1 [ "", (code, meta_code) ]
    in
    Inline.Code_span (code_span, meta)
  | { value = `Verbatim code; location } ->
    let meta = loc_to_meta location in
    let code_span = Inline.Code_span.make ~backtick_count:1 [ "", (code, Meta.none) ] in
    Inline.Code_span (code_span, meta)
  | { value = `Math_block code; location } ->
    let meta = loc_to_meta location in
    let code_span = Inline.Math_span.make ~display:true [ "", (code, Meta.none) ] in
    Inline.Ext_math_span (code_span, meta)

and nestable_block_element_list_to_inlines l =
  let inlines = List.map ~f:nestable_block_element_to_inlines l in
  Inline.Inlines (inlines, Meta.none)

and nestable_block_element_list_to_block nestables =
  let blocks = List.map ~f:nestable_block_element_to_block nestables in
  Block.Blocks (blocks, Meta.none)
;;

let strong_and_emphasis s =
  Inline.Emphasis
    ( Inline.Emphasis.make
        (Inline.Strong_emphasis
           (Inline.Emphasis.make (Inline.Text (s, Meta.none)), Meta.none))
    , Meta.none )
;;

let inline_code_span_of_string s =
  Inline.Code_span
    ( Inline.Code_span.make ~backtick_count:1 (Block_line.tight_list_of_string s)
    , Meta.none )
;;

let inline_link_of_string ~text uri =
  let ref = `Inline (Link_definition.make ~dest:(uri, Meta.none) (), Meta.none) in
  Inline.Link (Inline.Link.make (Inline.Text (text, Meta.none)) ref, Meta.none)
;;

let tag_to_block ~meta (tag : Odoc_parser.Ast.tag) =
  let format_tag_empty tag =
    Block.Paragraph (Block.Paragraph.make (strong_and_emphasis tag), Meta.none)
  in
  let format_tag_string tag text =
    let inline =
      Inline.Inlines
        ([ strong_and_emphasis tag; Inline.Text (" ", Meta.none); text ], Meta.none)
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
          ([ strong_and_emphasis tag; Inline.Text (" ", Meta.none); text ], Meta.none)
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
  | `Hidden -> format_tag_empty "@hidden"
;;

let rec block_element_to_block
  (block_element : Odoc_parser.Ast.block_element Odoc_parser.Loc.with_location)
  =
  match block_element with
  | { value = `Heading (level, _, content); location } ->
    let heading =
      let text = inline_element_list_to_inlines content in
      Block.Heading.make ~level:(level + 1) text
    in
    let meta = loc_to_meta location in
    Block.Heading (heading, meta)
  | { value = `Tag t; location } ->
    let meta = loc_to_meta location in
    tag_to_block ~meta t
  | { value =
        ( `Paragraph _
        | `List _
        | `Modules _
        | `Code_block _
        | `Verbatim _
        | `Table _
        | `Math_block _ )
    ; location = _
    } as nestable -> nestable_block_element_to_block nestable

and block_element_list_to_block l =
  let rec aux acc rest =
    match rest with
    | [] -> List.rev acc
    | el :: [] -> List.rev (block_element_to_block el :: acc)
    | el :: rest ->
      aux (Block.Blank_line ("", Meta.none) :: block_element_to_block el :: acc) rest
  in
  let blocks = aux [] l in
  Block.Blocks (blocks, Meta.none)
;;

let translate doc : t =
  Markdown
    (Odoc_parser.parse_comment ~location:Lexing.dummy_pos ~text:doc
     |> Odoc_parser.ast
     |> block_element_list_to_block
     |> Doc.make
     |> Cmarkit_commonmark.of_doc)
;;

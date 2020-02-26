open Omd
open Odoc_parser

let loc_value loc = Odoc_model.Location_.value loc

let list_to_markdown to_markdown doc =
  List.map (fun doc -> to_markdown (loc_value doc)) doc |> List.flatten

let heading_level level title md =
  let md =
    Text
      ( match title with
      | None -> ""
      | Some title -> title )
    :: md
  in
  match level with
  | 0 -> H1 md
  | 1 -> H2 md
  | 2 -> H3 md
  | 3 -> H4 md
  | 4 -> H5 md
  | _ -> H6 md

let style_to_markdown k md =
  match k with
  | `Bold -> [ Bold md ]
  | `Italic
  | `Emphasis ->
    [ Emph md ]
  | `Subscript
  | `Superscript ->
    md

(** Most tags are displayed inline, but we group params and return infos *)
let extract_tag (tag : Ast.tag) =
  match tag with
  | (`Param _ | `Raise _ | `Return _) as tag -> `Extract tag
  | tag -> `Inline tag

let rec inline_element_to_markdown doc =
  match doc with
  | `Word text -> [ Text text ]
  | `Space space -> [ Text space ]
  | `Code_span code -> [ Code ("ocaml", code) ]
  | `Raw_markup (_, _) ->
    (* TODO: support markown specific blocks *)
    []
  | `Styled (kind, text) ->
    let md = list_to_markdown inline_element_to_markdown text in
    style_to_markdown kind md
  | `Reference (_ref_kind, reference, descr) -> (
    let reference = loc_value reference in
    match descr with
    | [] -> [ Code ("ocaml", reference) ]
    | descr ->
      list_to_markdown inline_element_to_markdown descr
      @ [ Text " ( "; Code ("ocaml", reference); Text " ) " ] )
  | `Link (url, descr) ->
    let descr = list_to_markdown inline_element_to_markdown descr in
    [ Url (url, descr, "") ]

let rec nestable_element_to_markdown doc =
  match doc with
  | `Paragraph content ->
    [ Paragraph (list_to_markdown inline_element_to_markdown content) ]
  | `Code_block code -> [ Code_block ("ocaml", code) ]
  | `Verbatim text -> [ Raw text ]
  | `Modules _ -> []
  | `List (`Ordered, _, l) ->
    let md = List.map (list_to_markdown nestable_element_to_markdown) l in
    [ Ol md ]
  | `List (`Unordered, _, l) ->
    let md = List.map (list_to_markdown nestable_element_to_markdown) l in
    [ Ul md ]

let inline_tag_to_markdown tag =
  let format_tag_pair name content =
    [ Paragraph (name :: Text " " :: content) ]
  in
  let format_tag name content = format_tag_pair (Emph [ Text name ]) content in
  let format_unary_tag name = format_tag_pair (Emph [ Text name ]) [] in
  match tag with
  | `Author name -> format_tag "Author:" [ Text name ]
  | `Deprecated reason ->
    let reason = list_to_markdown nestable_element_to_markdown reason in
    format_tag "Deprecated:" reason
  | `See (see_ref, reference, content) -> (
    let format_see kind content =
      [ Paragraph (Emph [ Text "See " ] :: Text kind :: content) ]
    in
    let content = list_to_markdown nestable_element_to_markdown content in
    match see_ref with
    | `Url -> format_see "" [ Url (reference, content, "") ]
    | `File -> format_see "file " (Text reference :: content)
    | `Document -> format_see "document " (Text reference :: content) )
  | `Since vers -> format_tag "Since:" [ Code ("", vers) ]
  | `Before (vers, content) ->
    let content = list_to_markdown nestable_element_to_markdown content in
    format_tag "Before:" (Code ("", vers) :: Text ": " :: content)
  | `Version vers -> format_tag "Version:" [ Code ("", vers) ]
  | `Canonical canonical ->
    format_tag "Canonical:" [ Text (loc_value canonical) ]
  | `Inline -> format_unary_tag "inline"
  | `Open
  | `Closed ->
    []
  | _ -> []

let block_element_to_markdown doc =
  match doc with
  | `Heading (i, title, content) ->
    let md = list_to_markdown inline_element_to_markdown content in
    [ heading_level i title md ]
  | `Tag tag -> (
    match extract_tag tag with
    | `Extract _ -> (* Tag is not displayed inline *) []
    | `Inline tag -> inline_tag_to_markdown tag )
  | #Ast.nestable_block_element as elt -> nestable_element_to_markdown elt

let extract_tags elts =
  let open List in
  let params, raises, return =
    fold_left
      (fun ((params, raises, return) as acc) elt ->
        match loc_value elt with
        | `Tag tag -> (
          match extract_tag tag with
          | `Extract (`Param p) -> (p :: params, raises, return)
          | `Extract (`Raise r) -> (params, r :: raises, return)
          | `Extract (`Return r) -> (params, raises, r :: return)
          | _ -> acc )
        | _ -> acc)
      ([], [], []) elts
  in
  (rev params, rev raises, rev return)

let outlined_tags_to_markdown doc =
  let params, raises, return = extract_tags doc in
  let format_pair (name, descr) =
    Code ("ocaml", name)
    :: Text ": "
    :: list_to_markdown nestable_element_to_markdown descr
  in
  let params = List.map format_pair params in
  let raises = List.map format_pair raises in
  let return =
    List.map (list_to_markdown nestable_element_to_markdown) return
  in
  let cons_if_non_empty name l acc =
    match l with
    | [] -> acc
    | _ -> Text name :: Ul l :: acc
  in
  let infos =
    []
    |> cons_if_non_empty "Raises" raises
    |> cons_if_non_empty "Returns" return
    |> cons_if_non_empty "Parameters" params
  in
  Paragraph infos

let comment_to_markdown doc =
  let tags = outlined_tags_to_markdown doc in
  let comment = list_to_markdown block_element_to_markdown doc in
  tags :: comment

let doc_comment_to_markdown ?(location = Lexing.dummy_pos) doc =
  let doc = parse_comment_raw ~location ~text:doc in
  let doc = comment_to_markdown doc.Odoc_model.Error.value in
  Omd.to_markdown doc

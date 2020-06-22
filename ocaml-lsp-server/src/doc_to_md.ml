open Stdune

module Oct = Octavius

let { Logger.log } = Logger.for_section "doc_to_md"

let ocaml = "ocaml"

let to_inline_code code = Omd.Code (ocaml, code)

let to_code_block code = Omd.Code_block (ocaml, code)

let space = Omd.Text " "

let new_line = Omd.NL

let rec put_in_between elem = function
  | [] -> []
  | [ el ] -> [ el ]
  | el :: tail -> [ el; elem ] @ put_in_between elem tail

let heading_level level heading =
  let open Omd in
  match level with
  | 0 -> H1 heading
  | 1 -> H2 heading
  | 2 -> H3 heading
  | 3 -> H4 heading
  | 4 -> H5 heading
  | _ -> H6 heading

let style_markdown kind md =
  let open Oct.Types in
  let open Omd in
  match kind with
  | SK_bold -> [ Bold md ]
  | SK_italic
  | SK_emphasize ->
    [ Emph md ]
  | SK_center
  | SK_left
  | SK_right
  | SK_superscript
  | SK_subscript
  | SK_custom _ ->
    (* TODO: implement SK_{center, left, right, superscript, subscript, custom}
       using html blocks *)
    md

let rec text_to_markdown doc = List.concat_map ~f:text_element_to_markdown doc

and text_element_to_markdown (doc_elem : Oct.Types.text_element) =
  let open Omd in
  match doc_elem with
  | Raw text -> [ Text text ]
  | Code code -> [ to_inline_code code ]
  | PreCode code -> [ to_code_block code ]
  | Verbatim text -> [ Raw text ]
  | Style (kind, text) -> style_markdown kind @@ text_to_markdown text
  | List l -> [ Ul (text_elements_to_markdown l) ]
  | Enum l -> [ Ol (text_elements_to_markdown l) ]
  | Newline -> [ new_line; new_line ]
  | Title (i, _, content) ->
    (* TODO: along with cross-references support, add support for labels *)
    let heading = text_to_markdown content in
    [ heading_level i heading ]
  | Ref (RK_link, url, descr) ->
    let descr = Option.map descr ~f:text_to_markdown |> Option.value ~default:[] in
    let empty_title = "" in
    [ Url (url, descr, empty_title) ]
  | Ref (_ref_kind, reference, descr) ->
    (* TODO: add support for cross-references *)
    Option.map ~f:text_to_markdown descr
    |> Option.value ~default:[ to_inline_code reference ]
  | Special_ref _
  | Target (_, _) (* TODO: add support for markdown-specific blocks *) ->
    []

and text_elements_to_markdown lst = List.map ~f:text_to_markdown lst

let rec tags_to_markdown (tags : Oct.Types.tag list) =
  List.map ~f:tag_to_markdown tags
  |> put_in_between [ new_line; new_line ]
  |> List.concat

and tag_to_markdown tag : Omd.element list =
  let open Oct.Types in
  let open Omd in
  let format_tag tag = Bold [ Emph [ Text tag ] ] in
  let plain_tag_to_md tag descr = [ format_tag tag; space; Text descr ] in
  let tag_with_text_to_md tag text =
    format_tag tag :: space :: text_to_markdown text
  in
  let marked_tag_to_md tag mark =
    [ format_tag tag; space; to_inline_code mark ]
  in
  let marked_tag_with_text_to_md tag mark text =
    format_tag tag :: space :: to_inline_code mark :: space
    :: text_to_markdown text
  in
  let see_tag_to_md (see_ref, comment) =
    let content =
      match (see_ref, comment) with
      | See_url url, text ->
        let empty_hover_title = "" in
        let link_title = [ Text "link" ] in
        Url (url, link_title, empty_hover_title)
        :: space :: text_to_markdown text
      | See_file name, text
      | See_doc name, text ->
        let no_prog_lang = "" in
        (* TODO: add support to reference files and documents *)
        Code (no_prog_lang, name) :: space :: text_to_markdown text
    in
    format_tag "@see" :: space :: content
  in
  match tag with
  | Author a -> plain_tag_to_md "@author" a
  | Canonical c -> plain_tag_to_md "@canonical" c
  | Deprecated text -> tag_with_text_to_md "@deprecated" text
  | Return_value text -> tag_with_text_to_md "@return" text
  | Version v -> marked_tag_to_md "@version" v
  | Since v -> marked_tag_to_md "@since" v
  | Before (ver, text) -> (
    match text with
    | [] -> marked_tag_to_md "@before" ver
    | _ -> marked_tag_with_text_to_md "@before" ver text )
  | Param (name, text) -> (
    match text with
    | [] ->
      tag_with_text_to_md "@param" [ Raw name ] (* in case `id` is missing) *)
    | _ -> marked_tag_with_text_to_md "@param" name text )
  | Raised_exception (exn, text) -> marked_tag_with_text_to_md "@raise" exn text
  | See (r, s) -> see_tag_to_md (r, s)
  | Custom (name, text) ->
    [ Emph [ Text ("@" ^ name) ]; space ] @ text_to_markdown text
  | Inline -> [ Emph [ Text "@inline" ] ]

let comment_to_markdown (doc, tags) =
  let text = text_to_markdown doc in
  let tags = tags_to_markdown tags in
  match tags with
  | [] -> text
  | non_empty_tags ->
    let separation = Omd.[ NL; Hr ] in
    text @ separation @ non_empty_tags

let translate doc =
  let open Oct in
  match parse (Lexing.from_string doc) with
  | Error e ->
    let msg = Errors.message e.error in
    log ~title:"parse doc comment" "invalid doc comments %s" msg;
    `Raw (Omd.to_markdown [ Raw doc ])
  | Ok doc ->
    let doc = comment_to_markdown doc in
    `Markdown (Omd.to_markdown doc)

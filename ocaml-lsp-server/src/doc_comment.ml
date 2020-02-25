open Omd
open Octavius
open Printf

let { Logger.log } = Logger.for_section "ocaml-lsp-server"

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

let style_to_markdown (k : Types.style_kind) md =
  match k with
  | SK_bold -> [ Bold md ]
  | SK_italic
  | SK_emphasize ->
    [ Emph md ]
  | SK_center
  | SK_left
  | SK_right ->
    md
  | SK_superscript
  | SK_subscript ->
    md (* does not seem easy to implement without html block *)
  | SK_custom _ -> md

let rec text_to_markdown doc = List.map element_to_markdown doc |> List.flatten

and element_to_markdown (doc : Octavius.Types.text_element) =
  match doc with
  | Raw text -> [ Text text ]
  | Code code -> [ Code ("ocaml", code) ]
  | PreCode code -> [ Code_block ("ocaml", code) ]
  | Verbatim text -> [ Raw text ]
  | Style (kind, text) ->
    let md = text_to_markdown text in
    style_to_markdown kind md
  | List l ->
    let md = List.map text_to_markdown l in
    [ Ul md ]
  | Enum l ->
    let md = List.map text_to_markdown l in
    [ Ol md ]
  | Newline -> [ NL ]
  | Title (i, title, content) ->
    let md = text_to_markdown content in
    [ heading_level i title md ]
  | Ref (RK_link, url, descr) ->
    let descr =
      match descr with
      | None -> []
      | Some descr -> text_to_markdown descr
    in
    [ Url (url, descr, "") ]
  | Ref (_ref_kind, reference, descr) -> (
    match descr with
    | None -> [ Code ("ocaml", reference) ]
    | Some descr -> text_to_markdown descr )
  | Special_ref _ -> (* TODO : how to handle this ? *) []
  | Target (_, _) ->
    (* TODO: support markown specific blocks *)
    []

let rec filter_map f l =
  match l with
  | [] -> []
  | x :: l -> (
    match f x with
    | None -> filter_map f l
    | Some x -> x :: filter_map f l )

let tags_to_markdown tags =
  let format_tag_plain name content =
    let content =
      match content with
      | None -> []
      | Some content -> text_to_markdown content
    in
    name :: Text " " :: content
  in
  let format_tag name content = format_tag_plain (Emph [Text name]) content in
  let param_to_md = function
    | Types.Param (name, content) -> Some (format_tag_plain (Code ("ocaml", name)) (Some content))
    | _ -> None
  in
  let params = filter_map param_to_md tags in
  let returns =
    match
      List.find_opt
        (function
          | Types.Return_value _ -> true
          | _ -> false)
        tags
    with
    | Some (Return_value text) -> Text "Returns " :: text_to_markdown text
    | _ -> []
  in
  let raise_to_md = function
    | Types.Raised_exception (name, content) ->
      Some (format_tag_plain (Code ("ocaml", name)) (Some content))
    | _ -> None
  in
  let raise = filter_map raise_to_md tags in
  let tag_to_md = function
    | Types.Author name -> Some (format_tag "@author" (Some [ Raw name ]))
    | Version vers -> Some (format_tag "@version" (Some [ Raw vers ]))
    | See (see_ref, content) ->
      let format_see content = Emph [ Text "@see" ] :: Text " " :: content in
      let content = text_to_markdown content in
      Some
        ( match see_ref with
        | See_url url -> format_see [ Url (url, content, "") ]
        | See_file file -> format_see (Text (sprintf "'%s' " file) :: content)
        | See_doc file -> format_see (Text (sprintf "\"%s\"' " file) :: content)
        )
    | Since vers -> Some (format_tag "@since" (Some [ Raw vers ]))
    | Before (vers, content) ->
      Some
        ( Emph [ Text "@before" ]
        :: Text " " :: Text vers :: Text " " :: text_to_markdown content )
    | Deprecated reason -> Some (format_tag "@since" (Some reason))
    | Inline -> Some (format_tag "@inline" None)
    | Custom (name, text) -> Some (format_tag ("@" ^ name) (Some text))
    | Canonical name -> Some (format_tag "@canonical" (Some [ Raw name ]))
    | Param _
    | Raised_exception _
    | Return_value _ ->
      None
  in
  let tags = filter_map tag_to_md tags in
  let ret =
    match tags with
    | [] -> []
    | tags -> [ H2 [ Text "Infos"; Ul tags ] ]
  in
  let ret =
    match raise with
    | [] -> ret
    | raise -> H2 [ Text "Raised exceptions"; Ul raise ] :: ret
  in
  match params with
  | [] -> ret
  | params -> H2 (Text "Parameters" :: Ul params :: returns) :: ret

let comment_to_markdown (doc, tags) =
  let text = text_to_markdown doc in
  let tags = tags_to_markdown tags in
  text @ (NL :: NL :: Hr :: tags)

let doc_comment_to_markdown doc =
  match parse (Lexing.from_string doc) with
  | Error e ->
    let msg = Errors.message e.error in
    log ~title:"debug" "invalid doc comments %s" msg;
    `Raw (Omd.to_markdown [ Raw doc])
  | Ok doc ->
    let doc = comment_to_markdown doc in
    `Markdown (Omd.to_markdown doc)

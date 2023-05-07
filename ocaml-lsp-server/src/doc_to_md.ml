open Import
module Oct = Octavius
open Omd
open Omd.Ctor

let space = Omd.Ctor.txt " "

(* [put_in_between elem lst] inserts [elem] between all elements of [lst] *)
let put_in_between elem lst =
  let rec loop acc = function
    | [] -> acc
    | [ hd ] -> hd :: acc
    | hd :: tail -> loop (elem :: hd :: acc) tail
  in
  List.rev (loop [] lst)

let heading_level level heading =
  match level with
  | 0 -> h 1 heading
  | 1 -> h 2 heading
  | 2 -> h 3 heading
  | 3 -> h 4 heading
  | 4 -> h 5 heading
  | _ -> h 6 heading

let inline_with_style ?style ?(attrs = []) s =
  match style with
  | None -> s
  | Some Oct.Types.SK_bold -> Strong (attrs, s)
  | Some Oct.Types.(SK_italic | SK_emphasize) -> Emph (attrs, s)
  | Some
      Oct.Types.(
        ( SK_center
        | SK_left
        | SK_right
        | SK_superscript
        | SK_subscript
        | SK_custom _ )) -> s

let rec fold_text_inline ?style elements acc =
  match elements with
  | Oct.Types.Raw text :: rest ->
    fold_text_inline rest (inline_with_style ?style (txt text) :: acc)
  | Oct.Types.Code cd :: rest ->
    fold_text_inline rest (inline_with_style ?style (code cd) :: acc)
  | Oct.Types.Verbatim text :: rest ->
    fold_text_inline rest (inline_with_style ?style (txt text) :: acc)
  | Oct.Types.Style (style, text) :: rest ->
    let inner, _blocks = fold_text_inline ~style text [] in
    fold_text_inline rest (inner @ acc)
  | Oct.Types.Newline :: rest -> fold_text_inline rest [ nl; nl ]
  | Oct.Types.Ref (RK_link, url, descr) :: rest ->
    let descr =
      (* ISSUE: links should accept a list of inline elements *)
      match
        Option.map descr ~f:(fun x ->
            let inner, _blocks = fold_text_inline ?style x [] in
            inner)
      with
      | Some (el :: _rest) -> el
      | Some [] | None -> txt ""
    in
    let link =
      Link ([], { label = descr; destination = url; title = Some "" })
    in
    fold_text_inline rest (inline_with_style ?style link :: acc)
  | Oct.Types.Ref (_ref_kind, reference, descr) :: rest ->
    (* TODO: add support for cross-references *)
    let x =
      Option.map
        ~f:(fun x ->
          let inner, _blocks = fold_text_inline ?style x [] in
          inner)
        descr
      |> Option.value ~default:[ inline_with_style ?style (code reference) ]
    in
    fold_text_inline rest (x @ acc)
  | _rest -> (List.rev acc, elements)

let rec fold_text elements acc =
  match elements with
  | Oct.Types.(Raw _ | Code _ | Verbatim _ | Style _ | Newline | Ref _) :: _rest
    ->
    let inlines, rest = fold_text_inline elements [] in
    fold_text rest (p inlines :: acc)
  | Oct.Types.Title (i, _, content) :: rest ->
    (* TODO: along with cross-references support, add support for labels *)
    let heading, _blocks = fold_text_inline content [] in
    fold_text rest (h (i + 1) heading :: acc)
  | Oct.Types.PreCode code :: rest -> fold_text rest (code_bl code :: acc)
  | Oct.Types.List l :: rest ->
    fold_text rest (ul (text_elements_to_markdown l) :: acc)
  | Oct.Types.Enum l :: rest ->
    fold_text rest (ol (text_elements_to_markdown l) :: acc)
  | Oct.Types.Special_ref _ :: rest | Oct.Types.Target (_, _) :: rest ->
    (* TODO: add support for markdown-specific blocks *) fold_text rest acc
  | [] -> acc

and text_to_markdown doc = fold_text doc []

and text_elements_to_markdown lst = List.map ~f:text_to_markdown lst

let rec tags_to_markdown (tags : Oct.Types.tag list) =
  List.map ~f:tag_to_markdown tags |> put_in_between [ nl; nl ] |> List.concat

and tag_to_markdown tag =
  let format_tag tag = Strong ([], Emph ([], txt tag)) in
  let plain_tag_to_md tag descr = [ format_tag tag; space; txt descr ] in
  let tag_with_text_to_md tag text =
    let inlines, _blocks = fold_text_inline text [] in
    format_tag tag :: space :: inlines
  in
  let marked_tag_to_md tag mark = [ format_tag tag; space; code mark ] in
  let marked_tag_with_text_to_md tag mark text =
    let inlines, _blocks = fold_text_inline text [] in
    format_tag tag :: space :: code mark :: space :: inlines
  in
  let see_tag_to_md (see_ref, comment) =
    let content =
      match (see_ref, comment) with
      | Oct.Types.See_url url, text ->
        let inlines, _blocks = fold_text_inline text [] in
        a ~url ~title:"link" "" :: space :: inlines
      | See_file name, text | See_doc name, text ->
        (* TODO: add support to reference files and documents *)
        let inlines, _blocks = fold_text_inline text [] in
        code name :: space :: inlines
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
    | _ -> marked_tag_with_text_to_md "@before" ver text)
  | Param (name, text) -> (
    match text with
    | [] ->
      tag_with_text_to_md "@param" [ Raw name ] (* in case `id` is missing) *)
    | _ -> marked_tag_with_text_to_md "@param" name text)
  | Raised_exception (exn, text) -> marked_tag_with_text_to_md "@raise" exn text
  | See (r, s) -> see_tag_to_md (r, s)
  | Custom (name, text) ->
    let inlines, _blocks = fold_text_inline text [] in
    [ Emph ([], txt ("@" ^ name)); space ] @ inlines
  | Inline -> [ Emph ([], txt "@inline") ]

let comment_to_markdown (doc, tags) =
  let text = text_to_markdown doc in
  let tags = tags_to_markdown tags in
  match tags with
  | [] -> text
  | non_empty_tags -> text @ [ hr ] @ [ p non_empty_tags ]

type t =
  | Raw of string
  | Markdown of string

let translate doc : t =
  match Oct.parse (Lexing.from_string doc) with
  | Error e ->
    let msg = Oct.Errors.message e.error in
    Log.log ~section:"debug" (fun () ->
        Log.msg "invalid doc comments" [ ("msg", `String msg) ]);
    Raw (Omd.of_string doc)
  | Ok doc ->
    let doc = comment_to_markdown doc in
    Markdown (Omd.to_string doc)

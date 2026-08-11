open Import
open Fiber.O

(** A documentation comment, paired with what is needed to turn the spans
    odoc-parser reports back into source positions. *)
type comment =
  { parsed : Odoc_parser.t
  ; text : string (** the comment's contents, without its delimiters *)
  ; offset : int (** [pos_cnum] of the first character of [text] *)
  }

(* Merlin hands us the contents of a comment without its delimiters,
   so a documentation comment arrives with a single leading asterisk. *)
let parse (text, (loc : Loc.t)) =
  let open Option.O in
  let* text = String.chop_prefix text ~prefix:"*" in
  if String.is_prefix text ~prefix:"*"
  then None (* the comment is "(*** ... *)", not documentation *)
  else (
    let start = loc.loc_start in
    (* [parse_comment] wants the position of the character right after the
       three-character opening delimiter of a documentation comment. *)
    let offset = start.pos_cnum + 3 in
    let location = { start with Lexing.pos_cnum = offset } in
    Some { parsed = Odoc_parser.parse_comment ~location ~text; text; offset })
;;

let range t (span : Odoc_parser.Loc.span) =
  let open Option.O in
  let position point =
    Position.of_lexical_position (Odoc_parser.position_of_point t.parsed point)
  in
  let* start = position span.start in
  let+ end_ = position span.end_ in
  { Range.start; end_ }
;;

let index_of_point t point =
  (Odoc_parser.position_of_point t.parsed point).pos_cnum - t.offset
;;

(** Walk [text] from [from] to [to_], keeping track of the point [from]
    corresponds to. *)
let advance t ~from ~to_ point =
  let rec loop i (point : Odoc_parser.Loc.point) =
    if i >= to_
    then point
    else
      loop
        (i + 1)
        (if Char.equal t.text.[i] '\n'
         then { Odoc_parser.Loc.line = point.line + 1; column = 0 }
         else { point with column = point.column + 1 })
  in
  loop from point
;;

(** [`See] tags carry no location for their target, and the span of the tag
    itself covers the description that follows it, so locate the delimited
    target within the comment's text by hand. *)
let see_target_span t ~(tag : Odoc_parser.Loc.span) ~target =
  let open Option.O in
  let start_index = index_of_point t tag.start in
  let* () = Option.some_if (start_index >= 0) () in
  let* opening = String.index_from t.text start_index '<' in
  let target_start = opening + 1 in
  let target_stop = target_start + String.length target in
  let* () =
    Option.some_if
      (target_stop < String.length t.text && Char.equal t.text.[target_stop] '>')
      ()
  in
  let start = advance t ~from:start_index ~to_:target_start tag.start in
  let end_ = advance t ~from:target_start ~to_:target_stop start in
  Some { tag with Odoc_parser.Loc.start; end_ }
;;

let rec inline_elements
          t
          acc
          (elements : Odoc_parser.Ast.inline_element Odoc_parser.Loc.with_location list)
  =
  List.fold elements ~init:acc ~f:(fun acc { Odoc_parser.Loc.value; location } ->
    match value with
    | `Link (url, content) -> inline_elements t ((location, url) :: acc) content
    | `Reference (_, _, content) | `Styled (_, content) -> inline_elements t acc content
    | `Space _ | `Word _ | `Code_span _ | `Raw_markup _ | `Math_span _ -> acc)
;;

let rec nestable_block_elements
          t
          acc
          (elements :
            Odoc_parser.Ast.nestable_block_element Odoc_parser.Loc.with_location list)
  =
  List.fold elements ~init:acc ~f:(fun acc { Odoc_parser.Loc.value; location = _ } ->
    match value with
    | `Paragraph inlines -> inline_elements t acc inlines
    | `List (_, _, items) -> List.fold items ~init:acc ~f:(nestable_block_elements t)
    | `Table ((grid, _), _) ->
      List.fold grid ~init:acc ~f:(fun acc row ->
        List.fold row ~init:acc ~f:(fun acc (cell, _) ->
          nestable_block_elements t acc cell))
    | `Code_block { output = Some output; _ } -> nestable_block_elements t acc output
    | `Code_block _ | `Verbatim _ | `Modules _ | `Math_block _ -> acc)
;;

let tag t acc ~location (tag : Odoc_parser.Ast.tag) =
  match tag with
  | `See (`Url, target, content) ->
    let acc =
      match see_target_span t ~tag:location ~target with
      | None -> acc
      | Some span -> (span, target) :: acc
    in
    nestable_block_elements t acc content
  | `See (_, _, content)
  | `Deprecated content
  | `Return content
  | `Param (_, content)
  | `Raise (_, content)
  | `Before (_, content) -> nestable_block_elements t acc content
  | `Author _ | `Since _ | `Version _ | `Canonical _ | `Inline | `Open | `Closed | `Hidden
    -> acc
;;

let targets t =
  List.fold (Odoc_parser.ast t.parsed) ~init:[] ~f:(fun acc element ->
    match element.Odoc_parser.Loc.value with
    | `Heading (_, _, inlines) -> inline_elements t acc inlines
    | `Tag v -> tag t acc ~location:element.location v
    | #Odoc_parser.Ast.nestable_block_element as value ->
      nestable_block_elements t acc [ { element with value } ])
;;

let of_comment comment =
  match parse comment with
  | None -> []
  | Some t ->
    targets t
    |> List.rev_filter_map ~f:(fun (span, url) ->
      let open Option.O in
      let+ range = range t span in
      DocumentLink.create ~range ~target:(Uri.of_string url) ())
;;

let run (state : State.t) uri =
  let* () = Fiber.return () in
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let+ comments =
      Document.Merlin.with_pipeline_exn
        ~name:"document-link"
        merlin
        Mpipeline.reader_comments
    in
    Some (List.concat_map comments ~f:of_comment)
;;

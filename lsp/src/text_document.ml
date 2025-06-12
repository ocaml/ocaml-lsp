open Import

include struct
  open Types
  module DidOpenTextDocumentParams = DidOpenTextDocumentParams
  module DocumentUri = DocumentUri
  module Range = Range
  module TextDocumentItem = TextDocumentItem
  module TextDocumentContentChangeEvent = TextDocumentContentChangeEvent
  module TextEdit = TextEdit
end

type invalid_utf = String_zipper.invalid_utf =
  | Malformed of string
  | Insufficient_input

exception Invalid_utf = String_zipper.Invalid_utf

type t =
  { languageId : string
  ; (* text is stored as utf8 internally no matter what the encoding is *)
    mutable text : string option
  ; uri : DocumentUri.t
  ; version : int
  ; mutable zipper : String_zipper.t
  ; position_encoding : [ `UTF8 | `UTF16 ]
  }

let position_encoding t = t.position_encoding

let make
  ~position_encoding
  { DidOpenTextDocumentParams.textDocument =
      { TextDocumentItem.languageId; text; uri; version }
  }
  =
  let zipper = String_zipper.of_string text in
  { text = Some text; position_encoding; zipper; uri; version; languageId }
;;

let documentUri (t : t) = t.uri
let version (t : t) = t.version
let languageId (t : t) = t.languageId

let apply_change encoding sz (change : TextDocumentContentChangeEvent.t) =
  match change.range with
  | None -> String_zipper.of_string change.text
  | Some range -> String_zipper.apply_change sz range encoding ~replacement:change.text
;;

let apply_content_changes ?version t changes =
  let zipper =
    List.fold_left ~f:(apply_change t.position_encoding) ~init:t.zipper changes
  in
  let version =
    match version with
    | None -> t.version
    | Some version -> version
  in
  { t with zipper; text = None; version }
;;

let text t =
  match t.text with
  | Some text -> text
  | None ->
    let zipper, text = String_zipper.squash t.zipper in
    t.text <- Some text;
    t.zipper <- zipper;
    text
;;

module Edit_map = Map.Make (Position)

let add_edit map (change : TextEdit.t) =
  (* TODO check non overlapping property *)
  Edit_map.update map ~key:change.range.start ~f:(function
    | None -> Some [ change ]
    | Some changes -> Some (change :: changes))
;;

let apply_changes zipper encoding changes =
  let simplified = List.fold_left changes ~init:Edit_map.empty ~f:add_edit in
  let b = Buffer.create 0 in
  let pos = ref Position.zero in
  let zipper = String_zipper.goto_position zipper !pos encoding in
  let zipper = ref zipper in
  Edit_map.iter simplified ~f:(fun ~key:start ~data ->
    (* guaranteed by the non overlapping property we aren't yet checking *)
    assert (Position.compare start !pos >= 0);
    zipper := String_zipper.goto_position !zipper !pos encoding;
    let zipper' = String_zipper.goto_position !zipper start encoding in
    String_zipper.add_buffer_between b !zipper zipper';
    zipper := zipper';
    List.rev data
    |> List.iter ~f:(fun { TextEdit.newText; range } ->
      assert (Position.compare range.end_ !pos >= 0);
      pos := range.end_;
      Buffer.add_string b newText));
  let zipper = String_zipper.goto_position !zipper !pos encoding in
  let zipper' = String_zipper.goto_end zipper in
  String_zipper.add_buffer_between b zipper zipper';
  Buffer.contents b
;;

let set_version t ~version = { t with version }

let apply_text_document_edits t (edits : TextEdit.t list) =
  let text = apply_changes t.zipper t.position_encoding edits in
  let zipper = String_zipper.of_string text in
  { t with text = Some text; zipper }
;;

let absolute_position t pos =
  String_zipper.goto_position t.zipper pos t.position_encoding |> String_zipper.offset
;;

let absolute_range t (range : Range.t) =
  let zipper = String_zipper.goto_position t.zipper range.start t.position_encoding in
  let start = String_zipper.offset zipper in
  let zipper = String_zipper.goto_position zipper range.end_ t.position_encoding in
  let stop = String_zipper.offset zipper in
  start, stop
;;

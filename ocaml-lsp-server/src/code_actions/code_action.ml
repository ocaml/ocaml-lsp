open Import

(* TODO:

   - split the "needs merlin" part of the action out so that all actions can be
     batched *)

type t =
  { kind : CodeActionKind.t
  ; run :
      [ `Batchable of
          Mpipeline.t -> Document.t -> CodeActionParams.t -> CodeAction.t option
      | `Non_batchable of Document.t -> CodeActionParams.t -> CodeAction.t option Fiber.t
      ]
  }

let batchable kind run = { kind; run = `Batchable run }
let non_batchable kind run = { kind; run = `Non_batchable run }

let kind_is_requested only kind =
  match only with
  | None -> true
  | Some only ->
    let to_string kind =
      match CodeActionKind.yojson_of_t kind with
      | `String kind -> kind
      | _ -> assert false
    in
    let kind = to_string kind in
    List.exists only ~f:(fun requested ->
      let requested = to_string requested in
      String.equal requested kind || String.is_prefix kind ~prefix:(requested ^ "."))
;;

let source_text doc (loc : Loc.t) =
  let open Option.O in
  let source = Document.source doc in
  let* start = Position.of_lexical_position loc.loc_start in
  let+ end_ = Position.of_lexical_position loc.loc_end in
  let (`Offset start) = Msource.get_offset source (Position.logical start) in
  let (`Offset end_) = Msource.get_offset source (Position.logical end_) in
  String.sub (Msource.text source) ~pos:start ~len:(end_ - start)
;;

let workspace_edit doc text_edits =
  let uri = Document.uri doc in
  let version = Document.version doc in
  let textDocument = OptionalVersionedTextDocumentIdentifier.create ~uri ~version () in
  let edits = List.map text_edits ~f:(fun edit -> `TextEdit edit) in
  let edit = TextDocumentEdit.create ~textDocument ~edits in
  WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
;;

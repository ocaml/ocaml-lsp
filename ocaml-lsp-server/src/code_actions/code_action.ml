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

let source_text doc (loc : Loc.t) =
  let open Option.O in
  let source = Document.source doc in
  let* start = Position.of_lexical_position loc.loc_start in
  let+ end_ = Position.of_lexical_position loc.loc_end in
  let (`Offset start) = Msource.get_offset source (Position.logical start) in
  let (`Offset end_) = Msource.get_offset source (Position.logical end_) in
  String.sub (Msource.text source) ~pos:start ~len:(end_ - start)
;;

let workspace_edit = Document.edit

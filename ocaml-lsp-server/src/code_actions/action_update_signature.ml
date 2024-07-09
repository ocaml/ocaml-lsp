open Import
open Fiber.O

let action_kind = "update_intf"

let code_action_of_intf doc text_edits =
  let edit : WorkspaceEdit.t =
    let doc_edit =
      let edits = List.map text_edits ~f:(fun e -> `TextEdit e) in
      let textDocument =
        let uri = Document.uri doc in
        let version = Document.version doc in
        OptionalVersionedTextDocumentIdentifier.create ~uri ~version ()
      in
      TextDocumentEdit.create ~textDocument ~edits
    in
    WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit doc_edit ] ()
  in
  let title = String.capitalize_ascii "update signature(s) to match implementation" in
  CodeAction.create
    ~title
    ~kind:(CodeActionKind.Other action_kind)
    ~edit
    ~isPreferred:false
    ()
;;

let code_action (state : State.t) doc (params : CodeActionParams.t) =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin m when Document.Merlin.kind m = Impl -> Fiber.return None
  | `Merlin intf_merlin ->
    let* text_edits =
      Inference.update_signatures ~state ~doc ~range:params.range ~intf_merlin
    in
    (match text_edits with
     | [] -> Fiber.return None
     | _ -> Fiber.return (Some (code_action_of_intf doc text_edits)))
;;

let kind = CodeActionKind.Other action_kind
let t state = { Code_action.kind; run = `Non_batchable (code_action state) }

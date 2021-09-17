open Import

let action_kind = "inferred_intf"

let code_action_of_intf doc intf range =
  let edit : WorkspaceEdit.t =
    let edit =
      let textedit : TextEdit.t = { range; newText = intf } in
      let textDocument =
        let uri = Document.uri doc in
        let version = Document.version doc in
        OptionalVersionedTextDocumentIdentifier.create ~uri ~version ()
      in
      TextDocumentEdit.create ~textDocument ~edits:[ `TextEdit textedit ]
    in
    WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
  in
  let title = String.capitalize_ascii "Insert inferred interface" in
  CodeAction.create ~title ~kind:(CodeActionKind.Other action_kind) ~edit
    ~isPreferred:false ()

let code_action doc (state : State.t) (params : CodeActionParams.t) =
  let open Fiber.O in
  match Document.kind doc with
  | Impl -> Fiber.return None
  | Intf ->
    let+ intf = Inference.infer_intf ~force_open_impl:true state doc in
    Some (code_action_of_intf doc intf params.range)

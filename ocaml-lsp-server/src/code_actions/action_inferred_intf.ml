open Import
open Fiber.O

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
  | `Merlin _ ->
    let* intf = Inference.infer_intf state doc in
    (match intf with
     | None -> Fiber.return None
     | Some intf ->
       let+ formatted_intf =
         Ocamlformat_rpc.format_type state.ocamlformat_rpc ~typ:intf
       in
       let intf =
         match formatted_intf with
         | Ok formatted_intf -> formatted_intf
         | Error _ -> intf
       in
       Some (code_action_of_intf doc intf params.range))
;;

let kind = CodeActionKind.Other action_kind
let t state = { Code_action.kind; run = `Non_batchable (code_action state) }

open Import
open Fiber.O

let action_kind = "destruct (enumerate cases)"
let kind = CodeActionKind.Other action_kind

let code_action_of_case_analysis ~supportsJumpToNextHole doc uri (loc, newText) =
  let range : Range.t = Range.of_loc loc in
  let textedit : TextEdit.t = { range; newText } in
  let edit : WorkspaceEdit.t =
    let version = Document.version doc in
    let textDocument = OptionalVersionedTextDocumentIdentifier.create ~uri ~version () in
    let edit = TextDocumentEdit.create ~textDocument ~edits:[ `TextEdit textedit ] in
    WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
  in
  let title = String.capitalize_ascii action_kind in
  let command =
    if supportsJumpToNextHole
    then
      Some
        (Client.Custom_commands.next_hole
           ~in_range:(Range.resize_for_edit textedit)
           ~notify_if_no_hole:false
           ())
    else None
  in
  CodeAction.create
    ~title
    ~kind:(CodeActionKind.Other action_kind)
    ~edit
    ?command
    ~isPreferred:false
    ()
;;

let code_action (state : State.t) doc (params : CodeActionParams.t) =
  let uri = params.textDocument.uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin m when Document.Merlin.kind m = Intf -> Fiber.return None
  | `Merlin merlin ->
    let command =
      let start = Position.logical params.range.start in
      let finish = Position.logical params.range.end_ in
      Query_protocol.Case_analysis (start, finish)
    in
    let* res = Document.Merlin.dispatch ~name:"destruct" merlin command in
    (match res with
     | Ok (loc, newText) ->
       let+ newText =
         let+ formatted_text =
           Ocamlformat_rpc.format_type state.ocamlformat_rpc ~typ:newText
         in
         match formatted_text with
         | Ok formatted_text -> formatted_text
         | Error _ -> newText
       in
       let supportsJumpToNextHole =
         State.experimental_client_capabilities state
         |> Client.Experimental_capabilities.supportsJumpToNextHole
       in
       Some (code_action_of_case_analysis ~supportsJumpToNextHole doc uri (loc, newText))
     | Error
         { exn =
             ( Merlin_analysis.Destruct.Wrong_parent _
             | Query_commands.No_nodes
             | Merlin_analysis.Destruct.Not_allowed _
             | Merlin_analysis.Destruct.Useless_refine
             | Merlin_analysis.Destruct.Ill_typed
             | Merlin_analysis.Destruct.Nothing_to_do )
         ; backtrace = _
         } -> Fiber.return None
     | Error exn -> Exn_with_backtrace.reraise exn)
;;

let t state = { Code_action.kind; run = `Non_batchable (code_action state) }

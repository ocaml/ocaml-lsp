open Import
open Fiber.O

let action_kind = "destruct (enumerate cases)"
let kind = CodeActionKind.Other action_kind

let code_action_of_case_analysis ~action_kind ~supportsJumpToNextHole doc (loc, newText) =
  let range : Range.t = Range.of_loc loc in
  let textedit : TextEdit.t = { range; newText } in
  let edit = Text_document.workspace_edit (Document.text_document doc) [ textedit ] in
  let title = String.capitalize action_kind in
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

let run state doc merlin ~action_kind ~(range : Range.t) ~postprocess =
  let command =
    let start = Position.logical range.start in
    let finish = Position.logical range.end_ in
    Query_protocol.Case_analysis (start, finish)
  in
  let+ res = Document.Merlin.dispatch ~name:"destruct" merlin command in
  match res with
  | Ok reply ->
    let reply = postprocess reply in
    let supportsJumpToNextHole =
      State.experimental_client_capabilities state
      |> Client.Experimental_capabilities.supportsJumpToNextHole
    in
    Some (code_action_of_case_analysis ~action_kind ~supportsJumpToNextHole doc reply)
  | Error
      { exn =
          ( Merlin_analysis.Destruct.Wrong_parent _
          | Query_commands.No_nodes
          | Merlin_analysis.Destruct.Not_allowed _
          | Merlin_analysis.Destruct.Useless_refine
          | Merlin_analysis.Destruct.Ill_typed
          | Merlin_analysis.Destruct.Nothing_to_do )
      ; backtrace = _
      } -> None
  | Error exn -> Exn_with_backtrace.reraise exn
;;

let code_action (state : State.t) doc (params : CodeActionParams.t) =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin m when Document.Merlin.kind m = Intf -> Fiber.return None
  | `Merlin merlin ->
    run state doc merlin ~action_kind ~range:params.range ~postprocess:Fun.id
;;

let t state = { Code_action.kind; run = `Non_batchable (code_action state) }

open Import
open Fiber.O

let action_kind = "destruct (enumerate cases)"
let kind = CodeActionKind.Other action_kind

let code_action_of_case_analysis ~supportsJumpToNextHole doc (loc, newText) =
  let range : Range.t = Range.of_loc loc in
  let textedit : TextEdit.t = { range; newText } in
  let edit = Code_action.workspace_edit doc [ textedit ] in
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

type dispatch = Range.t -> (Loc.t * string, Exn_with_backtrace.t) result Fiber.t

let dispatch merlin (range : Range.t) =
  let command =
    let start = Position.logical range.start in
    let finish = Position.logical range.end_ in
    Query_protocol.Case_analysis (start, finish)
  in
  Document.Merlin.dispatch ~name:"destruct" merlin command
;;

let cached_dispatch merlin =
  let results = ref [] in
  let read result =
    let* response = Fiber.Ivar.read result in
    match response with
    | Ok response -> Fiber.return response
    | Error errors -> Fiber.reraise_all errors
  in
  fun range ->
    match List.find !results ~f:(fun (range', _) -> Poly.equal range range') with
    | Some (_, result) -> read result
    | None ->
      let result = Fiber.Ivar.create () in
      results := (range, result) :: !results;
      let* response = Fiber.collect_errors (fun () -> dispatch merlin range) in
      let* () = Fiber.Ivar.fill result response in
      (match response with
       | Ok response -> Fiber.return response
       | Error errors -> Fiber.reraise_all errors)
;;

let code_action (state : State.t) dispatch_ doc (params : CodeActionParams.t) =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin m when Document.Merlin.kind m = Intf -> Fiber.return None
  | `Merlin merlin ->
    let dispatch = Option.value dispatch_ ~default:(dispatch merlin) in
    let* res = dispatch params.range in
    (match res with
     | Ok (loc, newText) ->
       let supportsJumpToNextHole =
         State.experimental_client_capabilities state
         |> Client.Experimental_capabilities.supportsJumpToNextHole
       in
       let opt =
         Some (code_action_of_case_analysis ~supportsJumpToNextHole doc (loc, newText))
       in
       Fiber.return opt
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

let t ?dispatch state =
  { Code_action.kind; run = `Non_batchable (code_action state dispatch) }
;;

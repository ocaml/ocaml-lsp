open Import

let action_kind = "jump-to-target"

let targets =
  [ "fun"; "match"; "let"; "module"; "module-type"; "match-next-case"; "match-prev-case" ]
;;

(* Dispatch the jump request to Merlin and get the result *)
let dispatch_jump_request ~merlin ~position ~target =
  Document.Merlin.with_pipeline_exn merlin (fun pipeline ->
    let pposition = Position.logical position in
    let query = Query_protocol.Jump (target, pposition) in
    Query_commands.dispatch pipeline query)
;;

let code_action_of_jump start_pos (locs : Location.t list) =
  match locs with
  | [] -> None
  | _ ->
    let locations =
      `List
        (List.map locs ~f:(fun { Location.range; uri } ->
           `Assoc
             [ "uri", `String (Uri.to_string uri)
             ; "position", Position.yojson_of_t range.start
             ]))
    in
    let uri =
      match locs with
      | { Location.uri; _ } :: _ -> Uri.to_string uri
      | [] -> failwith "Unexpected empty locations list"
    in
    let command =
      Command.create
        ~title:"Merlin Jump"
        ~command:"editor.action.goToLocations"
        ~arguments:
          [ `Assoc
              [ "uri", `String uri
              ; "position", Position.yojson_of_t start_pos
              ; "locations", locations
              ; "multiple", `String "peek"
              ; "noResultsMessage", `String "No targets found"
              ]
          ]
        ()
    in
    Some
      (CodeAction.create
         ~title:"Jump to Target"
         ~kind:(CodeActionKind.Other action_kind)
         ~command
         ())
;;

let code_action doc (params : CodeActionParams.t) =
  let open Fiber.O in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let rec try_targets = function
      | [] -> Fiber.return None
      | target :: rest ->
        let* result =
          dispatch_jump_request ~merlin ~position:params.range.start ~target
        in
        (match result with
         | `Error _ -> try_targets rest
         | `Found pos ->
           (match Position.of_lexical_position pos with
            | None -> try_targets rest
            | Some position ->
              let range = { Range.start = position; end_ = position } in
              let locs = [ { Location.range; uri = params.textDocument.uri } ] in
              Fiber.return (code_action_of_jump params.range.start locs)))
    in
    try_targets targets
;;

let kind = CodeActionKind.Other action_kind
let t = Code_action.non_batchable kind code_action

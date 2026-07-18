open Import

let code_action
      (mode : [ `Qualify | `Unqualify ])
      (action_kind : string)
      pipeline
      doc
      (params : CodeActionParams.t)
  =
  let res =
    let command =
      let pos_start = Position.logical params.range.start in
      Query_protocol.Refactor_open (mode, pos_start)
    in
    Query_commands.dispatch pipeline command
  in
  match res with
  | [] -> None
  | changes ->
    let code_action =
      let edit : WorkspaceEdit.t =
        let changes =
          List.map changes ~f:(fun (newText, loc) ->
            let range = Range.of_loc loc |> Document.range_of_merlin_range doc in
            { TextEdit.newText; range })
        in
        WorkspaceEdit.create ~changes:[ params.textDocument.uri, changes ] ()
      in
      let kind = CodeActionKind.Other action_kind in
      let title = String.capitalize_ascii action_kind in
      CodeAction.create ~title ~kind ~edit ~isPreferred:false ()
    in
    Some code_action
;;

let unqualify =
  let action_kind = "remove module name from identifiers" in
  Code_action.batchable (Other action_kind) (code_action `Unqualify action_kind)
;;

let qualify =
  let action_kind = "put module name in identifiers" in
  Code_action.batchable (Other action_kind) (code_action `Qualify action_kind)
;;

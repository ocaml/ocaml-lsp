open Import

let code_action (mode : [ `Qualify | `Unqualify ]) (action_kind : string) doc
    (params : CodeActionParams.t) =
  let open Fiber.O in
  let+ res =
    let command =
      let pos_start = Position.logical params.range.start in
      Query_protocol.Refactor_open (mode, pos_start)
    in
    Document.dispatch_exn doc command
  in
  match res with
  | [] -> None
  | changes ->
    let code_action =
      let edit : WorkspaceEdit.t =
        let edits =
          List.map changes ~f:(fun (newText, loc) ->
              { TextEdit.newText; range = Range.of_loc loc })
        in
        let uri = params.textDocument.uri in
        WorkspaceEdit.create ~changes:[ (uri, edits) ] ()
      in
      let kind = CodeActionKind.Other action_kind in
      let title = String.capitalize_ascii action_kind in
      CodeAction.create ~title ~kind ~edit ~isPreferred:false ()
    in
    Some code_action

let unqualify =
  let action_kind = "remove module name from identifiers" in
  { Code_action.action_kind; run = code_action `Unqualify action_kind }

let qualify =
  let action_kind = "put module name in identifiers" in
  { Code_action.action_kind; run = code_action `Qualify action_kind }

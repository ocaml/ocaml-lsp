open Import

let code_action (mode : [ `Qualify | `Unqualify ]) (action_kind : string) doc
    (params : CodeActionParams.t) =
  let open Fiber.O in
  let uri = Uri.t_of_yojson (`String params.textDocument.uri) in
  let pos_start = Position.logical params.range.start in
  let command = Query_protocol.Refactor_open (mode, pos_start) in
  let+ res = Document.dispatch doc command in
  match res with
  | Ok [] -> Ok None
  | Error e -> Error (Jsonrpc.Response.Error.of_exn e)
  | Ok changes ->
    let title = Stdlib.String.capitalize_ascii action_kind in
    let kind = CodeActionKind.Other action_kind in
    let edit : WorkspaceEdit.t =
      let edits =
        List.map changes ~f:(fun (newText, loc) ->
            { TextEdit.newText; range = Range.of_loc loc })
      in
      let uri = Uri.to_string uri in
      WorkspaceEdit.create ~changes:[ (uri, edits) ] ()
    in
    let code_action =
      CodeAction.create ~title ~kind ~edit ~isPreferred:false ()
    in
    Ok (Some code_action)

module Unqualify = struct
  let action_kind = "remove module name from identifiers"

  let code_action doc params = code_action `Unqualify action_kind doc params
end

module Qualify = struct
  let action_kind = "put module name in identifiers"

  let code_action doc params = code_action `Qualify action_kind doc params
end

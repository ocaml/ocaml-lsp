open Import
open Fiber.O

let command_name = "ocamllsp/open-related-source"

let command_run server (params : ExecuteCommandParams.t) =
  let uri =
    match params.arguments with
    | Some [ json ] -> DocumentUri.t_of_yojson json
    | None | Some _ ->
      Jsonrpc.Response.Error.raise
      @@ Jsonrpc.Response.Error.make
           ~code:Jsonrpc.Response.Error.Code.InvalidParams
           ~message:"takes a single uri as input"
           ()
  in
  let+ { ShowDocumentResult.success } =
    let req = ShowDocumentParams.create ~uri ~takeFocus:true () in
    Server.request server (Server_request.ShowDocumentRequest req)
  in
  if not success
  then (
    let uri = Uri.to_string uri in
    Format.eprintf "failed to open %s@." uri);
  `Null
;;

let available (capabilities : ShowDocumentClientCapabilities.t option) =
  match capabilities with
  | None | Some { support = false } -> false
  | Some { support = true } -> true
;;

let for_uri (capabilities : ShowDocumentClientCapabilities.t option) doc =
  let uri = Document.uri doc in
  let merlin_doc =
    match Document.kind doc with
    | `Merlin doc -> Some doc
    | `Other -> None
  in
  match available capabilities with
  | false -> []
  | true ->
    Document.get_impl_intf_counterparts merlin_doc uri
    |> List.map ~f:(fun uri ->
      let path = Uri.to_path uri in
      let exists = Sys.file_exists path in
      let title =
        sprintf "%s %s" (if exists then "Open" else "Create") (Filename.basename path)
      in
      let command =
        let arguments = [ DocumentUri.yojson_of_t uri ] in
        Command.create ~title ~command:command_name ~arguments ()
      in
      let edit =
        match exists with
        | true -> None
        | false ->
          let documentChanges = [ `CreateFile (CreateFile.create ~uri ()) ] in
          Some (WorkspaceEdit.create ~documentChanges ())
      in
      CodeAction.create ?edit ~title ~kind:(CodeActionKind.Other "switch") ~command ())
;;

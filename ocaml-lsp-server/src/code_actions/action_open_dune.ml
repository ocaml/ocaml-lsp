open Import
open Fiber.O

let command_name = "ocamllsp/open-dune-file"
let kind = CodeActionKind.Other "open-dune"
let title = "Open dune file"

let available (capabilities : ShowDocumentClientCapabilities.t option) =
  match capabilities with
  | Some { support } -> support
  | None -> false
;;

let invalid_params message =
  Jsonrpc.Response.Error.raise
    (Jsonrpc.Response.Error.make ~code:InvalidParams ~message ())
;;

let command_run server (params : ExecuteCommandParams.t) =
  let uri =
    match params.arguments with
    | Some [ json ] -> DocumentUri.t_of_yojson json
    | None | Some _ -> invalid_params "takes a single URI as input"
  in
  let+ { ShowDocumentResult.success } =
    let req = ShowDocumentParams.create ~uri ~takeFocus:true () in
    Server.request server (Server_request.ShowDocumentRequest req)
  in
  if not success then Format.eprintf "failed to open %s@." (Uri.to_string uri);
  `Null
;;

let file_exists path = Sys.file_exists path && not (Sys.is_directory path)

let is_project_root dir =
  List.exists [ "dune-project"; "dune-workspace" ] ~f:(fun name ->
    Sys.file_exists (Filename.concat dir name))
;;

let rec find_closest_dune dir =
  let dune = Filename.concat dir "dune" in
  if file_exists dune
  then Some dune
  else if is_project_root dir
  then None
  else (
    let parent = Filename.dirname dir in
    if String.equal parent dir then None else find_closest_dune parent)
;;

let for_uri (capabilities : ShowDocumentClientCapabilities.t option) uri =
  match available capabilities with
  | false -> []
  | true ->
    (match
       let source = Uri.to_path uri in
       find_closest_dune (Filename.dirname source)
     with
     | None -> []
     | Some path ->
       let command =
         let arguments =
           let uri = DocumentUri.of_path path in
           [ DocumentUri.yojson_of_t uri ]
         in
         Command.create ~title ~command:command_name ~arguments ()
       in
       [ CodeAction.create ~title ~kind ~command () ])
;;

open Import
open Fiber.O

let command_name = "ocamllsp/show-document-text"

let command_run server store args =
  let uri =
    match args with
    | Some [ `String arg ] -> arg
    | _ -> assert false
  in
  let doc = Document_store.get store (Uri.t_of_yojson (`String uri)) |> Document.text in
  let uri, chan =
    Filename.open_temp_file
      (sprintf "ocamllsp-document.%d" (Unix.getpid ()))
      (Filename.extension uri)
  in
  output_string chan doc;
  close_out_noerr chan;
  let req =
    let uri = Uri.of_path uri in
    Server_request.ShowDocumentRequest (ShowDocumentParams.create ~uri ~takeFocus:true ())
  in
  let+ { ShowDocumentResult.success = _ } = Server.request server req in
  ()
;;

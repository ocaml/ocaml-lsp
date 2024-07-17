open Import
open Fiber.O

let command_name = "ocamllsp/show-merlin-config"

let command_run server store =
  let* json =
    let+ docs =
      Document_store.fold store ~init:[] ~f:(fun doc acc ->
        match Document.kind doc with
        | `Other -> acc
        | `Merlin m -> m :: acc)
      |> Fiber.parallel_map ~f:(fun doc ->
        let+ config = Document.Merlin.mconfig doc in
        let config : Json.t = (Mconfig.dump config :> Json.t) in
        let uri = Document.uri (Document.Merlin.to_doc doc) in
        Uri.to_string uri, config)
    in
    let json = `Assoc docs in
    Format.asprintf "%a@.%!" Json.pp json
  in
  let uri, chan =
    Filename.open_temp_file (sprintf "merlin-config.%d" (Unix.getpid ())) ".json"
  in
  output_string chan json;
  close_out_noerr chan;
  let req =
    let uri = Uri.of_path uri in
    Server_request.ShowDocumentRequest (ShowDocumentParams.create ~uri ~takeFocus:true ())
  in
  let+ { ShowDocumentResult.success = _ } = Server.request server req in
  ()
;;

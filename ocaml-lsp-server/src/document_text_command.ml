open Import

let command_name = "ocamllsp/show-document-text"

let command_run server store args =
  let uri =
    match args with
    | Some [ `String arg ] -> arg
    | _ -> assert false
  in
  let doc = Document_store.get store (Uri.t_of_yojson (`String uri)) |> Document.text in
  Client.show_document_contents
    server
    ~prefix:"ocamllsp-document"
    ~suffix:(Filename.extension uri)
    doc
;;

open Test.Import

let print_hover hover =
  match hover with
  | None -> print_endline "no hover response"
  | Some hover -> Lsp.Types.Hover.yojson_of_t hover |> Test.print_result
;;

let hover client position =
  Client.request
    client
    (TextDocumentHover
       { HoverParams.position
       ; textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri
       ; workDoneToken = None
       })
;;

let markdown_capabilities =
  let hover =
    HoverClientCapabilities.create
      ~dynamicRegistration:true
      ~contentFormat:[ MarkupKind.Markdown; MarkupKind.PlainText ]
      ()
  in
  let textDocument = TextDocumentClientCapabilities.create ~hover () in
  ClientCapabilities.create ~textDocument ()
;;

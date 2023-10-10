(**Opens a document with the language server. This must be done before trying to
   access it*)
val openDocument :
     client:'a Ocaml_lsp_e2e.Test.Import.Client.t
  -> uri:Lsp__Types.DocumentUri.t
  -> source:string
  -> unit Fiber.t

(**Performs the request you return from the makeRequest function and then gives
   it the the handler function you provide*)
val iter_LspResponse :
     ?prep:(unit Ocaml_lsp_e2e.Test.Import.Client.t -> unit Fiber.t)
  -> ?path:string
  -> makeRequest:
       (   Ocaml_lsp_e2e.Test.Import.TextDocumentIdentifier.t
        -> 'a Ocaml_lsp_e2e.Test.Import.Client.out_request)
  -> source:string
  -> ('a -> unit)
  -> unit

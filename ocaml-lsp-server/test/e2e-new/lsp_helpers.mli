open Test.Import

(** Send the given configuration to the language server *)
val change_config : client:'a Client.t -> DidChangeConfigurationParams.t -> unit Fiber.t

(** Opens a document with the language server. This must be done before trying
    to access it *)
val open_document
  :  client:'a Client.t
  -> uri:DocumentUri.t
  -> source:string
  -> unit Fiber.t

(** Performs the request you return from the makeRequest function and then gives
    it the the handler function you provide *)
val iter_lsp_response
  :  ?prep:(unit Client.t -> unit Fiber.t)
  -> ?path:string
  -> makeRequest:(TextDocumentIdentifier.t -> 'a Client.out_request)
  -> source:string
  -> ('a -> unit)
  -> unit

open Test.Import

(** Opens a document with the language server. This must be done before trying to access
    it *)
val open_document
  :  client:'a Client.t
  -> uri:DocumentUri.t
  -> source:string
  -> unit Fiber.t

(** Performs the request you return from the makeRequest function and then gives it the
    the handler function you provide *)
val iter_lsp_response
  :  ?prep:(unit Client.t -> unit Fiber.t)
  -> ?path:string
  -> makeRequest:(TextDocumentIdentifier.t -> 'a Client.out_request)
  -> source:string
  -> ('a -> unit)
  -> unit Async.Deferred.t

(* Opens the file in the specified path with source code as specified in src.
   This then waits for merlin to send diagnostic info and callls the diagnostics_callback
   function with the diagnostics it receives from merlin for the source. *)
val open_document_with_diagnostics_callback
  :  ?prep:(unit Client.t -> unit Fiber.t)
  -> ?path:string
  -> source:string
  -> diagnostics_callback:(PublishDiagnosticsParams.t -> unit)
  -> unit
  -> unit Async.Deferred.t

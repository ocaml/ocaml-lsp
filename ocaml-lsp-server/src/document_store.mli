type t

val make : unit -> t

val put : t -> Document.t -> unit

val get :
     t
  -> Lsp.Protocol.documentUri
  -> (Document.t, Lsp.Jsonrpc.Response.Error.t) result

val get_opt : t -> Lsp.Protocol.documentUri -> Document.t option

val remove_document : t -> Lsp.Protocol.documentUri -> unit

val get_size : t -> int

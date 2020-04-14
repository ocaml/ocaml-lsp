type t

val make : unit -> t

val put : t -> Document.t -> unit

val get : t -> Lsp.Uri.t -> (Document.t, Lsp.Jsonrpc.Response.Error.t) result

val get_opt : t -> Lsp.Uri.t -> Document.t option

val get_full_opt : t -> Lsp.Uri.t -> (Document.t * float) option

val remove_document : t -> Lsp.Uri.t -> unit

val get_size : t -> int

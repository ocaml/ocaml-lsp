open Lsp.Gprotocol

type t

val make : ?version:int -> uri:Lsp.Uri.t -> text:string -> unit -> t

val uri : t -> Lsp.Uri.t

val source : t -> Msource.t

val with_pipeline : t -> (Mpipeline.t -> 'a) -> 'a

val version : t -> int

val update_text : ?version:int -> TextDocumentContentChangeEvent.t -> t -> t

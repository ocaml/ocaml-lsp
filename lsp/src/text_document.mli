open Types

type t

val make : DidOpenTextDocumentParams.t -> t

val languageId : t -> string

val documentUri : t -> Uri.t

val version : t -> int

val text : t -> string

val apply_content_change :
  ?version:int -> TextDocumentContentChangeEvent.t -> t -> t

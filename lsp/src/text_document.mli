open Types

type t

val make : ?version:int -> Uri.t -> string -> t

val documentUri : t -> Uri.t

val version : t -> int

val text : t -> string

val apply_content_change :
  ?version:int -> TextDocumentContentChangeEvent.t -> t -> t

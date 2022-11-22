open Types

type t

val make :
  position_encoding:[ `UTF8 | `UTF16 ] -> DidOpenTextDocumentParams.t -> t

val languageId : t -> string

val documentUri : t -> Uri0.t

val version : t -> int

val text : t -> string

exception Invalid_utf8

val apply_content_changes :
  ?version:int -> t -> TextDocumentContentChangeEvent.t list -> t

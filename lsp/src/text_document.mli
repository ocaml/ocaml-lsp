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

val set_version : t -> version:int -> t

(** Apply a list of non overlapping text edits. The order of application matters
    when multiple inserts are done in the same position. All the offsets are
    interpreted relative to the original document. *)
val apply_text_document_edits : t -> TextEdit.t list -> t

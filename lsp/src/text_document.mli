open Types

type t

val make :
  position_encoding:[ `UTF8 | `UTF16 ] -> DidOpenTextDocumentParams.t -> t

val languageId : t -> string

val documentUri : t -> Uri0.t

val position_encoding : t -> [ `UTF16 | `UTF8 ]

val version : t -> int

val text : t -> string

type invalid_utf =
  | Malformed of string
  | Insufficient_input

exception Invalid_utf of invalid_utf

val apply_content_changes :
  ?version:int -> t -> TextDocumentContentChangeEvent.t list -> t

val set_version : t -> version:int -> t

(** Apply a list of non overlapping text edits. The order of application matters
    when multiple inserts are done in the same position. All the offsets are
    interpreted relative to the original document. *)
val apply_text_document_edits : t -> TextEdit.t list -> t

module Expert : sig
  (** These functions allow one to work with the underlying zipper. This gives
      the opportunity for better performance on chained edits. *)

  (** [goto t pos] move the zipper of [t] to [pos]. [pos] must be in utf8 *)
  val goto : t -> Position.t -> t

  (** [offset t] return the global offset in the string where the zipper is
      currently focused *)
  val offset : t -> int
end

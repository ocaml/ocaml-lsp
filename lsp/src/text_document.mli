open Types

type t

type encoding :=
  [ `UTF8
  | `UTF16
  ]

val make : position_encoding:encoding -> DidOpenTextDocumentParams.t -> t
val languageId : t -> string
val documentUri : t -> Uri0.t
val position_encoding : t -> encoding
val version : t -> int
val text : t -> string

type invalid_utf =
  | Malformed of string
  | Insufficient_input

exception Invalid_utf of invalid_utf

val apply_content_changes
  :  ?version:int
  -> t
  -> TextDocumentContentChangeEvent.t list
  -> t

val set_version : t -> version:int -> t

(** Apply a list of non overlapping text edits. The order of application matters
    when multiple inserts are done in the same position. All the offsets are
    interpreted relative to the original document. *)
val apply_text_document_edits : t -> TextEdit.t list -> t

(** [absolute_position t pos] returns the absolute position of [pos] inside
    [text t]. If the position is outside the bounds of the document, the offset
    returned will be the length of the document. [pos] is interpreted with
    [position_encoding t] *)
val absolute_position : t -> Position.t -> int

(* [absolute_range t range] same as [(absolute_position t range.start ,
   absolute_position t range.end_)] but possibly faster *)
val absolute_range : t -> Range.t -> int * int

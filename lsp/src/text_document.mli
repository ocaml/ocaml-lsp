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

(** [workspace_edit t edits] creates a versioned workspace edit that applies
    [edits] to [t]. *)
val workspace_edit : t -> TextEdit.t list -> WorkspaceEdit.t

(** [offset t position] returns the UTF-8 byte offset of [position] in [text t].
    If [position] is outside the document, the result is clamped to the end of
    the corresponding line or document. [position] is interpreted using
    [position_encoding t]. *)
val offset : t -> Position.t -> int

(** [offsets t range] is equivalent to
    [(offset t range.start, offset t range.end_)] but may be faster. *)
val offsets : t -> Range.t -> int * int

(** [position t ~offset] returns the position at the UTF-8 byte [offset] in
    [text t], using [position_encoding t]. Offsets outside the document are
    clamped to its bounds. *)
val position : t -> offset:int -> Position.t

(** [range t ~start_offset_inclusive ~end_offset_exclusive] converts a range of
    UTF-8 byte offsets to positions using [position_encoding t]. Offsets outside
    the document are clamped to its bounds. *)
val range : t -> start_offset_inclusive:int -> end_offset_exclusive:int -> Range.t

(** Compatibility alias for {!offset}. *)
val absolute_position : t -> Position.t -> int

(** Compatibility alias for {!offsets}. *)
val absolute_range : t -> Range.t -> int * int

(** [range_of_utf8_offsets t ~start_offset ~end_offset] converts the half-open
    UTF-8 byte offsets [[start_offset, end_offset)] in [text t] to a range in the
    document's position encoding. Raises [Invalid_argument] if the offsets are
    unordered, out of bounds, or not UTF-8 character boundaries. *)
val range_of_utf8_offsets : t -> start_offset:int -> end_offset:int -> Range.t

(** [substring t range] returns the text within [range], interpreted using the
    document's position encoding. Returns [None] when the range's start follows
    its end. *)
val substring : t -> Range.t -> string option

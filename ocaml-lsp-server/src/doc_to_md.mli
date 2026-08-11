type t =
  | Raw of string
  | Markdown of string

(** [translate ?resolve doc] renders the odoc markup [doc] as markdown.
    [resolve] says where a cross-reference points, as a URI; references it
    answers [None] for are rendered as their path alone, without a link. *)
val translate : ?resolve:(string -> string option) -> string -> t

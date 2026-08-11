open Import

(** An odoc cross-reference, as written between [{!] and [}] in a documentation
    comment, and the definition it names. *)

(** [split reference] separates the kind qualifying [reference] from its path.
    A reference may name the kind of item it points at, either as a leading
    [kind:] or as a [kind-] prefix on its last component. *)
val split : string -> string option * string

(** [path reference] is [reference] stripped of the kind qualifying it. *)
val path : string -> string

(** [resolve pipeline ~uri ~position reference] is the definition [reference]
    names, as a URI carrying the position in the file it points into.
    [position] is where the reference is written; [uri] is the document it is
    written in. [None] when merlin cannot place it. *)
val resolve : Mpipeline.t -> uri:Uri.t -> position:Position.t -> string -> Uri.t option

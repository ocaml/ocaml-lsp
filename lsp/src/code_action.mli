open Types

(** [kind_is_requested only kind] reports whether [kind] is selected by the
    optional hierarchical code-action kind filter [only]. *)
val kind_is_requested : CodeActionKind.t list option -> CodeActionKind.t -> bool

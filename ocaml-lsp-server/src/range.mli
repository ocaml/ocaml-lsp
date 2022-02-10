open Import

include module type of Lsp.Types.Range with type t = Lsp.Types.Range.t

(** [compare r1 r2] compares first start positions, if equal compares the end
    positions. *)
val compare : t -> t -> Ordering.t

val compare_size : t -> t -> Ordering.t

val first_line : t

val of_loc : Loc.t -> t

(** [resize_for_edit edit] returns shrunk, unchanged, or extended [edit.range]
    depending on the size of [edit.newText], e.g., if [edit.newText] contains
    less characters than [edit.range], the new range is shrunk to fit
    [edit.newText] only. *)
val resize_for_edit : TextEdit.t -> t

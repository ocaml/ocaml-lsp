open Import
include module type of Lsp.Range with type t = Lsp.Range.t

val to_dyn : t -> Dyn.t
val of_loc_opt : Loc.t -> t option

(** [of_loc loc] if fails to convert [loc] to [t] will return the first (or top)
    line in the document *)
val of_loc : Loc.t -> t

val contains_loc : Loc.t -> Position.t -> bool

(** [resize_for_edit edit] returns shrunk, unchanged, or extended [edit.range]
    depending on the size of [edit.newText], e.g., if [edit.newText] contains
    less characters than [edit.range], the new range is shrunk to fit
    [edit.newText] only. *)
val resize_for_edit : TextEdit.t -> t

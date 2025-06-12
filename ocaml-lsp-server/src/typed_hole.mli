open! Import
include module type of Merlin_analysis.Typed_hole

(** Creates the command used to jump to a hole within the new text. Used by construct and
    destruct-line. *)
val next_hole_cmd : state:State.t -> edit:Import.TextEdit.t -> Import.Command.t option

(** Finds the next typed hole location after [cursor] in [holes]. Called when handling the
    [jumpToHole] request. *)
val next_hole : holes:Range.t list -> cursor:Position.t -> Range.t option

(** Finds the previous typed hole location before [cursor] in [holes]. Called when
    handling the [jumpToHole] request. *)
val prev_hole : holes:Range.t list -> cursor:Position.t -> Range.t option

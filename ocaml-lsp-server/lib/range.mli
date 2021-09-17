open Import

include module type of Lsp.Types.Range with type t = Lsp.Types.Range.t

(** [compare r1 r2] compares first start positions, if equal compares the end
    positions. *)
val compare : t -> t -> Ordering.t

val compare_size : t -> t -> Ordering.t

val first_line : t

val of_loc : Loc.t -> t

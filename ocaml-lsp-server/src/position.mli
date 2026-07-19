open! Import
include module type of Lsp.Types.Position with type t = Lsp.Types.Position.t

(** [compare_inclusion position range] treats both range boundaries as
    inclusive. *)
val compare_inclusion : t -> Lsp.Types.Range.t -> [ `Inside | `Outside of t ]

val ( - ) : t -> t -> t
val compare : t -> t -> Ordering.t

(** [advance_text ~position_encoding position text] returns the position after
    reading the UTF-8 encoded [text] using the negotiated character units. *)
val advance_text : position_encoding:[ `UTF8 | `UTF16 ] -> t -> string -> t

val logical : t -> [> `Logical of int * int ]
val of_lexical_position : Lexing.position -> t option
val start : t
val to_dyn : t -> Dyn.t

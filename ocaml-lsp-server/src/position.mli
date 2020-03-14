open Import

include module type of Lsp.Types.Position with type t = Lsp.Types.Position.t

val compare_inclusion : t -> Range.t -> [ `Inside | `Outside of t ]

val ( - ) : t -> t -> t

val compare : t -> t -> Ordering.t

val logical : t -> [> `Logical of int * int ]

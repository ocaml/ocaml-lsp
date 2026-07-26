open! Import
include module type of Lsp.Types.Position with type t = Lsp.Types.Position.t

val logical : t -> [> `Logical of int * int ]
val of_lexical_position : Lexing.position -> t option
val to_dyn : t -> Dyn.t

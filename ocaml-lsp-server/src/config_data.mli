open Import

module ShortenMerlinDiagnostics : sig
  type t = { enable : bool }

  include Json.Jsonable.S with type t := t
end

module InlayHints : sig
  type t =
    { hint_pattern_variables : bool
    ; hint_let_bindings : bool
    ; hint_function_params : bool
    }

  include Json.Jsonable.S with type t := t
end

module Lens : sig
  type t =
    { enable : bool
    ; for_nested_bindings : bool
    }

  include Json.Jsonable.S with type t := t
end

module ExtendedHover : sig
  type t = { enable : bool }

  include Json.Jsonable.S with type t := t
end

module StandardHover : sig
  type t = { enable : bool }

  include Json.Jsonable.S with type t := t
end

module DuneDiagnostics : sig
  type t = { enable : bool }

  include Json.Jsonable.S with type t := t
end

module SyntaxDocumentation : sig
  type t = { enable : bool }

  include Json.Jsonable.S with type t := t
end

module MerlinJumpCodeActions : sig
  type t = { enable : bool }

  include Json.Jsonable.S with type t := t
end

type t =
  { codelens : Lens.t option
  ; extended_hover : ExtendedHover.t option
  ; standard_hover : StandardHover.t option
  ; inlay_hints : InlayHints.t option
  ; dune_diagnostics : DuneDiagnostics.t option
  ; syntax_documentation : SyntaxDocumentation.t option
  ; merlin_jump_code_actions : MerlinJumpCodeActions.t option
  ; shorten_merlin_diagnostics : ShortenMerlinDiagnostics.t option
  }

include Json.Jsonable.S with type t := t

val default : t

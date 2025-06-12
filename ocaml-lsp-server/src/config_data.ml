open Import

module InlayHints = struct
  type t =
    { hint_pattern_variables : bool [@key "hintPatternVariables"] [@default false]
    ; hint_let_bindings : bool [@key "hintLetBindings"] [@default false]
    ; hint_function_params : bool [@key "hintFunctionParams"] [@default false]
    ; hint_let_syntax_ppx : bool [@key "hintLetSyntaxPpx"] [@default false]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Lens = struct
  type t = { enable : bool [@default false] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module ExtendedHover = struct
  type t = { enable : bool [@default true] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module SyntaxDocumentation = struct
  type t = { enable : bool [@default false] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module MerlinDiagnostics = struct
  type t = { enable : bool [@default false] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module ShortenMerlinDiagnostics = struct
  type t = { enable : bool [@default false] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module PpxCssColors = struct
  type t = { enable : bool [@default true] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

type t =
  { codelens : Lens.t Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
  ; extended_hover : ExtendedHover.t Json.Nullable_option.t
       [@key "extendedHover"] [@default None] [@yojson_drop_default ( = )]
  ; inlay_hints : InlayHints.t Json.Nullable_option.t
       [@key "inlayHints"] [@default None] [@yojson_drop_default ( = )]
  ; syntax_documentation : SyntaxDocumentation.t Json.Nullable_option.t
       [@key "syntaxDocumentation"] [@default None] [@yojson_drop_default ( = )]
  ; merlin_diagnostics : MerlinDiagnostics.t Json.Nullable_option.t
       [@key "merlinDiagnostics"] [@default None] [@yojson_drop_default ( = )]
  ; shorten_merlin_diagnostics : MerlinDiagnostics.t Json.Nullable_option.t
       [@key "shortenMerlinDiagnostics"] [@default None] [@yojson_drop_default ( = )]
  ; ppx_css_colors : PpxCssColors.t Json.Nullable_option.t
       [@key "ppxCssColors"] [@default None] [@yojson_drop_default ( = )]
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]

let default =
  { codelens = Some { enable = false }
  ; extended_hover = Some { enable = true }
  ; inlay_hints =
      Some
        { hint_pattern_variables = false
        ; hint_let_bindings = false
        ; hint_function_params = false
        ; hint_let_syntax_ppx = false
        }
  ; syntax_documentation = Some { enable = false }
  ; merlin_diagnostics = Some { enable = false }
  ; shorten_merlin_diagnostics = Some { enable = false }
  ; ppx_css_colors = Some { enable = true }
  }
;;

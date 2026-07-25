open Test.Import

val annotate_src_with_tokens
  :  legend:SemanticTokensLegend.t
  -> encoded_tokens:int array
  -> annot_mods:bool
  -> string
  -> string

val single_line_non_overlapping_violations
  :  source:string
  -> encoded_tokens:int array
  -> string list

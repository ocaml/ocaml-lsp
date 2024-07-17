open Test.Import

val annotate_src_with_tokens
  :  legend:SemanticTokensLegend.t
  -> encoded_tokens:int array
  -> annot_mods:bool
  -> string
  -> string

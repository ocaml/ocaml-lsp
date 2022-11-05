open Test.Import

val iter_code_actions :
     ?path:string
  -> source:string
  -> Range.t
  -> (CodeActionResult.t -> unit)
  -> unit

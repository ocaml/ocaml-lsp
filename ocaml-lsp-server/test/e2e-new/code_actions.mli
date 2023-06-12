open Test.Import

val iter_code_actions :
     ?prep:(unit Test.Import.Client.t -> unit Fiber.t)
  -> ?path:string
  -> source:string
  -> Range.t
  -> (CodeActionResult.t -> unit)
  -> unit

val code_action_test : title:string -> source:string -> unit

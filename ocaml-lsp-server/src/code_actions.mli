open Import

val compute
  :  State.t Server.t
  -> CodeActionParams.t
  -> ([> `CodeAction of CodeAction.t ] list option Reply.t * State.t) Fiber.t

val resolve : State.t -> CodeAction.t -> CodeAction.t Fiber.t

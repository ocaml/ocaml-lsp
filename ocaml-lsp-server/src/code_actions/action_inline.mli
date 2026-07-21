open Import

val t : Code_action.t
val unresolved : Code_action.t
val resolve : State.t -> CodeAction.t -> CodeAction.t Fiber.t option

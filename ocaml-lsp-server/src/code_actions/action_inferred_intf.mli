open Import

val action_kind : string

val code_action :
  Document.t -> State.t -> CodeActionParams.t -> CodeAction.t option Fiber.t

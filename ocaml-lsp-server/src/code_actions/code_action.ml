open Import

type t =
  { action_kind : string
  ; run : Document.t -> CodeActionParams.t -> CodeAction.t option Fiber.t
  }

open Import

type t =
  { kind : CodeActionKind.t
  ; run : Document.t -> CodeActionParams.t -> CodeAction.t option Fiber.t
  }

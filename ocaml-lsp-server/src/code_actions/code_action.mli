open Import

type t =
  { kind : CodeActionKind.t
  ; run :
      [ `Batchable of
        Mpipeline.t -> Document.t -> CodeActionParams.t -> CodeAction.t option
      | `Non_batchable of
        Document.t -> CodeActionParams.t -> CodeAction.t option Fiber.t
      ]
  }

val batchable :
     CodeActionKind.t
  -> (Mpipeline.t -> Document.t -> CodeActionParams.t -> CodeAction.t option)
  -> t

val non_batchable :
     CodeActionKind.t
  -> (Document.t -> CodeActionParams.t -> CodeAction.t option Fiber.t)
  -> t

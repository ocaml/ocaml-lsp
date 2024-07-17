open Import

type t =
  { kind : CodeActionKind.t
  ; run :
      [ `Batchable of
        Mpipeline.t -> Document.t -> CodeActionParams.t -> CodeAction.t option
      | `Non_batchable of Document.t -> CodeActionParams.t -> CodeAction.t option Fiber.t
      ]
  (** A code action is either "batchable" or "non-batchable". Batchable
      actions do not use fibers internally, so they can be safely run
      inside a [with_pipeline] context. Non-batchable actions can use
      fibers. *)
  }

val batchable
  :  CodeActionKind.t
  -> (Mpipeline.t -> Document.t -> CodeActionParams.t -> CodeAction.t option)
  -> t

val non_batchable
  :  CodeActionKind.t
  -> (Document.t -> CodeActionParams.t -> CodeAction.t option Fiber.t)
  -> t

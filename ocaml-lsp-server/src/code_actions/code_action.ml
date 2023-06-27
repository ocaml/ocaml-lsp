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

let batchable kind run = { kind; run = `Batchable run }

let non_batchable kind run = { kind; run = `Non_batchable run }

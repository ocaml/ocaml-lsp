open Import

val prepare : State.t -> Lsp.Types.PrepareRenameParams.t -> Range.t option Fiber.t
val rename : State.t -> RenameParams.t -> WorkspaceEdit.t Fiber.t

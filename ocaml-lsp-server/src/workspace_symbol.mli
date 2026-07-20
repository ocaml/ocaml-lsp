open Import

val run : State.t -> WorkspaceSymbolParams.t -> SymbolInformation.t list option Fiber.t

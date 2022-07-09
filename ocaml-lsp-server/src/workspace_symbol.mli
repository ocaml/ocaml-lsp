open Import

type error = Build_dir_not_found of string

val run :
     WorkspaceSymbolParams.t
  -> WorkspaceFolder.t list
  -> Fiber.Cancel.t option
  -> ((SymbolInformation.t list, error) result list, [> `Cancelled ]) result

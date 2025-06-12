open Import

(** Produces a list of code actions based on Merlin-construct. Because the LSP doesn't
    have a clean way of offering a sub-menu after the user selects construct, the solution
    is to put all of the construct options directly into the code-action menu. *)
val get_construct_actions
  :  log_info:Lsp_timing_logger.t
  -> State.t
  -> Document.t
  -> CodeActionParams.t
  -> CodeAction.t list Fiber.t

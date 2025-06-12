open Import

(** This module contains functionality to handle `textDocument/hover` LSP request. *)

type extended_hover =
  { hover : Hover.t
  ; verbosity : int
  ; can_increase_verbosity : bool
  }

(** [handle server hover_params] provides a response for LSP request `textDocument/hover` *)
val handle
  :  log_info:Lsp_timing_logger.t
  -> State.t Server.t
  -> HoverParams.t
  -> Hover.t option Fiber.t

val handle_extended
  :  log_info:Lsp_timing_logger.t
  -> State.t Server.t
  -> HoverParams.t
  -> verbosity:int option
  -> extended_hover option Fiber.t

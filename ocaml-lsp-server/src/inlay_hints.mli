open Import

val compute
  :  log_info:Lsp_timing_logger.t
  -> State.t
  -> InlayHintParams.t
  -> InlayHint.t list option Fiber.t

(** Returns [true] if [position] occurs inside a comment in the document *)
val position_in_comment
  :  log_info:Lsp_timing_logger.t
  -> position:Position.t
  -> merlin:Document.Merlin.t
  -> bool Fiber.t

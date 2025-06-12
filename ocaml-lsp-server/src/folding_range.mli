(** This module is responsible for computing the folding range for OCaml source *)

open Import

(** Compute the folding range *)
val compute
  :  log_info:Lsp_timing_logger.t
  -> State.t
  -> FoldingRangeParams.t
  -> FoldingRange.t list option Fiber.t

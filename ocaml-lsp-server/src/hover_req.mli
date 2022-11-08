open Import

(** This module contains functionality to handle `textDocument/hover` LSP
    request. *)

type mode =
  | Default
  | Extended_fixed of int
  | Extended_variable

(** [handle server hover_params] provides a response for LSP request
    `textDocument/hover` *)
val handle : State.t Server.t -> HoverParams.t -> mode -> Hover.t option Fiber.t

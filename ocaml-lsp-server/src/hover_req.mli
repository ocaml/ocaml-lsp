open Import

(** This module contains functionality to handle `textDocument/hover` LSP
    request. *)

(** [handle server hover_params] provides a response for LSP request
    `textDocument/hover` *)
val handle : State.t Server.t -> HoverParams.t -> Hover.t option Fiber.t

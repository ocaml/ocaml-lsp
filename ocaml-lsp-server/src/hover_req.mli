open Import

(** This module contains functionality to handle `textDocument/hover` LSP
    request. *)

type mode =
  | Default
  | Extended_fixed of int
  | Extended_variable

type warning_action =
  | Enable of int
  | Disable of int
  | Enable_as_error of int
  | Enable_range of int * int
  | Disable_range of int * int
  | Enable_as_error_range of int * int
  | Enable_letter of char
  | Disable_letter of char
  | Enable_as_error_letter of char

val parse_warning_payload : string -> warning_action list

(** [handle server hover_params] provides a response for LSP request
    `textDocument/hover` *)
val handle : State.t Server.t -> HoverParams.t -> mode -> Hover.t option Fiber.t

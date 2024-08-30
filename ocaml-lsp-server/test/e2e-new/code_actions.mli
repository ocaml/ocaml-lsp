open Test.Import

val iter_code_actions
  :  ?prep:(unit Test.Import.Client.t -> unit Fiber.t)
  -> ?path:string
  -> ?diagnostics:Diagnostic.t list
  -> source:string
  -> Range.t
  -> (CodeActionResult.t -> unit)
  -> unit

val parse_selection : string -> string * Range.t

val apply_code_action
  :  ?diagnostics:Diagnostic.t list
  -> string
  -> string
  -> Range.t
  -> string option

(** [code_action_test title source] runs the code action with title [title] and
    prints the resulting source. *)
val code_action_test : title:string -> string -> unit

open Test.Import

val iter_code_actions :
     ?prep:(unit Test.Import.Client.t -> unit Fiber.t)
  -> ?path:string
  -> source:string
  -> Range.t
  -> (CodeActionResult.t -> unit)
  -> unit

(** [code_action_test ~diagnostics title source] runs the code action with title
    [title] and prints the resulting source. *)
val code_action_test :
  ?diagnostics:Diagnostic.t list -> title:string -> string -> unit

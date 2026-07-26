open Import

val kind : CodeActionKind.t

(** Run Merlin's case analysis and turn its reply into a code action.
    [postprocess] may adjust the replacement location and text. *)
val run
  :  State.t
  -> Document.t
  -> Document.Merlin.t
  -> action_kind:string
  -> range:Range.t
  -> postprocess:(Loc.t * string -> Loc.t * string)
  -> CodeAction.t option Fiber.t

val t : State.t -> Code_action.t

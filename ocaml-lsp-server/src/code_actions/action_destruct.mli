open Import

type dispatch = Range.t -> (Loc.t * string, Exn_with_backtrace.t) result Fiber.t

val kind : CodeActionKind.t
val cached_dispatch : Document.Merlin.t -> dispatch

(** Run Merlin's case analysis and turn its reply into a code action.
    [postprocess] may adjust the replacement location and text. *)
val run
  :  State.t
  -> Document.t
  -> dispatch:dispatch
  -> action_kind:string
  -> range:Range.t
  -> postprocess:(Loc.t * string -> Loc.t * string)
  -> CodeAction.t option Fiber.t

val t : dispatch:dispatch -> State.t -> Code_action.t

open Import

type dispatch = Range.t -> (Loc.t * string, Exn_with_backtrace.t) result Fiber.t

val kind : CodeActionKind.t
val cached_dispatch : Document.Merlin.t -> dispatch
val t : ?dispatch:dispatch -> State.t -> Code_action.t

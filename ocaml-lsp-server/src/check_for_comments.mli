(** Returns [true] if [position] occurs inside a comment in the document *)
val position_in_comment : position:Position.t -> merlin:Document.Merlin.t -> bool Fiber.t

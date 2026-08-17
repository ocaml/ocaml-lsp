open Import

(** Links found in the odoc markup of a document's documentation comments. *)
val run : State.t -> Uri.t -> DocumentLink.t list option Fiber.t

(** [resolve state link] points [link] at the definition of the cross-reference
    it was built from, or returns it unchanged when the reference cannot be
    placed. *)
val resolve : State.t -> DocumentLink.t -> DocumentLink.t Fiber.t

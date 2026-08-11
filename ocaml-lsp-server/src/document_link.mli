open Import

(** Links found in the odoc markup of a document's documentation comments. *)
val run : State.t -> Uri.t -> DocumentLink.t list option Fiber.t

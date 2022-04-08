open Import

type t

val make : unit -> t

val put : t -> Document.t -> unit

val get : t -> Uri.t -> Document.t

val get_opt : t -> Uri.t -> Document.t option

val close_document : t -> Uri.t -> unit Fiber.t

val close_all : t -> unit Fiber.t

open Import

type t

val make : _ Server.t Fdecl.t -> Fiber.Pool.t -> t

val open_document : t -> Document.t -> unit Fiber.t

val change_document : t -> Uri.t -> f:(Document.t -> Document.t) -> Document.t

val get : t -> Uri.t -> Document.t

val get_opt : t -> Uri.t -> Document.t option

val unregister_promotions : t -> Uri.t list -> unit Fiber.t

val register_promotions : t -> Uri.t list -> unit Fiber.t

val close_document : t -> Uri.t -> unit Fiber.t

val close_all : t -> unit Fiber.t

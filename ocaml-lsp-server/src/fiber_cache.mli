open Import

type ('key, 'value) t

(** [create key ~f] memoizes [f] by [key]. Concurrent lookups of the same key
    share one execution, including any error raised by [f]. *)
val create : 'key Hashtbl.Key.t -> f:('key -> 'value Fiber.t) -> ('key, 'value) t

val get : ('key, 'value) t -> 'key -> 'value Fiber.t

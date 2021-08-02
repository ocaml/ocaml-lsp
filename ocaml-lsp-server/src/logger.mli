open Import

(** A logger module that allows to

    - avoid passing a logging callback or server instance to functions that
      don't actually need access to server for anything but logging
    - set an application-wide logging level

    Each log level has an small integer value given below. Setting [log_level]
    on initialization tells the logger to ignore logs that have higher logging
    value. For users, it makes sense to put [Info] or something even lower.

    Error - 0, Warning - 1, Info - 2, Log - 3

    {b MUST} be initialized after server is created.

    TODO: extend logging to show messages, ie also support 'window/showMessage'
    notif-s *)

val initialize :
     log_level:MessageType.t
  -> show_level:MessageType.t
  -> send_notification:(Server_notification.t -> unit Fiber.t)
  -> unit

val log_e : (unit -> string) -> unit Fiber.t

val log_w : (unit -> string) -> unit Fiber.t

val log_i : (unit -> string) -> unit Fiber.t

val log_l : (unit -> string) -> unit Fiber.t

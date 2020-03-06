(** Manage printing user message and keeping progress information in the status
    line *)

module Display : sig
  type t =
    | Progress
    | Short
    | Verbose
    | Quiet

  val all : (string * t) list
end

val print : string -> unit

val print_user_message :
  ?config:User_message.Print_config.t -> User_message.t -> unit

val init : Display.t -> unit

val reset_terminal : unit -> unit

(** / *)

(** Everything below this line requires [init] to have been called earlier. *)

module Status_line : sig
  (** Function that produces the current status line *)
  type t = unit -> User_message.Style.t Pp.t option

  (** Change the status line if the display is in progress mode. *)
  val set : t -> unit

  val set_temporarily : t -> (unit -> 'a) -> 'a

  val refresh : unit -> unit
end

val display : unit -> Display.t

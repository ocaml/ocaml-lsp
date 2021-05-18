(** Output trace data to a file in Chrome's trace_event format. This format is
    compatible with chrome trace viewer [chrome://tracing].

    Trace viewer is a part of the catapult project
    (https://github.com/catapult-project/catapult/blob/master/tracing/README.md).

    The trace format is documented at:
    https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview *)

module Json : sig
  type t =
    [ `Assoc of (string * t) list
    | `Bool of bool
    | `Float of float
    | `Int of int
    | `Intlit of string
    | `List of t list
    | `Null
    | `String of string
    | `Tuple of t list
    | `Variant of string * t option
    ]

  val to_string : t -> string
end

module Event : sig
  type t

  module Timestamp : sig
    type t

    val of_float_seconds : float -> t

    val now : unit -> t
  end

  module Id : sig
    type t =
      [ `Int of int
      | `String of string
      ]
  end

  type common_fields

  val common_fields :
       ?tts:Timestamp.t
    -> ?cname:string
    -> ?cat:string list
    -> ?pid:int
    -> ?tid:int
    -> ts:Timestamp.t
    -> name:string
    -> unit
    -> common_fields

  val set_ts : common_fields -> Timestamp.t -> common_fields

  type args = (string * Json.t) list

  (** Create a counter event *)
  val counter : ?id:Id.t -> common_fields -> args -> t

  type async =
    | Start
    | Instant
    | End

  val async : ?scope:string -> ?args:args -> Id.t -> async -> common_fields -> t

  val complete :
    ?tdur:Timestamp.t -> ?args:args -> dur:Timestamp.t -> common_fields -> t

  val to_json : t -> Json.t
end

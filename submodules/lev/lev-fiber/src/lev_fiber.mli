open Stdune

module Timer : sig
  val sleepf : float -> unit Fiber.t
  (** [sleep f] wait for [f] seconds  *)

  module Wheel : sig
    type t
    (** wheel to handle many constant timeouts efficiently *)

    val create : delay:float -> t Fiber.t
    (** [create ~delay] will create a wheel that times out every task in [delay] *)

    val set_delay : t -> delay:float -> unit Fiber.t
    val delay : t -> float

    type task
    (** a task scheduled by the timer wheel *)

    val reset : task -> unit Fiber.t

    val task : t -> task Fiber.t
    (** create a new task *)

    val await : task -> [ `Ok | `Cancelled ] Fiber.t
    (** wait for delay seconds *)

    val cancel : task -> unit Fiber.t
    (** cancel waiting *)

    val run : t -> unit Fiber.t
    (** run the wheel forever *)

    val stop : t -> unit Fiber.t
    (** [stop t] stop running the wheel and cancel everything.
        it's an error to call [task t] after this. *)
  end
end

val waitpid : pid:int -> Unix.process_status Fiber.t
val signal : signal:int -> unit Fiber.t

module Thread : sig
  type t

  val create : unit -> t Fiber.t

  type 'a task

  val task : t -> f:(unit -> 'a) -> 'a task Fiber.t
  val cancel : 'a task -> unit Fiber.t

  val await :
    'a task ->
    ('a, [ `Exn of Exn_with_backtrace.t | `Cancelled ]) result Fiber.t

  val close : t -> unit
end

module Fd : sig
  type t

  val close : t -> unit
  val create : Unix.file_descr -> [ `Blocking | `Non_blocking of bool ] -> t
  val fd_exn : t -> Unix.file_descr
end

module Io : sig
  type input = Input
  type output = Output
  type 'a mode = Input : input mode | Output : output mode
  type 'a t

  val fd : _ t -> Fd.t
  val create : Fd.t -> 'a mode -> 'a t Fiber.t
  val create_rw : Fd.t -> (input t * output t) Fiber.t

  module Slice : sig
    type t = { pos : int; len : int }
  end

  module Reader : sig
    type t

    exception Unavailable

    val read_char_exn : t -> char
    val read_line : t -> (string, [ `Partial_eof of string ]) result Fiber.t

    val read_exactly :
      t -> int -> (string, [ `Partial_eof of string ]) result Fiber.t

    val to_string : t -> string Fiber.t
    (** [to_string t] read the entire stream into a string. Not recommended for serious use as
        this is inefficient *)

    module Expert : sig
      val available : t -> [ `Ok of int | `Eof ]
      val buffer : t -> Bytes.t * Slice.t
      val consume : t -> len:int -> unit
      val refill : ?size:int -> t -> unit Fiber.t
    end
  end

  module Writer : sig
    type t

    val flush : t -> unit Fiber.t
    val add_substring : t -> string -> pos:int -> len:int -> unit
    val add_string : t -> string -> unit

    module Expert : sig
      (* max size we can allocate for a transaction without resizing *)
      val available : t -> int
      val prepare : t -> len:int -> Bytes.t * Slice.t
      val commit : t -> len:int -> unit
    end
  end

  val with_read : input t -> f:(Reader.t -> 'a Fiber.t) -> 'a Fiber.t
  val with_write : output t -> f:(Writer.t -> 'a Fiber.t) -> 'a Fiber.t
  val close : 'a t -> unit
  val pipe : ?cloexec:bool -> unit -> (input t * output t) Fiber.t

  val stdin : input t Fiber.t
  (** [Unix.stdin] wrapped with [Io.t] *)

  val stderr : output t Fiber.t
  (** [Unix.stderr] wrapped with [Io.t] *)

  val stdout : output t Fiber.t
  (** [Unix.stdout] wrapped with [Io.t] *)
end

module Socket : sig
  module Server : sig
    type t

    val create : Fd.t -> Unix.sockaddr -> backlog:int -> t Fiber.t
    val close : t -> unit Fiber.t

    module Session : sig
      type t

      val fd : t -> Fd.t
      val sockaddr : t -> Unix.sockaddr
      val io : t -> (Io.input Io.t * Io.output Io.t) Fiber.t
    end

    val serve : t -> f:(Session.t -> unit Fiber.t) -> unit Fiber.t
  end

  val connect : Fd.t -> Unix.sockaddr -> unit Fiber.t
end

val yield : unit -> unit Fiber.t
(** [yield ()] wait for one iteration of the event loop *)

val run : ?flags:Lev.Loop.Flag.Set.t -> (unit -> 'a Fiber.t) -> 'a
(** If you set [flags] manually, you must include the [Nosigprocmask] flag *)

(** This library provides drop-in Async replacement for [Lev_fiber] and [Lev_fiber_csexp]
    from ocaml-lsp. We build ocaml-lsp with [-open Lev_fiber_async] to make it magically
    use Async as its scheduler instead of Lev. *)

(* Do not open anything here, as it can change the meaning of the signatures. *)

module Lev_fiber = struct
  module type S = sig
    module Timer : sig
      (** Return after [sec] seconds. *)
      val sleepf : float -> unit Fiber.t

      module Wheel : sig
        (** An interface for scheduling, rescheduling, and cancelling events. *)
        type t

        (** Create a [t] that can be used to schedule cancellable / reschedulable events
            that will happen after [delay] seconds. *)
        val create : delay:float -> t Fiber.t

        (** Change the [delay] value used for scheduling events. *)
        val set_delay : t -> delay:float -> unit Fiber.t

        (** Get the [delay] value. *)
        val delay : t -> float

        (* Event is a better name, but we keep [task] because it's what Lev uses. *)
        type task
        type event := task

        (** Create a new event that will happen in [delay] seconds. Better names for this
            function would be [schedule] or [create_event]. *)
        val task : t -> event Fiber.t

        (** Wait for event to happen or be cancelled. *)
        val await : event -> [ `Ok | `Cancelled ] Fiber.t

        (** Reschedule [event]. If it already happened, schedule a new one. *)
        val reset : event -> unit Fiber.t

        (** Cancel the event. *)
        val cancel : event -> unit Fiber.t

        (** In the Async implementation this is a no-op since [t] is implemented with a
            time source that is always available. *)
        val run : t -> unit Fiber.t

        (** Cancel all events. In the Lev implementation, this also stops running the
            wheel and makes scheduling new events with [task t] an error. In the Async
            implementation, scheduling more events is fine since the wheel is a
            [Time_source.t]. *)
        val stop : t -> unit Fiber.t
      end
    end

    val waitpid : pid:int -> Unix.process_status Fiber.t

    (** Register a signal handler for [signal]. When [signal] is received, unregister the
        handler and return. *)
    val signal : signal:int -> unit Fiber.t

    module Thread : sig
      type t

      (** Create a new system thread and a queue of jobs it will run. *)
      val create : unit -> t Fiber.t

      type 'a task

      (** Enqueue a job to the thread. *)
      val task : t -> f:(unit -> 'a) -> ('a task, [ `Stopped ]) result

      (** Cancel a job if it has not run yet. *)
      val cancel : 'a task -> unit Fiber.t

      (** Wait for a job to finish. *)
      val await
        :  'a task
        -> ('a, [ `Exn of Stdune.Exn_with_backtrace.t | `Cancelled ]) result Fiber.t

      (** Close the queue for the thread and return the thread to Async's thread pool.
          After [close] is called, calls to [task] will return [Error `Stopped]. *)
      val close : t -> unit
    end

    module Fd : sig
      type t

      val close : t -> unit

      (** Create a new [Fd.t]. The boolean argument of [`Non_blocking] indicates whether
          [Unix.set_nonblock] has already been invoked on this file descriptor - if it
          hasn't, it will be set during [create]. *)
      val create : Unix.file_descr -> [ `Blocking | `Non_blocking of bool ] -> t
    end

    module Io : sig
      type input = Input
      type output = Output

      type 'a mode =
        | Input : input mode
        | Output : output mode

      type 'a t

      val fd : _ t -> Fd.t

      (** Create a reader or writer for reading from / writing to [Fd.t] *)
      val create : Fd.t -> 'a mode -> 'a t Fiber.t

      (** Create a reader to read from [fd] and writer to write to [fd]. *)
      val create_rw : Fd.t -> (input t * output t) Fiber.t

      module Reader : sig
        type t

        val read_char_exn : t -> char
        val read_line : t -> (string, [ `Partial_eof of string ]) result Fiber.t
        val read_exactly : t -> int -> (string, [ `Partial_eof of string ]) result Fiber.t

        (** Read until EOF. *)
        val to_string : t -> string Fiber.t
      end

      module Writer : sig
        type t

        val flush : t -> unit Fiber.t
        val add_substring : t -> string -> pos:int -> len:int -> unit
        val add_string : t -> string -> unit
      end

      val with_read : input t -> f:(Reader.t -> 'a Fiber.t) -> 'a Fiber.t
      val with_write : output t -> f:(Writer.t -> 'a Fiber.t) -> 'a Fiber.t

      (* CR-someday ddickstein: When https://github.com/ocaml/ocaml-lsp/issues/1158 is
         resolved, update the comment below and update the logic in the read functions for
         handling closed readers. *)

      (** Close the reader / writer. Reading from a closed reader will result in EOF. *)
      val close : 'a t -> unit

      (** Create a unix pipe. Only pass [cloexec = false] if you are creating this pipe
          for the purpose of passing file descriptors to a child process. If this is not
          your intention, you risk leaking file descriptors, which is a security risk. *)
      val pipe : ?cloexec:bool -> unit -> (input t * output t) Fiber.t

      (** [Unix.stdin] wrapped with [Io.t] *)
      val stdin : input t Fiber.t

      (** [Unix.stderr] wrapped with [Io.t] *)
      val stderr : output t Fiber.t

      (** [Unix.stdout] wrapped with [Io.t] *)
      val stdout : output t Fiber.t
    end

    module Socket : sig
      (** Create a socket from [fd] and connect it to the given address. *)
      val connect : Fd.t -> Unix.sockaddr -> unit Fiber.t
    end

    (** Yield to the event loop. In the Lev implementation, this just yields once to the
        event loop (equivalent of binding on [return]). In the Async implementation, we
        use [Scheduler.yield ()], which waits until the next cycle, improving fairness. *)
    val yield : unit -> unit Fiber.t
  end
end

module Lev_fiber_csexp = struct
  module type Lev_fiber_types = sig
    module Io : sig
      type 'a t
      type input
      type output
    end

    module Fd : sig
      type t
    end
  end

  module Make (Lev_fiber : Lev_fiber_types) = struct
    module type S = sig
      (** Canonical S-expression RPC.

          This module implements a RPC mechanism for exchanging canonical S-expressions
          over unix or internet sockets. It allows a server to accept connections and a
          client to connect to a server.

          However, it doesn't explain how to encode queries, responses or generally any
          kind of messages as Canonical S-expressions. That part should be built on top of
          this module. *)

      module Session : sig
        (** Rpc session backed by an input & output stream. *)
        type t

        val create
          :  socket:bool
          -> Lev_fiber.Io.input Lev_fiber.Io.t
          -> Lev_fiber.Io.output Lev_fiber.Io.t
          -> t

        val close : t -> unit

        (* [write t sexps] writes the S-expressions [sexps]. *)
        val write : t -> Csexp.t list -> unit Fiber.t

        (** If [read] returns [None], the session is closed and all subsequent reads will
            return [None]. *)
        val read : t -> Csexp.t option Fiber.t
      end

      val connect : Lev_fiber.Fd.t -> Unix.sockaddr -> Session.t Fiber.t
    end
  end
end

module type Lev_fiber_async = sig
  module Lev_fiber : sig
    include Lev_fiber.S

    module type S =
      Lev_fiber.S
      with type Fd.t = Fd.t
       and type Io.input = Io.input
       and type Io.output = Io.output
       and type 'a Io.t = 'a Io.t

    val make : time_source:Async.Time_source.t -> (module S)
  end

  module Lev_fiber_csexp : Lev_fiber_csexp.Make(Lev_fiber).S
end

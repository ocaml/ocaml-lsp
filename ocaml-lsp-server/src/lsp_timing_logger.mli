(** Provides logging of timing information for LSP requests. *)

(** Carries the data needed for the various logging functions. Outside this module, an
    [Lsp_timing_logger.t] instance is always called [log_info].

    Many of the fields could be represented by an option instead of a list most of the
    time, but we allow for the possibility that a request involves multiple positions,
    or files. In the case of 0- or 1-entry lists, the resulting sexp is the same as if we
    used an option. *)
type t =
  { event_index : int
  (** Global index tracked in the lsp-server state; lets us associate multiple log entries
      to the same lsp request. *)
  ; action : string (** Name of the request/notification/etc. the lsp is handling. *)
  ; file : string list (** Absolute path(s). *)
  ; lines : int list (** Number of lines in the file(s). *)
  ; hash : int list (** A [String.hash] of the contents of the file(s). *)
  ; position : (int * int) list
  (** (line,col) for the position(s) in the request. There need not be any correspondence
      between the lists of positions and files. *)
  }
[@@deriving sexp]

(** Pack most of up the data that will be logged to CLM.
    @param in_jane The LSP server's best guess as to whether we're in a jane/... repo.
    @param uris Identifiers for the documents being acted on.
    @param texts
      Contents of the documents being acted on; used to log the number of lines and
      characters in each file.
    @param analyze_files
      Set to [false] to skip computing file lengths and hashes

      Other parameters are explained on the [t] type. *)
val make
  :  event_index:int
  -> action:string
  -> uris:Lsp.Uri.t list
  -> texts:string list
  -> positions:Position.t list
  -> ?analyze_files:bool
  -> unit
  -> t

(** Times the execution of function f and logs the result as an "lsp timing" entry.
    @param f a synchronous function to be timed *)
val with_logging : f:(unit -> 'a) -> t -> 'a

(** Times the execution of Fiber f and logs the result as an "lsp timing" entry.
    @param f an asynchronous fiber to be timed *)
val with_fiber_logging : f:(unit -> 'a Fiber.t) -> t -> 'a Fiber.t

(** Logs the timings produced by a Merlin pipeline as a "merlin timing" entry.
    @param wall_time time in miliseconds recorded by Single_pipeline.use
    @param timing_breakdown list of sub-timings in ms from Mpipeline.timing_information *)
val log_merlin_timing
  :  wall_time:float
  -> timing_breakdown:(string * float) list
  -> t
  -> unit

(** Logs the receipt of a notification or request as an "lsp event" entry. *)
val log_event_start : t -> unit

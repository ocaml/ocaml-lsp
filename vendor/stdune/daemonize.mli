(** Start, stop and synchronize with a daemon *)

(** The daemonize function will fork a daemon running the given function,
    guaranteeing that at most one instance will run at any given time. The
    daemon has to call a given callback to indicate that it has successfully
    started, unlocking all other potential start attempts. This callback can be
    given a payload that can be retrieved by the starting process and other
    start attempts - e.g. the endpoint to contact the damon on. *)

(** Result of a daemonization *)
type status =
  | Started of string * int
      (** The daemon was started in the background with the given payload and
          pid *)
  | Already_running of string * int
      (** The daemon is already running in the background with the given payload
          and pid *)
  | Finished  (** The daemon was run synchronously and exited. *)

val daemonize :
     ?workdir:Path.t (** The path to chdir to *)
  -> ?foreground:
       bool (** Whether to fork a daemon or run synchronously (defaults true) *)
  -> Path.t (** The path of the beacon file *)
  -> ((string -> unit) -> unit) (** The daemon main routine *)
  -> (status, string) Result.t

val stop : Path.t -> (unit, string) Result.t

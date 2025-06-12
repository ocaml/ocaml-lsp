open Core
open Async
open Fiber_async
open Lev_fiber_async_intf

let default_backtrace =
  (* If you get a backtrace to this point, interpret it as a missing backtrace. Base's
     exception-handling logic deliberately models backtraces as optional because
     [Printexc.get_raw_backtrace] may return a backtrace for a different exn, so there's a
     [phys_equal] test to guard returning the backtrace for the wrong exception. However,
     to conform to the [Thread] API, we always need to return a backtrace with an
     exception so we return this default one in the would-be [None] case. *)
  Backtrace.get ~at_most_num_frames:2 ()
;;

module Types = struct
  module Fd = struct
    type t = Fd.t Deferred.t
  end

  module Io = struct
    type input = Input
    type output = Output

    type 'a mode =
      | Input : input mode
      | Output : output mode

    type 'a t =
      | Reader : Reader.t -> input t
      | Writer : Writer.t -> output t
  end
end

module Lev_fiber = struct
  module type S =
    Lev_fiber.S
    with type Fd.t = Types.Fd.t
     and type Io.input = Types.Io.input
     and type Io.output = Types.Io.output
     and type 'a Io.t = 'a Types.Io.t

  let make ~time_source =
    (module struct
      module Timer = struct
        let sleepf span =
          Fiber.of_thunk (fun () ->
            Time_source.after time_source (Time_ns.Span.of_sec span) |> fiber_of_deferred)
        ;;

        module Wheel = struct
          type t =
            { mutable timeout : Time_ns.Span.t
            ; tasks : (unit, unit) Time_source.Event.t Bag.t
            }

          let create ~delay =
            Fiber.of_thunk (fun () ->
              Fiber.return { timeout = Time_ns.Span.of_sec delay; tasks = Bag.create () })
          ;;

          let delay t = Time_ns.Span.to_sec t.timeout

          let set_delay t ~delay =
            Fiber.of_thunk (fun () ->
              Fiber.return (t.timeout <- Time_ns.Span.of_sec delay))
          ;;

          type task =
            { wheel : t
            ; mutable event : (unit, unit) Time_source.Event.t
            }

          let new_event t =
            Fiber.of_thunk (fun () ->
              let event = Time_source.Event.run_after time_source t.timeout Fn.id () in
              let key = Bag.add t.tasks event in
              upon (Time_source.Event.fired event) (fun _ -> Bag.remove t.tasks key);
              Fiber.return event)
          ;;

          let task t =
            let%map.Fiber event = new_event t in
            { wheel = t; event }
          ;;

          let await task =
            Fiber.of_thunk (fun () ->
              match%map.Fiber Time_source.Event.fired task.event |> fiber_of_deferred with
              | Happened () -> `Ok
              | Aborted () -> `Cancelled)
          ;;

          let reset task =
            Fiber.of_thunk (fun () ->
              let timeout = task.wheel.timeout in
              match Time_source.Event.reschedule_after task.event timeout with
              | Ok -> Fiber.return ()
              | Previously_aborted _ | Previously_happened _ ->
                let%map.Fiber event = new_event task.wheel in
                task.event <- event)
          ;;

          let cancel task =
            Fiber.of_thunk (fun () ->
              Time_source.Event.abort_if_possible task.event ();
              Fiber.return ())
          ;;

          let run (_ : t) = Fiber.return ()

          let stop t =
            Fiber.of_thunk (fun () ->
              Bag.iter_elt t.tasks ~f:(fun elt ->
                let event = Bag.Elt.value elt in
                Time_source.Event.abort_if_possible event ());
              Fiber.return ())
          ;;
        end
      end

      let waitpid ~pid =
        Fiber.of_thunk (fun () ->
          match%map.Fiber Unix.waitpid (Pid.of_int pid) |> fiber_of_deferred with
          | Ok () -> UnixLabels.WEXITED 0
          | Error (`Exit_non_zero i) -> WEXITED i
          | Error (`Signal signal) -> WSIGNALED (Signal.to_caml_int signal))
      ;;

      let signal ~signal =
        Fiber.of_thunk (fun () ->
          let received = Ivar.create () in
          Signal.handle
            ~stop:(Ivar.read received)
            [ Signal.of_caml_int signal ]
            ~f:(fun (_ : Signal.t) ->
              (* ddickstein: The implementation of [Signal.handle] doesn't convince me
                 that this can't be called again after [stop] is filled before the [upon]
                 callback removes the registered handler, so I'm using
                 [Ivar.fill_if_empty] to be safe. *)
              Ivar.fill_if_empty received ());
          Ivar.read received |> fiber_of_deferred)
      ;;

      module Thread = struct
        module Exn_with_backtrace = Stdune.Exn_with_backtrace

        type 'a task =
          { f : unit -> 'a
          ; ivar : ('a, [ `Exn of Exn_with_backtrace.t | `Cancelled ]) Result.t Ivar.t
          }

        type packed_task = Task : 'a task -> packed_task

        type t =
          { writer : packed_task Pipe.Writer.t
          ; stop : unit -> unit
          }

        let create () =
          Fiber.of_thunk (fun () ->
            let reader, writer = Pipe.create () in
            let stopped = Ivar.create () in
            don't_wait_for
              (let%bind thread = In_thread.Helper_thread.create () in
               let%bind () =
                 Deferred.repeat_until_finished () (fun () ->
                   match%bind
                     choose
                       [ choice (Ivar.read stopped) (fun () -> `Stopped)
                       ; choice (Pipe.values_available reader) (function
                           | `Eof -> `Eof
                           | `Ok -> `Ok)
                       ]
                   with
                   | `Stopped | `Eof -> return (`Finished ())
                   | `Ok ->
                     (match Ivar.is_full stopped with
                      | true -> return (`Finished ())
                      | false ->
                        let (Task { f; ivar }) = Pipe.read_now_exn reader in
                        (match Ivar.is_full ivar with
                         | true -> return (`Repeat ())
                         | false ->
                           let%map result =
                             Monitor.try_with (fun () -> In_thread.run ~thread f)
                             >>| function
                             | Ok _ as ok -> ok
                             | Error (Monitor.Monitor_exn exn) ->
                               let backtrace =
                                 Monitor.Monitor_exn.backtrace exn
                                 |> Option.value ~default:default_backtrace
                               in
                               let exn = Monitor.Monitor_exn.extract_exn exn in
                               Error (`Exn { Exn_with_backtrace.exn; backtrace })
                             | Error exn ->
                               (* ddickstein: I don't think we will hit this case, but
                                  I've implemented it in case we do. *)
                               let backtrace =
                                 Backtrace.Exn.most_recent_for_exn exn
                                 |> Option.value ~default:default_backtrace
                               in
                               Error (`Exn { exn; backtrace })
                           in
                           Ivar.fill_if_empty ivar result;
                           `Repeat ())))
               in
               Pipe.iter_without_pushback reader ~f:(fun (Task { f = _; ivar }) ->
                 Ivar.fill_if_empty ivar (Error `Cancelled)));
            Fiber.return { writer; stop = Ivar.fill_if_empty stopped })
        ;;

        let task t ~f =
          match Pipe.is_closed t.writer with
          | true -> Error `Stopped
          | false ->
            let task = { f; ivar = Ivar.create () } in
            Pipe.write_without_pushback t.writer (Task task);
            Ok task
        ;;

        let cancel task =
          Fiber.of_thunk (fun () ->
            let { f = _; ivar } = task in
            Ivar.fill_if_empty ivar (Error `Cancelled);
            Fiber.return ())
        ;;

        let await task =
          Fiber.of_thunk (fun () ->
            let { f = _; ivar } = task in
            Ivar.read ivar |> fiber_of_deferred)
        ;;

        let close t =
          t.stop ();
          Pipe.close t.writer
        ;;
      end

      module Fd = struct
        include Types.Fd

        let create file_descr blocking =
          let () =
            match blocking with
            | `Blocking -> ()
            | `Non_blocking already_set ->
              (match already_set with
               | true -> ()
               | false -> Core_unix.set_nonblock file_descr)
          in
          let%map kind = Fd.Kind.infer_using_stat file_descr in
          Fd.create
            ~avoid_setting_nonblock:true
            kind
            file_descr
            (Info.of_string "Lev_fiber_async.Fd.t")
        ;;

        let close t = don't_wait_for (t >>= Fd.close)
      end

      module Io = struct
        include Types.Io

        let fd (type a) : a t -> Fd.t = function
          | Reader reader -> return (Reader.fd reader)
          | Writer writer -> return (Writer.fd writer)
        ;;

        let create (type a) fd mode =
          let%map.Fiber fd = fiber_of_deferred fd in
          match (mode : a mode) with
          | Input -> (Reader (Reader.create fd) : a t)
          | Output -> (Writer (Writer.create fd) : a t)
        ;;

        let create_rw fd =
          let%map.Fiber input = create fd Input
          and output = create fd Output in
          input, output
        ;;

        let with_read (Reader reader) ~f = Fiber.of_thunk (fun () -> f reader)
        let with_write (Writer writer) ~f = Fiber.of_thunk (fun () -> f writer)

        let close (type a) : a t -> unit = function
          | Reader reader -> don't_wait_for (Reader.close reader)
          | Writer writer -> don't_wait_for (Writer.close writer)
        ;;

        let pipe ?(cloexec = true) () =
          Fiber.of_thunk (fun () ->
            let%map.Fiber `Reader reader_fd, `Writer writer_fd =
              Unix.pipe (Info.of_string "Lev_fiber_async.Io.pipe") |> fiber_of_deferred
            in
            let () =
              match cloexec with
              | true -> ()
              | false ->
                Unix.clear_close_on_exec reader_fd;
                Unix.clear_close_on_exec writer_fd
            in
            Reader (Reader.create reader_fd), Writer (Writer.create writer_fd))
        ;;

        let stdin =
          Fiber.of_thunk (fun () ->
            let reader = force Reader.stdin in
            Fiber.return (Reader reader))
        ;;

        let stdout =
          Fiber.of_thunk (fun () ->
            let writer = force Writer.stdout in
            Fiber.return (Writer writer))
        ;;

        let stderr =
          Fiber.of_thunk (fun () ->
            let writer = force Writer.stderr in
            Fiber.return (Writer writer))
        ;;

        module Reader = struct
          type t = Reader.t

          let byte = Bytes.create 1

          let read_char_exn t =
            match Reader.read_available t byte with
            | 0 -> raise_s [%message "read_char_exn: Nothing available on the reader."]
            | 1 -> Bytes.unsafe_get byte 0
            | n -> raise_s [%message "BUG: Read impossible number of bytes" (n : int)]
          ;;

          let read_line t =
            Fiber.of_thunk (fun () ->
              match Reader.is_closed t with
              | true -> Fiber.return (Error (`Partial_eof ""))
              | false ->
                let strip_trailing_carriage_return line =
                  let length = String.length line in
                  match length >= 1 && Char.equal line.[length - 1] '\r' with
                  | true -> String.sub line ~pos:0 ~len:(length - 1)
                  | false -> line
                in
                (* [Reader.read_line] collapses the [`Ok] and [`Eof_without_delim] cases,
                   so it doesn't provide enough information to satisfy the interface. *)
                (match%map.Fiber
                   Reader.read_until t (`Char '\n') ~keep_delim:false |> fiber_of_deferred
                 with
                 | `Ok line -> Ok (strip_trailing_carriage_return line)
                 | `Eof -> Error (`Partial_eof "")
                 | `Eof_without_delim line ->
                   Error (`Partial_eof (strip_trailing_carriage_return line))))
          ;;

          let read_exactly t n =
            Fiber.of_thunk (fun () ->
              match Reader.is_closed t with
              | true -> Fiber.return (Error (`Partial_eof ""))
              | false ->
                let bytes = Bytes.create n in
                (match%map.Fiber Reader.really_read t bytes |> fiber_of_deferred with
                 | `Ok -> Ok (Bytes.to_string bytes)
                 | `Eof n_read ->
                   Error (`Partial_eof (Bytes.To_string.sub bytes ~pos:0 ~len:n_read))))
          ;;

          let to_string t =
            Fiber.of_thunk (fun () ->
              (* Copy of [Reader.contents] without closing the reader at the end. *)
              let buf = Buffer.create 1024 in
              let sbuf = Bytes.create 1024 in
              Deferred.repeat_until_finished () (fun () ->
                match Reader.is_closed t with
                | true -> return (`Finished (Buffer.contents buf))
                | false ->
                  (match%map Reader.read t sbuf with
                   | `Eof -> `Finished (Buffer.contents buf)
                   | `Ok l ->
                     Buffer.add_subbytes buf sbuf ~pos:0 ~len:l;
                     `Repeat ()))
              |> fiber_of_deferred)
          ;;
        end

        module Writer = struct
          type t = Writer.t

          let flush t =
            Fiber.of_thunk (fun () ->
              Writer.flushed_or_failed_unit t |> fiber_of_deferred)
          ;;

          let add_string t s = Writer.write t s
          let add_substring t s ~pos ~len = Writer.write ~pos ~len t s
        end
      end

      module Socket = struct
        let connect fd sockaddr =
          let%bind.Fiber fd = fiber_of_deferred fd in
          let connect socket_type where_to_connect =
            let socket = Socket.of_fd fd socket_type in
            where_to_connect
            |> Tcp.connect_sock ~socket
            |> Deferred.ignore_m
            |> fiber_of_deferred
          in
          match (sockaddr : Unix.sockaddr) with
          | ADDR_UNIX unix -> connect Socket.Type.unix (Tcp.Where_to_connect.of_file unix)
          | ADDR_INET (addr, port) ->
            connect
              Socket.Type.tcp
              (Tcp.Where_to_connect.of_inet_address (`Inet (addr, port)))
        ;;
      end

      let yield () = Fiber.of_thunk (fun () -> Scheduler.yield () |> fiber_of_deferred)
    end : S)
  ;;

  include (val make ~time_source:(Time_source.wall_clock ()))
end

module Lev_fiber_csexp = struct
  (* CR-someday ddickstein: Once https://github.com/ocaml-dune/csexp/issues/19 is
     resolved, remove this module. *)
  module Conv = struct
    (* We use a copy of [Csexp.Sexp] here to avoid relying on it - it's worth taking extra
       precautions around [Obj.magic]. *)
    module type Sexp = sig
      type t =
        | Atom of string
        | List of t list
    end

    module Make (M : Sexp) = struct
      let to_csexp (sexp : M.t) : Csexp.t = Obj.magic sexp
      let of_csexp (csexp : Csexp.t) : M.t = Obj.magic csexp
    end

    (* Prove that [Csexp] itself satisfies [Sexp]. *)
    module _ = Make (Csexp)
    include Make (Sexp)
  end

  module Session = struct
    type t =
      { reader : Reader.t
      ; writer : Writer.t
      }

    let create ~socket:_ (Types.Io.Reader reader) (Types.Io.Writer writer) =
      { reader; writer }
    ;;

    let close { reader; writer } =
      don't_wait_for (Writer.close writer);
      don't_wait_for (Reader.close reader)
    ;;

    let write t csexps =
      Fiber.of_thunk (fun () ->
        List.iter csexps ~f:(fun csexp ->
          let sexp = Conv.of_csexp csexp in
          Writer.write_sexp ~hum:false ~terminate_with:Space_if_needed t.writer sexp);
        Fiber.return ())
    ;;

    let read t =
      Fiber.of_thunk (fun () ->
        match%map.Fiber Reader.read_sexp t.reader |> fiber_of_deferred with
        | `Eof -> None
        | `Ok sexp -> Some (Conv.to_csexp sexp))
    ;;
  end

  let connect fd sockaddr =
    let%bind.Fiber () = Lev_fiber.Socket.connect fd sockaddr in
    let%map.Fiber input, output = Lev_fiber.Io.create_rw fd in
    Session.create ~socket:true input output
  ;;
end

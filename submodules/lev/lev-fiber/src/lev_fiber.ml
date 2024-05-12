open Stdune
open Fiber.O
open Lev_fiber_util
module Timestamp = Lev.Timestamp

module Signal_watcher = struct
  type t = {
    thread : Thread.t;
    old_sigmask : int list;
    old_sigpipe : Sys.signal_behavior option;
    old_sigchld : Sys.signal_behavior;
    sigchld_watcher : Lev.Async.t;
  }

  let stop_sig = Sys.sigusr2

  let blocked_signals =
    [ Sys.sigchld; stop_sig ] |> List.sort ~compare:Int.compare

  let stop t =
    Unix.kill (Unix.getpid ()) stop_sig;
    Thread.join t.thread;
    let used_mask =
      Unix.sigprocmask SIG_SETMASK t.old_sigmask
      |> List.sort ~compare:Int.compare
    in
    Option.iter t.old_sigpipe ~f:(Sys.set_signal Sys.sigpipe);
    Sys.set_signal Sys.sigchld t.old_sigchld;
    if used_mask <> blocked_signals then
      Code_error.raise "cannot restore old sigmask"
        [
          ("stop_sig", Dyn.int stop_sig);
          ("sigchld", Dyn.int stop_sig);
          ("used_mask", Dyn.(list int) used_mask);
          ("old_sigmask", Dyn.(list int) t.old_sigmask);
          ("blocked_signals", Dyn.(list int) blocked_signals);
        ]

  let run (watcher, loop) =
    while true do
      let signal = Thread.wait_signal blocked_signals in
      if signal = Sys.sigusr2 then raise_notrace Thread.Exit
      else Lev.Async.send watcher loop
    done

  let create ~sigpipe ~sigchld_watcher ~loop =
    let old_sigpipe =
      match sigpipe with
      | `Inherit -> None
      | `Ignore -> Some (Sys.signal Sys.sigpipe Sys.Signal_ignore)
    in
    let old_sigchld =
      Sys.signal Sys.sigchld (Sys.Signal_handle (fun (_ : int) -> ()))
    in
    let old_sigmask = Unix.sigprocmask SIG_BLOCK blocked_signals in
    let thread = Thread.create run (sigchld_watcher, loop) in
    { thread; old_sigmask; old_sigchld; old_sigpipe; sigchld_watcher }
end

module Process_watcher = struct
  module Process_table = struct
    type process = { pid : Pid.t; ivar : Unix.process_status Fiber.Ivar.t }
    type t = { loop : Lev.Loop.t; active : (Pid.t, process) Table.t }

    let create loop = { loop; active = Table.create (module Pid) 16 }

    let spawn t pid =
      Lev.Loop.ref t.loop;
      let ivar = Fiber.Ivar.create () in
      let process = { pid; ivar } in
      Table.add_exn t.active pid process;
      ivar

    let is_empty t = Table.length t.active = 0

    let reap t queue =
      Table.filteri_inplace t.active ~f:(fun ~key:pid ~data:process ->
          let pid, status = Unix.waitpid [ WNOHANG ] (Pid.to_int pid) in
          match pid with
          | 0 -> true
          | _ ->
              Lev.Loop.unref t.loop;
              Queue.push queue (Fiber.Fill (process.ivar, status));
              false)
  end

  type watcher = Signal of Lev.Async.t | Poll of Lev.Timer.t
  type t = { loop : Lev.Loop.t; table : Process_table.t; watcher : watcher }

  let create loop queue =
    let table = Process_table.create loop in
    let watcher =
      if Sys.win32 then
        let reap timer =
          Process_table.reap table queue;
          if Process_table.is_empty table then Lev.Timer.stop timer loop
        in
        let watcher = Lev.Timer.create ~repeat:0.05 ~after:0.05 reap in
        Poll watcher
      else
        let reap (_ : Lev.Async.t) = Process_table.reap table queue in
        let watcher = Lev.Async.create reap in
        Lev.Async.start watcher loop;
        Lev.Loop.unref loop;
        Signal watcher
    in
    { table; watcher; loop }

  let ensure_started t =
    match t.watcher with
    | Signal _ -> ()
    | Poll s -> if not (Lev.Timer.is_active s) then Lev.Timer.start s t.loop

  let waitpid t ~pid =
    ensure_started t;
    Process_table.spawn t.table pid

  let cleanup t =
    (* XXX shall we kill the running processes here? *)
    match t.watcher with
    | Poll s ->
        Lev.Timer.stop s t.loop;
        Lev.Timer.destroy s
    | Signal s ->
        Lev.Async.stop s t.loop;
        Lev.Async.destroy s
end

type thread_job_status = Active | Complete | Cancelled
type thread_job = { status : thread_job_status ref; ivar : Fiber.fill }
type worker = Worker : 'a Worker.t -> worker

type t = {
  loop : Lev.Loop.t;
  queue : Fiber.fill Queue.t;
  (* TODO stop when there are no threads *)
  async : Lev.Async.t;
  thread_jobs : thread_job Queue.t;
  thread_mutex : Mutex.t;
  process_watcher : Process_watcher.t;
  signal_watcher : Signal_watcher.t option (* [None] on windows *);
  mutable thread_workers : worker list;
}

type scheduler = t

let t : t Fiber.Var.t = Fiber.Var.create ()
let t_var = t
let scheduler = t

module Buffer = struct
  include Bip_buffer

  let default_size = 4096

  type nonrec t = bytes t

  let create ~size : t = create (Stdlib.Bytes.create size) ~len:size
end

module State = struct
  type ('a, 'b) t' = Open of 'a | Closed of 'b
  type ('a, 'b) t = ('a, 'b) t' ref

  let create a = ref (Open a)
end

module Thread = struct
  type job =
    | Job : {
        run : unit -> 'a;
        status : thread_job_status ref;
        ivar :
          ('a, [ `Exn of Exn_with_backtrace.t | `Cancelled ]) result
          Fiber.Ivar.t;
      }
        -> job

  type nonrec t = { worker : job Worker.t; scheduler : t }

  let create =
    let finish_job t fill =
      Mutex.lock t.thread_mutex;
      Queue.push t.thread_jobs fill;
      Mutex.unlock t.thread_mutex;
      Lev.Async.send t.async t.loop
    in
    fun () ->
      let+ t = Fiber.Var.get_exn t in
      let do_no_raise (Job { run; status; ivar }) =
        let res =
          match Exn_with_backtrace.try_with run with
          | Ok x -> Ok x
          | Error exn -> Error (`Exn exn)
        in
        finish_job t { status; ivar = Fiber.Fill (ivar, res) }
      in
      let worker =
        Worker.create ~do_no_raise ~spawn_thread:(fun f -> Thread.create f ())
      in
      t.thread_workers <- Worker worker :: t.thread_workers;
      { worker; scheduler = t }

  type 'a task = {
    ivar :
      ('a, [ `Exn of Exn_with_backtrace.t | `Cancelled ]) result Fiber.Ivar.t;
    task : Worker.task;
    status : thread_job_status ref;
    loop : Lev.Loop.t;
  }

  let task (t : t) ~f =
    let ivar = Fiber.Ivar.create () in
    let status = ref Active in
    match Worker.add_work t.worker (Job { run = f; status; ivar }) with
    | Error `Stopped -> Error `Stopped
    | Ok task ->
        Lev.Loop.ref t.scheduler.loop;
        Ok { ivar; task; status; loop = t.scheduler.loop }

  let await task = Fiber.Ivar.read task.ivar

  let cancel task =
    match !(task.status) with
    | Cancelled | Complete -> Fiber.return ()
    | Active ->
        Lev.Loop.unref task.loop;
        task.status := Cancelled;
        Worker.cancel_if_not_consumed task.task;
        Fiber.Ivar.fill task.ivar (Error `Cancelled)

  let close t =
    t.scheduler.thread_workers <-
      (let id = Worker.id t.worker in
       List.filter t.scheduler.thread_workers ~f:(fun (Worker w) ->
           let id' = Worker.id w in
           not (Worker.Id.equal id id')));
    Worker.complete_tasks_and_stop t.worker
end

module Timer = struct
  let sleepf after =
    let* t = Fiber.Var.get_exn t in
    let ivar = Fiber.Ivar.create () in
    let timer =
      Lev.Timer.create ~after (fun timer ->
          Lev.Timer.stop timer t.loop;
          Lev.Timer.destroy timer;
          Queue.push t.queue (Fiber.Fill (ivar, ())))
    in
    Lev.Timer.start timer t.loop;
    Fiber.Ivar.read ivar

  module Wheel = struct
    type running_state =
      | Idle
      | Sleeping of Lev.Timer.t * unit Fiber.Ivar.t
      (* set whenever the wheel is waiting for a new task *)
      | Waiting of { ivar : unit Fiber.Ivar.t; filled : bool }

    type elt = {
      ivar : [ `Ok | `Cancelled ] Fiber.Ivar.t;
      scheduled : Lev.Timestamp.t;
      mutable filled : bool;
      wheel : t;
    }

    and running = {
      queue : elt Removable_queue.t;
      delay : float;
      scheduler : scheduler;
      mutable state : running_state;
    }

    and state = Stopped of { delay : float } | Running of running
    and t = state ref

    let delay t =
      match !t with Stopped { delay } -> delay | Running { delay; _ } -> delay

    let create ~delay =
      let+ scheduler = Fiber.Var.get_exn t in
      ref
        (Running
           { queue = Removable_queue.create (); delay; scheduler; state = Idle })

    type task = elt Removable_queue.node ref
    type condition = { sleeping : bool; waiting : bool }

    let wakeup_if t { sleeping; waiting } =
      match t.state with
      | Sleeping (timer, ivar) when sleeping ->
          let* { loop; _ } = Fiber.Var.get_exn scheduler in
          Lev.Timer.stop timer loop;
          Lev.Timer.destroy timer;
          t.state <- Idle;
          Fiber.Ivar.fill ivar ()
      | Waiting { ivar; filled = false } when waiting ->
          t.state <- Idle;
          Fiber.Ivar.fill ivar ()
      | _ -> Fiber.return ()

    let set_delay t ~delay =
      match !t with
      | Stopped _ -> Code_error.raise "Wheel.set_delay" []
      | Running d ->
          t := Running { d with delay };
          wakeup_if d { sleeping = true; waiting = false }

    let task (t : t) : task Fiber.t =
      Fiber.of_thunk (fun () ->
          match !t with
          | Stopped _ -> Code_error.raise "Wheel.task" []
          | Running wheel ->
              let now = Lev.Loop.now wheel.scheduler.loop in
              let data =
                {
                  wheel = t;
                  ivar = Fiber.Ivar.create ();
                  scheduled = now;
                  filled = false;
                }
              in
              let res = Removable_queue.push wheel.queue data in
              let+ () = wakeup_if wheel { waiting = true; sleeping = false } in
              ref res)

    let reset (task : task) =
      Fiber.of_thunk (fun () ->
          let task' = Removable_queue.data !task in
          match !(task'.wheel) with
          | Stopped _ -> Code_error.raise "reset: wheel is stopped" []
          | Running wheel ->
              Removable_queue.remove !task;
              let now = Lev.Loop.now wheel.scheduler.loop in
              let filled = task'.filled in
              let task' =
                let task' = { task' with scheduled = now } in
                if filled then (
                  task'.filled <- false;
                  { task' with ivar = Fiber.Ivar.create () })
                else task'
              in
              let new_task = Removable_queue.push wheel.queue task' in
              task := new_task;
              if filled then
                wakeup_if wheel { sleeping = false; waiting = true }
              else Fiber.return ())

    let await (task : task) =
      Fiber.of_thunk (fun () ->
          let task = Removable_queue.data !task in
          Fiber.Ivar.read task.ivar)

    let cancel (node : task) =
      Fiber.of_thunk (fun () ->
          let task = Removable_queue.data !node in
          if task.filled then Fiber.return ()
          else (
            task.filled <- true;
            Removable_queue.remove !node;
            Fiber.Ivar.fill task.ivar `Cancelled))

    let rec run t =
      (* TODO do not allow double [run] *)
      match !t with
      | Stopped _ -> Fiber.return ()
      | Running r -> (
          match Removable_queue.peek r.queue with
          | None ->
              let ivar = Fiber.Ivar.create () in
              r.state <- Waiting { ivar; filled = false };
              let* () = Fiber.Ivar.read ivar in
              run t
          | Some node ->
              let task = Removable_queue.data node in
              let after =
                let now = Timestamp.to_float (Lev.Loop.now r.scheduler.loop) in
                let scheduled = Timestamp.to_float task.scheduled in
                scheduled -. now +. r.delay
              in
              let expired = after < 0. in
              let* () =
                if expired then (
                  Removable_queue.remove node;
                  if not task.filled then (
                    task.filled <- true;
                    Queue.push r.scheduler.queue (Fiber.Fill (task.ivar, `Ok)));
                  Fiber.return ())
                else
                  let scheduler = r.scheduler in
                  let ivar = Fiber.Ivar.create () in
                  let timer =
                    Lev.Timer.create ~after (fun timer ->
                        (* TODO reuse timer *)
                        Lev.Timer.destroy timer;
                        Queue.push scheduler.queue (Fiber.Fill (ivar, ())))
                  in
                  r.state <- Sleeping (timer, ivar);
                  Lev.Timer.start timer scheduler.loop;
                  Fiber.Ivar.read ivar
              in
              run t)

    let run t = Fiber.of_thunk (fun () -> run t)

    let stop =
      let rec cancel_all r =
        match Removable_queue.pop r.queue with
        | None -> Fiber.return ()
        | Some task ->
            let* () =
              if task.filled then Fiber.return ()
              else (
                task.filled <- true;
                Fiber.Ivar.fill task.ivar `Cancelled)
            in
            cancel_all r
      in
      fun t ->
        Fiber.of_thunk (fun () ->
            match !t with
            | Stopped _ -> Fiber.return ()
            | Running r ->
                t := Stopped { delay = r.delay };
                let* () = cancel_all r in
                wakeup_if r { sleeping = true; waiting = true })
  end
end

let waitpid ~pid =
  let pid = Pid.of_int pid in
  let* t = Fiber.Var.get_exn t in
  let ivar = Process_watcher.waitpid t.process_watcher ~pid in
  Fiber.Ivar.read ivar

let signal ~signal =
  let* { loop; queue; _ } = Fiber.Var.get_exn t in
  let ivar = Fiber.Ivar.create () in
  let signal =
    Lev.Signal.create ~signal (fun t ->
        Queue.push queue (Fiber.Fill (ivar, ()));
        Lev.Signal.stop t loop;
        Lev.Signal.destroy t)
  in
  Lev.Signal.start signal loop;
  Fiber.Ivar.read ivar

module Fd = struct
  type kind = Blocking | Non_blocking of { mutable set : bool }
  type t = { fd : Unix.file_descr; kind : kind; mutable closed : bool }

  let fd_exn t =
    if t.closed then raise (Unix.Unix_error (Unix.EBADF, "closed fd", ""));
    t.fd

  let close t =
    if not t.closed then (
      t.closed <- true;
      Unix.close t.fd)

  let create' fd kind = { kind; fd; closed = false }

  let create fd kind =
    let kind =
      match kind with
      | `Blocking -> Blocking
      | `Non_blocking set -> Non_blocking { set }
    in
    create' fd kind

  let set_nonblock t =
    assert (not t.closed);
    match t.kind with
    | Blocking -> ()
    | Non_blocking nb ->
        if not nb.set then (
          Unix.set_nonblock t.fd;
          nb.set <- true)

  let pipe =
    if Sys.win32 then fun ?cloexec () ->
      let r, w = Unix.pipe ?cloexec () in
      ( { fd = r; kind = Blocking; closed = false },
        { fd = w; kind = Blocking; closed = false } )
    else fun ?cloexec () ->
      let r, w = Unix.pipe ?cloexec () in
      Unix.set_nonblock r;
      Unix.set_nonblock w;
      ( { fd = r; kind = Non_blocking { set = true }; closed = false },
        { fd = w; kind = Non_blocking { set = true }; closed = false } )
end

module Lev_fd = struct
  module Event = Lev.Io.Event

  type open_ = {
    io : Lev.Io.t;
    fd : Fd.t;
    scheduler : scheduler;
    mutable events : Event.Set.t;
    read : [ `Ready | `Closed ] Fiber.Ivar.t Queue.t;
    write : [ `Ready | `Closed ] Fiber.Ivar.t Queue.t;
  }

  type state = Open of open_ | Closed of Fd.t
  type t = state ref

  let reset nb new_set =
    nb.events <- new_set;
    Lev.Io.stop nb.io nb.scheduler.loop;
    Lev.Io.modify nb.io nb.events;
    Lev.Io.start nb.io nb.scheduler.loop

  let await t (what : Lev.Io.Event.t) =
    let* () = Fiber.return () in
    match !t with
    | Closed _ -> Fiber.return `Closed
    | Open t ->
        if t.fd.closed then Fiber.return `Closed
        else (
          if not (Event.Set.mem t.events what) then
            reset t (Event.Set.add t.events what);
          let ivar = Fiber.Ivar.create () in
          let q = match what with Write -> t.write | Read -> t.read in
          Queue.push q ivar;
          let+ res = Fiber.Ivar.read ivar in
          match res with
          | `Closed -> `Closed
          | `Ready ->
              assert (not t.fd.closed);
              `Ready t.fd)

  let rec close_queue ivar_queue q =
    match Queue.pop q with
    | None -> ()
    | Some ivar ->
        Queue.push ivar_queue (Fiber.Fill (ivar, `Closed));
        close_queue ivar_queue q

  let close (t : t) =
    match !t with
    | Closed fd -> fd
    | Open { io; scheduler; fd; read; write; events = _ } ->
        t := Closed fd;
        Lev.Io.stop io scheduler.loop;
        Lev.Io.destroy io;
        Fd.close fd;
        close_queue scheduler.queue read;
        close_queue scheduler.queue write;
        fd

  let make_cb t scheduler _ _ set =
    match !(Fdecl.get t) with
    | Closed _ -> ()
    | Open nb ->
        let keep_read = ref true in
        let keep_write = ref true in
        (if Lev.Io.Event.Set.mem set Read then
         match Queue.pop nb.read with
         | Some ivar -> Queue.push scheduler.queue (Fiber.Fill (ivar, `Ready))
         | None -> keep_read := false);
        (if Lev.Io.Event.Set.mem set Write then
         match Queue.pop nb.write with
         | Some ivar -> Queue.push scheduler.queue (Fiber.Fill (ivar, `Ready))
         | None -> keep_write := false);
        let new_set =
          Event.Set.inter nb.events
            (Event.Set.create ~read:!keep_read ~write:!keep_write ())
        in
        if not (Event.Set.equal new_set nb.events) then reset nb new_set

  let create (fd : Fd.t) : t Fiber.t =
    if fd.closed then Code_error.raise "create: fd is closed" [];
    let+ scheduler = Fiber.Var.get_exn scheduler in
    let t : t Fdecl.t = Fdecl.create Dyn.opaque in
    let events = Event.Set.create () in
    let io = Lev.Io.create (make_cb t scheduler) fd.fd events in
    let read = Queue.create () in
    let write = Queue.create () in
    Fdecl.set t (ref (Open { events; fd; scheduler; io; read; write }));
    Lev.Io.start io scheduler.loop;
    Fdecl.get t
end

module Io = struct
  let callstack =
    match Sys.getenv_opt "LEV_DEBUG" with
    | None -> fun () -> None
    | Some _ -> fun () -> Some (Printexc.get_callstack 15)

  type input = Input
  type output = Output
  type 'a mode = Input : input mode | Output : output mode

  module Slice = Buffer.Slice

  type _ kind =
    | Write : { mutable flush_counter : int } -> output kind
    | Read : { mutable eof : bool } -> input kind

  type fd = Blocking of Thread.t * Fd.t | Non_blocking of Lev_fd.t

  let with_ fd (kind : Lev.Io.Event.t) ~f =
    let* () = Fiber.return () in
    match fd with
    | Non_blocking lev_fd -> (
        let+ event = Lev_fd.await lev_fd kind in
        match event with
        | `Closed -> Error `Eof
        | `Ready fd -> (
            match f fd with exception exn -> Error (`Exn exn) | s -> Ok s))
    | Blocking (th, fd) -> (
        let task =
          match Thread.task th ~f:(fun () -> f fd) with
          | Error `Stopped -> Code_error.raise "already stopped" []
          | Ok task -> task
        in
        let+ res = Thread.await task in
        match res with
        | Ok _ as s -> s
        | Error `Cancelled -> assert false
        | Error (`Exn exn) -> Error (`Exn exn.exn))

  type activity = Idle | Busy of Printexc.raw_backtrace option

  type 'a open_ = {
    mutable buffer : Buffer.t;
    kind : 'a kind;
    fd : fd;
    mutable activity : activity;
    source : Printexc.raw_backtrace option;
  }

  type 'a t = ('a open_, Fd.t * Printexc.raw_backtrace option) State.t

  let fd (t : _ t) =
    match !t with
    | Closed (fd, _) -> fd
    | Open { fd = Blocking (_, fd); _ } -> fd
    | Open { fd = Non_blocking fd; _ } -> (
        match !fd with Closed fd -> fd | Open f -> f.fd)

  let rec with_resize_buffer t ~len reserve_fail k =
    match Buffer.reserve t.buffer ~len with
    | Some dst_pos -> k t ~len ~dst_pos
    | None -> (
        match reserve_fail with
        | `Compress ->
            if Buffer.unused_space t.buffer >= len then
              Buffer.Bytes.compress t.buffer;
            with_resize_buffer t ~len `Resize k
        | `Resize ->
            let len = Buffer.length t.buffer + len in
            Buffer.Bytes.resize t.buffer ~len;
            with_resize_buffer t ~len `Fail k
        | `Fail -> assert false)

  module Writer = struct
    type nonrec t = output open_

    module Expert = struct
      let available t = Buffer.max_available t.buffer

      let prepare =
        let k t ~len ~dst_pos:pos =
          let buf = Buffer.buffer t.buffer in
          (buf, { Slice.pos; len })
        in
        fun t ~len -> with_resize_buffer t ~len `Compress k

      let commit t ~len = Buffer.commit t.buffer ~len
    end

    let flush =
      let rec loop t stop_count =
        (* TODO fix overflow issues *)
        if
          (match t.kind with Write { flush_counter } -> flush_counter)
          >= stop_count
        then Fiber.return ()
        else
          let* res =
            with_ t.fd Write ~f:(fun fd ->
                match Buffer.peek t.buffer with
                | None -> ()
                | Some { Slice.pos; len } -> (
                    let buffer = Buffer.buffer t.buffer in
                    let len = Unix.single_write fd.fd buffer pos len in
                    Buffer.junk t.buffer ~len;
                    match t.kind with
                    | Write t -> t.flush_counter <- t.flush_counter + len))
          in
          match res with
          | Ok () -> loop t stop_count
          | Error (`Exn (Unix.Unix_error (Unix.EAGAIN, _, _))) ->
              loop t stop_count
          | Error (`Exn (Unix.Unix_error (EPIPE, _, _))) | Error `Eof ->
              let args =
                [
                  ("remaining", Dyn.int stop_count);
                  ( "contents",
                    Dyn.string (Format.asprintf "%a@." Buffer.Bytes.pp t.buffer)
                  );
                ]
              in
              let args =
                match t.source with
                | None -> args
                | Some source ->
                    ( "source",
                      Dyn.string @@ Printexc.raw_backtrace_to_string source )
                    :: args
              in
              Code_error.raise "fd closed unflushed" args
          | Error (`Exn exn) -> reraise exn
      in
      fun t ->
        Fiber.of_thunk (fun () ->
            let stop_count =
              match t.kind with
              | Write { flush_counter } ->
                  flush_counter + Buffer.length t.buffer
            in
            loop t stop_count)

    let add_substring t str ~pos ~len =
      Buffer.Bytes.Writer.add_substring t.buffer str ~pos ~len

    let add_string t str = Buffer.Bytes.Writer.add_string t.buffer str
  end

  let create_gen (type a) ~source fd (mode : a mode) =
    let buffer = Buffer.create ~size:Buffer.default_size in
    let kind : a kind =
      match mode with
      | Input -> Read { eof = false }
      | Output -> Write { flush_counter = 0 }
    in
    State.create { buffer; fd; kind; activity = Idle; source }

  let create (type a) (fd : Fd.t) (mode : a mode) =
    let source = callstack () in
    match fd.kind with
    | Non_blocking _ ->
        let+ fd = Lev_fd.create fd in
        create_gen ~source (Non_blocking fd) mode
    | Blocking ->
        let+ thread = Thread.create () in
        create_gen ~source (Blocking (thread, fd)) mode

  let create_rw (fd : Fd.t) : (input t * output t) Fiber.t =
    let source = callstack () in
    match fd.kind with
    | Non_blocking _ ->
        let+ fd =
          let+ fd = Lev_fd.create fd in
          Non_blocking fd
        in
        let r = create_gen ~source fd Input in
        let w = create_gen ~source fd Output in
        (r, w)
    | Blocking ->
        let* r =
          let+ thread = Thread.create () in
          create_gen ~source (Blocking (thread, fd)) Input
        in
        let+ w =
          let+ thread = Thread.create () in
          create_gen ~source (Blocking (thread, fd)) Output
        in
        (r, w)

  let close =
    let close_fd t =
      match t with
      | Non_blocking fd -> Lev_fd.close fd
      | Blocking (th, fd) ->
          Thread.close th;
          Fd.close fd;
          fd
    in
    fun (type a) (t : a t) ->
      match !t with
      | State.Closed _ -> ()
      | Open o ->
          (match (o.kind : _ kind) with
          | Read r -> r.eof <- true
          | Write _ -> () (* TODO *));
          let fd = close_fd o.fd in
          t := Closed (fd, o.source)

  module Reader = struct
    type t = input open_

    exception Unavailable

    module Expert = struct
      let buffer t =
        match Buffer.peek t.buffer with
        | None -> raise Unavailable
        | Some { Buffer.Slice.pos; len } ->
            let b = Buffer.buffer t.buffer in
            (b, { Slice.pos; len })

      let consume (t : t) ~len = Buffer.junk t.buffer ~len

      let available t =
        let eof = match t.kind with Read { eof } -> eof in
        let available = Buffer.length t.buffer in
        if available = 0 && eof then `Eof else `Ok available

      let refill =
        let rec read t ~len ~dst_pos =
          let buffer = Buffer.buffer t.buffer in
          let* res =
            with_ t.fd Read ~f:(fun fd -> Unix.read fd.fd buffer 0 len)
          in
          match res with
          | Error (`Exn (Unix.Unix_error (Unix.EAGAIN, _, _))) ->
              read t ~len ~dst_pos
          | Error `Eof | Ok 0 ->
              (match t.kind with Read b -> b.eof <- true);
              Buffer.commit t.buffer ~len:0;
              Fiber.return ()
          | Ok len ->
              Buffer.commit t.buffer ~len;
              Fiber.return ()
          | Error (`Exn exn) -> reraise exn
        in
        fun ?(size = Buffer.default_size) (t : t) ->
          with_resize_buffer t ~len:size `Compress read
    end

    exception Found of int

    let read_char_exn t =
      let b, { Buffer.Slice.pos; len } = Expert.buffer t in
      assert (len > 0);
      let res = Bytes.get b pos in
      Expert.consume t ~len:1;
      res

    let read_line =
      let contents buf =
        let module Buffer = Stdlib.Buffer in
        let len = Buffer.length buf in
        if len = 0 then ""
        else if Buffer.nth buf (len - 1) = '\r' then Buffer.sub buf 0 (len - 1)
        else Buffer.contents buf
      in
      let find_nl b pos len =
        try
          for i = pos to pos + len - 1 do
            if Bytes.get b i = '\n' then raise_notrace (Found i)
          done;
          None
        with Found i -> Some i
      in
      let rec loop t buf =
        match Expert.available t with
        | `Eof ->
            Fiber.return (Error (`Partial_eof (Stdlib.Buffer.contents buf)))
        | `Ok 0 ->
            let* () = Expert.refill t in
            loop t buf
        | `Ok _ -> (
            let b, { Slice.pos; len } = Expert.buffer t in
            match find_nl b pos len with
            | Some i ->
                let len = i - pos in
                Stdlib.Buffer.add_subbytes buf b pos len;
                Buffer.junk t.buffer ~len:(len + 1);
                Fiber.return (Ok (contents buf))
            | None ->
                Stdlib.Buffer.add_subbytes buf b pos len;
                Buffer.junk t.buffer ~len;
                loop t buf)
      in
      let rec self t =
        (* we can always call loop, but we do a little optimization to see if we can
           read the line without an extra copy
        *)
        match Expert.available t with
        | `Eof -> Fiber.return (Error (`Partial_eof ""))
        | `Ok 0 ->
            let* () = Expert.refill t in
            self t
        | `Ok _ -> (
            let b, { Slice.pos; len } = Expert.buffer t in
            match find_nl b pos len with
            | Some i ->
                let len = i - pos in
                let res =
                  let len =
                    if len > 0 && Bytes.get b (i - 1) = '\r' then len - 1
                    else len
                  in
                  Bytes.sub b ~pos ~len
                in
                Buffer.junk t.buffer ~len:(len + 1);
                Fiber.return (Ok (Bytes.unsafe_to_string res))
            | None ->
                let buf = Stdlib.Buffer.create len in
                Stdlib.Buffer.add_subbytes buf b pos len;
                Buffer.junk t.buffer ~len;
                loop t buf)
      in
      fun t -> Fiber.of_thunk (fun () -> self t)

    let read_exactly =
      let rec loop_buffer t buf remains =
        if remains = 0 then Fiber.return (Ok (Stdlib.Buffer.contents buf))
        else
          match Expert.available t with
          | `Eof ->
              Fiber.return (Error (`Partial_eof (Stdlib.Buffer.contents buf)))
          | `Ok 0 ->
              let* () = Expert.refill t in
              loop_buffer t buf remains
          | `Ok _ ->
              let b, { Slice.pos; len } = Expert.buffer t in
              let len = min remains len in
              Stdlib.Buffer.add_subbytes buf b pos len;
              Buffer.junk t.buffer ~len;
              loop_buffer t buf (remains - len)
      in
      let rec self t len =
        (* we can always call loop, but we do a little optimization to see if we can
           read the line without an extra copy
        *)
        match Expert.available t with
        | `Eof -> Fiber.return (Error (`Partial_eof ""))
        | `Ok 0 ->
            let* () = Expert.refill t in
            self t len
        | `Ok _ ->
            let b, { Slice.pos; len = avail } = Expert.buffer t in
            if len <= avail then (
              let res = Bytes.sub b ~pos ~len in
              Buffer.junk t.buffer ~len;
              Fiber.return (Ok (Bytes.unsafe_to_string res)))
            else
              let buf = Stdlib.Buffer.create len in
              Stdlib.Buffer.add_subbytes buf b pos avail;
              Buffer.junk t.buffer ~len:avail;
              loop_buffer t buf (len - avail)
      in
      fun t len -> Fiber.of_thunk (fun () -> self t len)

    let to_string =
      let rec loop t buf =
        match Expert.available t with
        | `Eof -> Fiber.return (Stdlib.Buffer.contents buf)
        | `Ok 0 ->
            let* () = Expert.refill t in
            loop t buf
        | `Ok _ ->
            let b, { Slice.pos; len } = Expert.buffer t in
            Stdlib.Buffer.add_subbytes buf b pos len;
            Expert.consume t ~len;
            loop t buf
      in
      fun t -> Fiber.of_thunk (fun () -> loop t (Stdlib.Buffer.create 512))
  end

  let with_ (type a) (t : a t) ~f =
    let activity_source = callstack () in
    let* () = Fiber.return () in
    let t =
      match !(t : _ State.t) with
      | Open t -> t
      | Closed (_, source) ->
          let args =
            match source with
            | None -> []
            | Some source ->
                [
                  ( "source",
                    Dyn.string (Printexc.raw_backtrace_to_string source) );
                ]
          in
          Code_error.raise "Lev_fiber.Io: already closed" args
    in
    (match t.activity with
    | Idle -> t.activity <- Busy activity_source
    | Busy activity_source ->
        let args =
          let args =
            [
              ( "kind",
                Dyn.string
                  (match t.kind with Read _ -> "read" | Write _ -> "write") );
            ]
          in
          let args =
            match t.source with
            | None -> args
            | Some source ->
                ("source", Dyn.string (Printexc.raw_backtrace_to_string source))
                :: args
          in
          match activity_source with
          | None -> args
          | Some activity_source ->
              ( "activity_source",
                Dyn.string (Printexc.raw_backtrace_to_string activity_source) )
              :: args
        in
        Code_error.raise "Io.t is already busy" args);
    Fiber.finalize
      (fun () -> f t)
      ~finally:(fun () ->
        t.activity <- Idle;
        Fiber.return ())

  let with_read (t : input t) ~f = with_ t ~f
  let with_write (t : output t) ~f = with_ t ~f

  let pipe ?cloexec () : (input t * output t) Fiber.t =
    Fiber.of_thunk @@ fun () ->
    let r, w = Fd.pipe ?cloexec () in
    let* input = create r Input in
    let+ output = create w Output in
    (input, output)

  module Lazy_fiber : sig
    type 'a t

    val create : (unit -> 'a Fiber.t) -> 'a t
    val force : 'a t -> 'a Fiber.t
  end = struct
    type 'a t = {
      value : 'a Fiber.Ivar.t;
      mutable f : (unit -> 'a Fiber.t) option;
    }

    let create f = { f = Some f; value = Fiber.Ivar.create () }

    let force t =
      let open Fiber.O in
      match t.f with
      | None -> Fiber.Ivar.read t.value
      | Some f ->
          Fiber.of_thunk (fun () ->
              t.f <- None;
              let* v = f () in
              let+ () = Fiber.Ivar.fill t.value v in
              v)
  end

  let make_std_fd fd kind =
    Lazy_fiber.create (fun () ->
        let blockity =
          if Sys.win32 then `Blocking
          else (
            Unix.set_nonblock fd;
            `Non_blocking true)
        in
        create (Fd.create fd blockity) kind)

  let stdin = Lazy_fiber.force (make_std_fd Unix.stdin Input)
  let stderr = Lazy_fiber.force (make_std_fd Unix.stderr Output)
  let stdout = Lazy_fiber.force (make_std_fd Unix.stdout Output)
end

module Socket = struct
  let writeable_fd scheduler fd =
    let ivar = Fiber.Ivar.create () in
    let io =
      Lev.Io.create
        (fun io _ _ ->
          Queue.push scheduler.queue (Fiber.Fill (ivar, ()));
          Lev.Io.stop io scheduler.loop;
          Lev.Io.destroy io)
        fd
        (Lev.Io.Event.Set.create ~write:true ())
    in
    Lev.Io.start io scheduler.loop;
    Fiber.Ivar.read ivar

  let rec connect (fd : Fd.t) sock =
    let* scheduler = Fiber.Var.get_exn scheduler in
    Fd.set_nonblock fd;
    match Unix.connect fd.fd sock with
    | () -> Fiber.return ()
    | exception Unix.Unix_error (Unix.EISCONN, _, _) when Sys.win32 ->
        Fiber.return ()
    | exception Unix.Unix_error (Unix.EWOULDBLOCK, _, _) when Sys.win32 ->
        let* () = writeable_fd scheduler fd.fd in
        connect fd sock
    | exception Unix.Unix_error (Unix.EINPROGRESS, _, _) -> (
        let+ () = writeable_fd scheduler fd.fd in
        match Unix.getsockopt_error fd.fd with
        | None -> ()
        | Some err -> raise (Unix.Unix_error (err, "connect", "")))

  module Server = struct
    type t = {
      fd : Fd.t;
      pool : Fiber.Pool.t;
      io : Lev.Io.t;
      mutable close : bool;
      mutable await : unit Fiber.Ivar.t;
    }

    let create (fd : Fd.t) sockaddr ~backlog =
      let+ scheduler = Fiber.Var.get_exn scheduler in
      let pool = Fiber.Pool.create () in
      Fd.set_nonblock fd;
      Unix.bind fd.fd sockaddr;
      Unix.listen fd.fd backlog;
      let t = Fdecl.create Dyn.opaque in
      let io =
        Lev.Io.create
          (fun _ _ _ ->
            let t = Fdecl.get t in
            Queue.push scheduler.queue (Fiber.Fill (t.await, ())))
          fd.fd
          (Lev.Io.Event.Set.create ~read:true ())
      in
      Fdecl.set t { pool; await = Fiber.Ivar.create (); close = false; fd; io };
      Fdecl.get t

    let close t =
      Fiber.of_thunk (fun () ->
          if t.close then Fiber.return ()
          else
            let* scheduler = Fiber.Var.get_exn scheduler in
            Fd.close t.fd;
            Lev.Io.stop t.io scheduler.loop;
            Lev.Io.destroy t.io;
            t.close <- true;
            let* () = Fiber.Pool.stop t.pool in
            Fiber.Ivar.fill t.await ())

    module Session = struct
      type t = { fd : Fd.t; sockaddr : Unix.sockaddr }

      let fd t = t.fd
      let sockaddr t = t.sockaddr

      let io t =
        Fd.set_nonblock t.fd;
        Io.create_rw t.fd
    end

    let serve =
      let rec loop t f =
        let* () = Fiber.Ivar.read t.await in
        match t.close with
        | true -> Fiber.return ()
        | false ->
            t.await <- Fiber.Ivar.create ();
            let session =
              let fd, sockaddr = Unix.accept ~cloexec:true t.fd.fd in
              let fd = Fd.create' fd (Non_blocking { set = false }) in
              { Session.fd; sockaddr }
            in
            let* () = Fiber.Pool.task t.pool ~f:(fun () -> f session) in
            loop t f
      in
      fun (t : t) ~f ->
        let* scheduler = Fiber.Var.get_exn scheduler in
        Lev.Io.start t.io scheduler.loop;
        Fiber.fork_and_join_unit
          (fun () -> Fiber.Pool.run t.pool)
          (fun () -> loop t f)
  end
end

let yield () =
  let* scheduler = Fiber.Var.get_exn scheduler in
  let ivar = Fiber.Ivar.create () in
  Queue.push scheduler.queue (Fiber.Fill (ivar, ()));
  Fiber.Ivar.read ivar

module Error = struct
  type t = Aborted of Exn_with_backtrace.t | Already_reported | Deadlock

  let ok_exn = function
    | Ok s -> s
    | Error (Aborted exn) -> Exn_with_backtrace.reraise exn
    | Error Already_reported -> Code_error.raise "Already_reported" []
    | Error Deadlock -> Code_error.raise "Deadlock" []
end

exception Deadlock

let run (type a) ?(sigpipe = `Inherit)
    ?(flags = Lev.Loop.Flag.Set.singleton Nosigmask) (f : unit -> a Fiber.t) :
    (a, Error.t) result =
  if not (Lev.Loop.Flag.Set.mem flags Nosigmask) then
    Code_error.raise "flags must include Nosigmask" [];
  let lev_loop = Lev.Loop.create ~flags () in
  let thread_jobs = Queue.create () in
  let thread_mutex = Mutex.create () in
  let queue = Queue.create () in
  let async =
    Lev.Async.create (fun _ ->
        Mutex.lock thread_mutex;
        while not (Queue.is_empty thread_jobs) do
          let { ivar; status } = Queue.pop_exn thread_jobs in
          match !status with
          | Active ->
              Lev.Loop.unref lev_loop;
              status := Complete;
              Queue.push queue ivar
          | Cancelled -> ()
          | Complete -> assert false
        done;
        Mutex.unlock thread_mutex)
  in
  Lev.Async.start async lev_loop;
  Lev.Loop.unref lev_loop;
  let process_watcher = Process_watcher.create lev_loop queue in
  let signal_watcher =
    if Sys.win32 then None
    else
      let sigchld_watcher =
        match process_watcher.watcher with
        | Signal s -> s
        | Poll _ -> assert false
      in
      Some (Signal_watcher.create ~sigpipe ~sigchld_watcher ~loop:lev_loop)
  in
  let t =
    {
      loop = lev_loop;
      signal_watcher;
      queue;
      async;
      thread_mutex;
      thread_jobs;
      process_watcher;
      thread_workers = [];
    }
  in
  let rec events q acc =
    match Queue.pop q with None -> acc | Some e -> events q (e :: acc)
  in
  let rec iter_or_deadlock q =
    match Nonempty_list.of_list (events q []) with
    | Some e -> e
    | None -> raise_notrace Deadlock
  and iter loop q =
    match Nonempty_list.of_list (events q []) with
    | Some e -> e
    | None -> (
        let res = Lev.Loop.run loop Once in
        match res with
        | `No_more_active_watchers -> iter_or_deadlock q
        | `Otherwise -> iter loop q)
  in
  let f =
    let on_error exn =
      Format.eprintf "%a@." Exn_with_backtrace.pp_uncaught exn;
      Fiber.return ()
    in
    let f () = Fiber.Var.set t_var t f in
    Fiber.map_reduce_errors (module Monoid.Unit) ~on_error f
  in
  let res : (a, Error.t) result =
    match Fiber.run f ~iter:(fun () -> iter lev_loop queue) with
    | Error () -> Error Already_reported
    | Ok s -> Ok s
    | exception Deadlock -> Error Deadlock
    | exception exn ->
        let exn = Exn_with_backtrace.capture exn in
        Error (Aborted exn)
  in
  let () =
    Process_watcher.cleanup process_watcher;
    Lev.Async.stop async lev_loop;
    Option.iter signal_watcher ~f:Signal_watcher.stop;
    List.iter t.thread_workers ~f:(fun (Worker w) ->
        Worker.complete_tasks_and_stop w;
        Worker.join w);
    Lev.Async.destroy async;
    Lev.Loop.destroy lev_loop
  in
  res

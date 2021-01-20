open Import

module Process : sig
  type t

  val pid : t -> Pid.t

  val thread : t -> Scheduler.thread

  val client : t -> Ocamlformat_rpc_lib.client

  val create :
       logger:(type_:MessageType.t -> message:string -> unit -> unit Fiber.t)
    -> bin:Fpath.t
    -> unit
    -> (t, [> `No_process ]) result Fiber.t

  val run : t -> unit Fiber.t
end = struct
  type t =
    { pid : Pid.t
    ; input : in_channel
    ; output : out_channel
    ; io_thread : Scheduler.thread
    ; client : Ocamlformat_rpc_lib.client
    }

  let pid t = t.pid

  let thread t = t.io_thread

  let client t = t.client

  let supported_versions = [ "v1" ]

  let pick_client ~pid input output io_thread =
    let open Fiber.O in
    match
      Scheduler.async io_thread (fun () ->
          Ocamlformat_rpc_lib.pick_client ~pid input output supported_versions)
    with
    | Error `Stopped -> Code_error.raise "pick_client: stopped thread" []
    | Ok res -> (
      let+ res = Scheduler.await_no_cancel res in
      match res with
      | Ok s -> s
      | Error e -> Exn_with_backtrace.reraise e)

  let configure ~logger { io_thread; client; _ } =
    let open Fiber.O in
    (* We ask for 64 columns formatting as this appear to be the maximum size of
       VScode popups. TODO We should probably allow some flexibility for other
       editors that use the server. *)
    match
      Scheduler.async io_thread (fun () ->
          Ocamlformat_rpc_lib.config
            [ ("module-item-spacing", "compact"); ("margin", "63") ]
            client)
    with
    | Error `Stopped -> Fiber.return ()
    | Ok res -> (
      let* res = Scheduler.await_no_cancel res in
      match res with
      | Ok (Ok ()) -> Fiber.return ()
      | Ok (Error (`Msg msg)) ->
        let message =
          Printf.sprintf "An error occured while configuring ocamlformat: %s"
            msg
        in
        logger ~type_:MessageType.Warning ~message ()
      | Error e -> Exn_with_backtrace.reraise e)

  let create ~logger ~bin () =
    let bin = Fpath.to_string bin in
    let argv = [| bin |] in
    let pid, stdout, stdin =
      let stdin_i, stdin_o = Unix.pipe () in
      let stdout_i, stdout_o = Unix.pipe () in
      let pid = Unix.create_process bin argv stdin_i stdout_o Unix.stderr in
      Unix.close stdin_i;
      Unix.close stdout_o;
      (pid, stdout_i, stdin_o)
    in
    let open Fiber.O in
    let* io_thread = Scheduler.create_thread () in
    let input = Unix.in_channel_of_descr stdout in
    let output = Unix.out_channel_of_descr stdin in
    let* client = pick_client ~pid input output io_thread in
    match client with
    | Error (`Msg msg) ->
      (* The process did start but something went wrong when negociating the
         version so we need to kill it *)
      Unix.kill pid Sys.sigkill;
      Scheduler.stop io_thread;
      let* () =
        let message =
          Printf.sprintf
            "An error happened when negociating with the OCamlformat-RPC \
             server: %s"
            msg
        in
        logger ~type_:MessageType.Error ~message ()
      in
      Fiber.return @@ Error `No_process
    | Ok client ->
      let process =
        { pid = Pid.of_int pid; input; output; io_thread; client }
      in
      let* () = configure ~logger process in
      let+ () =
        let message =
          Printf.sprintf "Ocamlformat-RPC server started with PID %i" pid
        in
        logger ~type_:MessageType.Info ~message ()
      in
      Ok process

  let run { pid; input; output; io_thread; _ } =
    let open Fiber.O in
    let+ (_ : Unix.process_status) = Scheduler.wait_for_process pid in
    close_in_noerr input;
    close_out_noerr output;
    Scheduler.stop io_thread
end

type state =
  | Stopped
  | Waiting_for_init of
      { ask_init : unit Fiber.Ivar.t
      ; wait_init : unit Fiber.Ivar.t
      }
  | Running of Process.t

type t = state ref

let get_process t =
  let open Fiber.O in
  match !t with
  | Running p -> Fiber.return @@ Ok p
  | Stopped -> Fiber.return @@ Error `No_process
  | Waiting_for_init { ask_init; wait_init } -> (
    let* () = Fiber.Ivar.fill ask_init () in
    let+ () = Fiber.Ivar.read wait_init in
    match !t with
    | Running p -> Ok p
    | Stopped -> Error `No_process
    | Waiting_for_init _ ->
      Code_error.raise
        "Expected to receive `Started` or `Stopped` after mailing `Start`" [])

let format_type t ~typ =
  let open Fiber.O in
  let* p = get_process t in
  match p with
  | Error `No_process -> Fiber.return @@ Error `No_process
  | Ok p -> (
    match
      Scheduler.async (Process.thread p) (fun () ->
          Ocamlformat_rpc_lib.format typ (Process.client p))
    with
    | Error `Stopped -> Fiber.return @@ Error `No_process
    | Ok res -> (
      let+ res = Scheduler.await_no_cancel res in
      match res with
      | Ok s -> s
      | Error e -> Exn_with_backtrace.reraise e))

let create_state () =
  Waiting_for_init
    { ask_init = Fiber.Ivar.create (); wait_init = Fiber.Ivar.create () }

let create () = ref (create_state ())

let maybe_fill ivar x =
  let open Fiber.O in
  let* v = Fiber.Ivar.peek ivar in
  match v with
  | Some _ -> Fiber.return ()
  | None -> Fiber.Ivar.fill ivar x

let stop t =
  match !t with
  | Stopped -> Fiber.return ()
  | Waiting_for_init { wait_init; ask_init } ->
    (* If the server was never started we still need to fill the ivar for the
       fiber to finish *)
    t := Stopped;
    Fiber.fork_and_join_unit (maybe_fill wait_init) (maybe_fill ask_init)
  | Running process ->
    let pid = Pid.to_int (Process.pid process) in
    t := Stopped;
    Unix.kill pid Sys.sigkill;
    Fiber.return ()

let run_rpc ~logger ~bin t =
  let open Fiber.O in
  match !t with
  | Stopped -> Code_error.raise "ocamlformat already stopped" []
  | Running _ -> Code_error.raise "ocamlformat already running" []
  | Waiting_for_init { ask_init; wait_init } -> (
    let* () = Fiber.Ivar.read ask_init in
    match !t with
    | Stopped -> Fiber.return ()
    | Running _ -> assert false
    | Waiting_for_init _ -> (
      let* process = Process.create ~logger ~bin () in
      let* () = Fiber.Ivar.fill wait_init () in
      match process with
      | Error `No_process ->
        t := Stopped;
        Fiber.return ()
      | Ok process -> (
        t := Running process;
        let+ () = Process.run process in
        match !t with
        | Running _ -> t := create_state ()
        | _ -> ())))

let run ~logger t =
  let open Fiber.O in
  match Bin.which "ocamlformat-rpc" with
  | None ->
    t := Stopped;
    Fiber.return (Error `Binary_not_found)
  | Some bin ->
    let rec loop () =
      match !t with
      | Stopped -> Fiber.return (Ok ())
      | Running _ -> assert false
      | Waiting_for_init { ask_init; wait_init = _ } -> (
        (* We wait for the first query to start the server or for ocamllsp to
           exit *)
        let* () = Fiber.Ivar.read ask_init in
        match !t with
        | Waiting_for_init _ ->
          let* () = run_rpc ~logger ~bin t in
          (* We loop to automatically restart the server if it stopped *)
          loop ()
        | Running _ -> assert false
        | Stopped -> Fiber.return (Ok ()))
    in
    loop ()

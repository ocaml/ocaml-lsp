open Import
open Fiber.O

let type_option = [ "module-item-spacing", "compact"; "margin", "63" ]

module Ocamlformat_rpc = Ocamlformat_rpc_lib.Make (struct
    type 'a t = 'a Fiber.t

    let return a = Fiber.return a
    let ( >>= ) x f = Fiber.bind x ~f

    type ic = Lev_fiber_csexp.Session.t
    type oc = Lev_fiber_csexp.Session.t

    let read = Lev_fiber_csexp.Session.read
    let write t s = Lev_fiber_csexp.Session.write t s
  end)

module Process : sig
  type t

  val pid : t -> Pid.t
  val client : t -> Ocamlformat_rpc.client

  val create
    :  logger:(type_:MessageType.t -> message:string -> unit Fiber.t)
    -> bin:Fpath.t
    -> unit
    -> (t, [> `No_process ]) result Fiber.t

  val run : t -> unit Fiber.t
end = struct
  type t =
    { pid : Pid.t
    ; session : Lev_fiber_csexp.Session.t
    ; client : Ocamlformat_rpc.client
    }

  let pid t = t.pid
  let client t = t.client
  let supported_versions = [ "v2"; "v1" ]

  let pick_client ~pid session =
    Ocamlformat_rpc.pick_client ~pid session session supported_versions
  ;;

  let configure ~logger { client; _ } =
    (* We ask for 64 columns formatting as this appear to be the maximum size of
       VScode popups. TODO We should probably allow some flexibility for other
       editors that use the server. *)
    match client with
    | `V2 _ -> Fiber.return ()
    | `V1 client ->
      let* res = Ocamlformat_rpc.V1.Client.config type_option client in
      (match res with
       | Ok () -> Fiber.return ()
       | Error (`Msg msg) ->
         let message =
           Printf.sprintf "An error occured while configuring ocamlformat: %s" msg
         in
         logger ~type_:MessageType.Warning ~message)
  ;;

  let create ~logger ~bin () =
    let bin = Fpath.to_string bin in
    let* pid, stdout, stdin =
      let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
      let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
      let pid = Spawn.spawn ~prog:bin ~argv:[ bin ] ~stdin:stdin_i ~stdout:stdout_o () in
      Unix.close stdin_i;
      Unix.close stdout_o;
      let blockity =
        if Sys.win32
        then `Blocking
        else (
          Unix.set_nonblock stdin_o;
          Unix.set_nonblock stdout_i;
          `Non_blocking true)
      in
      let make fd what =
        let fd = Lev_fiber.Fd.create fd blockity in
        Lev_fiber.Io.create fd what
      in
      let* stdin = make stdin_o Output in
      let+ stdout = make stdout_i Input in
      pid, stdout, stdin
    in
    let session = Lev_fiber_csexp.Session.create ~socket:false stdout stdin in
    let* client = pick_client ~pid session in
    match client with
    | Error (`Msg msg) ->
      (* The process did start but something went wrong when negotiating the
         version so we need to kill it *)
      Unix.kill pid Sys.sigterm;
      let* () =
        let message =
          Printf.sprintf
            "An error happened when negotiating with the OCamlformat-RPC server: %s"
            msg
        in
        logger ~type_:MessageType.Error ~message
      in
      Fiber.return @@ Error `No_process
    | Ok client ->
      let process = { pid = Pid.of_int pid; session; client } in
      let* () = configure ~logger process in
      let+ () =
        let message = Printf.sprintf "Ocamlformat-RPC server started with PID %i" pid in
        logger ~type_:MessageType.Info ~message
      in
      Ok process
  ;;

  let run { pid; session; _ } =
    let+ (_ : Unix.process_status) = Lev_fiber.waitpid ~pid:(Pid.to_int pid) in
    Lev_fiber_csexp.Session.close session
  ;;
end

type state =
  | Disabled
  | Stopped
  | Waiting_for_init of
      { ask_init : unit Fiber.Ivar.t
      ; wait_init : unit Fiber.Ivar.t
      }
  | Running of Process.t

type t = state ref

let maybe_fill ivar x =
  let* v = Fiber.Ivar.peek ivar in
  match v with
  | Some _ -> Fiber.return ()
  | None -> Fiber.Ivar.fill ivar x
;;

let get_process t =
  match !t with
  | Running p -> Fiber.return @@ Ok p
  | Disabled | Stopped -> Fiber.return @@ Error `No_process
  | Waiting_for_init { ask_init; wait_init } ->
    let* () = maybe_fill ask_init () in
    let+ () = Fiber.Ivar.read wait_init in
    (match !t with
     | Running p -> Ok p
     | Disabled | Stopped -> Error `No_process
     | Waiting_for_init _ ->
       Code_error.raise
         "Expected to receive `Started` or `Stopped` after mailing `Start`"
         [])
;;

let format_type t ~typ =
  let* p = get_process t in
  match p with
  | Error `No_process -> Fiber.return @@ Error `No_process
  | Ok p ->
    (match Process.client p with
     | `V1 p -> Ocamlformat_rpc.V1.Client.format typ p
     | `V2 p ->
       let config = Some type_option in
       Ocamlformat_rpc.V2.Client.format
         ~format_args:{ Ocamlformat_rpc_lib.empty_args with config }
         typ
         p)
;;

let format_doc t doc =
  let txt = Document.source doc |> Msource.text in
  let path = Some (Document.uri doc |> Uri.to_path) in
  let* p = get_process t in
  match p with
  | Error `No_process -> Fiber.return @@ Error `No_process
  | Ok p ->
    (match Process.client p with
     | `V2 p ->
       let+ res =
         Ocamlformat_rpc.V2.Client.format
           ~format_args:Ocamlformat_rpc_lib.{ empty_args with path }
           txt
           p
       in
       Result.map res ~f:(fun to_ -> Diff.edit ~from:txt ~to_)
     | `V1 _ -> Fiber.return @@ Error `No_V2)
;;

let create_state () =
  Waiting_for_init { ask_init = Fiber.Ivar.create (); wait_init = Fiber.Ivar.create () }
;;

let create () = ref (if Sys.win32 then Disabled else create_state ())

let stop t =
  match !t with
  | Disabled | Stopped -> Fiber.return ()
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
;;

let run_rpc ~logger ~bin t =
  match !t with
  | Disabled -> assert false
  | Stopped -> Code_error.raise "ocamlformat already stopped" []
  | Running _ -> Code_error.raise "ocamlformat already running" []
  | Waiting_for_init { ask_init; wait_init } ->
    let* () = Fiber.Ivar.read ask_init in
    (match !t with
     | Disabled | Stopped -> Fiber.return ()
     | Running _ -> assert false
     | Waiting_for_init _ ->
       let* process = Process.create ~logger ~bin () in
       let* () = Fiber.Ivar.fill wait_init () in
       (match process with
        | Error `No_process ->
          t := Stopped;
          Fiber.return ()
        | Ok process ->
          t := Running process;
          let+ () = Process.run process in
          (match !t with
           | Running _ -> t := create_state ()
           | _ -> ())))
;;

let run ~logger t =
  match !t with
  | Disabled -> Fiber.return (Error `Disabled)
  | _ ->
    (match Bin.which "ocamlformat-rpc" with
     | None ->
       t := Stopped;
       Fiber.return (Error `Binary_not_found)
     | Some bin ->
       let rec loop () =
         match !t with
         | Disabled -> assert false
         | Stopped -> Fiber.return (Ok ())
         | Running _ -> assert false
         | Waiting_for_init { ask_init; wait_init = _ } ->
           (* We wait for the first query to start the server or for ocamllsp to
              exit *)
           let* () = Fiber.Ivar.read ask_init in
           (match !t with
            | Waiting_for_init _ ->
              let* () = run_rpc ~logger ~bin t in
              (* We loop to automatically restart the server if it stopped *)
              loop ()
            | Disabled | Running _ -> assert false
            | Stopped -> Fiber.return (Ok ()))
       in
       loop ())
;;

open Import
open Fiber.O

module Id = struct
  include Id
  module Table = Stdlib.MoreLabels.Hashtbl.Make (Id)
end

module Notify = struct
  type t =
    | Stop
    | Continue
end

module Sender = struct
  type t =
    { mutable called : bool
    ; for_ : Id.t
    ; send : Response.t -> unit Fiber.t
    }

  let make id send = { for_ = id; called = false; send }

  let send t (r : Response.t) : unit Fiber.t =
    Fiber.of_thunk (fun () ->
      if t.called
      then Code_error.raise "cannot send response twice" []
      else if not (Id.equal t.for_ r.id)
      then Code_error.raise "invalid id" []
      else t.called <- true;
      t.send r)
  ;;
end

exception Stopped of Request.t

let () =
  Printexc.register_printer (function
    | Stopped req ->
      let json = Request.yojson_of_t req in
      Some ("Session closed. Request will not be answered. " ^ Json.to_pretty_string json)
    | _ -> None)
;;

module Reply = struct
  type t =
    | Now of Response.t
    | Later of ((Response.t -> unit Fiber.t) -> unit Fiber.t)

  let now (r : Response.t) = Now r
  let later f = Later f

  let send (t : t) sender =
    match t with
    | Now r -> Sender.send sender r
    | Later f -> f (fun (r : Response.t) -> Sender.send sender r)
  ;;
end

module Make (Chan : sig
    type t

    val send : t -> Packet.t list -> unit Fiber.t
    val recv : t -> Packet.t option Fiber.t
    val close : t -> [ `Read | `Write ] -> unit Fiber.t
  end) =
struct
  type 'state t =
    { chan : Chan.t
    ; on_request : ('state, Request.t) context -> (Reply.t * 'state) Fiber.t
    ; on_notification : ('state, Notification.t) context -> (Notify.t * 'state) Fiber.t
    ; pending : (Response.t, [ `Stopped | `Cancelled ]) result Fiber.Ivar.t Id.Table.t
    ; stopped : unit Fiber.Ivar.t
    ; name : string
    ; mutable running : bool
    ; mutable tick : int
    ; mutable state : 'state
    ; mutable pending_requests_stopped : bool
    }

  and ('a, 'message) context = 'a t * 'message

  type cancel = unit Fiber.t

  let fire cancel = cancel

  module Context = struct
    type nonrec ('a, 'id) t = ('a, 'id) context

    let message = snd
    let session = fst
    let state t = (session t).state
  end

  let log t = Log.log ~section:t.name

  let response_of_exn id (exn : Exn_with_backtrace.t) =
    let error =
      match exn.exn with
      | Jsonrpc.Response.Error.E resp -> resp
      | _ ->
        let data = exn |> Exn_with_backtrace.to_dyn |> Json.of_dyn in
        Response.Error.make ~code:InternalError ~data ~message:"uncaught exception" ()
    in
    Response.error id error
  ;;

  let on_request_fail ctx : (Reply.t * _) Fiber.t =
    let req : Request.t = Context.message ctx in
    let state = Context.state ctx in
    let error = Response.Error.make ~code:InternalError ~message:"not implemented" () in
    Fiber.return (Reply.now (Response.error req.id error), state)
  ;;

  let state t = t.state

  let on_notification_fail ctx =
    let state = Context.state ctx in
    Fiber.return (Notify.Continue, state)
  ;;

  let stop_pending_requests t =
    Fiber.of_thunk (fun () ->
      if t.pending_requests_stopped
      then Fiber.return ()
      else (
        t.pending_requests_stopped <- true;
        let to_cancel =
          Id.Table.fold t.pending ~init:[] ~f:(fun ~key:_ ~data:x acc -> x :: acc)
        in
        Id.Table.clear t.pending;
        Fiber.parallel_iter to_cancel ~f:(fun ivar ->
          let* res = Fiber.Ivar.peek ivar in
          match res with
          | Some _ -> Fiber.return ()
          | None -> Fiber.Ivar.fill ivar (Error `Stopped))))
  ;;

  let create
    ?(on_request = on_request_fail)
    ?(on_notification = on_notification_fail)
    ~name
    chan
    state
    =
    let pending = Id.Table.create 10 in
    { chan
    ; on_request
    ; on_notification
    ; pending
    ; stopped = Fiber.Ivar.create ()
    ; name
    ; running = false
    ; tick = 0
    ; state
    ; pending_requests_stopped = false
    }
  ;;

  let stopped t = Fiber.Ivar.read t.stopped

  let stop t =
    Fiber.fork_and_join_unit
      (fun () -> Chan.close t.chan `Read)
      (fun () -> stop_pending_requests t)
  ;;

  let close t =
    Fiber.all_concurrently_unit
      [ Chan.close t.chan `Read
      ; Chan.close t.chan `Write
      ; Fiber.Ivar.fill t.stopped ()
      ; stop_pending_requests t
      ]
  ;;

  let run t =
    let send_response resp =
      log t (fun () ->
        Log.msg "sending response" [ "response", Response.yojson_of_t resp ]);
      Chan.send t.chan [ Response resp ]
    in
    let later = Fiber.Pool.create () in
    let rec loop () =
      t.tick <- t.tick + 1;
      log t (fun () -> Log.msg "new tick" [ "tick", `Int t.tick ]);
      let* res = Chan.recv t.chan in
      log t (fun () -> Log.msg "waited for something" []);
      match res with
      | None -> Fiber.return ()
      | Some packet ->
        (match packet with
         | Notification r -> on_notification r
         | Request r -> on_request r
         | Response r ->
           let* () = Fiber.Pool.task later ~f:(fun () -> on_response r) in
           loop ()
         | Batch_call _ -> Code_error.raise "batch requests aren't supported" []
         | Batch_response _ -> assert false)
    and on_response r =
      let log (what : string) =
        log t (fun () -> Log.msg ("response " ^ what) [ "r", Response.yojson_of_t r ])
      in
      match Id.Table.find_opt t.pending r.id with
      | None ->
        log "dropped";
        Fiber.return ()
      | Some ivar ->
        log "acknowledged";
        Id.Table.remove t.pending r.id;
        let* resp = Fiber.Ivar.peek ivar in
        (match resp with
         | Some _ -> Fiber.return ()
         | None -> Fiber.Ivar.fill ivar (Ok r))
    and on_request (r : Request.t) =
      log t (fun () -> Log.msg "handling request" []);
      let* result =
        let sent = ref false in
        Fiber.map_reduce_errors
          (module Stdune.Monoid.Unit)
          ~on_error:(fun exn_bt ->
            if !sent
            then (* TODO log *)
              Fiber.return ()
            else (
              let response = response_of_exn r.id exn_bt in
              sent := true;
              Fiber.Pool.task later ~f:(fun () -> send_response response)))
          (fun () -> t.on_request (t, r))
      in
      log t (fun () -> Log.msg "received result" []);
      match result with
      | Error () -> loop ()
      | Ok (reply, state) ->
        t.state <- state;
        let sender = Sender.make r.id send_response in
        let* () =
          Fiber.Pool.task later ~f:(fun () ->
            let+ res =
              Fiber.map_reduce_errors
                (module Stdune.Monoid.Unit)
                (fun () -> Reply.send reply sender)
                ~on_error:(fun exn_bt ->
                  if sender.called
                  then (* TODO we should log *)
                    Fiber.return ()
                  else (
                    let resp = response_of_exn r.id exn_bt in
                    Sender.send sender resp))
            in
            match res with
            | Ok () -> ()
            | Error () -> ())
        in
        loop ()
    and on_notification (r : Notification.t) : unit Fiber.t =
      let* res = Fiber.collect_errors (fun () -> t.on_notification (t, r)) in
      match res with
      | Ok (next, state) ->
        t.state <- state;
        (match next with
         | Stop -> Fiber.return ()
         | Continue -> loop ())
      | Error errors ->
        Format.eprintf
          "Uncaught error when handling notification:@.%a@.Error:@.%s@."
          Json.pp
          (Notification.yojson_of_t r)
          (Dyn.to_string (Dyn.list Exn_with_backtrace.to_dyn errors));
        loop ()
    in
    Fiber.of_thunk (fun () ->
      t.running <- true;
      let* () =
        Fiber.fork_and_join_unit
          (fun () ->
            let* () = loop () in
            Fiber.Pool.stop later)
          (fun () -> Fiber.Pool.run later)
      in
      close t)
  ;;

  let check_running t =
    if not t.running then Code_error.raise "jsonrpc must be running" []
  ;;

  let notification t (n : Notification.t) =
    Fiber.of_thunk (fun () ->
      check_running t;
      Chan.send t.chan [ Notification n ])
  ;;

  let register_request_ivar t id ivar =
    match Id.Table.find_opt t.pending id with
    | Some _ -> Code_error.raise "duplicate request id" []
    | None -> Id.Table.add t.pending ~key:id ~data:ivar
  ;;

  let read_request_ivar req ivar =
    let+ res = Fiber.Ivar.read ivar in
    match res with
    | Ok s -> s
    | Error `Cancelled -> assert false
    | Error `Stopped -> raise (Stopped req)
  ;;

  let request t (req : Request.t) =
    Fiber.of_thunk (fun () ->
      check_running t;
      let* () = Chan.send t.chan [ Request req ] in
      let ivar = Fiber.Ivar.create () in
      register_request_ivar t req.id ivar;
      read_request_ivar req ivar)
  ;;

  let request_with_cancel t (req : Request.t) =
    let ivar = Fiber.Ivar.create () in
    let cancel = Fiber.Ivar.fill ivar (Error `Cancelled) in
    let resp =
      Fiber.of_thunk (fun () ->
        check_running t;
        let* () =
          let+ () = Chan.send t.chan [ Request req ] in
          register_request_ivar t req.id ivar
        in
        let+ res = Fiber.Ivar.read ivar in
        match res with
        | Ok s -> `Ok s
        | Error `Cancelled -> `Cancelled
        | Error `Stopped -> raise (Stopped req))
    in
    cancel, resp
  ;;

  module Batch = struct
    type response =
      Jsonrpc.Request.t
      * (Jsonrpc.Response.t, [ `Stopped | `Cancelled ]) result Fiber.Ivar.t

    type t = [ `Notification of Notification.t | `Request of response ] list ref

    let await (req, resp) = read_request_ivar req resp
    let create () = ref []
    let notification t n = t := `Notification n :: !t

    let request (t : t) r : response =
      let ivar = Fiber.Ivar.create () in
      let resp = r, ivar in
      t := `Request resp :: !t;
      resp
    ;;
  end

  let submit (t : _ t) (batch : Batch.t) =
    Fiber.of_thunk (fun () ->
      check_running t;
      let pending = !batch in
      batch := [];
      let pending, ivars =
        List.fold_left pending ~init:([], []) ~f:(fun (pending, ivars) ->
            function
            | `Notification n -> Jsonrpc.Packet.Notification n :: pending, ivars
            | `Request ((r : Request.t), ivar) ->
              Jsonrpc.Packet.Request r :: pending, (r.id, ivar) :: ivars)
      in
      List.iter ivars ~f:(fun (id, ivar) -> register_request_ivar t id ivar);
      Chan.send t.chan pending)
  ;;
end

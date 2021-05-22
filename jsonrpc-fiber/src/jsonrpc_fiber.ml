open Import

module Id = struct
  include Id

  let to_dyn t : Dyn.t =
    match t with
    | `Int x -> Int x
    | `String x -> String x
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
    if t.called then
      Code_error.raise "cannot send response twice" []
    else if not (Id.equal t.for_ r.id) then
      Code_error.raise "invalid id" []
    else (
      t.called <- true;
      t.send r
    )
end

exception Stopped of Message.request

let () =
  Printexc.register_printer (function
    | Stopped req ->
      let json = Message.yojson_of_request req in
      Some
        ("Session closed. Request will not be answered. "
       ^ Json.to_pretty_string json)
    | _ -> None)

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
end

module Make (Chan : sig
  type t

  val send : t -> packet list -> unit Fiber.t

  val recv : t -> packet option Fiber.t

  val close : t -> [ `Read | `Write ] -> unit Fiber.t
end) =
struct
  type 'state t =
    { chan : Chan.t
    ; on_request : ('state, Id.t) context -> (Reply.t * 'state) Fiber.t
    ; on_notification : ('state, unit) context -> (Notify.t * 'state) Fiber.t
    ; pending : (Id.t, (Response.t, [ `Stopped ]) result Fiber.Ivar.t) Table.t
    ; stopped : unit Fiber.Ivar.t
    ; name : string
    ; mutable running : bool
    ; mutable tick : int
    ; mutable state : 'state
    ; stop_pending_requests : unit Fiber.t Lazy.t
    }

  and ('a, 'id) context = 'a t * 'id Message.t

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
        Response.Error.make ~code:InternalError ~data
          ~message:"uncaught exception" ()
    in
    Response.error id error

  let on_request_fail ctx : (Reply.t * _) Fiber.t =
    let req : Message.request = Context.message ctx in
    let state = Context.state ctx in
    let error =
      Response.Error.make ~code:InternalError ~message:"not implemented" ()
    in
    Fiber.return (Reply.now (Response.error req.id error), state)

  let state t = t.state

  let stopped t = Fiber.Ivar.read t.stopped

  let stop t =
    Fiber.fork_and_join_unit
      (fun () -> Chan.close t.chan `Read)
      (fun () -> Lazy.force t.stop_pending_requests)

  let close t =
    Fiber.parallel_iter
      ~f:(fun f -> f ())
      [ (fun () -> Chan.close t.chan `Read)
      ; (fun () -> Chan.close t.chan `Write)
      ; (fun () -> Fiber.Ivar.fill t.stopped ())
      ; (fun () -> Lazy.force t.stop_pending_requests)
      ]

  let run t =
    let open Fiber.O in
    let send_response resp =
      log t (fun () ->
          Log.msg "sending response" [ ("response", Response.yojson_of_t resp) ]);
      Chan.send t.chan [ Response resp ]
    in
    let later = Fiber.Pool.create () in
    let rec loop () =
      t.tick <- t.tick + 1;
      log t (fun () -> Log.msg "new tick" [ ("tick", `Int t.tick) ]);
      let* res = Chan.recv t.chan in
      match res with
      | None -> Fiber.return ()
      | Some packet -> (
        match packet with
        | Message r -> on_message r
        | Response r ->
          let* () = Fiber.Pool.task later ~f:(fun () -> on_response r) in
          loop ())
    and on_message (r : _ Message.t) =
      log t (fun () ->
          let what =
            match r.id with
            | None -> "notification"
            | Some _ -> "request"
          in
          Log.msg ("received " ^ what) [ ("r", Message.yojson_of_either r) ]);
      match r.id with
      | Some id -> on_request { r with id }
      | None -> on_notification { r with id = () }
    and on_response r =
      let log (what : string) =
        log t (fun () ->
            Log.msg ("response " ^ what) [ ("r", Response.yojson_of_t r) ])
      in
      match Table.find t.pending r.id with
      | None ->
        log "dropped";
        Fiber.return ()
      | Some ivar ->
        log "acknowledged";
        Table.remove t.pending r.id;
        Fiber.Ivar.fill ivar (Ok r)
    and on_request (r : Id.t Message.t) =
      let* result =
        let sent = ref false in
        Fiber.map_reduce_errors
          (module Stdune.Monoid.Unit)
          ~on_error:(fun exn_bt ->
            if !sent then
              (* TODO log *)
              Fiber.return ()
            else
              let response = response_of_exn r.id exn_bt in
              sent := true;
              Fiber.Pool.task later ~f:(fun () -> send_response response))
          (fun () -> t.on_request (t, r))
      in
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
                    if sender.called then
                      (* TODO we should log *)
                      Fiber.return ()
                    else
                      let resp = response_of_exn r.id exn_bt in
                      Sender.send sender resp)
              in
              match res with
              | Ok () -> ()
              | Error () -> ())
        in
        loop ()
    and on_notification (r : unit Message.t) : unit Fiber.t =
      let* res = Fiber.collect_errors (fun () -> t.on_notification (t, r)) in
      match res with
      | Ok (next, state) -> (
        t.state <- state;
        match next with
        | Stop -> Fiber.return ()
        | Continue -> loop ())
      | Error errors ->
        Format.eprintf
          "Uncaught error when handling notification:@.%a@.Error:@.%s@." Json.pp
          (Message.yojson_of_notification r)
          (Dyn.to_string (Dyn.Encoder.list Exn_with_backtrace.to_dyn errors));
        loop ()
    in
    t.running <- true;
    let* () =
      Fiber.fork_and_join_unit
        (fun () ->
          let* () = loop () in
          Fiber.Pool.stop later)
        (fun () -> Fiber.Pool.run later)
    in
    close t

  let on_notification_fail ctx =
    let state = Context.state ctx in
    Fiber.return (Notify.Continue, state)

  let make_stop_pending_requests pending =
    lazy
      (let to_cancel = Table.fold pending ~init:[] ~f:(fun x acc -> x :: acc) in
       Table.clear pending;
       Fiber.parallel_iter to_cancel ~f:(fun ivar ->
           Fiber.Ivar.fill ivar (Error `Stopped)))

  let create ?(on_request = on_request_fail)
      ?(on_notification = on_notification_fail) ~name chan state =
    let pending = Table.create (module Id) 10 in
    { chan
    ; on_request
    ; on_notification
    ; pending
    ; stopped = Fiber.Ivar.create ()
    ; name
    ; running = false
    ; tick = 0
    ; state
    ; stop_pending_requests = make_stop_pending_requests pending
    }

  let check_running t =
    (* TODO we should also error out when making requests after a disconnect. *)
    if not t.running then Code_error.raise "jsonrpc must be running" []

  let notification t (req : Message.notification) =
    check_running t;
    let req = { req with Message.id = None } in
    Chan.send t.chan [ Message req ]

  let register_request_ivar t id ivar =
    match Table.find t.pending id with
    | Some _ -> Code_error.raise "duplicate request id" []
    | None -> Table.add_exn t.pending id ivar

  let read_request_ivar req ivar =
    let open Fiber.O in
    let+ res = Fiber.Ivar.read ivar in
    match res with
    | Ok s -> s
    | Error `Stopped -> raise (Stopped req)

  let request t (req : Message.request) =
    check_running t;
    let open Fiber.O in
    let* () =
      let req = { req with Message.id = Some req.id } in
      Chan.send t.chan [ Message req ]
    in
    let ivar = Fiber.Ivar.create () in
    register_request_ivar t req.id ivar;
    read_request_ivar req ivar

  module Batch = struct
    type t =
      [ `Notification of Message.notification
      | `Request of
        Message.request * (Response.t, [ `Stopped ]) result Fiber.Ivar.t
      ]
      list
      ref

    let create () = ref []

    let notification t n = t := `Notification n :: !t

    let request t r =
      let ivar = Fiber.Ivar.create () in
      t := `Request (r, ivar) :: !t;
      read_request_ivar r ivar
  end

  let submit (t : _ t) (batch : Batch.t) =
    check_running t;
    let pending = !batch in
    batch := [];
    let pending, ivars =
      List.fold_left pending ~init:([], []) ~f:(fun (pending, ivars) -> function
        | `Notification n ->
          (Jsonrpc.Message { n with Message.id = None } :: pending, ivars)
        | `Request ((r : Message.request), ivar) ->
          ( Jsonrpc.Message { r with Message.id = Some r.id } :: pending
          , (r.id, ivar) :: ivars ))
    in
    List.iter ivars ~f:(fun (id, ivar) -> register_request_ivar t id ivar);
    Chan.send t.chan pending
end

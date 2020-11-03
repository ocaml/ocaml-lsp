open Import
open Jsonrpc

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

module Make (Chan : sig
  type t

  val send : t -> packet -> unit Fiber.t

  val recv : t -> packet option Fiber.t

  val close : t -> unit Fiber.t
end) =
struct
  type 'state t =
    { chan : Chan.t
    ; on_request : ('state, Id.t) context -> (Response.t * 'state) Fiber.t
    ; on_notification : ('state, unit) context -> (Notify.t * 'state) Fiber.t
    ; pending : (Id.t, Response.t Fiber.Ivar.t) Table.t
    ; stop_requested : unit Fiber.Ivar.t
    ; stopped : unit Fiber.Ivar.t
    ; name : string
    ; mutable running : bool
    ; mutable tick : int
    ; mutable state : 'state
    }

  and ('a, 'id) context = 'a t * 'id Message.t

  module Context = struct
    type nonrec ('a, 'id) t = ('a, 'id) context

    let message = snd

    let session = fst

    let state t = (session t).state
  end

  let log t = Log.log ~section:t.name

  let response_of_result id = function
    | Ok (x, _) -> x
    | Error exns ->
      let data : Json.t =
        `List
          (List.map
             ~f:(fun e -> e |> Exn_with_backtrace.to_dyn |> Json.of_dyn)
             exns)
      in
      let error =
        Response.Error.make ~code:InternalError ~data
          ~message:"uncaught exception" ()
      in
      Response.error id error

  let on_request_fail ctx : (Response.t * _) Fiber.t =
    let req : Message.request = Context.message ctx in
    let state = Context.state ctx in
    let error =
      Response.Error.make ~code:InternalError ~message:"not implemented" ()
    in
    Fiber.return (Response.error req.id error, state)

  let state t = t.state

  let stopped t = Fiber.Ivar.read t.stopped

  let stop t =
    let open Fiber.O in
    let* res = Fiber.Ivar.peek t.stop_requested in
    Fiber.fork_and_join_unit
      (fun () ->
        match res with
        | None -> Fiber.return ()
        | Some _ -> Fiber.Ivar.fill t.stop_requested ())
      (fun () -> stopped t)

  let close t =
    let open Fiber.O in
    let* () = Chan.close t.chan in
    Fiber.Ivar.fill t.stopped ()

  let run t =
    let stop_requested = Fiber.Ivar.read t.stop_requested in
    let open Fiber.O in
    let on_notification (r : unit Message.t) =
      let+ res = Fiber.collect_errors (fun () -> t.on_notification (t, r)) in
      match res with
      | Ok (next, state) ->
        t.state <- state;
        next
      | Error errors ->
        Format.eprintf
          "Uncaught error when handling notification:@.%a@.Error:@.%s@." Json.pp
          (Message.yojson_of_notification r)
          (Dyn.to_string (Dyn.Encoder.list Exn_with_backtrace.to_dyn errors));
        Notify.Continue
    in
    let on_request (r : Id.t Message.t) =
      let* resp = Fiber.collect_errors (fun () -> t.on_request (t, r)) in
      let jsonrpc_resp = response_of_result r.id resp in
      log t (fun () ->
          Log.msg "sending response"
            [ ("response", Response.yojson_of_t jsonrpc_resp) ]);
      let+ () = Chan.send t.chan (Response jsonrpc_resp) in
      ( match resp with
      | Ok (_, state) -> t.state <- state
      | Error _ -> () );
      Notify.Continue
    in
    let rec loop () =
      t.tick <- t.tick + 1;
      log t (fun () -> Log.msg "new tick" [ ("tick", `Int t.tick) ]);
      let* res =
        Fiber.fork_and_race
          (fun () -> Chan.recv t.chan)
          (fun () -> stop_requested)
      in
      match res with
      | Either.Right () ->
        log t (fun () -> Log.msg "shutdown granted" []);
        Chan.close t.chan
      | Left None -> Fiber.return ()
      | Left (Some packet) -> (
        let* next_step =
          match packet with
          | Message r -> on_message r
          | Response r ->
            let+ () = on_response r in
            Notify.Continue
        in
        match next_step with
        | Notify.Continue -> loop ()
        | Stop -> Fiber.return () )
    and on_message (r : _ Message.t) =
      log t (fun () ->
          let what =
            match r.id with
            | None -> "notification"
            | Some _ -> "request"
          in
          Log.msg ("received " ^ what) [ ("r", Message.yojson_of_either r) ]);
      match r.id with
      | None -> on_notification { r with id = () }
      | Some id -> on_request { r with id }
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
        Fiber.Ivar.fill ivar r
    in
    t.running <- true;
    let* () = loop () in
    close t

  let on_notification_fail ctx =
    let state = Context.state ctx in
    Fiber.return (Notify.Continue, state)

  let create ?(on_request = on_request_fail)
      ?(on_notification = on_notification_fail) ~name chan state =
    { chan
    ; on_request
    ; on_notification
    ; pending = Table.create (module Id) 10
    ; stop_requested = Fiber.Ivar.create ()
    ; stopped = Fiber.Ivar.create ()
    ; name
    ; running = false
    ; tick = 0
    ; state
    }

  let notification t (req : Message.notification) =
    if not t.running then Code_error.raise "jsonrpc must be running" [];
    let req = { req with Message.id = None } in
    Chan.send t.chan (Message req)

  let request t (req : Message.request) =
    if not t.running then Code_error.raise "jsonrpc must be running" [];
    let open Fiber.O in
    let* () =
      let req = { req with Message.id = Some req.id } in
      Chan.send t.chan (Message req)
    in
    let ivar = Fiber.Ivar.create () in
    Table.add_exn t.pending req.id ivar;
    Fiber.Ivar.read ivar
end

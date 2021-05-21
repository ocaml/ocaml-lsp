open Import
module Id = Jsonrpc.Id
module Response = Jsonrpc.Response
module Session = Jsonrpc_fiber.Make (Fiber_io)

module Reply = struct
  type 'r t =
    | Now of 'r
    | Later of (('r -> unit Fiber.t) -> unit Fiber.t)

  let now r = Now r

  let later f = Later f

  let to_jsonrpc t id to_json : Jsonrpc_fiber.Reply.t =
    let f x = Jsonrpc.Response.ok id (to_json x) in
    match t with
    | Now r -> Jsonrpc_fiber.Reply.now (f r)
    | Later k -> Jsonrpc_fiber.Reply.later (fun send -> k (fun r -> send (f r)))
end

module Cancel = struct
  type state =
    | Pending of { mutable callbacks : (unit -> unit Fiber.t) list }
    | Finished

  type t = state ref

  let var = Fiber.Var.create ()

  let register f =
    let open Fiber.O in
    let+ cancel = Fiber.Var.get var in
    match cancel with
    | None -> ()
    | Some cancel -> (
      match !cancel with
      | Finished -> ()
      | Pending p -> p.callbacks <- f :: p.callbacks)

  let create () = ref (Pending { callbacks = [] })

  let destroy t = t := Finished

  let cancel t =
    match !t with
    | Finished -> Fiber.return ()
    | Pending { callbacks } ->
      t := Finished;
      Fiber.parallel_iter callbacks ~f:(fun f -> f ())
end

module State = struct
  type t =
    | Waiting_for_init
    | Running
    | Closed
end

module type S = sig
  type 'a out_request

  type out_notification

  type 'a in_request

  type in_notification

  type 'state t

  module Handler : sig
    type 'a session

    type 'state on_request =
      { on_request :
          'a. 'state session -> 'a in_request -> ('a Reply.t * 'state) Fiber.t
      }

    type 'state t

    val make :
         ?on_request:'state on_request
      -> ?on_notification:('state session -> in_notification -> 'state Fiber.t)
      -> unit
      -> 'state t
  end
  with type 'a session := 'a t

  val state : 'a t -> 'a

  val make : 'state Handler.t -> Fiber_io.t -> 'state -> 'state t

  val stop : _ t -> unit Fiber.t

  val request : _ t -> 'resp out_request -> 'resp Fiber.t

  val notification : _ t -> out_notification -> unit Fiber.t

  val on_cancel : (unit -> unit Fiber.t) -> unit Fiber.t

  module Batch : sig
    type t

    type _ session

    val create : _ session -> t

    val notification : t -> out_notification -> unit

    val request : t -> 'resp out_request -> 'resp Fiber.t

    val submit : t -> unit Fiber.t
  end
  with type 'a session := 'a t
end

module type Request_intf = sig
  type 'a t

  type packed = E : 'r t -> packed

  val of_jsonrpc : Jsonrpc.Message.request -> (packed, string) result

  val yojson_of_result : 'a t -> 'a -> Json.t

  val to_jsonrpc_request : 'a t -> id:Id.t -> Jsonrpc.Message.request

  val response_of_json : 'a t -> Json.t -> 'a
end

module type Notification_intf = sig
  type t

  val of_jsonrpc : Jsonrpc.Message.notification -> (t, string) result

  val to_jsonrpc : t -> Jsonrpc.Message.notification
end

module Make (Initialize : sig
  type t
end)
(Out_request : Request_intf)
(Out_notification : Notification_intf)
(In_request : Request_intf)
(In_notification : Notification_intf) =
struct
  type 'a out_request = 'a Out_request.t

  type 'a in_request = 'a In_request.t

  type out_notification = Out_notification.t

  type in_notification = In_notification.t

  type 'state t =
    { io : Fiber_io.t
    ; (* mutable only to initialiaze this record *)
      mutable session : 'state Session.t Fdecl.t
    ; (* Internal state of the session *)
      mutable state : State.t
    ; (* Filled when the server is initialied *)
      initialized : Initialize.t Fiber.Ivar.t
    ; mutable req_id : int
    ; pending : (Jsonrpc.Id.t, Cancel.t) Table.t
    ; detached : Fiber.Pool.t
    }

  and 'state on_request =
    { on_request :
        'a. 'state t -> 'a in_request -> ('a Reply.t * 'state) Fiber.t
    }

  and 'state handler =
    { h_on_request : 'state on_request
    ; h_on_notification : 'state t -> In_notification.t -> 'state Fiber.t
    }

  module Handler = struct
    type nonrec 'state on_request = 'state on_request =
      { on_request :
          'a. 'state t -> 'a in_request -> ('a Reply.t * 'state) Fiber.t
      }

    type nonrec 'state t = 'state handler =
      { h_on_request : 'state on_request
      ; h_on_notification : 'state t -> In_notification.t -> 'state Fiber.t
      }

    let on_notification_default _ _ =
      Format.eprintf "dropped notification@.%!";
      assert false

    let make ?on_request ?(on_notification = on_notification_default) () =
      let h_on_request =
        match on_request with
        | Some t -> t
        | None -> assert false
      in
      { h_on_request; h_on_notification = on_notification }
  end

  let state t = Session.state (Fdecl.get t.session)

  let to_jsonrpc (type state) (t : state t) h_on_request h_on_notification =
    let open Fiber.O in
    let on_request (ctx : (state, Id.t) Session.Context.t) =
      let req = Session.Context.message ctx in
      let state = Session.Context.state ctx in
      match In_request.of_jsonrpc req with
      | Error message ->
        let code = Jsonrpc.Response.Error.Code.InvalidParams in
        let error = Jsonrpc.Response.Error.make ~code ~message () in
        Fiber.return
          (Jsonrpc_fiber.Reply.now (Jsonrpc.Response.error req.id error), state)
      | Ok (In_request.E r) ->
        let cancel = Cancel.create () in
        Table.set t.pending req.id cancel;
        let+ response, state =
          Fiber.finalize
            (fun () ->
              Fiber.Var.set Cancel.var cancel (fun () ->
                  h_on_request.on_request t r))
            ~finally:(fun () ->
              Cancel.destroy cancel;
              Table.remove t.pending req.id;
              Fiber.return ())
        in
        let reply =
          Reply.to_jsonrpc response req.id (In_request.yojson_of_result r)
        in
        (reply, state)
    in
    let on_notification ctx =
      let r = Session.Context.message ctx in
      match In_notification.of_jsonrpc r with
      | Ok r -> h_on_notification t r
      | Error error ->
        Log.log ~section:"lsp" (fun () ->
            Log.msg "Invalid notification" [ ("error", `String error) ]);
        let state = Session.Context.state ctx in
        Fiber.return (Jsonrpc_fiber.Notify.Continue, state)
    in
    (on_request, on_notification)

  let make ~name h_on_request h_on_notification io state =
    let t =
      { io
      ; state = Waiting_for_init
      ; session = Fdecl.create Dyn.Encoder.opaque
      ; initialized = Fiber.Ivar.create ()
      ; req_id = 1
      ; pending = Table.create (module Jrpc_id) 32
      ; detached = Fiber.Pool.create ()
      }
    in
    let session =
      let on_request, on_notification =
        to_jsonrpc t h_on_request h_on_notification
      in
      Session.create ~on_request ~on_notification ~name io state
    in
    Fdecl.set t.session session;
    t

  let gen_request (type r) (t : _ t) (req : r Out_request.t) k : r Fiber.t =
    let id = `Int t.req_id in
    let jsonrpc_request =
      t.req_id <- t.req_id + 1;
      Out_request.to_jsonrpc_request req ~id
    in
    let open Fiber.O in
    let+ (resp : Jsonrpc.Response.t) = k jsonrpc_request in
    match resp.result |> Result.map ~f:(Out_request.response_of_json req) with
    | Ok s -> s
    | Error e -> raise (Jsonrpc.Response.Error.E e)

  let request (type r) (t : _ t) (req : r Out_request.t) : r Fiber.t =
    gen_request t req (Session.request (Fdecl.get t.session))

  let notification (t : _ t) (n : Out_notification.t) : unit Fiber.t =
    let jsonrpc_request = Out_notification.to_jsonrpc n in
    Session.notification (Fdecl.get t.session) jsonrpc_request

  module Batch = struct
    type session = E : 'a t -> session

    type t =
      { batch : Session.Batch.t
      ; session : session
      }

    let create session =
      { batch = Session.Batch.create (); session = E session }

    let notification t n =
      let n = Out_notification.to_jsonrpc n in
      Session.Batch.notification t.batch n

    let request (t : t) req =
      let (E session) = t.session in
      gen_request session req (Session.Batch.request t.batch)

    let submit { session = E session; batch } =
      let t = Fdecl.get session.session in
      Session.submit t batch
  end

  let initialized t = Fiber.Ivar.read t.initialized

  let stop t =
    let open Fiber.O in
    let+ () = Session.stop (Fdecl.get t.session) in
    t.state <- Closed

  let start_loop t =
    Fiber.fork_and_join_unit
      (fun () ->
        let open Fiber.O in
        let* () = Session.run (Fdecl.get t.session) in
        Fiber.Pool.stop t.detached)
      (fun () -> Fiber.Pool.run t.detached)

  let handle_cancel_req t id =
    let open Fiber.O in
    let+ () =
      match Table.find t.pending id with
      | None -> Fiber.return ()
      | Some id -> Fiber.Pool.task t.detached ~f:(fun () -> Cancel.cancel id)
    in
    (Jsonrpc_fiber.Notify.Continue, state t)

  let on_cancel = Cancel.register
end

module Client = struct
  open Types
  include
    Make (InitializeResult) (Client_request) (Client_notification)
      (Server_request)
      (Server_notification)

  let h_on_notification handler t n =
    match n with
    | Server_notification.CancelRequest id -> handle_cancel_req t id
    | _ ->
      let open Fiber.O in
      let+ res = handler.h_on_notification t n in
      (Jsonrpc_fiber.Notify.Continue, res)

  let make handler io =
    let h_on_notification = h_on_notification handler in
    make ~name:"client" handler.h_on_request h_on_notification io

  let start (t : _ t) (p : InitializeParams.t) =
    assert (t.state = Waiting_for_init);
    let open Fiber.O in
    let loop = start_loop t in
    let init () =
      let* resp = request t (Client_request.Initialize p) in
      Log.log ~section:"client" (fun () ->
          let resp = InitializeResult.yojson_of_t resp in
          Log.msg "initialized" [ ("resp", resp) ]);
      t.state <- Running;
      Fiber.Ivar.fill t.initialized resp
    in
    Fiber.fork_and_join_unit (fun () -> loop) init
end

module Server = struct
  open Types
  include
    Make (InitializeParams) (Server_request) (Server_notification)
      (Client_request)
      (Client_notification)

  let h_on_notification handler t n =
    let open Fiber.O in
    match n with
    | Client_notification.Exit ->
      Log.log ~section:"server" (fun () ->
          Log.msg "received exit notification" []);
      let* () = stop t in
      Fiber.return (Jsonrpc_fiber.Notify.Stop, state t)
    | Client_notification.CancelRequest id -> handle_cancel_req t id
    | _ ->
      if t.state = Waiting_for_init then
        let state = state t in
        Fiber.return (Jsonrpc_fiber.Notify.Continue, state)
      else
        let+ state = handler.h_on_notification t n in
        (Jsonrpc_fiber.Notify.Continue, state)

  let on_request handler t in_r =
    let open Fiber.O in
    match Client_request.E in_r with
    | Client_request.E (Client_request.Initialize i) ->
      if t.state = Waiting_for_init then (
        let* result = handler.h_on_request.on_request t in_r in
        t.state <- Running;
        (* XXX Should we wait for the waiter of initialized to finish? *)
        let* () = Fiber.Ivar.fill t.initialized i in
        Fiber.return result
      ) else
        let code = Response.Error.Code.InvalidRequest in
        let message = "already initialized" in
        raise
          (Jsonrpc.Response.Error.E
             (Jsonrpc.Response.Error.make ~code ~message ()))
    | Client_request.E _ ->
      if t.state = Waiting_for_init then
        let code = Response.Error.Code.ServerNotInitialized in
        let message = "not initialized" in
        raise
          (Jsonrpc.Response.Error.E
             (Jsonrpc.Response.Error.make ~code ~message ()))
      else
        handler.h_on_request.on_request t in_r

  let make (type s) (handler : s Handler.t) io (initial_state : s) =
    let h_on_request : _ Handler.on_request =
      { Handler.on_request = (fun t x -> on_request handler t x) }
    in
    let h_on_notification = h_on_notification handler in
    make ~name:"server" h_on_request h_on_notification io initial_state

  let start t = start_loop t
end

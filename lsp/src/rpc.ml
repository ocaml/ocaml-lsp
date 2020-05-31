open Import
open Jsonrpc

module Message = struct
  type ('request, 'notif) t =
    | Request of Id.t * 'request
    | Notification of 'notif

  let of_jsonrpc req notif (packet : Request.t) =
    let open Result.O in
    match packet.id with
    | None ->
      let+ n = notif packet in
      Notification n
    | Some id ->
      let+ req = req packet in
      Request (id, req)
end

module Raw_io = struct
  let { Logger.log } = Logger.for_section "lsp_io"

  type t =
    { ic : in_channel
    ; oc : out_channel
    }

  let close { ic; oc } =
    close_in_noerr ic;
    close_out_noerr oc

  let make ic oc =
    set_binary_mode_in ic true;
    set_binary_mode_out oc true;
    { ic; oc }

  let send { oc; ic = _ } (packet : packet) =
    let json = Jsonrpc.yojson_of_packet packet in
    log ~title:Logger.Title.LocalDebug "send: %a"
      (fun () -> Yojson.Safe.pretty_to_string ~std:false)
      json;
    let data = Yojson.Safe.to_string json in
    let content_length = String.length data in
    let header = Header.create ~content_length in
    Header.write header oc;
    output_string oc data;
    flush oc

  let read_content ic =
    match Header.read ic with
    | exception End_of_file -> None
    | header ->
      let len = Header.content_length header in
      let buffer = Bytes.create len in
      let rec read_loop read =
        if read < len then
          let n = input ic buffer read (len - read) in
          read_loop (read + n)
      in
      let () = read_loop 0 in
      Some (Bytes.to_string buffer)

  let read { ic; oc = _ } : Json.t option =
    read_content ic |> Option.map ~f:Yojson.Safe.from_string

  let read (t : t) : packet option =
    let open Option.O in
    let+ json = read t in
    let open Json.O in
    let req json = Request (Jsonrpc.Request.t_of_yojson json) in
    let resp json = Response (Jsonrpc.Response.t_of_yojson json) in
    (req <|> resp) json
end

module Stream_io = struct
  open Fiber_stream

  type t = Jsonrpc.packet In.t * Jsonrpc.packet Out.t

  let close (_, o) = Out.write o None

  let send (_, o) p = Out.write o (Some p)

  let recv (i, _) = In.read i

  let make s io =
    let r = Scheduler.create_thread s in
    let w = Scheduler.create_thread s in
    let i =
      Fiber_stream.In.create (fun () ->
          let open Fiber.O in
          let+ res = Scheduler.async r (fun () -> Raw_io.read io) in
          Result.ok_exn res)
    in
    let o =
      let open Fiber.O in
      Fiber_stream.Out.create (fun t ->
          let+ res =
            Scheduler.async w
              ( match t with
              | None -> fun () -> Raw_io.close io
              | Some p -> fun () -> Raw_io.send io p )
          in
          Result.ok_exn res)
    in
    (i, o)
end

module Session = Session (Stream_io)

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
          'a.    'state session -> 'a in_request
          -> ('a * 'state, Response.Error.t) result Fiber.t
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

  val make : 'state Handler.t -> Stream_io.t -> 'state -> 'state t

  val stop : _ t -> unit Fiber.t

  val request :
    _ t -> 'resp out_request -> ('resp, Response.Error.t) result Fiber.t

  val notification : _ t -> out_notification -> unit Fiber.t
end

module type Request_intf = sig
  type 'a t

  type packed = E : 'r t -> packed

  val of_jsonrpc : Jsonrpc.Request.t -> (packed, string) result

  val yojson_of_result : 'a t -> 'a -> Json.t option

  val to_jsonrpc_request : 'a t -> id:Id.t -> Jsonrpc.Request.t

  val response_of_json : 'a t -> Json.t -> 'a
end

module type Notification_intf = sig
  type t

  val of_jsonrpc : Jsonrpc.Request.t -> (t, string) result

  val to_jsonrpc : t -> Jsonrpc.Request.t
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
    { handler : 'state handler
    ; io : Stream_io.t
    ; mutable session : 'state Session.t option
    ; mutable state : State.t
    ; initialized : Initialize.t Fiber.Ivar.t
    ; mutable req_id : int
    }

  and 'state on_request =
    { on_request :
        'a.    'state t -> 'a in_request
        -> ('a * 'state, Response.Error.t) result Fiber.t
    }

  and 'state handler =
    { h_on_request : 'state on_request
    ; h_on_notification : 'state t -> In_notification.t -> 'state Fiber.t
    }

  module Handler = struct
    type nonrec 'state on_request = 'state on_request =
      { on_request :
          'a.    'state t -> 'a in_request
          -> ('a * 'state, Response.Error.t) result Fiber.t
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

  let state t = Session.state (Option.value_exn t.session)

  let to_jsonrpc (type state) (t : state t)
      ({ Handler.h_on_request; h_on_notification } : state Handler.t) =
    let open Fiber.O in
    let on_request (ctx : state Session.Context.t) :
        (Response.t * state) Fiber.t =
      let req = Session.Context.request ctx in
      match In_request.of_jsonrpc req with
      | Error e -> Code_error.raise e []
      | Ok (In_request.E r) -> (
        let id = Option.value_exn req.id in
        let+ response =
          let session = Session.Context.session ctx in
          h_on_request.on_request { t with session = Some session } r
        in
        match response with
        | Error e ->
          let state = Session.Context.state ctx in
          (Response.error id e, state)
        | Ok (response, state) ->
          let json = In_request.yojson_of_result r response in
          let result = Option.value_exn json in
          let response = Response.ok id result in
          (response, state) )
    in
    let on_notification ctx : state Fiber.t =
      let r = Session.Context.request ctx in
      match In_notification.of_jsonrpc r with
      | Error e -> Code_error.raise e []
      | Ok r ->
        let session = Session.Context.session ctx in
        h_on_notification { t with session = Some session } r
    in
    (on_request, on_notification)

  let make ~name (handler : _ Handler.t) io state =
    let t =
      { handler
      ; io
      ; state = Waiting_for_init
      ; session = None
      ; initialized = Fiber.Ivar.create ()
      ; req_id = 1
      }
    in
    let session =
      let on_request, on_notification = to_jsonrpc t handler in
      Session.create ~on_request ~on_notification ~name io state
    in
    t.session <- Some session;
    t

  let request (type r) (t : _ t) (req : r Out_request.t) :
      (r, Jsonrpc.Response.Error.t) result Fiber.t =
    let id = Either.Right t.req_id in
    let jsonrpc_request =
      t.req_id <- t.req_id + 1;
      Out_request.to_jsonrpc_request req ~id
    in
    let open Fiber.O in
    let+ resp = Session.request (Option.value_exn t.session) jsonrpc_request in
    resp.result |> Result.map ~f:(Out_request.response_of_json req)

  let notification (t : _ t) (n : Out_notification.t) : unit Fiber.t =
    let jsonrpc_request = Out_notification.to_jsonrpc n in
    Session.notification (Option.value_exn t.session) jsonrpc_request

  let initialized t = Fiber.Ivar.read t.initialized

  let stop t =
    let open Fiber.O in
    let+ () = Session.stop (Option.value_exn t.session) in
    t.state <- Closed

  let start_loop t = Session.run (Option.value_exn t.session)
end

module Client = struct
  open Types
  include Make (InitializeResult) (Client_request) (Client_notification)
            (Server_request)
            (Server_notification)

  let make handler io = make ~name:"client" handler io

  let start (t : _ t) (p : InitializeParams.t) =
    assert (t.state = Waiting_for_init);
    let open Fiber.O in
    let loop = start_loop t in
    let init () =
      let* resp = request t (Client_request.Initialize p) in
      Log.log ~section:"client" (fun () ->
          let resp =
            match resp with
            | Ok s -> InitializeResult.yojson_of_t s
            | Error s -> Jsonrpc.Response.Error.yojson_of_t s
          in
          Log.msg "initialized" [ ("resp", resp) ]);
      t.state <- Running;
      match resp with
      | Ok resp -> Fiber.Ivar.fill t.initialized resp
      | Error _ -> Fiber.return ()
    in
    let* () = Scheduler.detach (Scheduler.scheduler ()) init in
    loop
end

module Server = struct
  open Types
  include Make (InitializeParams) (Server_request) (Server_notification)
            (Client_request)
            (Client_notification)

  let make handler io state =
    let t = make ~name:"server" handler io state in
    let handler =
      let h_on_request : _ Handler.on_request =
        { Handler.on_request =
            (fun t in_r ->
              let open Fiber.O in
              let initialize () =
                match Client_request.E in_r with
                | Client_request.E (Client_request.Initialize i) ->
                  Fiber.Ivar.fill t.initialized i
                | _ -> Fiber.return ()
              in
              let* result = t.handler.h_on_request.on_request t in_r in
              let+ () = initialize () in
              result)
        }
      in
      { t.handler with h_on_request }
    in
    { t with handler }

  let start t = start_loop t
end

module Io = struct
  include Raw_io

  let send t x = Fiber.return (send t x)

  let read t = Fiber.return (read t)
end

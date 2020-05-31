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

  module Handler : sig
    type t

    type on_request =
      { on_request : 'a. 'a in_request -> ('a, Response.Error.t) result Fiber.t
      }

    val make :
         ?on_request:on_request
      -> ?on_notification:(in_notification -> unit Fiber.t)
      -> unit
      -> t
  end

  type t

  val make : Handler.t -> Stream_io.t -> t

  val stop : t -> unit Fiber.t

  val request :
    t -> 'resp out_request -> ('resp, Response.Error.t) result Fiber.t

  val notification : t -> out_notification -> unit Fiber.t
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

  module Handler = struct
    type on_request =
      { on_request : 'a. 'a in_request -> ('a, Response.Error.t) result Fiber.t
      }

    type t =
      { on_request : on_request
      ; on_notification : In_notification.t -> unit Fiber.t
      }

    let on_notification_default _ =
      Format.eprintf "dropped notification@.%!";
      Fiber.return ()

    let make ?on_request ?(on_notification = on_notification_default) () =
      let on_request =
        match on_request with
        | Some t -> t
        | None -> assert false
      in
      { on_request; on_notification }

    let to_jsonrpc { on_request; on_notification } =
      let on_request _ (req : Request.t) : Response.t Fiber.t =
        match In_request.of_jsonrpc req with
        | Error e -> Code_error.raise e []
        | Ok (In_request.E r) -> (
          let open Fiber.O in
          let id = Option.value_exn req.id in
          let+ response = on_request.on_request r in
          match response with
          | Error e -> Response.error id e
          | Ok response ->
            let json = In_request.yojson_of_result r response in
            let result = Option.value_exn json in
            Response.ok id result )
      in
      let on_notification _ r =
        match In_notification.of_jsonrpc r with
        | Error e -> Code_error.raise e []
        | Ok r -> on_notification r
      in
      (on_request, on_notification)
  end

  type t =
    { handler : Handler.t
    ; io : Stream_io.t
    ; session : Session.t
    ; mutable state : State.t
    ; initialized : Initialize.t Fiber.Ivar.t
    ; mutable req_id : int
    }

  let make ~name handler io =
    let session =
      let on_request, on_notification = Handler.to_jsonrpc handler in
      Session.create ~on_request ~on_notification ~name io
    in
    { handler
    ; io
    ; state = Waiting_for_init
    ; session
    ; initialized = Fiber.Ivar.create ()
    ; req_id = 1
    }

  let request (type r) (t : t) (req : r Out_request.t) :
      (r, Jsonrpc.Response.Error.t) result Fiber.t =
    let id = Either.Right t.req_id in
    let jsonrpc_request =
      t.req_id <- t.req_id + 1;
      Out_request.to_jsonrpc_request req ~id
    in
    let open Fiber.O in
    let+ resp = Session.request t.session jsonrpc_request in
    resp.result |> Result.map ~f:(Out_request.response_of_json req)

  let notification (t : t) (n : Out_notification.t) : unit Fiber.t =
    let jsonrpc_request = Out_notification.to_jsonrpc n in
    Session.notification t.session jsonrpc_request

  let initialized t = Fiber.Ivar.read t.initialized

  let stop t =
    let open Fiber.O in
    let+ () = Session.stop t.session in
    t.state <- Closed

  let start_loop t = Session.run t.session
end

module Client = struct
  open Types
  include Make (InitializeResult) (Client_request) (Client_notification)
            (Server_request)
            (Server_notification)

  let make handler io = make ~name:"client" handler io

  let start (t : t) (p : InitializeParams.t) =
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

  let make handler io =
    let t = make ~name:"server" handler io in
    let handler =
      let on_request : Handler.on_request =
        { Handler.on_request =
            (fun in_r ->
              let open Fiber.O in
              let initialize () =
                match Client_request.E in_r with
                | Client_request.E (Client_request.Initialize i) ->
                  Fiber.Ivar.fill t.initialized i
                | _ -> Fiber.return ()
              in
              let* result = t.handler.on_request.on_request in_r in
              let+ () = initialize () in
              result)
        }
      in
      { t.handler with on_request }
    in
    { t with handler }

  let start t = start_loop t
end

module Io = struct
  include Raw_io

  let send t x = Fiber.return (send t x)

  let read t = Fiber.return (read t)
end

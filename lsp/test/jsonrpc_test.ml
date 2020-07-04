open! Lsp.Import
open Lsp
open Lsp.Fiber_stream
open Jsonrpc

module Stream_chan = struct
  type t = Jsonrpc.packet In.t * Jsonrpc.packet Out.t

  let close (_, o) = Out.write o None

  let send (_, o) p = Out.write o (Some p)

  let recv (i, _) = In.read i
end

module Session = Jsonrpc.Session (Stream_chan)
open Lsp.Import

let print_json json = print_endline (Json.to_string json)

let () =
  Printexc.record_backtrace true;
  let response =
    let i = ref 0 in
    fun () ->
      incr i;
      `Int !i
  in
  let on_request ctx =
    let req = Session.Context.message ctx in
    let state = Session.Context.state ctx in
    Fiber.return (Jsonrpc.Response.ok req.id (response ()), state)
  in
  let on_notification ctx =
    let n = Session.Context.message ctx in
    if n.method_ = "raise" then failwith "special failure";
    let json = Message.yojson_of_notification n in
    print_endline ">> received notification";
    print_json json;
    Fiber.return (Notify.Continue, ())
  in
  let responses = ref [] in
  let initial_requests =
    let request ?params id method_ =
      Jsonrpc.Message.create ?params ~id:(Some id) ~method_ ()
    in
    let notification ?params method_ =
      Jsonrpc.Message.create ~id:None ?params ~method_ ()
    in
    [ Message (request (Either.Right 10) "foo")
    ; Message (request ~params:`Null (Either.Left "testing") "bar")
    ; Message (notification ~params:`Null "notif1")
    ; Message (notification ~params:`Null "notif2")
    ; Message (notification ~params:`Null "raise")
    ]
  in
  let reqs_in, reqs_out = Fiber_stream.pipe () in
  let chan =
    let out = Out.of_ref responses in
    (reqs_in, out)
  in
  let session =
    Session.create ~on_notification ~on_request ~name:"test" chan ()
  in
  let scheduler = Scheduler.create () in
  let write_reqs () =
    let open Fiber.O in
    let* () =
      Fiber.sequential_iter initial_requests ~f:(fun req ->
          Fiber_stream.Out.write reqs_out (Some req))
    in
    Fiber_stream.Out.write reqs_out None
  in
  let (), () =
    Scheduler.run scheduler
      (Fiber.fork_and_join write_reqs (fun () -> Session.run session))
  in
  List.rev !responses
  |> List.iter ~f:(fun packet ->
         let json = Jsonrpc.yojson_of_packet packet in
         print_json json);
  print_endline "finished test"

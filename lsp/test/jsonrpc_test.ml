open Lsp.Import
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

let print_json json = print_endline (Yojson.Safe.pretty_to_string json)

let () =
  Printexc.record_backtrace true;
  let response =
    let i = ref 0 in
    fun () ->
      incr i;
      `Int !i
  in
  let on_request ctx =
    let req = Session.Context.request ctx in
    let id = Option.value_exn req.id in
    Fiber.return (Jsonrpc.Response.ok id (response ()))
  in
  let on_notification ctx =
    let n = Session.Context.request ctx in
    if n.method_ = "raise" then failwith "special failure";
    let json = Request.yojson_of_t n in
    print_endline ">> received notification";
    print_json json;
    Fiber.return ()
  in
  let responses = ref [] in
  let initial_requests =
    [ Request (Jsonrpc.Request.create ~id:(Right 10) ~method_:"foo" ())
    ; Request
        (Jsonrpc.Request.create ~params:`Null ~id:(Left "testing")
           ~method_:"bar" ())
    ; Request (Jsonrpc.Request.create ~params:`Null ~method_:"notif1" ())
    ; Request (Jsonrpc.Request.create ~params:`Null ~method_:"notif2" ())
    ; Request (Jsonrpc.Request.create ~params:`Null ~method_:"raise" ())
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

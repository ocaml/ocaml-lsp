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
  let response =
    let i = ref 0 in
    fun () ->
      incr i;
      `Int !i
  in
  let on_request (req : Request.t) =
    let id = Option.value_exn req.id in
    Fiber.return (Jsonrpc.Response.ok id (response ()))
  in
  let on_notification n =
    let json = Request.yojson_of_t n in
    print_json json;
    Fiber.return ()
  in
  let responses = ref [] in
  let chan =
    let in_ =
      In.of_list
        [ Request (Jsonrpc.Request.create ~id:(Right 10) ~method_:"foo" ())
        ; Request
            (Jsonrpc.Request.create ~params:`Null ~id:(Left "testing")
               ~method_:"bar" ())
        ]
    in
    let out = Out.of_ref responses in
    (in_, out)
  in
  let session = Session.create ~on_notification ~on_request chan in
  let scheduler = Scheduler.create () in
  let () = Scheduler.run scheduler (Session.run session) in
  List.rev !responses
  |> List.iter ~f:(fun packet ->
         let json =
           match packet with
           | Request r -> Request.yojson_of_t r
           | Response r -> Response.yojson_of_t r
         in
         print_json json);
  print_endline "finished test"

open Fiber.O
open Lsp
open Lsp.Types
open Lsp_fiber

module Test = struct
  module Client = struct
    let run
          ?(capabilities = ClientCapabilities.create ())
          ?on_request
          ?on_notification
          state
          (in_, out)
      =
      let initialize = InitializeParams.create ~capabilities () in
      let client =
        let stream_io = Lsp_fiber.Fiber_io.make in_ out in
        let handler = Client.Handler.make ?on_request ?on_notification () in
        Client.make handler stream_io state
      in
      client, Client.start client initialize
    ;;
  end

  module Server = struct
    let run ?on_request ?on_notification state (in_, out) =
      let server =
        let stream_io = Fiber_io.make in_ out in
        let handler = Server.Handler.make ?on_request ?on_notification () in
        Server.make handler stream_io state
      in
      let running =
        Fiber.finalize
          ~finally:(fun () -> Server.close server)
          (fun () -> Server.start server)
      in
      server, running
    ;;
  end
end

let pipe () = Lev_fiber.Io.pipe ~cloexec:true ()

let test make_client make_server =
  Printexc.record_backtrace false;
  let run () =
    let* client_in, server_out = pipe () in
    let* server_in, client_out = pipe () in
    let server () = make_server (server_in, server_out) in
    let client () = make_client (client_in, client_out) in
    let+ () = Fiber.fork_and_join_unit server client in
    print_endline "Successful termination of test"
  in
  Lev_fiber.run run |> Lev_fiber.Error.ok_exn;
  print_endline "[TEST] finished"
;;

let json_pp = Yojson.Safe.pretty_print ~std:false

module End_to_end_client = struct
  let on_request (type a) _ (_ : a Server_request.t) =
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make ~message:"not implemented" ~code:InternalError ())
  ;;

  let on_notification (client : _ Client.t) n =
    let state = Client.state client in
    let received_notification = state in
    let req = Server_notification.to_jsonrpc n in
    Format.eprintf
      "client: received notification@.%a@.%!"
      json_pp
      (Jsonrpc.Notification.yojson_of_t req);
    let+ () = Fiber.Ivar.fill received_notification () in
    Format.eprintf "client: filled received_notification@.%!";
    state
  ;;

  let run io =
    let detached = Fiber.Pool.create () in
    let received_notification = Fiber.Ivar.create () in
    let client, running =
      let on_request = { Client.Handler.on_request } in
      Test.Client.run ~on_request ~on_notification received_notification io
    in
    let init () : unit Fiber.t =
      Format.eprintf "client: waiting for initialization@.%!";
      let* (_ : InitializeResult.t) = Client.initialized client in
      Format.eprintf "client: server initialized. sending request@.%!";
      let cancel = Fiber.Cancel.create () in
      let* () =
        Fiber.Pool.task detached ~f:(fun () ->
          Format.eprintf
            "client: waiting to receive notification before cancelling the request@.%!";
          let* () = Fiber.Ivar.read received_notification in
          Format.eprintf "client: received notification, cancelling the request@.%!";
          Fiber.Cancel.fire cancel)
      in
      let* res_cancel =
        let req_cancel =
          Client_request.ExecuteCommand
            (ExecuteCommandParams.create ~command:"cmd_cancel" ())
        in
        Format.eprintf "client: sending request cmd_cancel@.%!";
        Client.request_with_cancel client cancel req_cancel
      and* res_reply =
        let req_reply =
          Client_request.ExecuteCommand
            (ExecuteCommandParams.create ~command:"cmd_reply" ())
        in
        Format.eprintf "client: sending request cmd_reply@.%!";
        Client.request client req_reply
      in
      (match res_cancel with
       | `Cancelled -> Format.eprintf "client: req_cancel got cancelled@.%!"
       | `Ok _ -> assert false);
      Format.eprintf
        "client: Successfully executed req_reply with result:@.%a@."
        json_pp
        res_reply;
      Format.eprintf "client: sending request to shutdown@.%!";
      let* () = Fiber.Pool.stop detached in
      Client.notification client Exit
    in
    Fiber.fork_and_join_unit init (fun () ->
      Fiber.fork_and_join_unit (fun () -> running) (fun () -> Fiber.Pool.run detached))
  ;;
end

module End_to_end_server = struct
  type status =
    | Started
    | Initialized

  let on_request =
    let on_request (type a) self (req : a Client_request.t) : (a Rpc.Reply.t * _) Fiber.t =
      let state = Server.state self in
      let _status, detached = state in
      match req with
      | Client_request.Initialize _ ->
        let capabilities = ServerCapabilities.create () in
        let result = InitializeResult.create ~capabilities () in
        Format.eprintf "server: initializing server@.";
        Format.eprintf "server: returning initialization result@.%!";
        Fiber.return (Rpc.Reply.now result, (Initialized, detached))
      | Client_request.ExecuteCommand { command; _ } ->
        Format.eprintf "server: received command %s@.%!" command;
        let* () =
          match command with
          | "cmd_cancel" ->
            Fiber.Pool.task detached ~f:(fun () ->
              Format.eprintf "server: sending message notification to client@.%!";
              let msg =
                ShowMessageParams.create
                  ~type_:MessageType.Info
                  ~message:"notifying client"
              in
              Server.notification self (Server_notification.ShowMessage msg))
          | _ -> Fiber.return ()
        in
        let* () = Fiber.Pool.stop detached in
        let result = `String "successful execution" in
        let* cancel = Rpc.Server.cancel_token () in
        (match command with
         | "cmd_cancel" ->
           let+ () = Lev_fiber.Timer.sleepf 0.2 in
           ( Rpc.Reply.later (fun k ->
               let* cancel = Rpc.Server.cancel_token () in
               (* Make sure that we can access the cancel token in a Reply
                  response *)
               assert (Option.is_some cancel);
               k result)
           , state )
         | _ ->
           (* Make sure that we can access the cancel token in a Now response *)
           assert (Option.is_some cancel);
           Fiber.return (Rpc.Reply.now result, state))
      | _ ->
        Jsonrpc.Response.Error.raise
          (Jsonrpc.Response.Error.make ~code:InternalError ~message:"not supported" ())
    in
    { Server.Handler.on_request }
  ;;

  let on_notification self _ =
    let state = Server.state self in
    Format.eprintf "server: Received notification@.%!";
    Fiber.return state
  ;;

  let run io =
    let detached = Fiber.Pool.create () in
    let _server, running =
      Test.Server.run ~on_request ~on_notification (Started, detached) io
    in
    Fiber.fork_and_join_unit (fun () -> running) (fun () -> Fiber.Pool.run detached)
  ;;
end

let%expect_test "server enforces initialization ordering" =
  let run () =
    let notifications = ref [] in
    let* client_in, server_out = pipe () in
    let* server_in, client_out = pipe () in
    let server_io = Lsp_fiber.Fiber_io.make server_in server_out in
    let client_io = Lsp_fiber.Fiber_io.make client_in client_out in
    let on_request =
      let on_request
        : type response.
          unit Server.t
          -> response Client_request.t
          -> (response Rpc.Reply.t * unit) Fiber.t
        =
        fun _ request ->
        match request with
        | Initialize _ ->
          let capabilities = ServerCapabilities.create () in
          let result = InitializeResult.create ~capabilities () in
          Fiber.return (Rpc.Reply.now result, ())
        | _ ->
          Jsonrpc.Response.Error.raise
            (Jsonrpc.Response.Error.make ~code:InternalError ~message:"unexpected" ())
      in
      { Server.Handler.on_request }
    in
    let server =
      let on_notification _ notification =
        let method_ = (Client_notification.to_jsonrpc notification).method_ in
        notifications := method_ :: !notifications;
        Fiber.return ()
      in
      let handler = Server.Handler.make ~on_request ~on_notification () in
      Server.make handler server_io ()
    in
    let request ~id ~method_ params =
      let params = Jsonrpc.Structured.t_of_yojson params in
      let request = Jsonrpc.Request.create ~id:(`Int id) ~method_ ~params () in
      let* () = Fiber_io.send client_io [ Jsonrpc.Packet.Request request ] in
      let+ packet = Fiber_io.recv client_io in
      match packet with
      | Some (Jsonrpc.Packet.Response response) -> response
      | Some (Notification _ | Request _ | Batch_call _ | Batch_response _) | None ->
        failwith "expected a response"
    in
    let print_response label (response : Jsonrpc.Response.t) =
      match response.result with
      | Ok _ -> Printf.printf "%s: ok\n" label
      | Error error ->
        Printf.printf "%s: %s\n" label (Jsonrpc.Response.Error.Code.to_string error.code)
    in
    let send_notification notification =
      Fiber_io.send client_io [ Jsonrpc.Packet.Notification notification ]
    in
    let configuration_notification () =
      send_notification
        (Jsonrpc.Notification.create
           ~method_:"workspace/didChangeConfiguration"
           ~params:(`Assoc [ "settings", `Assoc [] ])
           ())
    in
    let exchange () =
      let* () = configuration_notification () in
      let* response = request ~id:0 ~method_:"initialize" (`List []) in
      print_response "malformed initialize" response;
      let command = ExecuteCommandParams.create ~command:"before-init" () in
      let* response =
        request
          ~id:1
          ~method_:"workspace/executeCommand"
          (ExecuteCommandParams.yojson_of_t command)
      in
      print_response "before initialize" response;
      let initialize =
        InitializeParams.create ~capabilities:(ClientCapabilities.create ()) ()
      in
      let* response =
        request ~id:2 ~method_:"initialize" (InitializeParams.yojson_of_t initialize)
      in
      print_response "initialize" response;
      let* () = configuration_notification () in
      let* response = request ~id:3 ~method_:"workspace/executeCommand" (`List []) in
      print_response "malformed request" response;
      let* response =
        request ~id:4 ~method_:"initialize" (InitializeParams.yojson_of_t initialize)
      in
      print_response "initialize again" response;
      List.iter (Printf.printf "notification handled: %s\n") (List.rev !notifications);
      send_notification (Jsonrpc.Notification.create ~method_:"exit" ())
    in
    Fiber.all_concurrently_unit [ Server.start server; exchange () ]
  in
  Lev_fiber.run run |> Lev_fiber.Error.ok_exn;
  [%expect
    {|
    malformed initialize: InvalidParams
    before initialize: ServerNotInitialized
    initialize: ok
    malformed request: InvalidParams
    initialize again: InvalidRequest
    notification handled: workspace/didChangeConfiguration |}]
;;

let%expect_test "remote request cancellation returns Cancelled" =
  let make_server io =
    let on_request =
      let on_request
        : type response.
          unit Server.t
          -> response Client_request.t
          -> (response Rpc.Reply.t * unit) Fiber.t
        =
        fun _ request ->
        match request with
        | Initialize _ ->
          let capabilities = ServerCapabilities.create () in
          let result = InitializeResult.create ~capabilities () in
          Fiber.return (Rpc.Reply.now result, ())
        | ExecuteCommand _ ->
          Jsonrpc.Response.Error.raise
            (Jsonrpc.Response.Error.make
               ~message:"cancelled remotely"
               ~code:RequestCancelled
               ())
        | _ ->
          Jsonrpc.Response.Error.raise
            (Jsonrpc.Response.Error.make ~message:"unexpected" ~code:InternalError ())
      in
      { Server.Handler.on_request }
    in
    let _, running = Test.Server.run ~on_request () io in
    running
  in
  let make_client io =
    let client, running = Test.Client.run () io in
    let request () =
      let* (_ : InitializeResult.t) = Client.initialized client in
      let command = ExecuteCommandParams.create ~command:"cancel" () in
      let cancel = Fiber.Cancel.create () in
      let* result =
        Fiber.collect_errors (fun () ->
          Client.request_with_cancel client cancel (Client_request.ExecuteCommand command))
      in
      (match result with
       | Error [ _ ] -> print_endline "request_with_cancel: raised"
       | Ok `Cancelled -> print_endline "request_with_cancel: cancelled"
       | Ok (`Ok _) | Error _ -> print_endline "request_with_cancel: unexpected");
      Client.notification client Exit
    in
    Fiber.fork_and_join_unit (fun () -> running) request
  in
  test make_client make_server;
  [%expect
    {|
    request_with_cancel: cancelled
    Successful termination of test
    [TEST] finished |}]
;;

let%expect_test "duplicate incoming request IDs are rejected" =
  let run () =
    let request_started = Fiber.Ivar.create () in
    let release_requests = Fiber.Ivar.create () in
    let* client_in, server_out = pipe () in
    let* server_in, client_out = pipe () in
    let server_io = Lsp_fiber.Fiber_io.make server_in server_out in
    let client_io = Lsp_fiber.Fiber_io.make client_in client_out in
    let on_request =
      let on_request
        : type response.
          unit Server.t
          -> response Client_request.t
          -> (response Rpc.Reply.t * unit) Fiber.t
        =
        fun _ request ->
        match request with
        | Initialize _ ->
          let capabilities = ServerCapabilities.create () in
          let result = InitializeResult.create ~capabilities () in
          Fiber.return (Rpc.Reply.now result, ())
        | ExecuteCommand _ ->
          let* started = Fiber.Ivar.peek request_started in
          let* () =
            match started with
            | Some () -> Fiber.return ()
            | None -> Fiber.Ivar.fill request_started ()
          in
          let reply =
            Rpc.Reply.later (fun send ->
              let* () = Fiber.Ivar.read release_requests in
              send (`String "done"))
          in
          Fiber.return (reply, ())
        | _ ->
          Jsonrpc.Response.Error.raise
            (Jsonrpc.Response.Error.make ~code:InternalError ~message:"unexpected" ())
      in
      { Server.Handler.on_request }
    in
    let server =
      let handler = Server.Handler.make ~on_request () in
      Server.make handler server_io ()
    in
    let send_request request =
      Fiber_io.send client_io [ Jsonrpc.Packet.Request request ]
    in
    let receive_response () =
      let+ packet = Fiber_io.recv client_io in
      match packet with
      | Some (Jsonrpc.Packet.Response response) -> response
      | Some (Notification _ | Request _ | Batch_call _ | Batch_response _) | None ->
        failwith "expected a response"
    in
    let exchange () =
      let initialize =
        InitializeParams.create ~capabilities:(ClientCapabilities.create ()) ()
      in
      let params =
        InitializeParams.yojson_of_t initialize |> Jsonrpc.Structured.t_of_yojson
      in
      let initialize =
        Jsonrpc.Request.create ~id:(`Int 0) ~method_:"initialize" ~params ()
      in
      let* () = send_request initialize in
      let* (_ : Jsonrpc.Response.t) = receive_response () in
      let command = ExecuteCommandParams.create ~command:"slow" () in
      let params =
        ExecuteCommandParams.yojson_of_t command |> Jsonrpc.Structured.t_of_yojson
      in
      let request =
        Jsonrpc.Request.create ~id:(`Int 1) ~method_:"workspace/executeCommand" ~params ()
      in
      let* () = send_request request in
      let* () = Fiber.Ivar.read request_started in
      let* () = send_request request in
      let* duplicate = receive_response () in
      let* () = Fiber.Ivar.fill release_requests () in
      let* original = receive_response () in
      let classify (response : Jsonrpc.Response.t) =
        match response.result with
        | Ok _ -> "ok"
        | Error error -> Jsonrpc.Response.Error.Code.to_string error.code
      in
      let responses =
        List.sort String.compare [ classify duplicate; classify original ]
      in
      Printf.printf "responses: %s\n" (String.concat ", " responses);
      let exit = Jsonrpc.Notification.create ~method_:"exit" () in
      Fiber_io.send client_io [ Jsonrpc.Packet.Notification exit ]
    in
    Fiber.all_concurrently_unit [ Server.start server; exchange () ]
  in
  Lev_fiber.run run |> Lev_fiber.Error.ok_exn;
  [%expect {| responses: InvalidRequest, ok |}]
;;

let%expect_test "concurrent lazy fibers share the computation" =
  let run () =
    let runs = ref 0 in
    let lazy_fiber =
      Lazy_fiber.create (fun () ->
        incr runs;
        Fiber.return 42)
    in
    let first = Lazy_fiber.force lazy_fiber in
    let second = Lazy_fiber.force lazy_fiber in
    let+ result =
      Fiber.collect_errors (fun () ->
        let+ _, _ = Fiber.fork_and_join (fun () -> first) (fun () -> second) in
        ())
    in
    let result =
      match result with
      | Ok () -> "ok"
      | Error [ _ ] -> "error"
      | Error _ -> "unexpected"
    in
    Printf.printf "result: %s; runs: %d\n" result !runs
  in
  Lev_fiber.run run |> Lev_fiber.Error.ok_exn;
  [%expect {| result: ok; runs: 1 |}]
;;

let%expect_test "concurrent failing lazy fibers share the computation" =
  let run () =
    let runs = ref 0 in
    let lazy_fiber =
      Lazy_fiber.create (fun () ->
        incr runs;
        failwith "failure")
    in
    let first = Lazy_fiber.force lazy_fiber in
    let second = Lazy_fiber.force lazy_fiber in
    let+ first, second =
      Fiber.fork_and_join
        (fun () -> Fiber.collect_errors (fun () -> first))
        (fun () -> Fiber.collect_errors (fun () -> second))
    in
    let classify = function
      | Error [ _ ] -> "error"
      | Ok _ | Error _ -> "unexpected"
    in
    Printf.printf "results: %s, %s; runs: %d\n" (classify first) (classify second) !runs
  in
  Lev_fiber.run run |> Lev_fiber.Error.ok_exn;
  [%expect {| results: error, error; runs: 1 |}]
;;

let%expect_test "end to end run of lsp tests" =
  test End_to_end_client.run End_to_end_server.run;
  [%expect
    {|
    client: waiting for initialization
    server: initializing server
    server: returning initialization result
    client: server initialized. sending request
    client: sending request cmd_cancel
    client: sending request cmd_reply
    client: waiting to receive notification before cancelling the request
    server: received command cmd_cancel
    server: sending message notification to client
    client: received notification
    {
      "params": { "message": "notifying client", "type": 3 },
      "method": "window/showMessage",
      "jsonrpc": "2.0"
    }
    client: filled received_notification
    client: received notification, cancelling the request
    server: received command cmd_reply
    client: req_cancel got cancelled
    client: Successfully executed req_reply with result:
    "successful execution"
    client: sending request to shutdown
    Successful termination of test
    [TEST] finished |}]
;;

module Progress_lifecycle = struct
  (* [Lsp_fiber.Progress] shadows [Lsp.Progress] because [Lsp_fiber] is opened
     after [Lsp]. Refer to the wire format explicitly. *)
  module Wire = Lsp.Progress

  let record events params =
    let json = ProgressParams.yojson_of_t Wire.yojson_of_t params in
    events := Yojson.Safe.to_string json :: !events;
    Fiber.return ()
  ;;

  let record_create events params =
    let json = WorkDoneProgressCreateParams.yojson_of_t params in
    events := ("create " ^ Yojson.Safe.to_string json) :: !events;
    Fiber.return ()
  ;;

  let run events f =
    let lifecycle =
      Progress.create
        ~create_task:(record_create events)
        ~report_progress:(record events)
        ()
    in
    Fiber_test.test Dyn.opaque (fun () ->
      let+ () = f lifecycle in
      List.iter print_endline (List.rev !events))
  ;;
end

let%expect_test "work done progress: single task lifecycle" =
  let events = ref [] in
  Progress_lifecycle.run events (fun lifecycle ->
    let* task =
      Progress.start lifecycle ~token_name:"build" ~title:"Build" ~message:"started" ()
    in
    let* () = Progress.report lifecycle task ~percentage:42 ~message:"Building [1/2]" in
    let+ () = Progress.end_ lifecycle task ~message:"Build finished" in
    ());
  [%expect
    {|
    create {"token":"build-0"}
    {"token":"build-0","value":{"kind":"begin","message":"started","title":"Build"}}
    {"token":"build-0","value":{"kind":"report","message":"Building [1/2]","percentage":42}}
    {"token":"build-0","value":{"kind":"end","message":"Build finished"}}
    <opaque>
    |}]
;;

let%expect_test "work done progress: concurrent tasks are independent" =
  let events = ref [] in
  Progress_lifecycle.run events (fun lifecycle ->
    let* build =
      Progress.start lifecycle ~token_name:"build" ~title:"Build" ~message:"started" ()
    in
    let* index =
      Progress.start lifecycle ~token_name:"index" ~title:"Index" ~message:"started" ()
    in
    let* () = Progress.report lifecycle build ~percentage:50 ~message:"Building [1/2]" in
    let* () = Progress.report lifecycle index ~percentage:10 ~message:"Indexing" in
    let* () = Progress.end_ lifecycle build ~message:"Build finished" in
    let+ () = Progress.end_ lifecycle index ~message:"Index finished" in
    ());
  [%expect
    {|
    create {"token":"build-0"}
    {"token":"build-0","value":{"kind":"begin","message":"started","title":"Build"}}
    create {"token":"index-1"}
    {"token":"index-1","value":{"kind":"begin","message":"started","title":"Index"}}
    {"token":"build-0","value":{"kind":"report","message":"Building [1/2]","percentage":50}}
    {"token":"index-1","value":{"kind":"report","message":"Indexing","percentage":10}}
    {"token":"build-0","value":{"kind":"end","message":"Build finished"}}
    {"token":"index-1","value":{"kind":"end","message":"Index finished"}}
    <opaque>
    |}]
;;

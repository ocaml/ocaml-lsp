open Stdune
open Fiber.O
module Csexp_rpc = Lev_fiber_csexp

let%expect_test "serve/connect" =
  let path = "levfibercsexp.sock" in
  (try Unix.unlink path with Unix.Unix_error _ -> ());
  let server_sockaddr =
    if Sys.win32 then Unix.ADDR_INET (Unix.inet_addr_loopback, 0)
    else Unix.ADDR_UNIX path
  in
  let domain = Unix.domain_of_sockaddr server_sockaddr in
  let socket () =
    Lev_fiber.Fd.create
      (Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0)
      (`Non_blocking false)
  in
  let run () =
    let ready_client = Fiber.Ivar.create () in
    let client () =
      let* sockaddr = Fiber.Ivar.read ready_client in
      printfn "client: connecting";
      let* session = Csexp_rpc.connect (socket ()) sockaddr in
      let* () =
        Csexp_rpc.Session.write session
          [ Csexp.List [ Atom "one"; List [ Atom "two"; Atom "three" ] ] ]
      in
      let+ response = Csexp_rpc.Session.read session in
      (match response with
      | None -> assert false
      | Some s ->
          let resp = Csexp.to_string s in
          printfn "client: received %S" resp);
      Csexp_rpc.Session.close session
    and server () =
      let fd = socket () in
      let unix_fd = Lev_fiber.Fd.fd_exn fd in
      Unix.setsockopt unix_fd Unix.SO_REUSEADDR true;
      let* server =
        Lev_fiber.Socket.Server.create fd server_sockaddr ~backlog:10
      in
      printfn "server: started";
      let listening_addr = Unix.getsockname unix_fd in
      let* () = Fiber.Ivar.fill ready_client listening_addr in
      Csexp_rpc.serve server ~f:(fun session ->
          printfn "server: new session";
          let* req = Csexp_rpc.Session.read session in
          (match req with
          | None -> assert false
          | Some req -> printfn "server: request %S" (Csexp.to_string req));
          let* () =
            Csexp_rpc.Session.write session
              [ Atom "response"; List [ Atom "foo" ] ]
          in
          Csexp_rpc.Session.close session;
          Lev_fiber.Socket.Server.close server)
    in
    Fiber.fork_and_join_unit client server
  in
  Lev_fiber.run run |> Lev_fiber.Error.ok_exn;
  [%expect
    {|
    server: started
    client: connecting
    server: new session
    server: request "(3:one(3:two5:three))"
    client: received "8:response" |}]

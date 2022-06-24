(* Benchmark with:
   {[
   $ SOCK=/tmp/sock
   $ dune exec -- bench/ping_pong.exe $SOCK
   $ redis-benchmark -s $SOCK -t ping -n 100000 -P 1
   ]}
*)
open Lev

let client_events = Io.Event.Set.create ~read:true ~write:true ()
let response = Bytes.of_string "+PONG\r\n"

type client = {
  mutable writeable : bool;
  mutable pending_responses : int;
  buf : Bytes.t;
}

let new_client buf = { writeable = false; pending_responses = 0; buf }

let server socket =
  let loop = Loop.default () in
  let buf = Bytes.create 1024 in
  let cleanup io loop fd =
    Unix.close fd;
    Io.stop io loop
  in
  let client_loop client io fd events =
    if Io.Event.Set.mem events Write then client.writeable <- true;
    (if Io.Event.Set.mem events Read then
     match Unix.read fd client.buf 0 (Bytes.length client.buf) with
     | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
     | exception _ -> Io.stop io loop
     | 0 -> cleanup io loop fd
     | read ->
         for i = 0 to read - 1 do
           if Bytes.get client.buf i = '\n' then
             client.pending_responses <- client.pending_responses + 1
         done);
    if client.writeable then
      let () = client.writeable <- client.pending_responses = 0 in
      try
        (* we don't bother buffering writes for this benchmark *)
        while client.pending_responses > 0 do
          let len = Bytes.length response in
          assert (Unix.write fd response 0 len = len);
          client.pending_responses <- client.pending_responses - 1
        done
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
      | _ -> cleanup io loop fd
  in
  let accept =
    Io.create
      (fun _io fd _ ->
        let client, _ = Unix.accept ~cloexec:true fd in
        Unix.set_nonblock client;
        let io =
          Io.create (client_loop (new_client buf)) client client_events
        in
        Io.start io loop)
      socket
      (Io.Event.Set.create ~read:true ())
  in
  Io.start accept loop;
  Loop.run_until_done loop

let run sock_path =
  let delete () = try Unix.unlink sock_path with Unix.Unix_error _ -> () in
  delete ();
  let socket = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.set_nonblock socket;
  Unix.bind socket (Unix.ADDR_UNIX sock_path);
  at_exit delete;
  Unix.listen socket 128;
  server socket

let () = run Sys.argv.(1)

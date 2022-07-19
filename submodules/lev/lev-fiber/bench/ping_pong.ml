open! Stdune
open Fiber.O
open Lev_fiber

let () =
  let current = Gc.get () in
  Gc.set { current with max_overhead = 1000000; allocation_policy = 1 }

let response = "+PONG\r\n"

let pong o times =
  Io.with_write o ~f:(fun writer ->
      for _ = 1 to times do
        Io.Writer.add_string writer response
      done;
      Io.Writer.flush writer)

let rec process_bytes buf pos len count =
  if pos >= len then count
  else
    let c = Bytes.get buf pos in
    let count = if c = '\n' then count + 1 else count in
    process_bytes buf (pos + 1) len count

let rec read o reader =
  match Io.Reader.Expert.available reader with
  | `Eof -> Fiber.return ()
  | `Ok 0 ->
      let* () = Io.Reader.Expert.refill reader in
      read o reader
  | `Ok _ ->
      let buf, { Io.Slice.pos; len } = Io.Reader.Expert.buffer reader in
      let times = process_bytes buf pos len 0 in
      Io.Reader.Expert.consume reader ~len;
      let* () = pong o times in
      read o reader

let serve session =
  let* i, o = Socket.Server.Session.io session in
  let+ () = Io.with_read i ~f:(fun reader -> read o reader) in
  Io.close i;
  Io.close o

let run sock_path =
  let delete () = try Unix.unlink sock_path with Unix.Unix_error _ -> () in
  delete ();
  let socket =
    let socket = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Lev_fiber.Fd.create socket (`Non_blocking false)
  in
  let addr = Unix.ADDR_UNIX sock_path in
  let* server = Socket.Server.create ~backlog:128 socket addr in
  at_exit delete;
  let serve session =
    Fiber.with_error_handler
      (fun () -> serve session)
      ~on_error:(fun exn ->
        Format.eprintf "%a@.%!" Exn_with_backtrace.pp_uncaught exn;
        Exn_with_backtrace.reraise exn)
  in
  Socket.Server.serve server ~f:serve

let () = Lev_fiber.run (fun () -> run Sys.argv.(1)) |> Lev_fiber.Error.ok_exn

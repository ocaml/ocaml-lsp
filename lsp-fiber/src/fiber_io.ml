open Import
open Fiber.O

type t =
  { in_thread : Scheduler.thread option ref
  ; out_thread : Scheduler.thread option ref
  ; io : Io.t
  }

let close_out out_thread =
  match !out_thread with
  | None -> ()
  | Some thread ->
    Scheduler.stop thread;
    out_thread := None

let close_in in_thread =
  match !in_thread with
  | None -> ()
  | Some thread ->
    Scheduler.stop thread;
    in_thread := None

let send t packets =
  match !(t.out_thread) with
  | None ->
    List.iter packets ~f:(fun packet ->
        Log.log ~section:"Stream_io" (fun () ->
            Log.msg "dropped write"
              [ ("packet", Jsonrpc.yojson_of_packet packet) ]));
    Fiber.return ()
  | Some thread -> (
    let+ res =
      Scheduler.async_exn thread (fun () ->
          List.iter packets ~f:(Io.send t.io);
          Io.flush t.io)
      |> Scheduler.await_no_cancel
    in
    match res with
    | Ok s -> s
    | Error exn -> Exn_with_backtrace.reraise exn)

let close_write t =
  match !(t.out_thread) with
  | None -> Fiber.return ()
  | Some thread -> (
    let+ res =
      Scheduler.async_exn thread (fun () -> Io.close_out t.io)
      |> Scheduler.await_no_cancel
    in
    close_out t.out_thread;
    match res with
    | Ok s -> s
    | Error exn -> Exn_with_backtrace.reraise exn)

let recv t =
  let open Fiber.O in
  match !(t.in_thread) with
  | None -> Fiber.return None
  | Some thread ->
    let task =
      Scheduler.async_exn thread (fun () ->
          let res = Io.read t.io in
          (match res with
          | None -> Io.close_in t.io
          | Some _ -> ());
          res)
    in
    let+ res = Scheduler.await_no_cancel task in
    let res =
      match res with
      | Ok s -> s
      | Error exn -> Exn_with_backtrace.reraise exn
    in
    (match res with
    | None -> close_in t.in_thread
    | Some _ -> ());
    res

let make io =
  let* in_thread =
    let+ th = Scheduler.create_thread () in
    ref (Some th)
  in
  let+ out_thread =
    let+ th = Scheduler.create_thread () in
    ref (Some th)
  in
  { in_thread; out_thread; io }

let close (t : t) what =
  match what with
  | `Write -> close_write t
  | `Read -> (
    match !(t.in_thread) with
    | None -> Fiber.return ()
    | Some thread -> (
      let open Fiber.O in
      let+ close =
        Scheduler.async_exn thread (fun () -> Io.close_in t.io)
        |> Scheduler.await_no_cancel
      in
      close_in t.in_thread;
      match close with
      | Ok s -> s
      | Error exn -> Exn_with_backtrace.reraise exn))

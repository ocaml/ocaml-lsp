open Import
open Fiber.O

type t =
  { in_thread : Scheduler.thread option ref
  ; out_thread : Scheduler.thread option ref
  ; io : in_channel * out_channel
  }

module Io =
  Io.Make
    (struct
      type 'a t = 'a

      let return x = x

      let raise exn = raise exn

      module O = struct
        let ( let* ) x f = f x

        let ( let+ ) x f = f x
      end
    end)
    (struct
      type nonrec t = in_channel * out_channel

      let read_line (ic, _) =
        match input_line ic with
        | s -> Some s
        | exception End_of_file -> None

      let read_exactly (ic, _) len =
        let buffer = Bytes.create len in
        let rec read_loop read =
          if read < len then
            let n = input ic buffer read (len - read) in
            read_loop (read + n)
        in
        match read_loop 0 with
        | () -> Some (Bytes.to_string buffer)
        | exception End_of_file -> None

      let write (_, oc) s = output_string oc s
    end)

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
          List.iter packets ~f:(Io.write t.io);
          flush (snd t.io))
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
      Scheduler.async_exn thread (fun () -> close_out_noerr (snd t.io))
      |> Scheduler.await_no_cancel
    in
    close_out t.out_thread;
    match res with
    | Ok s -> s
    | Error exn -> Exn_with_backtrace.reraise exn)

let recv t =
  match !(t.in_thread) with
  | None -> Fiber.return None
  | Some thread ->
    let task =
      Scheduler.async_exn thread (fun () ->
          let res = Io.read t.io in
          (match res with
          | exception End_of_file -> close_in_noerr (fst t.io)
          | _ -> ());
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

let make ic oc =
  let* in_thread =
    let+ th = Scheduler.create_thread () in
    ref (Some th)
  in
  let+ out_thread =
    let+ th = Scheduler.create_thread () in
    ref (Some th)
  in
  { in_thread; out_thread; io = (ic, oc) }

let close (t : t) what =
  match what with
  | `Write -> close_write t
  | `Read -> (
    match !(t.in_thread) with
    | None -> Fiber.return ()
    | Some thread -> (
      let+ close =
        Scheduler.async_exn thread (fun () -> close_in_noerr (fst t.io))
        |> Scheduler.await_no_cancel
      in
      close_in t.in_thread;
      match close with
      | Ok s -> s
      | Error exn -> Exn_with_backtrace.reraise exn))

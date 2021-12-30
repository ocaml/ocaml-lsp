open Import
open Fiber.O

type t =
  { in_thread : Lev_fiber.Thread.t option ref
  ; out_thread : Lev_fiber.Thread.t option ref
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
      type input = in_channel

      type output = out_channel

      let read_line ic =
        match input_line ic with
        | s -> Some s
        | exception End_of_file -> None

      let read_exactly ic len =
        let buffer = Bytes.create len in
        let rec read_loop read =
          if read < len then
            let n = input ic buffer read (len - read) in
            read_loop (read + n)
        in
        match read_loop 0 with
        | () -> Some (Bytes.to_string buffer)
        | exception End_of_file -> None

      let write oc s = output_string oc s
    end)

let close_out out_thread =
  match !out_thread with
  | None -> ()
  | Some thread ->
    Lev_fiber.Thread.close thread;
    out_thread := None

let close_in in_thread =
  match !in_thread with
  | None -> ()
  | Some thread ->
    Lev_fiber.Thread.close thread;
    in_thread := None

let await_no_cancel task =
  let+ res = Lev_fiber.Thread.await task in
  match res with
  | Ok s -> s
  | Error `Cancelled -> assert false
  | Error (`Exn exn) -> Exn_with_backtrace.reraise exn

let send t packets =
  match !(t.out_thread) with
  | None ->
    List.iter packets ~f:(fun packet ->
        Log.log ~section:"Stream_io" (fun () ->
            Log.msg "dropped write"
              [ ("packet", Jsonrpc.yojson_of_packet packet) ]));
    Fiber.return ()
  | Some thread ->
    let* task =
      Lev_fiber.Thread.task thread ~f:(fun () ->
          List.iter packets ~f:(Io.write (snd t.io));
          flush (snd t.io))
    in
    await_no_cancel task

let close_write t =
  match !(t.out_thread) with
  | None -> Fiber.return ()
  | Some thread -> (
    let+ res =
      let* task =
        Lev_fiber.Thread.task thread ~f:(fun () -> close_out_noerr (snd t.io))
      in
      let+ res = Lev_fiber.Thread.await task in
      match res with
      | Ok _ as s -> s
      | Error `Cancelled -> assert false
      | Error (`Exn e) -> Error e
    in
    close_out t.out_thread;
    match res with
    | Ok s -> s
    | Error exn -> Exn_with_backtrace.reraise exn)

let recv t =
  match !(t.in_thread) with
  | None -> Fiber.return None
  | Some thread ->
    let* task =
      Lev_fiber.Thread.task thread ~f:(fun () ->
          let res = Io.read (fst t.io) in
          (match res with
          | exception End_of_file -> close_in_noerr (fst t.io)
          | _ -> ());
          res)
    in
    let+ res = await_no_cancel task in
    (match res with
    | None -> close_in t.in_thread
    | Some _ -> ());
    res

let make ic oc =
  let* in_thread =
    let+ th = Lev_fiber.Thread.create () in
    ref (Some th)
  in
  let+ out_thread =
    let+ th = Lev_fiber.Thread.create () in
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
        let* task =
          Lev_fiber.Thread.task thread ~f:(fun () -> close_in_noerr (fst t.io))
        in
        let+ res = Lev_fiber.Thread.await task in
        match res with
        | Ok _ as s -> s
        | Error `Cancelled -> assert false
        | Error (`Exn e) -> Error e
      in
      close_in t.in_thread;
      match close with
      | Ok s -> s
      | Error exn -> Exn_with_backtrace.reraise exn))

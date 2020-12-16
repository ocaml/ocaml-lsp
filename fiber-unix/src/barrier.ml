open Import

type state =
  | Closed
  | Active of
      { r : Unix.file_descr
      ; w : Unix.file_descr
      ; await_mutex : Mutex.t
      ; mutex : Mutex.t
      ; buf : Bytes.t
      }

type t = state ref

let create () =
  let r, w = Unix.pipe () in
  ref
    (Active
       { r
       ; w
       ; mutex = Mutex.create ()
       ; await_mutex = Mutex.create ()
       ; buf = Bytes.create 1
       })

let close t =
  match !t with
  | Closed -> ()
  | Active { mutex; await_mutex = _; r; w; buf = _ } ->
    Mutex.lock mutex;
    (try Unix.close w with Unix.Unix_error _ -> ());
    (try Unix.close r with Unix.Unix_error _ -> ());
    t := Closed;
    Mutex.unlock mutex

let with_unix_error f =
  match f () with
  | s -> Ok s
  | exception Unix.Unix_error (Unix.EBADF, _, _) -> Error `Closed
  | exception Unix.Unix_error (e, _, _) -> failwith (Unix.error_message e)

let select fd timeout =
  let open Result.O in
  let* res = with_unix_error (fun () -> Unix.select [ fd ] [] [] timeout) in
  match res with
  | [], _, _ -> Ok `Empty
  | [ _ ], _, _ -> Ok `Ready_to_read
  | _ -> assert false

let rec drain_pipe fd buf read_once =
  match with_unix_error (fun () -> Unix.read fd buf 0 1) with
  | Error `Closed -> Error (`Closed (`Read read_once))
  | Ok read -> (
    let read_once = read_once || read = 1 in
    if read = 0 then (
      assert read_once;
      Ok ()
    ) else
      match select fd 0. with
      | Ok `Empty -> Ok ()
      | Ok `Ready_to_read -> drain_pipe fd buf read_once
      | Error `Closed -> Error (`Closed (`Read read_once)) )

let await ?(timeout = -1.) t =
  match !t with
  | Closed -> Error (`Closed (`Read false))
  | Active t ->
    with_mutex t.await_mutex ~f:(fun () ->
        match select t.r timeout with
        | Ok `Empty -> Error `Timeout
        | Ok `Ready_to_read -> drain_pipe t.r t.buf false
        | Error `Closed -> Error (`Closed (`Read false)))

let signal t =
  match !t with
  | Closed -> Error `Closed
  | Active { w; buf; _ } -> (
    match Unix.write w buf 0 1 with
    | exception Unix.Unix_error (Unix.EBADF, _, _) ->
      close t;
      Error `Closed
    | 1 -> Ok ()
    | _ -> assert false )

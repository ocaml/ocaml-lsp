open Import

type state =
  | Closed
  | Active of
      { r : Unix.file_descr
      ; w : Unix.file_descr
      ; mutex : Mutex.t
      ; buf : Bytes.t
      }

type t = state ref

let create () =
  let r, w = Unix.pipe () in
  ref (Active { r; w; mutex = Mutex.create (); buf = Bytes.create 1 })

let close t =
  match !t with
  | Closed -> ()
  | Active { mutex = _; r; w; buf = _ } ->
    (try Unix.close w with Unix.Unix_error _ -> ());
    (try Unix.close r with Unix.Unix_error _ -> ());
    t := Closed

let select fd timeout =
  match Unix.select [ fd ] [] [] timeout with
  | [], _, _ -> `Empty
  | [ _ ], _, _ -> `Ready_to_read
  | _ -> assert false
  | exception Unix.Unix_error (Unix.EBADF, _, _) -> `Closed
  | exception Unix.Unix_error (e, _, _) ->
    failwith ("select: " ^ Unix.error_message e)

let rec drain_pipe fd buf read_once =
  let read = Unix.read fd buf 0 1 in
  let read_once = read_once || read = 1 in
  if read = 0 then (
    assert read_once;
    Ok ()
  ) else
    match select fd 0. with
    | `Empty -> Ok ()
    | `Closed -> Error (`Closed (`Read read_once))
    | `Ready_to_read -> drain_pipe fd buf read_once

let await ?(timeout = -1.) t =
  match !t with
  | Closed -> Error (`Closed (`Read false))
  | Active t ->
    with_mutex t.mutex ~f:(fun () ->
        match select t.r timeout with
        | `Empty -> Error `Timeout
        | `Ready_to_read -> drain_pipe t.r t.buf false
        | `Closed -> Error (`Closed (`Read false)))

let signal t =
  match !t with
  | Closed -> Error `Closed
  | Active t -> (
    match Unix.write t.w t.buf 0 1 with
    | 1 -> Ok ()
    | _ -> assert false )

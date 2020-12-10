type state =
  | Closed
  | Active of
      { r : Unix.file_descr
      ; w : Unix.file_descr
      ; buf : Bytes.t
      }

type t = state ref

let create () =
  let r, w = Unix.pipe () in
  ref (Active { r; w; buf = Bytes.create 1 })

let close t =
  match !t with
  | Closed -> ()
  | Active { r; w; buf = _ } ->
    Unix.close w;
    Unix.close r;
    t := Closed

let rec empty_fd fd buf =
  match Unix.select [ fd ] [] [] 0.0 with
  | [], _, _ -> Ok ()
  | _ ->
    let _ = Unix.read fd buf 0 1 in
    empty_fd fd buf
  | exception Unix.Unix_error (Unix.EBADF, _, _) -> Error (`Closed (`Read true))
  | exception Unix.Unix_error (e, _, _) ->
    failwith ("read :" ^ Unix.error_message e)

let await ?(timeout = -1.) t =
  match !t with
  | Closed -> Error (`Closed (`Read false))
  | Active t -> (
    match Unix.select [ t.r ] [] [] timeout with
    | [], _, _ -> Error `Timeout
    | _ -> empty_fd t.r t.buf
    | exception Unix.Unix_error (Unix.EBADF, _, _) ->
      Error (`Closed (`Read false))
    | exception Unix.Unix_error (e, _, _) ->
      failwith ("read :" ^ Unix.error_message e ^ " " ^ string_of_float timeout)
    )

let b = Bytes.make 1 'O'

let signal t =
  match !t with
  | Closed -> Error `Closed
  | Active t -> (
    match Unix.write t.w b 0 1 with
    | 1 -> Ok ()
    | _ -> assert false )

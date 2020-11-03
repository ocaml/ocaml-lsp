type t =
  { r : Unix.file_descr
  ; w : Unix.file_descr
  ; buf : Bytes.t
  }

let create () =
  let r, w = Unix.pipe () in
  Unix.set_nonblock r;
  { r; w; buf = Bytes.create 8 }

let close t =
  Unix.close t.w;
  Unix.close t.r

let read_one t =
  match Unix.read t.r t.buf 0 (Bytes.length t.buf) with
  | 0 -> Error `No_data
  | _ -> Ok ()
  | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> Error `Unavailable
  | exception Unix.Unix_error (Unix.EBADF, _, _) -> Error `Bad_fd
  | exception Unix.Unix_error (e, _, _) -> failwith (Unix.error_message e)

let read_all t =
  let rec loop read_once =
    match read_one t with
    | Ok () -> loop true
    | Error `Bad_fd -> Error (`Bad_fd, read_once)
    | Error `Unavailable
    | Error `No_data ->
      Ok read_once
  in
  loop false

let rec await ?(timeout = -1.) t =
  match Unix.select [ t.r ] [] [] timeout with
  | [], _, _ -> Error `Timeout
  | _ -> (
    match read_all t with
    | Ok true -> Ok ()
    | Ok false -> await ~timeout t
    | Error (`Bad_fd, read_once) -> Error (`Closed (`Read read_once)) )
  | exception Unix.Unix_error (Unix.EBADF, _, _) ->
    Error (`Closed (`Read false))
  | exception Unix.Unix_error (e, _, _) ->
    failwith ("read :" ^ Unix.error_message e ^ " " ^ string_of_float timeout)

let b = Bytes.make 1 'O'

let signal t =
  match Unix.write t.w b 0 1 with
  | 1 -> ()
  | _ -> assert false

open Import
open Fiber.O

let protocol_version = "1"

module Process = struct
  type t =
    { pid : Pid.t
    ; session : Lev_fiber_csexp.Session.t
    }

  let kill { pid; _ } =
    match Unix.kill (Pid.to_int pid) Sys.sigterm with
    | () -> ()
    | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ()
  ;;

  let stop ({ pid; session } as t) =
    Lev_fiber_csexp.Session.close session;
    kill t;
    let+ (_ : Unix.process_status) = Lev_fiber.waitpid ~pid:(Pid.to_int pid) in
    ()
  ;;

  let create bin =
    let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
    let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
    let pid =
      Spawn.spawn ~prog:bin ~argv:[ bin ] ~stdin:stdin_i ~stdout:stdout_o () |> Pid.of_int
    in
    Unix.close stdin_i;
    Unix.close stdout_o;
    let blockity =
      if Sys.win32
      then `Blocking
      else (
        Unix.set_nonblock stdin_o;
        Unix.set_nonblock stdout_i;
        `Non_blocking true)
    in
    let make fd what =
      let fd = Lev_fiber.Fd.create fd blockity in
      Lev_fiber.Io.create fd what
    in
    let* stdin = make stdin_o Output in
    let* stdout = make stdout_i Input in
    let session = Lev_fiber_csexp.Session.create ~socket:false stdout stdin in
    let process = { pid; session } in
    let* hello = Lev_fiber_csexp.Session.read session in
    match hello with
    | Some (List [ Atom "version"; Atom version ])
      when String.equal version protocol_version -> Fiber.return (Ok process)
    | hello ->
      let message =
        sprintf
          "ocp-indent-rpc protocol negotiation failed: %s"
          (match hello with
           | None -> "process exited before sending its version"
           | Some sexp -> Csexp.to_string sexp)
      in
      let+ () = stop process in
      Error (`Msg message)
  ;;
end

type state =
  | Not_started
  | Unavailable
  | Running of Process.t
  | Stopped

type t =
  { mutable state : state
  ; mutex : Fiber.Mutex.t
  }

type error =
  [ `Binary_not_found
  | `Msg of string
  ]

let create () = { state = Not_started; mutex = Fiber.Mutex.create () }

let start t =
  match t.state with
  | Running process -> Fiber.return (Ok process)
  | Unavailable | Stopped -> Fiber.return (Error `Binary_not_found)
  | Not_started ->
    (* Change the state before doing any work so that PATH is consulted at most once. *)
    t.state <- Unavailable;
    (match Bin.which "ocp-indent-rpc" with
     | None -> Fiber.return (Error `Binary_not_found)
     | Some bin ->
       let+ process = Process.create bin in
       Result.map process ~f:(fun process ->
         t.state <- Running process;
         process))
;;

let stop t =
  Fiber.Mutex.with_lock t.mutex ~f:(fun () ->
    match t.state with
    | Not_started | Unavailable | Stopped ->
      t.state <- Stopped;
      Fiber.return ()
    | Running process ->
      t.state <- Stopped;
      Process.stop process)
;;

let indentation t ~path ~line ~source ~syntaxes =
  Fiber.Mutex.with_lock t.mutex ~f:(fun () ->
    let* process = start t in
    match process with
    | Error _ as error -> Fiber.return error
    | Ok ({ Process.session; _ } as process) ->
      let request =
        Csexp.List
          [ Atom "indent"
          ; Atom path
          ; Atom (Int.to_string line)
          ; Atom source
          ; List (List.map syntaxes ~f:(fun syntax -> Csexp.Atom syntax))
          ]
      in
      let* () = Lev_fiber_csexp.Session.write session [ request ] in
      let* response = Lev_fiber_csexp.Session.read session in
      (match response with
       | Some (List [ Atom "ok"; Atom indentation ]) ->
         (match Int.of_string_opt indentation with
          | Some indentation -> Fiber.return (Ok indentation)
          | None ->
            let message = "ocp-indent-rpc returned an invalid indentation" in
            t.state <- Unavailable;
            let+ () = Process.stop process in
            Error (`Msg message))
       | Some (List [ Atom "error"; Atom message ]) -> Fiber.return (Error (`Msg message))
       | response ->
         let message =
           sprintf
             "invalid response from ocp-indent-rpc: %s"
             (match response with
              | None -> "end of file"
              | Some sexp -> Csexp.to_string sexp)
         in
         t.state <- Unavailable;
         let+ () = Process.stop process in
         Error (`Msg message)))
;;

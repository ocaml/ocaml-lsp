open Import
open Fiber.O

(* TODO: Bump the version or negotiate capabilities whenever adding requests;
   the helper is installed independently and may be older than the server. *)
let protocol_version = "1"

let error_message context errors =
  let errors =
    List.map errors ~f:(fun (error : Exn_with_backtrace.t) ->
      Printexc.to_string error.exn)
    |> String.concat ~sep:"; "
  in
  sprintf "%s: %s" context errors
;;

module Process = struct
  type t =
    { pid : Pid.t
    ; session : Lev_fiber_csexp.Session.t
    }

  let kill_pid pid =
    match Unix.kill (Pid.to_int pid) Sys.sigterm with
    | () -> ()
    | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ()
  ;;

  let stop { pid; session } =
    let catch_errors f =
      match Exn_with_backtrace.try_with f with
      | Ok () -> []
      | Error e -> [ e ]
    in
    let errors =
      catch_errors (fun () -> Lev_fiber_csexp.Session.close session)
      @ catch_errors (fun () -> kill_pid pid)
    in
    let* waited =
      Fiber.collect_errors (fun () -> Lev_fiber.waitpid ~pid:(Pid.to_int pid))
    in
    match
      match waited with
      | Ok (_ : Unix.process_status) -> errors
      | Error wait_errors -> wait_errors @ errors
    with
    | [] -> Fiber.return ()
    | errors -> Fiber.reraise_all (List.rev errors)
  ;;

  let spawn bin =
    let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
    let stdout_i, stdout_o =
      match Unix.pipe ~cloexec:true () with
      | pipe -> pipe
      | exception exn ->
        Unix.close stdin_i;
        Unix.close stdin_o;
        raise exn
    in
    match Spawn.spawn ~prog:bin ~argv:[ bin ] ~stdin:stdin_i ~stdout:stdout_o () with
    | pid ->
      Unix.close stdin_i;
      Unix.close stdout_o;
      Pid.of_int pid, stdin_o, stdout_i
    | exception exn ->
      List.iter [ stdin_i; stdin_o; stdout_i; stdout_o ] ~f:Unix.close;
      raise exn
  ;;

  let create_exn bin =
    let pid, stdin_o, stdout_i = spawn bin in
    let process = ref None in
    let keep_running = ref false in
    Fiber.finalize
      (fun () ->
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
         let* session =
           let* stdin = make stdin_o Output in
           let+ stdout = make stdout_i Input in
           Lev_fiber_csexp.Session.create ~socket:false stdout stdin
         in
         let running = { pid; session } in
         process := Some running;
         Lev_fiber_csexp.Session.read session
         >>= function
         | Some (List [ Atom "version"; Atom version ])
           when String.equal version protocol_version ->
           keep_running := true;
           Fiber.return (Ok running)
         | hello ->
           let message =
             sprintf
               "ocp-indent-rpc protocol negotiation failed: %s"
               (match hello with
                | None -> "process exited before sending its version"
                | Some sexp -> Csexp.to_string sexp)
           in
           Fiber.return (Error (`Msg message)))
      ~finally:(fun () ->
        if !keep_running
        then Fiber.return ()
        else (
          match !process with
          | Some process -> stop process
          | None ->
            Unix.close stdin_o;
            Unix.close stdout_i;
            kill_pid pid;
            let+ (_ : Unix.process_status) = Lev_fiber.waitpid ~pid:(Pid.to_int pid) in
            ()))
  ;;

  let create bin =
    Fiber.collect_errors (fun () -> create_exn bin)
    >>| function
    | Ok result -> result
    | Error errors -> Error (`Msg (error_message "failed to start ocp-indent-rpc" errors))
  ;;

  let request { session; _ } request =
    Fiber.collect_errors (fun () ->
      let* () = Lev_fiber_csexp.Session.write session [ request ] in
      Lev_fiber_csexp.Session.read session)
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
       Process.create bin
       >>| Result.map ~f:(fun process ->
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

let make_unavailable t process message =
  t.state <- Unavailable;
  Fiber.collect_errors (fun () -> Process.stop process)
  >>| function
  | Ok () -> Error (`Msg message)
  | Error errors ->
    let cleanup = error_message "failed to stop ocp-indent-rpc" errors in
    Error (`Msg (message ^ "; " ^ cleanup))
;;

let indentation t ~path ~line ~source ~syntaxes =
  Fiber.Mutex.with_lock t.mutex ~f:(fun () ->
    start t
    >>= function
    | Error _ as error -> Fiber.return error
    | Ok process ->
      let request =
        Csexp.List
          [ Atom "indent"
          ; Atom path
          ; Atom (Int.to_string line)
          ; Atom source
          ; List (List.map syntaxes ~f:(fun syntax -> Csexp.Atom syntax))
          ]
      in
      Process.request process request
      >>= (function
       | Error errors ->
         let message = error_message "ocp-indent-rpc request failed" errors in
         make_unavailable t process message
       | Ok (Some (List [ Atom "ok"; Atom indentation ])) ->
         (match Int.of_string_opt indentation with
          | Some indentation -> Fiber.return (Ok indentation)
          | None ->
            make_unavailable t process "ocp-indent-rpc returned an invalid indentation")
       | Ok (Some (List [ Atom "error"; Atom message ])) ->
         Fiber.return (Error (`Msg message))
       | Ok response ->
         let message =
           sprintf
             "invalid response from ocp-indent-rpc: %s"
             (match response with
              | None -> "end of file"
              | Some sexp -> Csexp.to_string sexp)
         in
         make_unavailable t process message))
;;

let line source line = List.nth (String.split source ~on:'\n') line

let leading_whitespace_length line =
  String.lfindi line ~f:(fun (_ : int) -> function
    | ' ' | '\t' -> false
    | _ -> true)
  |> Option.value ~default:(String.length line)
;;

let format_on_type t doc (position : Position.t) =
  match
    match Document.syntax doc with
    | Ocaml -> Some []
    | Ocamllex -> Some [ "mll" ]
    | Cram | Dune | Menhir | Mlx | Reason -> None
  with
  | None -> Fiber.return None
  | Some syntaxes ->
    let source = Document.text doc in
    let* indentation =
      let path = Document.uri doc |> Uri.to_path in
      indentation t ~path ~line:(position.line + 1) ~source ~syntaxes
    in
    (match indentation, line source position.line with
     | Error _, _ | _, None -> Fiber.return None
     | Ok indentation, Some line ->
       let edits =
         let whitespace_length = leading_whitespace_length line in
         let newText = String.make indentation ' ' in
         if String.equal newText (String.prefix line whitespace_length)
         then []
         else (
           let range =
             Range.create
               ~start:(Position.create ~line:position.line ~character:0)
               ~end_:(Position.create ~line:position.line ~character:whitespace_length)
           in
           [ TextEdit.create ~range ~newText ])
       in
       Fiber.return (Some edits))
;;

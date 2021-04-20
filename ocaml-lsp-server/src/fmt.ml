open Import

type command_result =
  { stdout : string
  ; stderr : string
  ; status : Unix.process_status
  }

type t =
  { stdin : Scheduler.thread Lazy.t
  ; stderr : Scheduler.thread Lazy.t
  ; stdout : Scheduler.thread Lazy.t
  ; scheduler : Scheduler.t
  }

let create scheduler =
  let stdout = lazy (Scheduler.create_thread scheduler) in
  let stderr = lazy (Scheduler.create_thread scheduler) in
  let stdin = lazy (Scheduler.create_thread scheduler) in
  { stdout; stderr; stdin; scheduler }

let run_command state bin stdin_value args : command_result Fiber.t =
  let open Fiber.O in
  let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
  let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
  let stderr_i, stderr_o = Unix.pipe ~cloexec:true () in
  let pid =
    let args = Array.of_list (bin :: args) in
    Unix.create_process bin args stdin_i stdout_o stderr_o |> Stdune.Pid.of_int
  in
  Unix.close stdin_i;
  Unix.close stdout_o;
  Unix.close stderr_o;
  let stdin () =
    let+ res =
      Scheduler.async_exn (Lazy.force state.stdin) (fun () ->
          let out_chan = Unix.out_channel_of_descr stdin_o in
          output_string out_chan stdin_value;
          flush out_chan;
          close_out out_chan)
      |> Scheduler.await_no_cancel
    in
    match res with
    | Ok s -> s
    | Error e -> Exn_with_backtrace.reraise e
  in
  let read th from =
    let+ res =
      Scheduler.async_exn th (fun () ->
          let in_ = Unix.in_channel_of_descr from in
          let contents = Stdune.Io.read_all in_ in
          close_in_noerr in_;
          contents)
      |> Scheduler.await_no_cancel
    in
    match res with
    | Ok s -> s
    | Error e -> Exn_with_backtrace.reraise e
  in
  let stdout () = read (Lazy.force state.stdout) stdout_i in
  let stderr () = read (Lazy.force state.stderr) stderr_i in
  let+ status, (stdout, stderr) =
    Fiber.fork_and_join
      (fun () -> Scheduler.wait_for_process state.scheduler pid)
      (fun () ->
        Fiber.fork_and_join_unit stdin (fun () ->
            Fiber.fork_and_join stdout stderr))
  in
  { stdout; stderr; status }

type error =
  | Unsupported_syntax of Document.Syntax.t
  | Missing_binary of { binary : string }
  | Unexpected_result of { message : string }
  | Unknown_extension of Uri.t

let message = function
  | Unsupported_syntax syntax ->
    sprintf "formatting %s files is not supported"
      (Document.Syntax.human_name syntax)
  | Missing_binary { binary } ->
    sprintf
      "Unable to find %s binary. You need to install %s manually to use the \
       formatting feature."
      binary binary
  | Unknown_extension uri ->
    Printf.sprintf "Unable to format. File %s has an unknown extension"
      (Uri.to_path uri)
  | Unexpected_result { message } -> message

type formatter =
  | Reason of Document.Kind.t
  | Ocaml of Uri.t

let args = function
  | Ocaml uri -> [ sprintf "--name=%s" (Uri.to_path uri); "-" ]
  | Reason kind -> (
    [ "--parse"; "re"; "--print"; "re" ]
    @
    match kind with
    | Impl -> []
    | Intf -> [ "--interface=true" ])

let binary_name t =
  match t with
  | Ocaml _ -> "ocamlformat"
  | Reason _ -> "refmt"

let binary t =
  let name = binary_name t in
  match Bin.which name with
  | None -> Result.Error (Missing_binary { binary = name })
  | Some b -> Ok b

let formatter doc =
  match Document.syntax doc with
  | (Ocamllex | Menhir) as s -> Error (Unsupported_syntax s)
  | Ocaml -> Ok (Ocaml (Document.uri doc))
  | Reason -> Ok (Reason (Document.kind doc))

let exec state bin args stdin =
  let refmt = Fpath.to_string bin in
  let open Fiber.O in
  let+ res = run_command state refmt stdin args in
  match res.status with
  | Unix.WEXITED 0 -> Result.Ok res.stdout
  | _ -> Result.Error (Unexpected_result { message = res.stderr })

let run state doc : (string, error) result Fiber.t =
  let res =
    let open Result.O in
    let* formatter = formatter doc in
    let args = args formatter in
    let+ binary = binary formatter in
    (binary, args, Document.source doc |> Msource.text)
  in
  match res with
  | Error e -> Fiber.return (Error e)
  | Ok (binary, args, contents) -> exec state binary args contents

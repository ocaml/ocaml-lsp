open Import
open Fiber.O

type command_result =
  { stdout : string
  ; stderr : string
  ; status : Unix.process_status
  }

type t =
  { stdin : Lev_fiber.Thread.t Lazy_fiber.t
  ; stderr : Lev_fiber.Thread.t Lazy_fiber.t
  ; stdout : Lev_fiber.Thread.t Lazy_fiber.t
  }

let create () =
  let stdout = Lazy_fiber.create Lev_fiber.Thread.create in
  let stderr = Lazy_fiber.create Lev_fiber.Thread.create in
  let stdin = Lazy_fiber.create Lev_fiber.Thread.create in
  { stdout; stderr; stdin }

let await_no_cancel task =
  let+ res = Lev_fiber.Thread.await task in
  match res with
  | Ok s -> s
  | Error `Cancelled -> assert false
  | Error (`Exn exn) -> Exn_with_backtrace.reraise exn

let run_command state prog stdin_value args : command_result Fiber.t =
  let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
  let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
  let stderr_i, stderr_o = Unix.pipe ~cloexec:true () in
  let pid =
    let argv = prog :: args in
    Spawn.spawn ~prog ~argv ~stdin:stdin_i ~stdout:stdout_o ~stderr:stderr_o ()
    |> Stdune.Pid.of_int
  in
  Unix.close stdin_i;
  Unix.close stdout_o;
  Unix.close stderr_o;
  let stdin () =
    let* thread = Lazy_fiber.force state.stdin in
    let* task =
      Lev_fiber.Thread.task thread ~f:(fun () ->
          let out_chan = Unix.out_channel_of_descr stdin_o in
          output_string out_chan stdin_value;
          flush out_chan;
          close_out out_chan)
    in
    await_no_cancel task
  in
  let read th from =
    let* task =
      Lev_fiber.Thread.task th ~f:(fun () ->
          let in_ = Unix.in_channel_of_descr from in
          let contents = Stdune.Io.read_all in_ in
          close_in_noerr in_;
          contents)
    in
    await_no_cancel task
  in
  let stdout () =
    let* th = Lazy_fiber.force state.stdout in
    read th stdout_i
  in
  let stderr () =
    let* th = Lazy_fiber.force state.stderr in
    read th stderr_i
  in
  let+ status, (stdout, stderr) =
    Fiber.fork_and_join
      (fun () -> Lev_fiber.waitpid ~pid:(Pid.to_int pid))
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
  | (Dune | Cram | Ocamllex | Menhir) as s -> Error (Unsupported_syntax s)
  | Ocaml -> Ok (Ocaml (Document.uri doc))
  | Reason -> Ok (Reason (Document.kind doc))

let exec state bin args stdin =
  let refmt = Fpath.to_string bin in
  let+ res = run_command state refmt stdin args in
  match res.status with
  | Unix.WEXITED 0 -> Result.Ok res.stdout
  | _ -> Result.Error (Unexpected_result { message = res.stderr })

let run state doc : (TextEdit.t list, error) result Fiber.t =
  let res =
    let open Result.O in
    let* formatter = formatter doc in
    let args = args formatter in
    let+ binary = binary formatter in
    (binary, args, Document.source doc |> Msource.text)
  in
  match res with
  | Error e -> Fiber.return (Error e)
  | Ok (binary, args, contents) ->
    exec state binary args contents
    |> Fiber.map ~f:(Result.map ~f:(fun to_ -> Diff.edit ~from:contents ~to_))

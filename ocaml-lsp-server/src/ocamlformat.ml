open Import
open Fiber.O

type command_result =
  { stdout : string
  ; stderr : string
  ; status : Unix.process_status
  }

let run_command cancel prog stdin_value args =
  Fiber.of_thunk (fun () ->
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
    let maybe_cancel =
      match cancel with
      | None ->
        fun f ->
          let+ res = f () in
          res, Fiber.Cancel.Not_cancelled
      | Some token ->
        let on_cancel () =
          Unix.kill (Pid.to_int pid) Sys.sigterm;
          Fiber.return ()
        in
        fun f -> Fiber.Cancel.with_handler token ~on_cancel f
    in
    maybe_cancel
    @@ fun () ->
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
    let* stdin_o = make stdin_o Output in
    let* stdout_i = make stdout_i Input in
    let* stderr_i = make stderr_i Input in
    let stdin () =
      Fiber.finalize
        ~finally:(fun () ->
          Lev_fiber.Io.close stdin_o;
          Fiber.return ())
        (fun () ->
           Lev_fiber.Io.with_write stdin_o ~f:(fun w ->
             Lev_fiber.Io.Writer.add_string w stdin_value;
             Lev_fiber.Io.Writer.flush w))
    in
    let read from () =
      Fiber.finalize
        ~finally:(fun () ->
          Lev_fiber.Io.close from;
          Fiber.return ())
        (fun () -> Lev_fiber.Io.with_read from ~f:Lev_fiber.Io.Reader.to_string)
    in
    let+ status, (stdout, stderr) =
      Fiber.fork_and_join
        (fun () -> Lev_fiber.waitpid ~pid:(Pid.to_int pid))
        (fun () ->
           Fiber.fork_and_join_unit stdin (fun () ->
             Fiber.fork_and_join (read stdout_i) (read stderr_i)))
    in
    { stdout; stderr; status })
;;

type error =
  | Unsupported_syntax of Document.Syntax.t
  | Missing_binary of { binary : string }
  | Unexpected_result of { message : string }
  | Unknown_extension of Uri.t

let message = function
  | Unsupported_syntax syntax ->
    sprintf "formatting %s files is not supported" (Document.Syntax.human_name syntax)
  | Missing_binary { binary } ->
    sprintf
      "Unable to find %s binary. You need to install %s manually to use the formatting \
       feature."
      binary
      binary
  | Unknown_extension uri ->
    Printf.sprintf "Unable to format. File %s has an unknown extension" (Uri.to_path uri)
  | Unexpected_result { message } -> message
;;

type formatter =
  | Reason of Document.Kind.t
  | Ocaml of Uri.t
  | Mlx of Uri.t

let args = function
  | Ocaml uri ->
    let name = Uri.to_path uri in
    let flag =
      if String.is_suffix name ~suffix:".mli" || String.is_suffix name ~suffix:".ml"
      then []
      else [ "--impl" ]
    in
    flag @ [ sprintf "--name=%s" (Uri.to_path uri); "-" ]
  | Mlx uri -> [ "--impl"; sprintf "--name=%s" (Uri.to_path uri); "-" ]
  | Reason kind ->
    [ "--parse"; "re"; "--print"; "re" ]
    @
      (match kind with
      | Impl -> []
      | Intf -> [ "--interface=true" ])
;;

let binary_name t =
  match t with
  | Ocaml _ -> "ocamlformat"
  | Mlx _ -> "ocamlformat-mlx"
  | Reason _ -> "refmt"
;;

let binary t =
  let name = binary_name t in
  match Bin.which name with
  | None -> Result.Error (Missing_binary { binary = name })
  | Some b -> Ok b
;;

let formatter doc =
  match Document.syntax doc with
  | (Dune | Cram | Ocamllex | Menhir) as s -> Error (Unsupported_syntax s)
  | Ocaml -> Ok (Ocaml (Document.uri doc))
  | Mlx -> Ok (Mlx (Document.uri doc))
  | Reason ->
    Ok
      (Reason
         (match Document.kind doc with
          | `Merlin m -> Document.Merlin.kind m
          | `Other -> Code_error.raise "unable to format non merlin document" []))
;;

let range_formatter doc =
  match Document.syntax doc with
  | (Dune | Cram) as s -> Error (Unsupported_syntax s)
  | Ocaml | Ocamllex | Menhir -> Ok (Ocaml (Document.uri doc))
  | Mlx -> Ok (Mlx (Document.uri doc))
  | Reason ->
    Ok
      (Reason
         (match Document.kind doc with
          | `Merlin m -> Document.Merlin.kind m
          | `Other -> Code_error.raise "unable to format non merlin document" []))
;;

let exec cancel refmt args stdin =
  let+ res, cancel = run_command cancel refmt stdin args in
  match cancel with
  | Cancelled () ->
    let e = Jsonrpc.Response.Error.make ~code:RequestCancelled ~message:"cancelled" () in
    raise (Jsonrpc.Response.Error.E e)
  | Not_cancelled ->
    (match res.status with
     | Unix.WEXITED 0 -> Result.Ok res.stdout
     | _ -> Result.Error (Unexpected_result { message = res.stderr }))
;;

let run doc cancel : (TextEdit.t list, error) result Fiber.t =
  let res =
    let open Result.O in
    let* formatter = formatter doc in
    let args = args formatter in
    let+ binary = binary formatter in
    binary, args, Document.source doc |> Msource.text
  in
  match res with
  | Error e -> Fiber.return (Error e)
  | Ok (binary, args, contents) ->
    exec cancel binary args contents
    |> Fiber.map ~f:(Result.map ~f:(fun to_ -> Diff.edit ~from:contents ~to_))
;;

let run_range doc range cancel : (TextEdit.t list, error) result Fiber.t =
  let res =
    let open Result.O in
    let* formatter = range_formatter doc in
    let args = args formatter in
    let+ binary = binary formatter in
    let absolute = Text_document.absolute_range (Document.tdoc doc) range in
    let contents = Document.source doc |> Msource.text in
    binary, args, contents, absolute
  in
  match res with
  | Error e -> Fiber.return (Error e)
  | Ok (binary, args, contents, (start, stop)) ->
    let prefix, to_format, suffix =
      ( String.sub contents ~pos:0 ~len:start
      , String.sub contents ~pos:start ~len:(stop - start)
      , String.sub contents ~pos:stop ~len:(String.length contents - stop) )
    in
    exec cancel binary args to_format
    |> Fiber.map
         ~f:
           (Result.map ~f:(fun r ->
              let to_ = prefix ^ r ^ suffix in
              Diff.edit ~from:contents ~to_))
;;

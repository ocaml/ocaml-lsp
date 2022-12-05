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
        Spawn.spawn
          ~prog
          ~argv
          ~stdin:stdin_i
          ~stdout:stdout_o
          ~stderr:stderr_o
          ()
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
            (res, Fiber.Cancel.Not_cancelled)
        | Some token ->
          let on_cancel () =
            Unix.kill (Pid.to_int pid) Sys.sigint;
            Fiber.return ()
          in
          fun f -> Fiber.Cancel.with_handler token ~on_cancel f
      in
      maybe_cancel @@ fun () ->
      let blockity =
        if Sys.win32 then `Blocking
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
          (fun () ->
            Lev_fiber.Io.with_read from ~f:Lev_fiber.Io.Reader.to_string)
      in
      let+ status, (stdout, stderr) =
        Fiber.fork_and_join
          (fun () -> Lev_fiber.waitpid ~pid:(Pid.to_int pid))
          (fun () ->
            Fiber.fork_and_join_unit stdin (fun () ->
                Fiber.fork_and_join (read stdout_i) (read stderr_i)))
      in
      { stdout; stderr; status })

type error =
  | Unsupported_syntax of Document.Syntax.t
  | Missing_binary of { binary : string }
  | Unexpected_result of { message : string }
  | Unknown_extension of Uri.t

let err_msg ~fallback_msg ~compiler_err =
  let print_loc ppf ({ lines; chars; path = _ } : Ocamlc_loc.loc) =
    let print_line ppf lines =
      match (lines : Ocamlc_loc.lines) with
      | Single line -> Format.fprintf ppf "line %d" line
      | Range (l0, l1) -> Format.fprintf ppf "lines %d-%d" l0 l1
    in
    let print_chars ppf = function
      | None -> ()
      | Some (c0, c1) -> Format.fprintf ppf ", characters %d-%d" c0 c1
    in
    Format.fprintf ppf "%a%a" print_line lines print_chars chars
  in

  let print_msg ppf err_msg =
    if not (String.equal err_msg "Syntax error") then
      Format.fprintf
        ppf
        ": %s"
        (String.drop_prefix_if_exists err_msg ~prefix:"Syntax error: ")
  in

  let reports = Ocamlc_loc.parse compiler_err in

  let fmter = "ocamlformat" in
  match reports with
  | [] -> fallback_msg
  | [ { loc; severity; message; related = _ } ] ->
    let reason =
      if String.equal message "Syntax error" then "syntax error"
      else
        match severity with
        | Ocamlc_loc.Error None -> "error"
        | Ocamlc_loc.Error (Some (Ocamlc_loc.Code { code; name })) ->
          sprintf "error %d (%s)" code name
        | Ocamlc_loc.Error (Some (Ocamlc_loc.Alert a)) ->
          sprintf "error \"%s\"" a
        | Ocamlc_loc.Warning (Ocamlc_loc.Code { code; name }) ->
          sprintf "warning %d (%s)" code name
        | Ocamlc_loc.Warning (Ocamlc_loc.Alert a) -> sprintf "error \"%s\"" a
    in
    Format.asprintf
      "%s: %s on %a%a"
      fmter
      reason
      print_loc
      loc
      print_msg
      message
  | _ :: _ ->
    (* TODO: come up with a reasonable way to display several error messages,
       given vscode popup doesn't allow newlines. *)
    fallback_msg

let message err =
  match err with
  | Unsupported_syntax syntax ->
    sprintf
      "formatting %s files is not supported"
      (Document.Syntax.human_name syntax)
  | Missing_binary { binary } ->
    sprintf
      "Unable to find %s binary. You need to install %s manually to use the \
       formatting feature."
      binary
      binary
  | Unknown_extension uri ->
    Printf.sprintf
      "Unable to format. File %s has an unknown extension"
      (Uri.to_path uri)
  | Unexpected_result { message } -> (
    let lines = String.split ~on:'\n' message in
    match lines with
    | fst :: compiler_error_lines
      when String.is_suffix fst ~suffix:"(syntax error)" ->
      let compiler_err = String.concat ~sep:"\n" compiler_error_lines in
      err_msg ~fallback_msg:message ~compiler_err
    | fst :: rest
      when String.is_suffix
             fst
             ~suffix:"(misplaced documentation comments - warning 50)" -> (
      let rest = List.filter ~f:(fun s -> not (String.is_empty s)) rest in
      match List.destruct_last rest with
      | None -> message
      | Some (compiler_err_lines, hint) ->
        let compiler_err = String.concat ~sep:"\n" compiler_err_lines in
        let err_msg = err_msg ~fallback_msg:message ~compiler_err in
        String.concat ~sep:". " [ err_msg; hint ])
    | _ -> message)

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
  | Reason ->
    Ok
      (Reason
         (match Document.kind doc with
         | `Merlin m -> Document.Merlin.kind m
         | `Other -> Code_error.raise "unable to format non merlin document" []))

let exec cancel bin args stdin =
  let refmt = Fpath.to_string bin in
  let+ res, cancel = run_command cancel refmt stdin args in
  match cancel with
  | Cancelled () ->
    let e =
      Jsonrpc.Response.Error.make ~code:RequestCancelled ~message:"cancelled" ()
    in
    raise (Jsonrpc.Response.Error.E e)
  | Not_cancelled -> (
    match res.status with
    | Unix.WEXITED 0 -> Result.Ok res.stdout
    | _ -> Result.Error (Unexpected_result { message = res.stderr }))

let run doc cancel : (TextEdit.t list, error) result Fiber.t =
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
    exec cancel binary args contents
    |> Fiber.map ~f:(Result.map ~f:(fun to_ -> Diff.edit ~from:contents ~to_))

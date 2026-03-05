open Import
open Fiber.O

type command_result =
  { stdout : string
  ; stderr : string
  ; status : Unix.process_status
  }

let run_command ?(cwd = Spawn.Working_dir.Inherit) cancel prog stdin_value args =
  Fiber.of_thunk (fun () ->
    let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
    let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
    let stderr_i, stderr_o = Unix.pipe ~cloexec:true () in
    let pid =
      let argv = prog :: args in
      Spawn.spawn ~cwd ~prog ~argv ~stdin:stdin_i ~stdout:stdout_o ~stderr:stderr_o ()
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

let exec ?cwd cancel refmt args stdin =
  let+ res, cancel = run_command ?cwd cancel refmt stdin args in
  match cancel with
  | Cancelled () ->
    let e = Jsonrpc.Response.Error.make ~code:RequestCancelled ~message:"cancelled" () in
    raise (Jsonrpc.Response.Error.E e)
  | Not_cancelled ->
    (match res.status with
     | Unix.WEXITED 0 -> Result.Ok res
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
    |> Fiber.map
         ~f:(Result.map ~f:(fun { stdout = to_; _ } -> Diff.edit ~from:contents ~to_))
;;

(** Tries to access the formatter's configuration to determine the existing
  margin, and returns an argument which sets the margin to the existing margin
  minus the offset
*)
let compute_modified_margin binary cancel offset formatter =
  let default = 80 in
  match formatter with
  | Ocaml uri | Mlx uri ->
    let path = Uri.to_path uri |> Filename.dirname in
    (* specifying cwd makes sure ocamlformat finds the correct root,
       since we're not actually passing any file here *)
    exec ~cwd:(Spawn.Working_dir.Path path) cancel binary [ "--print-config" ] ""
    |> Fiber.map ~f:(fun res ->
      let margin =
        match res with
        | Ok { stderr = config; _ } ->
          config
          |> String.split_lines
          |> List.find_map ~f:(fun line ->
            match String.drop_prefix line ~prefix:"margin=" with
            | None -> None
            | Some margin ->
              String.split margin ~on:' ' |> List.hd |> Option.bind ~f:Int.of_string)
          |> Option.value ~default
        | Error _ -> default
      in
      let margin = margin - offset in
      "--margin=" ^ Int.to_string margin)
  | Reason _ ->
    let margin =
      Sys.getenv_opt "REFMT_PRINT_WIDTH"
      |> Option.bind ~f:Int.of_string
      |> Option.value ~default
    in
    let margin = margin - offset in
    Fiber.return ("--print-width=" ^ Int.to_string margin)
;;

let range_formatter doc =
  match Document.syntax doc with
  (* to support formatting semantic action snippets in these two files *)
  | Ocamllex | Menhir -> Ok (Ocaml (Document.uri doc))
  | _ -> formatter doc
;;

let run_on_range doc range cancel : (TextEdit.t list, error) result Fiber.t =
  let res =
    let open Result.O in
    let* formatter = range_formatter doc in
    let+ binary = binary formatter in
    binary, formatter
  in
  match res with
  | Error e -> Fiber.return (Error e)
  | Ok (binary, formatter) ->
    let contents = Document.source doc |> Msource.text in
    let start, stop = Text_document.absolute_range (Document.text_document doc) range in
    (* basic idea:
    - slice out the range to be formatted, send to ocamlformat
    - whitespace pad the start of lines in the reply based on the selection start
    - stitch everything back together. *)
    let prefix, to_format, suffix =
      ( String.sub contents ~pos:0 ~len:start
      , String.sub contents ~pos:start ~len:(stop - start)
      , String.sub contents ~pos:stop ~len:(String.length contents - stop) )
    in
    let args = args formatter in
    let args =
      (* if we're formatting the start of a [let ... in] construct,
        don't emit [;;] before the [in]! *)
      let next =
        String.trim suffix ~drop:(function
          | ' ' | '\n' | '\r' | '\t' -> true
          | _ -> false)
      in
      if String.is_prefix ~prefix:"in" next || String.is_prefix ~prefix:";;" next
      then "--let-binding-spacing=compact" :: args
      else args
    in
    let column_offset = range.start.character in
    let pad s =
      let padding = Bytes.make column_offset ' ' |> Bytes.to_string in
      String.concat ~sep:"\n" (List.map (String.split_lines s) ~f:(( ^ ) padding))
      |> String.trim ~drop:(( = ) ' ')
    in
    let open Fiber.O in
    let* margin = compute_modified_margin binary cancel column_offset formatter in
    let args = margin :: args in
    let+ r = exec cancel binary args to_format in
    Result.map r ~f:(fun { stdout = formatted; _ } ->
      let formatted =
        (* if the return is unchanged, don't insert extra padding *)
        if String.equal formatted to_format then to_format else pad formatted
      in
      let to_ = prefix ^ formatted ^ suffix in
      Diff.edit ~from:contents ~to_)
;;

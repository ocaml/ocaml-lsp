module Import = struct
  include struct
    open Base
    module Array = Array
    module Buffer = Buffer
    module Char = Char
    module Either = Either
    module Float = Float
    module Hashtbl = Hashtbl
    module Int = Int
    module List = List
    module Map = Map
    module Option = Option
    module Poly = Poly
    module Queue = Queue
    module Result = Result
    module Set = Set
    module String = String
  end

  let sprintf = Printf.sprintf

  module Option = struct
    include Option

    module O = struct
      let ( let+ ) value f = map value ~f
      let ( let* ) value f = bind value ~f
    end
  end

  module Exn_with_backtrace = Stdune.Exn_with_backtrace

  module Array_iter : sig
    type 'a t

    val create : 'a array -> 'a t
    val has_next : 'a t -> bool
    val next : 'a t -> 'a option
    val next_exn : 'a t -> 'a
  end = struct
    type 'a t =
      { contents : 'a array
      ; mutable ix : int
      }

    let create contents = { contents; ix = 0 }
    let has_next t = t.ix < Array.length t.contents

    let next_exn t =
      let { contents; ix } = t in
      let v = contents.(ix) in
      t.ix <- ix + 1;
      v
    ;;

    let next t = if has_next t then Some (next_exn t) else None
  end

  include Fiber.O
  module Bin = Ocaml_lsp_server.Testing.Bin
  module Client = Lsp_fiber.Client
  include Lsp.Types
  module Uri = Lsp.Uri
  module Position = Ocaml_lsp_server.Position
end

open Import

let start_client
      ?(capabilities = ClientCapabilities.create ())
      ?workspaceFolders
      ?trace
      client
  =
  Client.start client (InitializeParams.create ~capabilities ?workspaceFolders ?trace ())
;;

let shutdown_client client =
  let* () = Client.request client Shutdown in
  Client.stop client
;;

let exit_client client =
  let* () = Client.request client Shutdown in
  Client.notification client Exit
;;

let bin = Bin.which "ocamllsp" |> Option.value_exn

let waitpid ?(timeout = 5.0) pid =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec wait () =
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ when Float.(Unix.gettimeofday () < deadline) ->
      Unix.sleepf 0.01;
      wait ()
    | 0, _ ->
      (match Unix.kill pid Sys.sigkill with
       | () -> ()
       | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ());
      Unix.waitpid [] pid |> snd
    | _, status -> status
  in
  wait ()
;;

module T : sig
  val run_with_status
    :  ?extra_env:string list
    -> ?handler:unit Client.Handler.t
    -> ?stderr:Unix.file_descr
    -> ?timeout:float
    -> ?on_spawn:(int -> unit)
    -> (unit Client.t -> 'a Fiber.t)
    -> Unix.process_status * 'a

  val run
    :  ?extra_env:string list
    -> ?handler:unit Client.Handler.t
    -> ?stderr:Unix.file_descr
    -> ?timeout:float
    -> ?on_spawn:(int -> unit)
    -> (unit Client.t -> 'a Fiber.t)
    -> 'a

  (** Run a test after starting and initializing its client. The test remains
      responsible for shutting the client down. *)
  val run_initialized
    :  ?extra_env:string list
    -> ?handler:unit Client.Handler.t
    -> ?stderr:Unix.file_descr
    -> ?timeout:float
    -> ?capabilities:ClientCapabilities.t
    -> ?workspaceFolders:WorkspaceFolder.t list option
    -> ?trace:TraceValue.t
    -> ?on_spawn:(int -> unit)
    -> (unit Client.t -> 'a Fiber.t)
    -> 'a
end = struct
  let run_with_status
        ?(extra_env = [])
        ?handler
        ?(stderr = Unix.stderr)
        ?(timeout = 3.0)
        ?on_spawn
        f
    =
    let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
    let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
    let pid =
      let env = extra_env @ Array.to_list (Unix.environment ()) |> Spawn.Env.of_list in
      Spawn.spawn ~env ~prog:bin ~argv:[ bin ] ~stdin:stdin_i ~stdout:stdout_o ~stderr ()
    in
    Option.iter on_spawn ~f:(fun on_spawn -> on_spawn pid);
    Unix.close stdin_i;
    Unix.close stdout_o;
    let handler =
      match handler with
      | Some h -> h
      | None -> Client.Handler.make ()
    in
    let init =
      let blockity =
        if Sys.win32
        then `Blocking
        else (
          Unix.set_nonblock stdout_i;
          Unix.set_nonblock stdin_o;
          `Non_blocking true)
      in
      let make fd what =
        let fd = Lev_fiber.Fd.create fd blockity in
        Lev_fiber.Io.create fd what
      in
      let* in_ = make stdout_i Input in
      let* out = make stdin_o Output in
      let io = Lsp_fiber.Fiber_io.make in_ out in
      let client = Client.make handler io () in
      f client
    in
    (* TODO replace the wheel once we can cancel sleep *)
    let waitpid wheel =
      let* timeout = Lev_fiber.Timer.Wheel.task wheel in
      Fiber.finalize ~finally:(fun () -> Lev_fiber.Timer.Wheel.stop wheel)
      @@ fun () ->
      let cancelled = ref false in
      Fiber.fork_and_join_unit
        (fun () ->
           Lev_fiber.Timer.Wheel.await timeout
           >>| function
           | `Cancelled -> ()
           | `Ok ->
             Unix.kill pid Sys.sigkill;
             cancelled := true)
        (fun () ->
           let* (server_exit_status : Unix.process_status) = Lev_fiber.waitpid ~pid in
           let+ () =
             if !cancelled then Fiber.return () else Lev_fiber.Timer.Wheel.cancel timeout
           in
           server_exit_status)
    in
    Lev_fiber.run (fun () ->
      let* wheel = Lev_fiber.Timer.Wheel.create ~delay:timeout in
      let+ res = init
      and+ status =
        Fiber.fork_and_join_unit
          (fun () -> Lev_fiber.Timer.Wheel.run wheel)
          (fun () -> waitpid wheel)
      in
      status, res)
    |> Lev_fiber.Error.ok_exn
  ;;

  let run ?extra_env ?handler ?stderr ?timeout ?on_spawn f =
    snd @@ run_with_status ?extra_env ?handler ?stderr ?timeout ?on_spawn f
  ;;

  let run_initialized
        ?extra_env
        ?handler
        ?stderr
        ?timeout
        ?(capabilities = ClientCapabilities.create ())
        ?workspaceFolders
        ?trace
        ?on_spawn
        f
    =
    run ?extra_env ?handler ?stderr ?timeout ?on_spawn
    @@ fun client ->
    let run_client () = start_client ~capabilities ?workspaceFolders ?trace client in
    let run_test () =
      let* (_ : InitializeResult.t) = Client.initialized client in
      f client
    in
    let+ (), result = Fiber.fork_and_join run_client run_test in
    result
  ;;
end

include T

let write_file path data = Fs_io.write_file ~perm:0o666 ~path ~data |> Result.ok_exn

let temp_dir ?temp_dir prefix =
  let dir = Filename.temp_file ?temp_dir prefix "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  dir
;;

let run_command ?cwd command =
  let command =
    match cwd with
    | None -> command
    | Some cwd -> Printf.sprintf "cd %s && %s" (Filename.quote cwd) command
  in
  if Sys.command command <> 0 then failwith command
;;

let null_device = if Sys.win32 then "NUL" else "/dev/null"

let drain_diagnostics () =
  let diagnostics = Fiber.Ivar.create () in
  let on_notification _ = function
    | Lsp.Server_notification.PublishDiagnostics _ ->
      let* diag = Fiber.Ivar.peek diagnostics in
      (match diag with
       | Some _ -> Fiber.return ()
       | None -> Fiber.Ivar.fill diagnostics ())
    | _ -> Fiber.return ()
  in
  on_notification, diagnostics
;;

let run_request ?(prep = fun _ -> Fiber.return ()) ?settings request =
  let on_notification, diagnostics = drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  let capabilities =
    let window =
      let showDocument = ShowDocumentClientCapabilities.create ~support:true in
      WindowClientCapabilities.create ~showDocument ()
    in
    ClientCapabilities.create ~window ()
  in
  run_initialized ~handler ~capabilities
  @@ fun client ->
  let* () = prep client in
  let* () =
    match settings with
    | Some settings -> Client.notification client (ChangeConfiguration { settings })
    | None -> Fiber.return ()
  in
  let* ret = Client.request client request in
  let* () = Fiber.Ivar.read diagnostics in
  let+ () = Client.stop client in
  ret
;;

let custom_request client meth params =
  let params = Some (Jsonrpc.Structured.t_of_yojson params) in
  Client.request client (UnknownRequest { meth; params })
;;

let open_document ?(language_id = "ocaml") ~client ~uri ~source () =
  let textDocument =
    TextDocumentItem.create
      ~uri
      ~languageId:(LanguageKind.Other language_id)
      ~version:0
      ~text:source
  in
  Client.notification
    client
    (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
;;

let offset_of_position src (pos : Position.t) =
  let line_offset =
    String.split_lines src
    |> fun lines ->
    List.take lines pos.line |> List.fold_left ~init:0 ~f:(fun s l -> s + String.length l)
  in
  line_offset + pos.line (* account for line endings *) + pos.character
;;

let apply_edits src edits =
  let edits =
    List.sort edits ~compare:(fun (e : TextEdit.t) (e' : TextEdit.t) ->
      Lsp.Position.compare e.range.start e'.range.start)
  in
  (* check that edits are non-overlapping *)
  let rec overlaps : TextEdit.t list -> _ = function
    | [] | [ _ ] -> false
    | e :: e' :: es ->
      if Lsp.Position.compare e.range.end_ e'.range.start > 0
      then true
      else overlaps (e' :: es)
  in
  if overlaps edits then failwith "overlapping edits";
  let _, edits =
    (* compute start and end character offsets for each edit *)
    List.map edits ~f:(fun (e : TextEdit.t) ->
      e.newText, offset_of_position src e.range.start, offset_of_position src e.range.end_)
    (* update the offsets to account for preceding edits *)
    |> List.fold_map ~init:0 ~f:(fun offset (new_text, start, end_) ->
      if end_ < start then failwith "invalid edit: end before start";
      ( offset + (String.length new_text - (end_ - start))
      , (new_text, start + offset, end_ + offset) ))
  in
  (* apply edits *)
  List.fold_left edits ~init:src ~f:(fun src (new_text, start, end_) ->
    String.prefix src start ^ new_text ^ String.drop_prefix src end_)
;;

let print_result result =
  result |> Yojson.Safe.pretty_to_string ~std:false |> print_endline
;;

let print_list yojson_of_t xs = print_result (`List (List.map xs ~f:yojson_of_t))

let print_option ?(none = "[]") yojson_of_t = function
  | None -> print_endline none
  | Some x -> print_result (yojson_of_t x)
;;

let print_option_list ?(none = "[]") yojson_of_t = function
  | None -> print_endline none
  | Some xs -> print_list yojson_of_t xs
;;

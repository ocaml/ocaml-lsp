module Import = struct
  include struct
    include Stdune

    module List = struct
      include List

      let find_mapi ~f l =
        let rec k i = function
          | [] -> None
          | x :: xs ->
            (match f i x with
             | Some x' -> Some x'
             | None -> (k [@tailcall]) (i + 1) xs)
        in
        k 0 l
      ;;

      let take n l =
        let rec take acc n l =
          if n = 0
          then acc
          else (
            match l with
            | [] -> failwith "list shorter than n"
            | x :: xs -> (take [@tailcall]) (x :: acc) (n - 1) xs)
        in
        List.rev (take [] n l)
      ;;
    end

    module Array = struct
      include Array

      module Iter : sig
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
    end
  end

  include Fiber.O
  module Client = Lsp_fiber.Client
  include Lsp.Types
  module Uri = Lsp.Uri
  module Position = Ocaml_lsp_server.Position
end

open Import

module T : sig
  val run_with_status
    :  ?extra_env:string list
    -> ?handler:unit Client.Handler.t
    -> (unit Client.t -> 'a Fiber.t)
    -> Unix.process_status * 'a

  val run
    :  ?extra_env:string list
    -> ?handler:unit Client.Handler.t
    -> (unit Client.t -> 'a Fiber.t)
    -> 'a
end = struct
  let _PATH = Bin.parse_path (Option.value ~default:"" @@ Env.get Env.initial "PATH")
  let bin = Bin.which "ocamllsp" ~path:_PATH |> Option.value_exn |> Path.to_string

  let run_with_status ?(extra_env = []) ?handler f =
    let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
    let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
    let pid =
      let env =
        let current = Unix.environment () in
        Array.to_list current @ extra_env |> Spawn.Env.of_list
      in
      Spawn.spawn ~env ~prog:bin ~argv:[ bin ] ~stdin:stdin_i ~stdout:stdout_o ()
    in
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
      let* wheel = Lev_fiber.Timer.Wheel.create ~delay:3.0 in
      let+ res = init
      and+ status =
        Fiber.fork_and_join_unit
          (fun () -> Lev_fiber.Timer.Wheel.run wheel)
          (fun () -> waitpid wheel)
      in
      status, res)
    |> Lev_fiber.Error.ok_exn
  ;;

  let run ?extra_env ?handler f = snd @@ run_with_status ?extra_env ?handler f
end

include T

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
  run ~handler
  @@ fun client ->
  let run_client () =
    let capabilities =
      let window =
        let showDocument = ShowDocumentClientCapabilities.create ~support:true in
        WindowClientCapabilities.create ~showDocument ()
      in
      ClientCapabilities.create ~window ()
    in
    Client.start client (InitializeParams.create ~capabilities ())
  in
  let run =
    let* (_ : InitializeResult.t) = Client.initialized client in
    let* () = prep client in
    let* () =
      match settings with
      | Some settings -> Client.notification client (ChangeConfiguration { settings })
      | None -> Fiber.return ()
    in
    Client.request client request
  in
  Fiber.fork_and_join_unit run_client (fun () ->
    let* ret = run in
    let* () = Fiber.Ivar.read diagnostics in
    let+ () = Client.stop client in
    ret)
;;

let openDocument ~client ~uri ~source =
  let textDocument =
    TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text:source
  in
  Client.notification
    client
    (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
;;

let offset_of_position src (pos : Position.t) =
  let line_offset =
    String.split_lines src
    |> List.take pos.line
    |> List.fold_left ~init:0 ~f:(fun s l -> s + String.length l)
  in
  line_offset + pos.line (* account for line endings *) + pos.character
;;

let apply_edits src edits =
  let edits =
    List.sort edits ~compare:(fun (e : TextEdit.t) (e' : TextEdit.t) ->
      Position.compare e.range.start e'.range.start)
  in
  (* check that edits are non-overlapping *)
  let rec overlaps : TextEdit.t list -> _ = function
    | [] | [ _ ] -> false
    | e :: e' :: es ->
      (match Position.compare e.range.end_ e'.range.start with
       | Gt -> true
       | Lt | Eq -> overlaps (e' :: es))
  in
  if overlaps edits then failwith "overlapping edits";
  let _, edits =
    (* compute start and end character offsets for each edit *)
    List.map edits ~f:(fun (e : TextEdit.t) ->
      e.newText, offset_of_position src e.range.start, offset_of_position src e.range.end_)
    (* update the offsets to account for preceding edits *)
    |> List.fold_left_map ~init:0 ~f:(fun offset (new_text, start, end_) ->
      if end_ < start then failwith "invalid edit: end before start";
      ( offset + (String.length new_text - (end_ - start))
      , (new_text, start + offset, end_ + offset) ))
  in
  (* apply edits *)
  List.fold_left edits ~init:src ~f:(fun src (new_text, start, end_) ->
    String.take src start ^ new_text ^ String.drop src end_)
;;

let print_result result =
  result |> Yojson.Safe.pretty_to_string ~std:false |> print_endline
;;

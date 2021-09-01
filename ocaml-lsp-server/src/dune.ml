open! Import
open Fiber.O

let dev_null () =
  (* TODO stdune should provide an api to simplify this *)
  Unix.openfile
    (if Sys.win32 then
      "nul"
    else
      "/dev/null")
    [ Unix.O_RDWR ] 0o666

module Csexp_rpc = Csexp_rpc.Make (struct
  type t = Scheduler.thread

  let stop t = Scheduler.stop t

  let create () = Scheduler.create_thread ()

  let task t ~f =
    let open Fiber.O in
    let res = Scheduler.async t f in
    match res with
    | Error `Stopped -> Fiber.return (Error `Stopped)
    | Ok t -> (
      let+ res = Scheduler.await_no_cancel t in
      match res with
      | Ok s -> Ok s
      | Error e -> Error (`Exn e))
end)

module Chan : sig
  type t

  val create : Csexp_rpc.Session.t -> t

  val write : t -> Csexp.t list option -> unit Fiber.t

  val read : t -> Csexp.t option Fiber.t

  val stop : t -> unit Fiber.t

  val run : t -> unit Fiber.t
end = struct
  open Fiber.O

  type t =
    { session : Csexp_rpc.Session.t
    ; finished : unit Fiber.Ivar.t
    }

  let stop t = Csexp_rpc.Session.write t.session None

  let write t sexp = Csexp_rpc.Session.write t.session sexp

  let read t =
    let* read = Csexp_rpc.Session.read t.session in
    match read with
    | Some _ -> Fiber.return read
    | None ->
      let+ () = Fiber.Ivar.fill t.finished () in
      read

  let create session =
    let finished = Fiber.Ivar.create () in
    { session; finished }

  let run t = Fiber.Ivar.read t.finished
end

module Client =
  Drpc.Client.Make
    (struct
      include Fiber

      let parallel_iter t ~f =
        let stream = Fiber.Stream.In.create t in
        Fiber.Stream.In.parallel_iter stream ~f
    end)
    (Chan)

module Where =
  Drpc.Where.Make
    (struct
      type 'a t = 'a

      let return x = x

      module O = struct
        let ( let* ) x f = f x

        let ( let+ ) x f = f x
      end
    end)
    (struct
      let getenv = Sys.getenv_opt

      let is_win32 () = Sys.win32

      let read_file f = Stdune.Io.String_path.read_file f

      let readlink s =
        match Unix.readlink s with
        | s -> Some s
        | exception Unix.Unix_error (Unix.EINVAL, _, _) -> None

      let analyze_path s =
        match (Unix.stat s).st_kind with
        | Unix.S_SOCK -> `Unix_socket
        | S_REG -> `Normal_file
        | _
        | (exception Unix.Unix_error (Unix.ENOENT, _, _)) ->
          `Other
    end)

type run =
  | Binary_not_found
  | Out_of_date

type state =
  | Waiting_for_init of
      { diagnostics : Diagnostics.t
      ; progress : Progress.t
      ; build_dir : string
      ; poll_thread : Scheduler.thread
      }
  | Active of
      { diagnostics : Diagnostics.t
      ; progress : Progress.t
      ; finish : unit Fiber.Ivar.t
      ; chan : Chan.t
      }
  | Closed

type t = state ref

(* TODO we an atomic version of this *)
let maybe_fill ivar x =
  let* res = Fiber.Ivar.peek ivar in
  match res with
  | Some _ -> Fiber.return ()
  | None -> Fiber.Ivar.fill ivar x

let stop (t : t) =
  match !t with
  | Closed -> Fiber.return ()
  | Waiting_for_init _ ->
    t := Closed;
    Fiber.return ()
  | Active { finish; chan; progress = _; diagnostics = _ } ->
    t := Closed;
    let* () = Chan.stop chan in
    maybe_fill finish ()

let create ~build_dir diagnostics progress =
  let+ poll_thread = Scheduler.create_thread () in
  ref (Waiting_for_init { build_dir; diagnostics; progress; poll_thread })

let lsp_of_dune dune =
  let module D = Drpc.Diagnostic in
  let range_of_loc loc =
    let loc =
      let loc_start = Drpc.Loc.start loc in
      let loc_end = Drpc.Loc.stop loc in
      { Loc.loc_start; loc_end; loc_ghost = false }
    in
    Range.of_loc loc
  in
  let range =
    match D.loc dune with
    | None -> Range.first_line
    | Some loc -> range_of_loc loc
  in
  let severity =
    D.severity dune
    |> Option.map ~f:(function
         | D.Error -> DiagnosticSeverity.Error
         | Warning -> DiagnosticSeverity.Warning)
  in
  let make_message message =
    String.trim (Format.asprintf "%a@." Pp.to_fmt message)
  in
  let relatedInformation =
    match D.related dune with
    | [] -> None
    | related ->
      Some
        (List.map related ~f:(fun related ->
             let message = make_message (D.Related.message related) in
             let loc = D.Related.loc related in
             let uri =
               let start = Drpc.Loc.start loc in
               Uri.of_path start.pos_fname
             in
             let location =
               let range = range_of_loc loc in
               Location.create ~uri ~range
             in
             DiagnosticRelatedInformation.create ~location ~message))
  in
  let message = make_message (D.message dune) in
  Diagnostic.create ?relatedInformation ~range ?severity ~source:"dune" ~message
    ()

let poll_where ~poll_thread ~build_dir ~delay =
  let* timer = Scheduler.create_timer ~delay in
  let poll () = Where.get ~build_dir in
  let rec loop_sleep () =
    let* res = Scheduler.schedule timer Fiber.return in
    match res with
    | Ok () -> loop ()
    | Error `Cancelled -> assert false
  and loop () =
    let* where =
      let task = Scheduler.async_exn poll_thread poll in
      Scheduler.await_no_cancel task
    in
    match where with
    | Error e -> Exn_with_backtrace.reraise e
    | Ok None -> loop_sleep ()
    | Ok (Some where) -> (
      let sock =
        match where with
        | `Unix s -> Unix.ADDR_UNIX s
        | `Ip (`Host h, `Port p) ->
          Unix.ADDR_INET (Unix.inet_addr_of_string h, p)
      in
      let* client = Csexp_rpc.Client.create sock in
      let* session = Csexp_rpc.Client.connect client in
      match session with
      | Ok session -> Fiber.return (Chan.create session)
      | Error _ ->
        Csexp_rpc.Client.stop client;
        loop_sleep ())
  in
  let+ res = loop () in
  res

let run_rpc (t : t) =
  match !t with
  | Closed -> Code_error.raise "dune already closed" []
  | Active _ -> Code_error.raise "dune alrady running" []
  | Waiting_for_init { poll_thread; diagnostics; progress; build_dir } ->
    Diagnostics.update_dune_status diagnostics Disconnected;
    let open Fiber.O in
    let finish = Fiber.Ivar.create () in
    let* chan =
      Fiber.fork_and_join_unit
        (fun () -> Diagnostics.send diagnostics)
        (fun () -> poll_where ~poll_thread ~delay:0.3 ~build_dir)
    in
    t := Active { diagnostics; finish; chan; progress };
    let* () =
      Fiber.all_concurrently_unit
        [ (let* () = Chan.run chan in
           (* TODO ideally, we should notify the users that the diagnostics are
              stale until they run dune again *)
           maybe_fill finish ())
        ; (let init =
             Drpc.Initialize.create ~id:(Drpc.Id.make (Atom "ocamllsp"))
           in
           let handler =
             let build_progress =
               if Progress.should_report_build_progress progress then
                 Some (Progress.build_progress progress)
               else
                 None
             in
             let diagnostic evs =
               List.iter evs ~f:(fun (ev : Drpc.Diagnostic.Event.t) ->
                   let id =
                     Drpc.Diagnostic.id
                       (match ev with
                       | Add x -> x
                       | Remove x -> x)
                   in
                   match ev with
                   | Remove _ -> Diagnostics.remove diagnostics (`Dune id)
                   | Add d ->
                     let uri : Uri.t =
                       match Drpc.Diagnostic.loc d with
                       | None -> Diagnostics.workspace_root diagnostics
                       | Some loc ->
                         let { Lexing.pos_fname; _ } = Drpc.Loc.start loc in
                         Uri.of_path pos_fname
                     in
                     Diagnostics.set diagnostics
                       (`Dune (id, uri, lsp_of_dune d)));
               Diagnostics.send diagnostics
             in
             Client.Handler.create ?build_progress ~diagnostic ()
           in
           Client.connect ~handler chan init ~f:(fun client ->
               Diagnostics.update_dune_status diagnostics Connected;
               let* () =
                 let sub what =
                   Client.notification client Drpc.Notification.subscribe what
                 in
                 if Progress.should_report_build_progress progress then
                   Fiber.fork_and_join_unit
                     (fun () -> sub Diagnostics)
                     (fun () -> sub Build_progress)
                 else
                   sub Diagnostics
               in
               Fiber.Ivar.read finish))
        ]
    in
    t := Waiting_for_init { poll_thread; diagnostics; progress; build_dir };
    Progress.end_build_if_running progress

let run t : (unit, run) result Fiber.t =
  match !t with
  | Closed -> Code_error.raise "dune already closed" []
  | Active _ -> Code_error.raise "dune already running" []
  | Waiting_for_init _ -> (
    let open Fiber.O in
    match Bin.which "dune" with
    | None -> Fiber.return (Error Binary_not_found)
    | Some bin -> (
      let bin = Fpath.to_string bin in
      let stdin = dev_null () in
      let stdout = dev_null () in
      let stderr = dev_null () in
      let pid =
        let args = Array.of_list [ bin; "rpc"; "--help=plain" ] in
        Unix.create_process bin args stdin stdout stderr |> Pid.of_int
      in
      let* status = Scheduler.wait_for_process pid in
      match status with
      | Unix.WEXITED 0 ->
        let rec loop () =
          match !t with
          | Closed -> Fiber.return (Ok ())
          | Waiting_for_init _ ->
            let* () = run_rpc t in
            loop ()
          | Active _ -> assert false
        in
        loop ()
      | _ ->
        t := Closed;
        Fiber.return (Error Out_of_date)))

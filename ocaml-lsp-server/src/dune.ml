open! Import

let dev_null () =
  (* TODO stdune should provide an api to simplify this *)
  Unix.openfile
    (if Sys.win32 then
      "nul"
    else
      "/dev/null")
    [ Unix.O_RDWR ] 0o666

module Chan : sig
  type t

  val create : string -> t Fiber.t

  val write : t -> Csexp.t list option -> unit Fiber.t

  val read : t -> Csexp.t option Fiber.t

  val pid : t -> Pid.t

  val run : t -> unit Fiber.t
end = struct
  open Fiber.O

  type t =
    { in_thread : Scheduler.thread
    ; out_thread : Scheduler.thread
    ; out_chan : out_channel
    ; in_chan : in_channel
    ; pid : Pid.t
    }

  let pid t = t.pid

  let write t sexp =
    match
      Scheduler.async t.out_thread (fun () ->
          match sexp with
          | None ->
            close_in_noerr t.in_chan;
            close_out_noerr t.out_chan
          | Some sexps ->
            List.iter sexps ~f:(Csexp.to_channel t.out_chan);
            flush t.out_chan)
    with
    | Error `Stopped ->
      (* It's ok to ignore this write. It will be not cause a deadlock because
         all pending requests will be cancelled. *)
      Fiber.return ()
    | Ok task -> (
      let+ res = Scheduler.await_no_cancel task in
      match res with
      | Error e -> Exn_with_backtrace.reraise e
      | Ok s -> s)

  let read t =
    match
      Scheduler.async t.in_thread (fun () ->
          match Csexp.input_opt t.in_chan with
          | Error _
          | Ok None ->
            close_in_noerr t.in_chan;
            None
          | Ok (Some s) -> Some s)
    with
    | Error `Stopped -> Fiber.return None
    | Ok res -> (
      let+ res = Scheduler.await_no_cancel res in
      match res with
      | Ok s -> s
      | Error e -> Exn_with_backtrace.reraise e)

  let create dune =
    let args = Array.of_list [ dune; "rpc"; "init"; "--wait" ] in
    let pid, stdout, stdin =
      let stdin_i, stdin_o = Unix.pipe () in
      let stdout_i, stdout_o = Unix.pipe () in
      let pid =
        Unix.create_process dune args stdin_i stdout_o Unix.stderr |> Pid.of_int
      in
      (pid, stdout_i, stdin_o)
    in
    let* in_thread = Scheduler.create_thread () in
    let+ out_thread = Scheduler.create_thread () in
    let in_chan = Unix.in_channel_of_descr stdout in
    let out_chan = Unix.out_channel_of_descr stdin in
    { pid; in_chan; out_chan; in_thread; out_thread }

  let run t =
    let+ (_ : Unix.process_status) = Scheduler.wait_for_process t.pid in
    Scheduler.stop t.in_thread;
    Scheduler.stop t.out_thread;
    close_out_noerr t.out_chan;
    close_in_noerr t.in_chan
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

type run =
  | Binary_not_found
  | Out_of_date

type state =
  | Waiting_for_init of
      { diagnostics : Diagnostics.t
      ; progress : Progress.t
      }
  | Active of
      { diagnostics : Diagnostics.t
      ; progress : Progress.t
      ; finish : unit Fiber.Ivar.t
      ; chan : Chan.t
      }
  | Closed

type t = state ref

let stop (t : t) =
  match !t with
  | Closed -> Fiber.return ()
  | Waiting_for_init _ ->
    t := Closed;
    Fiber.return ()
  | Active { finish; chan; progress = _; diagnostics = _ } ->
    t := Closed;
    let pid = Chan.pid chan in
    Unix.kill (Pid.to_int pid) Sys.sigstop;
    Fiber.Ivar.fill finish ()

let create diagnostics progress =
  ref (Waiting_for_init { diagnostics; progress })

let lsp_of_dune uri dune =
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
  let make_message message = Format.asprintf "%a@." Pp.to_fmt message in
  let relatedInformation =
    match D.related dune with
    | [] -> None
    | related ->
      Some
        (List.map related ~f:(fun related ->
             let message = make_message (D.Related.message related) in
             let location =
               let range = range_of_loc (D.Related.loc related) in
               Location.create ~uri ~range
             in
             DiagnosticRelatedInformation.create ~location ~message))
  in
  let message = make_message (D.message dune) in
  Diagnostic.create ?relatedInformation ~range ?severity ~source:"dune" ~message
    ()

let run_rpc (t : t) bin =
  match !t with
  | Closed -> Code_error.raise "dune already closed" []
  | Active _ -> Code_error.raise "dune alrady running" []
  | Waiting_for_init { diagnostics; progress } ->
    Diagnostics.update_dune_status diagnostics Disconnected;
    let open Fiber.O in
    let finish = Fiber.Ivar.create () in
    let* chan = Chan.create bin in
    t := Active { diagnostics; finish; chan; progress };
    let* () =
      Fiber.parallel_iter
        ~f:(fun f -> f ())
        [ (fun () -> Diagnostics.send diagnostics)
        ; (fun () ->
            let* () = Chan.run chan in
            (* TODO ideally, we should notify the users that the diagnostics are
               stale until they run dune again *)
            Fiber.Ivar.fill finish ())
        ; (fun () ->
            let init =
              Drpc.Initialize.create ~id:(Drpc.Id.make (Atom "ocamllsp"))
            in
            let handler =
              let build_event, build_progress =
                if Progress.should_report_build_progress progress then
                  Progress.
                    (Some (build_event progress), Some (build_progress progress))
                else
                  (None, None)
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
                        (`Dune (id, uri, lsp_of_dune uri d)));
                Diagnostics.send diagnostics
              in
              Client.Handler.create ?build_event ?build_progress ~diagnostic ()
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
    t := Waiting_for_init { diagnostics; progress };
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
            let* () = run_rpc t bin in
            loop ()
          | Active _ -> assert false
        in
        loop ()
      | _ ->
        t := Closed;
        Fiber.return (Error Out_of_date)))

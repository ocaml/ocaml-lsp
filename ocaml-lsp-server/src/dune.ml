open! Import
open Fiber.O

let view_promotion_capability = ("diagnostic_promotions", `Bool true)

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

module Registry = Drpc.Registry

module Client =
  Drpc.Client.Make
    (struct
      include Fiber

      let parallel_iter t ~f =
        let stream = Fiber.Stream.In.create t in
        Fiber.Stream.In.parallel_iter stream ~f
    end)
    (Chan)

module Poll =
  Drpc.Registry.Poll
    (Fiber)
    (struct
      let scandir s =
        Fiber.return
          (match Sys.readdir s with
          | s -> Ok (Array.to_list s)
          | exception Sys_error _ -> Ok []
          | exception exn -> Error exn)

      let stat s =
        Fiber.return
          (match Unix.stat s with
          | exception exn -> Error exn
          | s -> Ok (`Mtime s.st_mtime))

      let read_file s =
        Fiber.return (Result.try_with (fun () -> Io.String_path.read_file s))
    end)

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
      let read_file f =
        Result.try_with (fun () -> Stdune.Io.String_path.read_file f)

      let analyze_path s =
        match (Unix.stat s).st_kind with
        | Unix.S_SOCK -> Ok `Unix_socket
        | S_REG -> Ok `Normal_file
        | _
        | (exception Unix.Unix_error (Unix.ENOENT, _, _)) ->
          Ok `Other
        | exception exn -> Error exn
    end)

type config =
  { diagnostics : Diagnostics.t
  ; include_promotions : bool
  ; progress : Progress.t
  ; log : type_:MessageType.t -> message:string -> unit Fiber.t
  }

module Instance : sig
  type t

  val stop : t -> unit Fiber.t

  val run : t -> unit Fiber.t

  val source : t -> Registry.Dune.t

  val create : Registry.Dune.t -> config -> t
end = struct
  type state =
    | Idle
    | Running of
        { chan : Chan.t
        ; finish : unit Fiber.Ivar.t
        }
    | Finished

  type t =
    { config : config
    ; source : Drpc.Registry.Dune.t
    ; mutable state : state
    }

  let source t = t.source

  let lsp_of_dune ~include_promotions dune =
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
    let data =
      match include_promotions with
      | false -> None
      | true -> (
        match D.promotion dune with
        | [] -> None
        | promotions ->
          let promotions =
            List.map promotions ~f:(fun p ->
                `Assoc
                  [ ("in_build", `String (D.Promotion.in_build p))
                  ; ("in_source", `String (D.Promotion.in_source p))
                  ])
          in
          Some (`Assoc [ (fst view_promotion_capability, `List promotions) ]))
    in
    Diagnostic.create ?relatedInformation ~range ?severity ~source:"dune"
      ~message ?data ()

  let progress_loop client progress =
    match Progress.should_report_build_progress progress with
    | false -> Fiber.return ()
    | true -> (
      let* res = Client.poll client Drpc.Sub.progress in
      match res with
      | Error v -> raise (Drpc.Version_error.E v)
      | Ok poll ->
        Fiber.repeat_while ~init:() ~f:(fun () ->
            let* res = Client.Stream.next poll in
            match res with
            | None -> Fiber.return None
            | Some p ->
              let+ () = Progress.build_progress progress p in
              Some ()))

  let diagnostic_loop client diagnostics ~include_promotions =
    let* res = Client.poll client Drpc.Sub.diagnostic in
    let send_diagnostics evs =
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
              (`Dune (id, uri, lsp_of_dune ~include_promotions d)));
      Diagnostics.send diagnostics
    in
    match res with
    | Error v -> raise (Drpc.Version_error.E v)
    | Ok poll ->
      Fiber.repeat_while ~init:() ~f:(fun () ->
          let* res = Client.Stream.next poll in
          match res with
          | None -> Fiber.return None
          | Some p ->
            let+ () = send_diagnostics p in
            Some ())

  (* TODO we an atomic version of this *)
  let maybe_fill ivar x =
    let* res = Fiber.Ivar.peek ivar in
    match res with
    | Some _ -> Fiber.return ()
    | None -> Fiber.Ivar.fill ivar x

  let stop t =
    match t.state with
    | Running { chan; _ } -> Chan.stop chan
    | _ -> Fiber.return ()

  let create source config = { config; source; state = Idle }

  let run ({ config; source; _ } as t) =
    assert (
      match t.state with
      | Idle -> true
      | _ -> false);
    let* () =
      let message =
        sprintf "Connecting to dune %s (%s)"
          (Registry.Dune.root source)
          (match Registry.Dune.where source with
          | `Unix s -> s
          | `Ip (`Host h, `Port p) -> sprintf "%s:%d" h p)
      in
      config.log ~type_:Info ~message
    in
    let where = Registry.Dune.where source in
    let sock =
      match where with
      | `Unix s -> Unix.ADDR_UNIX s
      | `Ip (`Host h, `Port p) -> Unix.ADDR_INET (Unix.inet_addr_of_string h, p)
    in
    let* client = Csexp_rpc.Client.create sock in
    let* session = Csexp_rpc.Client.connect client in
    match session with
    | Error exn ->
      let message =
        sprintf "unable to connect to dune %s" (Registry.Dune.root source)
      in
      let* () = config.log ~type_:Error ~message in
      t.state <- Finished;
      Exn_with_backtrace.reraise exn
    | Ok session ->
      let chan = Chan.create session in
      let finish = Fiber.Ivar.create () in
      t.state <- Running { chan; finish };
      let { progress; diagnostics; include_promotions; log = _ } = config in
      let* () =
        Fiber.all_concurrently_unit
          [ (let* () = Chan.run chan in
             (* TODO ideally, we should notify the users that the diagnostics
                are stale until they run dune again *)
             maybe_fill finish ())
          ; (let init =
               Drpc.Initialize.create ~id:(Drpc.Id.make (Atom "ocamllsp"))
             in
             Client.connect chan init ~f:(fun client ->
                 Diagnostics.update_dune_status diagnostics Connected;
                 let progress () = progress_loop client progress in
                 let diagnostics () =
                   diagnostic_loop client diagnostics ~include_promotions
                 in
                 let* () = Fiber.fork_and_join_unit progress diagnostics in
                 Fiber.Ivar.read finish))
          ]
      in
      Progress.end_build_if_running progress
end

module Dune_map = Map.Make (struct
  include Registry.Dune

  let compare x y = Ordering.of_int (compare x y)
end)

type active =
  { mutable instances : Instance.t Dune_map.t
  ; mutable workspaces : Workspaces.t
  ; finish : unit Fiber.Ivar.t
  ; registry : Registry.t
  ; config : config
  ; poll_thread : Scheduler.thread
  ; pool : Fiber.Pool.t
  }

let cwd = lazy (Sys.getcwd ())

let workspace_dune_overlap =
  (* Copy pasted from dune.

     All of this is really hacky and error prone. We should let the user
     associate dune instances with workspace folders somehow *)
  let is_dir_sep =
    if Sys.win32 || Sys.cygwin then
      fun c ->
    c = '/' || c = '\\' || c = ':'
    else
      fun c ->
    c = '/'
  in
  let explode_path =
    let rec start acc path i =
      if i < 0 then
        acc
      else if is_dir_sep (String.unsafe_get path i) then
        start acc path (i - 1)
      else
        component acc path i (i - 1)
    and component acc path end_ i =
      if i < 0 then
        String.take path (end_ + 1) :: acc
      else if is_dir_sep (String.unsafe_get path i) then
        start (String.sub path ~pos:(i + 1) ~len:(end_ - i) :: acc) path (i - 1)
      else
        component acc path end_ (i - 1)
    in
    fun path ->
      if path = Filename.current_dir_name then
        [ path ]
      else
        match start [] path (String.length path - 1) with
        | "." :: xs -> xs
        | xs -> xs
  in
  fun (wsf : WorkspaceFolder.t) (dune : Registry.Dune.t) ->
    let dune_root = Registry.Dune.root dune in
    let path =
      let path = Uri.to_path wsf.uri in
      if Filename.is_relative path then
        Filename.concat (Lazy.force cwd) path
      else
        path
    in
    let rec loop xs ys =
      match (xs, ys) with
      | x :: xs, y :: ys -> x = y && loop xs ys
      | [], _
      | _, [] ->
        true
    in
    loop (explode_path dune_root) (explode_path path)

let poll active =
  (* a single workspaces value for one iteration of the loop *)
  let workspaces = active.workspaces in
  let workspace_folders = Workspaces.workspace_folders workspaces in
  let* res = Poll.poll active.registry in
  match res with
  | Error _ -> (* TODO warn *) assert false
  | Ok _refresh ->
    let remaining, to_kill =
      Dune_map.partition active.instances ~f:(fun (running : Instance.t) ->
          let source = Instance.source running in
          List.exists workspace_folders ~f:(fun wsf ->
              workspace_dune_overlap wsf source))
    in
    let to_kill = Dune_map.values to_kill in
    active.instances <- remaining;
    let kill to_kill = Fiber.parallel_iter to_kill ~f:Instance.stop in
    let to_create =
      (* won't work very well with large workspaces and many instances of
         dune *)
      let is_running dune = Dune_map.mem active.instances dune in
      Registry.current active.registry
      |> List.fold_left ~init:[] ~f:(fun acc dune ->
             if
               (not (is_running dune))
               && List.exists workspace_folders
                    ~f:(fun (wsf : WorkspaceFolder.t) ->
                      workspace_dune_overlap wsf dune)
             then
               Instance.create dune active.config :: acc
             else
               acc)
    in
    active.instances <-
      List.fold_left to_create ~init:active.instances
        ~f:(fun acc (instance : Instance.t) ->
          let source = Instance.source instance in
          Dune_map.add_exn acc source instance);
    let create to_create =
      Fiber.parallel_iter to_create ~f:(fun instance ->
          let cleanup =
            lazy
              (active.instances <-
                Dune_map.remove active.instances (Instance.source instance))
          in
          let+ (_ : (unit, unit) result) =
            Fiber.map_reduce_errors
              (module Monoid.Unit)
              ~on_error:(fun _ ->
                Lazy.force cleanup;
                Fiber.return ())
              (fun () -> Instance.run instance)
          in
          Lazy.force cleanup)
    in
    let send f x =
      if x = [] then
        Fiber.return ()
      else
        let* running = Fiber.Pool.running active.pool in
        match running with
        | false -> Fiber.return ()
        | true -> Fiber.Pool.task active.pool ~f:(fun () -> f x)
    in
    let* () = send create to_create in
    send kill to_kill

type state =
  | Closed
  | Active of active

type t = state ref

let stop (t : t) =
  Fiber.of_thunk (fun () ->
      match !t with
      | Closed -> Fiber.return ()
      | Active active ->
        t := Closed;
        Fiber.fork_and_join_unit
          (fun () -> Fiber.Pool.stop active.pool)
          (fun () ->
            Dune_map.values active.instances
            |> Fiber.parallel_iter ~f:Instance.stop))

let create workspaces (client_capabilities : ClientCapabilities.t) diagnostics
    progress ~log =
  let+ poll_thread = Scheduler.create_thread () in
  let config =
    let include_promotions =
      match client_capabilities.experimental with
      | Some (`Assoc xs) -> (
        match List.assoc xs (fst view_promotion_capability) with
        | Some (`Bool b) -> b
        | _ -> false)
      | _ -> false
    in
    { diagnostics; progress; include_promotions; log }
  in
  let finish = Fiber.Ivar.create () in
  let registry =
    Registry.create (Registry.Config.create (Xdg.create ~env:Sys.getenv_opt ()))
  in
  ref
    (Active
       { pool = Fiber.Pool.create ()
       ; instances = Dune_map.empty
       ; config
       ; poll_thread
       ; registry
       ; finish
       ; workspaces
       })

let run_loop t =
  Fiber.repeat_while ~init:() ~f:(fun () ->
      match !t with
      | Closed -> Fiber.return None
      | Active active ->
        let* () = poll active in
        (* TODO make this a bit more dynamic. if poll completes fast, wait more,
           if it's slow, then wait less *)
        let+ () = Scheduler.sleep 0.25 in
        Some ())

let run t : unit Fiber.t =
  Fiber.of_thunk (fun () ->
      match !t with
      | Closed -> Code_error.raise "dune already closed" []
      | Active active ->
        let+ () =
          Fiber.fork_and_join_unit
            (fun () -> run_loop t)
            (fun () -> Fiber.Pool.run active.pool)
        in
        Format.eprintf "finished dune loop@.%!")

let update_workspaces t workspaces =
  match !t with
  | Closed -> Code_error.raise "dune is already closed" []
  | Active active -> active.workspaces <- workspaces

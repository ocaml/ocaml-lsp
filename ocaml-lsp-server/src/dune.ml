open! Import
open Fiber.O
module Registry = Drpc.Registry

let view_promotion_capability = ("diagnostic_promotions", `Bool true)

module For_diff = struct
  module Diff = struct
    type t =
      { in_source : string
      ; in_build : string
      }

    let of_promotion p =
      let module D = Drpc.Diagnostic in
      { in_build = D.Promotion.in_build p; in_source = D.Promotion.in_source p }

    let yojson_of_t { in_source; in_build } =
      `Assoc
        [ ("in_build", `String in_build); ("in_source", `String in_source) ]
  end

  type t = Diff.t list

  let yojson_of_t : t -> Json.t = Json.yojson_of_list Diff.yojson_of_t

  let diagnostic_data t = (fst view_promotion_capability, yojson_of_t t)
end

module Csexp_rpc = Csexp_rpc.Make (struct
  type t = Lev_fiber.Thread.t

  let stop t = Lev_fiber.Thread.close t

  let create () = Lev_fiber.Thread.create ()

  let task t ~f =
    let* task = Lev_fiber.Thread.task t ~f in
    let+ res = Lev_fiber.Thread.await task in
    match res with
    | Ok s -> Ok s
    | Error `Cancelled -> assert false
    | Error (`Exn e) -> Error (`Exn e)
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

  val format_dune_file : t -> Document.t -> string Fiber.t

  val stop : t -> unit Fiber.t

  val run : t -> unit Fiber.t

  val source : t -> Registry.Dune.t

  val create : Registry.Dune.t -> config -> t

  val promotions : t -> Drpc.Diagnostic.Promotion.t String.Map.t

  val client : t -> Client.t option
end = struct
  type running =
    { chan : Chan.t
    ; finish : unit Fiber.Ivar.t
    ; diagnostics_id : Diagnostics.Dune.t
    ; mutable client : Client.t option
    ; mutable promotions : Drpc.Diagnostic.Promotion.t String.Map.t
    }

  type state =
    | Idle
    | Running of running
    | Finished

  type t =
    { config : config
    ; source : Drpc.Registry.Dune.t
    ; mutable state : state
    }

  let client t =
    match t.state with
    | Idle
    | Finished ->
      None
    | Running r -> r.client

  let promotions t =
    match t.state with
    | Idle
    | Finished ->
      String.Map.empty
    | Running r -> r.promotions

  let source t = t.source

  let lsp_of_dune ~include_promotions diagnostic =
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
      match D.loc diagnostic with
      | None -> Range.first_line
      | Some loc -> range_of_loc loc
    in
    let severity =
      D.severity diagnostic
      |> Option.map ~f:(function
           | D.Error -> DiagnosticSeverity.Error
           | Warning -> DiagnosticSeverity.Warning)
    in
    let make_message message =
      String.trim (Format.asprintf "%a@." Pp.to_fmt message)
    in
    let relatedInformation =
      match D.related diagnostic with
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
    let message = make_message (D.message diagnostic) in
    let data =
      match include_promotions with
      | false -> None
      | true -> (
        match D.promotion diagnostic with
        | [] -> None
        | promotions ->
          let promotions = List.map promotions ~f:For_diff.Diff.of_promotion in
          Some (`Assoc [ For_diff.diagnostic_data promotions ]))
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

  let diagnostic_loop client running dune_diagnostic_id diagnostics
      ~include_promotions =
    let* res = Client.poll client Drpc.Sub.diagnostic in
    let send_diagnostics evs =
      List.iter evs ~f:(fun (ev : Drpc.Diagnostic.Event.t) ->
          let id =
            Drpc.Diagnostic.id
              (match ev with
              | Add x -> x
              | Remove x -> x)
          in
          let fold_promotions d ~f =
            let promotions = Drpc.Diagnostic.promotion d in
            List.fold_left promotions ~init:running.promotions
              ~f:(fun acc promotion ->
                let in_source = Drpc.Diagnostic.Promotion.in_source promotion in
                f acc in_source promotion)
          in
          match ev with
          | Remove d ->
            running.promotions <-
              fold_promotions d ~f:(fun acc path _promotion ->
                  String.Map.remove acc path);
            Diagnostics.remove diagnostics (`Dune (dune_diagnostic_id, id))
          | Add d ->
            running.promotions <- fold_promotions d ~f:String.Map.set;
            let uri : Uri.t =
              match Drpc.Diagnostic.loc d with
              | None -> Diagnostics.workspace_root diagnostics
              | Some loc ->
                let { Lexing.pos_fname; _ } = Drpc.Loc.start loc in
                Uri.of_path pos_fname
            in
            Diagnostics.set diagnostics
              (`Dune
                (dune_diagnostic_id, id, uri, lsp_of_dune ~include_promotions d)));
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
            let+ () = send_diagnostics p `All in
            Some ())

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
      let running =
        { chan
        ; finish
        ; promotions = String.Map.empty
        ; client = None
        ; diagnostics_id = Diagnostics.Dune.gen ()
        }
      in
      t.state <- Running running;
      let { progress; diagnostics; include_promotions; log = _ } = config in
      let* () =
        Fiber.all_concurrently_unit
          [ (let* () = Chan.run chan in
             t.state <- Finished;
             Diagnostics.disconnect diagnostics running.diagnostics_id;
             let* () = Diagnostics.send diagnostics `All in
             Fiber.Ivar.fill finish ())
          ; (let init =
               Drpc.Initialize.create ~id:(Drpc.Id.make (Atom "ocamllsp"))
             in
             Client.connect chan init ~f:(fun client ->
                 t.state <- Running { running with client = Some client };
                 let progress = progress_loop client progress in
                 let diagnostics =
                   diagnostic_loop client running running.diagnostics_id
                     diagnostics ~include_promotions
                 in
                 Fiber.all_concurrently_unit
                   [ progress; diagnostics; Fiber.Ivar.read finish ]))
          ]
      in
      Progress.end_build_if_running progress

  let format_dune_file t doc =
    match t.state with
    | Running { client = Some client; _ } -> (
      let* req =
        Client.Versioned.prepare_request client Drpc.Request.format_dune_file
      in
      let req =
        match req with
        | Error _ -> assert false
        | Ok req -> req
      in
      let+ res =
        let path = Document.uri doc |> Uri.to_path |> Drpc.Path.absolute in
        Client.request client req (path, `Contents (Document.text doc))
      in
      match res with
      | Ok res -> res
      | Error _ ->
        Jsonrpc.Response.Error.(
          raise (make ~message:"dune failed to format" ~code:InternalError ())))
    | Idle
    | Finished
    | Running _ ->
      assert false
  (* TODO wait for initialization *)
end

module Dune_map = Map.Make (Registry.Dune)

type active =
  { mutable instances : Instance.t String.Map.t (* keyed by root *)
  ; mutable workspaces : Workspaces.t
  ; registry : Registry.t
  ; config : config
  ; pool : Fiber.Pool.t
  }

let cwd = lazy (Sys.getcwd ())

let uri_dune_overlap =
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
  fun (uri : Uri.t) (dune : Registry.Dune.t) ->
    let dune_root = Registry.Dune.root dune in
    let path =
      let path = Uri.to_path uri in
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
  | Error _ -> (* TODO warn *) Fiber.return ()
  | Ok _refresh ->
    let remaining, to_kill =
      String.Map.partition active.instances ~f:(fun (running : Instance.t) ->
          let source = Instance.source running in
          List.exists workspace_folders ~f:(fun (wsf : WorkspaceFolder.t) ->
              uri_dune_overlap wsf.uri source))
    in
    let to_kill = String.Map.values to_kill in
    active.instances <- remaining;
    let kill to_kill = Fiber.parallel_iter to_kill ~f:Instance.stop in
    let to_create =
      (* won't work very well with large workspaces and many instances of
         dune *)
      let is_running dune =
        String.Map.mem active.instances (Registry.Dune.root dune)
      in
      Registry.current active.registry
      |> List.fold_left ~init:[] ~f:(fun acc dune ->
             if
               (not (is_running dune))
               && List.exists workspace_folders
                    ~f:(fun (wsf : WorkspaceFolder.t) ->
                      uri_dune_overlap wsf.uri dune)
             then
               Instance.create dune active.config :: acc
             else
               acc)
    in
    active.instances <-
      List.fold_left to_create ~init:active.instances
        ~f:(fun acc (instance : Instance.t) ->
          let source = Instance.source instance in
          (* XXX fragile. what if a server was killed and didn't cleanup *)
          String.Map.add_exn acc (Registry.Dune.root source) instance);
    let create to_create =
      Fiber.parallel_iter to_create ~f:(fun instance ->
          let cleanup =
            lazy
              (active.instances <-
                String.Map.remove active.instances
                  (Registry.Dune.root (Instance.source instance)))
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
            String.Map.values active.instances
            |> Fiber.parallel_iter ~f:Instance.stop))

let create workspaces (client_capabilities : ClientCapabilities.t) diagnostics
    progress ~log =
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
  let registry =
    Registry.create (Registry.Config.create (Xdg.create ~env:Sys.getenv_opt ()))
  in
  ref
    (Active
       { pool = Fiber.Pool.create ()
       ; instances = String.Map.empty
       ; config
       ; registry
       ; workspaces
       })

let enabled = false

let create_disabled () = ref Closed

let create workspaces (client_capabilities : ClientCapabilities.t) diagnostics
    progress ~log =
  if enabled then
    create workspaces client_capabilities diagnostics progress ~log
  else
    create_disabled ()

let run_loop t =
  Fiber.repeat_while ~init:() ~f:(fun () ->
      match !t with
      | Closed -> Fiber.return None
      | Active active ->
        let* () = poll active in
        (* TODO make this a bit more dynamic. if poll completes fast, wait more,
           if it's slow, then wait less *)
        let+ () = Lev_fiber.Timer.sleepf 0.25 in
        Some ())

let run t : unit Fiber.t =
  Fiber.of_thunk (fun () ->
      match !t with
      | Closed -> Fiber.return ()
      | Active active ->
        Fiber.fork_and_join_unit
          (fun () -> run_loop t)
          (fun () -> Fiber.Pool.run active.pool))

let update_workspaces t workspaces =
  match !t with
  | Closed -> Code_error.raise "dune is already closed" []
  | Active active -> active.workspaces <- workspaces

module Promote = struct
  module Input = struct
    type t =
      { dune : string
      ; in_source : string
      }

    let yojson_of_t { dune; in_source } : Json.t =
      `Assoc
        [ ("dune", `String dune)
        ; ("in_source", Json.Conv.yojson_of_string in_source)
        ]

    let t_of_yojson = function
      | `Assoc fields ->
        let dune = Json.field_exn fields "dune" Json.Conv.string_of_yojson in
        let in_source =
          Json.field_exn fields "in_source" Json.Conv.string_of_yojson
        in
        { dune; in_source }
      | json -> Json.error "invalid promote data" json

    let create (dune : Registry.Dune.t)
        (promotion : Drpc.Diagnostic.Promotion.t) =
      { dune = Registry.Dune.root dune
      ; in_source = Drpc.Diagnostic.Promotion.in_source promotion
      }
  end

  let name = "dune/promote"

  let run t (command : ExecuteCommandParams.t) =
    match !t with
    | Closed -> Fiber.return ()
    | Active active -> (
      let promote =
        match command.arguments with
        | Some [ arg ] -> Input.t_of_yojson arg
        | _ -> assert false
      in
      match String.Map.find active.instances promote.dune with
      | None -> (* TODO error *) Fiber.return ()
      | Some instance -> (
        match Instance.client instance with
        | None -> (* TODO error *) Fiber.return ()
        | Some client -> (
          let* req =
            Client.Versioned.prepare_request client Drpc.Request.promote
          in
          let req =
            match req with
            | Error _ -> assert false
            | Ok req -> req
          in
          let* res =
            Client.request client req (Drpc.Path.absolute promote.in_source)
          in
          match res with
          | Ok () -> Fiber.return ()
          | Error _ -> Fiber.return ())))
end

let commands = [ Promote.name ]

let code_actions (t : t) (doc : Document.t) =
  match !t with
  | Closed -> []
  | Active active ->
    let path = Document.uri doc |> Uri.to_path in
    String.Map.fold active.instances ~init:[] ~f:(fun dune acc ->
        let promotions = Instance.promotions dune in
        match String.Map.find promotions path with
        | None -> acc
        | Some promotion ->
          let command =
            let promote =
              Promote.Input.create (Instance.source dune) promotion
            in
            Command.create ~title:"Promote" ~command:Promote.name
              ~arguments:[ Promote.Input.yojson_of_t promote ]
              ()
          in
          let action =
            CodeAction.create ~title:"Promote" ~kind:CodeActionKind.QuickFix
              ~command ()
          in
          action :: acc)

let on_command t (cmd : ExecuteCommandParams.t) =
  if cmd.command <> Promote.name then
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make ~code:InvalidRequest
         ~message:"invalid command" ());
  let* () = Promote.run t cmd in
  Fiber.return `Null

let for_doc t doc =
  match !t with
  | Closed -> []
  | Active t ->
    let uri = Document.uri doc in
    String.Map.fold ~init:[] t.instances ~f:(fun instance acc ->
        if uri_dune_overlap uri (Instance.source instance) then
          instance :: acc
        else
          acc)

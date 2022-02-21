open! Import
open Fiber.O
module Registry = Drpc.Registry
module Csexp_rpc = Lev_fiber_csexp

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
        Fiber.of_thunk (fun () ->
            Fiber.return
              (match Sys.readdir s with
              | s -> Ok (Array.to_list s)
              | exception Sys_error _ -> Ok []
              | exception exn -> Error exn))

      let stat s =
        Fiber.of_thunk (fun () ->
            Fiber.return
              (match Unix.stat s with
              | exception exn -> Error exn
              | s -> Ok (`Mtime s.st_mtime)))

      let read_file s =
        Fiber.of_thunk (fun () ->
            Fiber.return
              (Result.try_with (fun () -> Io.String_path.read_file s)))
    end)

type server = Server : _ Server.t -> server

type config =
  { diagnostics : Diagnostics.t
  ; include_promotions : bool
  ; progress : Progress.t
  ; log : type_:MessageType.t -> message:string -> unit Fiber.t
  ; server : server
  }

module Promotion_action = struct
  let method_ = "textDocument/codeAction"

  let id d =
    let in_source = Drpc.Diagnostic.Promotion.in_source d in
    "ocamllsp-promote-action/" ^ in_source

  let unregistration d =
    let unregisterations =
      let id = id d in
      [ Unregistration.create ~id ~method_ ]
    in
    UnregistrationParams.create ~unregisterations

  let registration (d : Drpc.Diagnostic.Promotion.t) =
    let registrations =
      let in_source = Drpc.Diagnostic.Promotion.in_source d in
      let code_action =
        let id = id d in
        let registerOptions =
          let documentSelector =
            [ DocumentFilter.create ~pattern:in_source () ]
          in
          CodeActionRegistrationOptions.create ~documentSelector
            ~codeActionKinds:[ CodeActionKind.Other "Promote" ]
            ()
          |> CodeActionRegistrationOptions.yojson_of_t
        in
        Registration.create ~id ~method_ ~registerOptions ()
      in
      [ code_action ]
    in
    RegistrationParams.create ~registrations
end

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
  module Id = Stdune.Id.Make ()

  type running =
    { chan : Chan.t
    ; finish : unit Fiber.Ivar.t
    ; diagnostics_id : Diagnostics.Dune.t
    ; id : Id.t
    ; mutable client : Client.t option
    ; mutable promotions :
        (* TODO we need to clean these up in the finalizer *)
        Drpc.Diagnostic.Promotion.t String.Map.t
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

  let fold_promotions promotions diagnostic ~f =
    let promotions, requests =
      Drpc.Diagnostic.promotion diagnostic
      |> List.fold_left ~init:(promotions, []) ~f:(fun (ps, acc) promotion ->
             let src = Drpc.Diagnostic.Promotion.in_source promotion in
             match f promotion (String.Map.find promotions src) with
             | None -> (ps, acc)
             | Some (update, requests) ->
               (* XXX not very efficient *)
               (String.Map.update ps src ~f:(fun _ -> update), requests :: acc))
    in
    (promotions, List.flatten requests)

  let add_promotions promotions diagnostic =
    fold_promotions promotions diagnostic ~f:(fun promotion status ->
        match status with
        | None ->
          Some (Some promotion, [ Promotion_action.registration promotion ])
        | Some _ ->
          (* TODO: ideally, we should update with the new promotion, but then we
             need to store the actual registration ID *)
          None)

  let remove_promotions running promotions =
    fold_promotions running promotions ~f:(fun promotion status ->
        match status with
        | None ->
          Log.log ~section:"warning" (fun () ->
              Log.msg "removing non existant promotion"
                [ ( "promotion"
                  , `String (Drpc.Diagnostic.Promotion.in_source promotion) )
                ]);
          None
        | Some _ -> Some (None, [ Promotion_action.unregistration promotion ]))

  let diagnostic_loop client config (running : running) diagnostics =
    let* res = Client.poll client Drpc.Sub.diagnostic in
    let send_diagnostics evs =
      let promotions, requests =
        List.fold_left evs ~init:(running.promotions, [])
          ~f:(fun (promotions, acc) (ev : Drpc.Diagnostic.Event.t) ->
            let id =
              Drpc.Diagnostic.id
                (match ev with
                | Add x -> x
                | Remove x -> x)
            in
            match ev with
            | Remove d ->
              let promotions, requests = remove_promotions promotions d in
              Diagnostics.remove diagnostics
                (`Dune (running.diagnostics_id, id));
              (promotions, List.map requests ~f:(fun r -> `Remove r) :: acc)
            | Add d ->
              let promotions, requests = add_promotions promotions d in
              let uri : Uri.t =
                match Drpc.Diagnostic.loc d with
                | None -> Diagnostics.workspace_root diagnostics
                | Some loc ->
                  let { Lexing.pos_fname; _ } = Drpc.Loc.start loc in
                  Uri.of_path pos_fname
              in
              Diagnostics.set diagnostics
                (`Dune
                  ( running.diagnostics_id
                  , id
                  , uri
                  , lsp_of_dune ~include_promotions:config.include_promotions d
                  ));
              (promotions, List.map requests ~f:(fun r -> `Add r) :: acc))
      in
      (promotions, List.flatten requests)
    in
    match res with
    | Error v -> raise (Drpc.Version_error.E v)
    | Ok poll ->
      Fiber.repeat_while ~init:() ~f:(fun () ->
          let* res = Client.Stream.next poll in
          match res with
          | None -> Fiber.return None
          | Some p ->
            let promotions, requests = send_diagnostics p in
            running.promotions <- promotions;
            let* () = Diagnostics.send diagnostics `All in
            let (Server server) = config.server in
            let batch = Server.Batch.create server in
            let requests =
              List.map requests ~f:(fun req ->
                  let req =
                    match req with
                    | `Add r -> Server_request.ClientRegisterCapability r
                    | `Remove r -> Server_request.ClientUnregisterCapability r
                  in
                  Server.Batch.request batch req)
            in
            let+ () =
              Fiber.fork_and_join_unit
                (fun () -> Server.Batch.submit batch)
                (* I suppose we can ignore requests altogether? *)
                  (fun () -> Fiber.parallel_iter requests ~f:Server.Batch.await)
            in
            Some ())

  let stop t =
    match t.state with
    | Running { chan; _ } -> Chan.stop chan
    | _ -> Fiber.return ()

  let create source config = { config; source; state = Idle }

  let run ({ config; source; _ } as t) =
    let* () =
      Fiber.of_thunk (fun () ->
          assert (
            match t.state with
            | Idle -> true
            | _ -> false);
          Fiber.return ())
    in
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
    let sockaddr =
      match where with
      | `Unix s -> Unix.ADDR_UNIX s
      | `Ip (`Host h, `Port p) -> Unix.ADDR_INET (Unix.inet_addr_of_string h, p)
    in
    let* session =
      let sock =
        let domain = Unix.domain_of_sockaddr sockaddr in
        let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
        Lev_fiber.Fd.create socket (`Non_blocking false)
      in
      Fiber.map_reduce_errors
        (module Monoid.List (Exn_with_backtrace))
        ~on_error:(fun exn -> Fiber.return [ exn ])
        (fun () -> Csexp_rpc.connect sock sockaddr)
    in
    match session with
    | Error exns ->
      let exn = List.hd exns in
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
        ; id = Id.gen ()
        }
      in
      t.state <- Running running;
      let { progress; diagnostics; include_promotions = _; log = _; server = _ }
          =
        config
      in
      let* () =
        Fiber.all_concurrently_unit
          [ (let* () = Chan.run chan in
             t.state <- Finished;
             Diagnostics.disconnect diagnostics running.diagnostics_id;
             let* () = Diagnostics.send diagnostics `All in
             Fiber.Ivar.fill finish ())
          ; (let init =
               let id =
                 Drpc.Id.make
                   (List
                      [ Atom "ocamllsp"
                      ; Atom (Int.to_string (Id.to_int running.id))
                      ])
               in
               Drpc.Initialize.create ~id
             in
             let where =
               match where with
               | `Unix s -> sprintf "unix://%s" s
               | `Ip (`Host h, `Port p) -> sprintf "%s:%d" h p
             in
             let* () =
               let message =
                 sprintf "client %d: connecting..." (Id.to_int running.id)
               in
               config.log ~type_:Info ~message
             in
             Client.connect chan init ~f:(fun client ->
                 running.client <- Some client;
                 t.state <- Running running;
                 let* () =
                   let message =
                     sprintf "client %d: connected to dune at %s"
                       (Id.to_int running.id) where
                   in
                   config.log ~type_:Info ~message
                 in
                 let progress = progress_loop client progress in
                 let diagnostics =
                   diagnostic_loop client config running diagnostics
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

let make_finalizer active (instance : Instance.t) =
  Lazy_fiber.create (fun () ->
      active.instances <-
        String.Map.remove active.instances
          (Registry.Dune.root (Instance.source instance));
      let (Server server) = active.config.server in
      let batch = Server.Batch.create server in
      let to_unregister =
        Instance.promotions instance
        |> String.Map.to_list_map ~f:(fun _ promotion ->
               let req =
                 Server_request.ClientUnregisterCapability
                   (Promotion_action.unregistration promotion)
               in
               Server.Batch.request batch req)
      in
      Fiber.fork_and_join_unit
        (fun () -> Server.Batch.submit batch)
        (* I suppose we can ignore requests altogether? *)
          (fun () -> Fiber.parallel_iter to_unregister ~f:Server.Batch.await))

let poll active =
  (* a single workspaces value for one iteration of the loop *)
  let workspaces = active.workspaces in
  let workspace_folders = Workspaces.workspace_folders workspaces in
  let* res = Poll.poll active.registry in
  match res with
  | Error exn ->
    let message =
      sprintf "failed to poll dune registry. %s" (Printexc.to_string exn)
    in
    active.config.log ~type_:MessageType.Warning ~message
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
      Fiber.parallel_iter to_create ~f:(fun (instance : Instance.t) ->
          let cleanup = make_finalizer active instance in
          let* (_ : (unit, unit) result) =
            Fiber.map_reduce_errors
              (module Monoid.Unit)
              ~on_error:(fun exn ->
                let message =
                  Format.asprintf "disconnected %s:@.%a"
                    (Registry.Dune.root (Instance.source instance))
                    Exn_with_backtrace.pp_uncaught exn
                in
                let* () = active.config.log ~type_:Error ~message in
                Lazy_fiber.force cleanup)
              (fun () -> Instance.run instance)
          in
          Lazy_fiber.force cleanup)
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
    progress server ~log =
  let config =
    let include_promotions =
      match client_capabilities.experimental with
      | Some (`Assoc xs) -> (
        match List.assoc xs (fst view_promotion_capability) with
        | Some (`Bool b) -> b
        | _ -> false)
      | _ -> false
    in
    { diagnostics; progress; include_promotions; log; server = Server server }
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

let enabled = true

let create_disabled () = ref Closed

let create workspaces (client_capabilities : ClientCapabilities.t) diagnostics
    progress server ~log =
  if enabled then
    create workspaces client_capabilities diagnostics progress server ~log
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
    Fiber.of_thunk (fun () ->
        match !t with
        | Closed -> Fiber.return ()
        | Active active -> (
          let promote =
            match command.arguments with
            | Some [ arg ] -> Input.t_of_yojson arg
            | _ -> assert false
          in
          match String.Map.find active.instances promote.dune with
          | None ->
            let message = sprintf "dune %S already disconected" promote.dune in
            Jsonrpc.Response.Error.raise
              (Jsonrpc.Response.Error.make ~code:InternalError ~message ())
          | Some instance -> (
            match Instance.client instance with
            | None ->
              (* we can also just wait for initialization? *)
              let message = sprintf "dune %S is not initialized" promote.dune in
              Jsonrpc.Response.Error.raise
                (Jsonrpc.Response.Error.make ~code:InternalError ~message ())
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
              | Error _ -> Fiber.return ()))))
end

let commands = [ Promote.name ]

let code_actions (t : t) (uri : Uri.t) =
  match !t with
  | Closed -> []
  | Active active ->
    let path = Uri.to_path uri in
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
  Fiber.of_thunk (fun () ->
      if cmd.command <> Promote.name then
        Jsonrpc.Response.Error.raise
          (Jsonrpc.Response.Error.make ~code:InvalidRequest
             ~message:"invalid command" ());
      let* () = Promote.run t cmd in
      Fiber.return `Null)

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

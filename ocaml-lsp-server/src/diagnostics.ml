open Import

module Uri = struct
  include Uri

  let compare x y = Ordering.of_int (Uri.compare x y)
end

module Uri_c = Comparable.Make (Uri)
module Uri_set = Uri_c.Set

type dune_status =
  | Inactive
  | Connected
  | Disconnected

type t =
  { dune : (Drpc.Diagnostic.Id.t, Uri.t * Diagnostic.t) Table.t
  ; mutable dune_status : dune_status
  ; workspace_root : Uri.t Lazy.t
  ; merlin : (Uri.t, Diagnostic.t list) Table.t
  ; send : PublishDiagnosticsParams.t list -> unit Fiber.t
  ; mutable dirty_uris : Uri_set.t
  }

let workspace_root t = Lazy.force t.workspace_root

module Id = struct
  include Drpc.Diagnostic.Id

  let equal x y = compare x y = 0

  let to_dyn = Dyn.Encoder.opaque
end

let create send ~workspace_root =
  { dune = Table.create (module Id) 32
  ; merlin = Table.create (module Uri) 32
  ; dirty_uris = Uri_set.empty
  ; dune_status = Inactive
  ; send
  ; workspace_root
  }

let update_dune_status t status =
  Table.filteri_inplace t.dune ~f:(fun ~key:_ ~data:(uri, _) ->
      t.dirty_uris <- Uri_set.add t.dirty_uris uri;
      false);
  t.dirty_uris <- Uri_set.add t.dirty_uris (Lazy.force t.workspace_root);
  t.dune_status <- status

let dune_status_diagnostic t =
  match t.dune_status with
  | Disconnected ->
    Some
      (Diagnostic.create ~severity:Information ~source:"dune"
         ~range:Range.first_line
         ~message:
           "Dune diagnostic status may be stale. Please run dune in watch mode\n\
            to see up to date diagnostics"
         ())
  | _ -> None

let send t =
  let pending = Table.create (module Uri) 32 in
  Uri_set.iter t.dirty_uris ~f:(fun uri ->
      let diagnostics = Table.Multi.find t.merlin uri in
      Table.set pending uri diagnostics);
  dune_status_diagnostic t
  |> Option.iter ~f:(fun d ->
         Table.Multi.cons pending (Lazy.force t.workspace_root) d);
  Table.iter t.dune ~f:(fun (uri, diagnostic) ->
      if Uri_set.mem t.dirty_uris uri then
        Table.Multi.cons pending uri diagnostic);
  t.dirty_uris <- Uri_set.empty;
  Table.foldi pending ~init:[] ~f:(fun uri diagnostics acc ->
      PublishDiagnosticsParams.create ~uri ~diagnostics () :: acc)
  |> t.send

let set t what =
  let uri =
    match what with
    | `Dune (_, uri, _) -> uri
    | `Merlin (uri, _) -> uri
  in
  t.dirty_uris <- Uri_set.add t.dirty_uris uri;
  match what with
  | `Dune (id, uri, diagnostics) -> Table.set t.dune id (uri, diagnostics)
  | `Merlin (uri, diagnostics) -> Table.set t.merlin uri diagnostics

let remove t = function
  | `Dune id ->
    Table.find t.dune id
    |> Option.iter ~f:(fun (uri, _) ->
           Table.remove t.dune id;
           t.dirty_uris <- Uri_set.add t.dirty_uris uri)
  | `Merlin uri ->
    t.dirty_uris <- Uri_set.add t.dirty_uris uri;
    Table.remove t.merlin uri

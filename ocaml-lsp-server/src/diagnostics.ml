open Import

module Uri = struct
  include Uri

  let compare x y = Ordering.of_int (Uri.compare x y)
end

module Uri_c = Comparable.Make (Uri)
module Uri_set = Uri_c.Set

module Id = struct
  include Drpc.Diagnostic.Id

  let equal x y = compare x y = Eq

  let to_dyn = Dyn.opaque
end

module Dune = Stdune.Id.Make ()

type t =
  { dune :
      (Dune.t, (Drpc.Diagnostic.Id.t, Uri.t * Diagnostic.t) Table.t) Table.t
  ; workspace_root : Uri.t Lazy.t
  ; merlin : (Uri.t, Diagnostic.t list) Table.t
  ; send : PublishDiagnosticsParams.t list -> unit Fiber.t
  ; mutable dirty_uris : Uri_set.t
  }

let workspace_root t = Lazy.force t.workspace_root

let create send ~workspace_root =
  { dune = Table.create (module Dune) 32
  ; merlin = Table.create (module Uri) 32
  ; dirty_uris = Uri_set.empty
  ; send
  ; workspace_root
  }

let send t which =
  Fiber.of_thunk (fun () ->
      let dirty_uris =
        match which with
        | `All -> t.dirty_uris
        | `One uri -> Uri_set.singleton uri
      in
      let pending = Table.create (module Uri) 4 in
      Uri_set.iter dirty_uris ~f:(fun uri ->
          let diagnostics = Table.Multi.find t.merlin uri in
          Table.set pending uri diagnostics);
      Table.iter t.dune ~f:(fun per_dune ->
          Table.iter per_dune ~f:(fun (uri, diagnostic) ->
              if Uri_set.mem dirty_uris uri then
                Table.Multi.cons pending uri diagnostic));
      t.dirty_uris <-
        (match which with
        | `All -> Uri_set.empty
        | `One uri -> Uri_set.remove t.dirty_uris uri);
      Table.foldi pending ~init:[] ~f:(fun uri diagnostics acc ->
          (* we don't include a version because some of the diagnostics might
             come from dune which reads from the file system and not from the
             editor's view*)
          PublishDiagnosticsParams.create ~uri ~diagnostics () :: acc)
      |> t.send)

let set t what =
  let uri =
    match what with
    | `Dune (_, _, uri, _) -> uri
    | `Merlin (uri, _) -> uri
  in
  t.dirty_uris <- Uri_set.add t.dirty_uris uri;
  match what with
  | `Merlin (uri, diagnostics) -> Table.set t.merlin uri diagnostics
  | `Dune (dune, id, uri, diagnostics) ->
    let dune_table =
      Table.find_or_add t.dune dune ~f:(fun _ -> Table.create (module Id) 16)
    in
    Table.set dune_table id (uri, diagnostics)

let remove t = function
  | `Dune (dune, diagnostic) ->
    Table.find t.dune dune
    |> Option.iter ~f:(fun dune ->
           Table.find dune diagnostic
           |> Option.iter ~f:(fun (uri, _) ->
                  Table.remove dune diagnostic;
                  t.dirty_uris <- Uri_set.add t.dirty_uris uri))
  | `Merlin uri ->
    t.dirty_uris <- Uri_set.add t.dirty_uris uri;
    Table.remove t.merlin uri

let disconnect t dune =
  Table.find t.dune dune
  |> Option.iter ~f:(fun dune_diagnostics ->
         Table.iter dune_diagnostics ~f:(fun (uri, _) ->
             t.dirty_uris <- Uri_set.add t.dirty_uris uri);
         Table.remove t.dune dune)

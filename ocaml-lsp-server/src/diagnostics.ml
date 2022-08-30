open Import

let ocamllsp_source = "ocamllsp"

let dune_source = "dune"

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

module Dune = struct
  module Id = Stdune.Id.Make ()

  module T = struct
    type t =
      { pid : Pid.t
      ; id : Id.t
      }

    let compare = Poly.compare

    let equal x y = Ordering.is_eq (compare x y)

    let hash = Poly.hash

    let to_dyn = Dyn.opaque
  end

  include T
  module C = Comparable.Make (T)

  let gen pid = { pid; id = Id.gen () }
end

let equal_message =
  (* because the compiler and merlin wrap messages differently *)
  let is_space = function
    | ' ' | '\r' | '\n' | '\t' -> true
    | _ -> false
  in
  let eat_space s i =
    while !i < String.length s && is_space s.[!i] do
      incr i
    done
  in
  fun m1 m2 ->
    let i = ref 0 in
    let j = ref 0 in
    try
      eat_space m1 i;
      eat_space m2 j;
      while !i < String.length m1 && !j < String.length m2 do
        let ci = m1.[!i] in
        let cj = m2.[!j] in
        if is_space ci && is_space cj then (
          eat_space m1 i;
          eat_space m2 j)
        else if Char.equal ci cj then (
          incr i;
          incr j)
        else raise_notrace Exit
      done;
      eat_space m1 i;
      eat_space m2 j;
      (* we make sure that everything is consumed *)
      !i = String.length m1 && !j = String.length m2
    with Exit -> false

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

let send =
  let module Range_map = Map.Make (Range) in
  (* TODO deduplicate related errors as well *)
  let add_dune_diagnostic pending uri (diagnostic : Diagnostic.t) =
    let value =
      match Table.find pending uri with
      | None -> Range_map.singleton diagnostic.range [ diagnostic ]
      | Some map ->
        Range_map.update map diagnostic.range ~f:(fun diagnostics ->
            Some
              (match diagnostics with
              | None -> [ diagnostic ]
              | Some diagnostics ->
                if
                  List.exists diagnostics ~f:(fun (d : Diagnostic.t) ->
                      match d.source with
                      | None -> assert false
                      | Some source ->
                        String.equal ocamllsp_source source
                        && equal_message d.message diagnostic.message)
                then diagnostics
                else diagnostic :: diagnostics))
    in
    Table.set pending uri value
  in
  let range_map_of_unduplicated_diagnostics diagnostics =
    List.rev_map diagnostics ~f:(fun (d : Diagnostic.t) -> (d.range, d))
    |> Range_map.of_list_multi
  in
  fun t which ->
    Fiber.of_thunk (fun () ->
        let dirty_uris =
          match which with
          | `All -> t.dirty_uris
          | `One uri -> Uri_set.singleton uri
        in
        let pending = Table.create (module Uri) 4 in
        Uri_set.iter dirty_uris ~f:(fun uri ->
            let diagnostics = Table.Multi.find t.merlin uri in
            Table.set pending uri
              (range_map_of_unduplicated_diagnostics diagnostics));
        let set_dune_source =
          let annotate_dune_pid = Table.length t.dune > 1 in
          if annotate_dune_pid then fun pid (d : Diagnostic.t) ->
            let source = Some (sprintf "dune (pid=%d)" (Pid.to_int pid)) in
            { d with source }
          else fun _pid x -> x
        in
        Table.foldi ~init:() t.dune ~f:(fun dune per_dune () ->
            Table.iter per_dune ~f:(fun (uri, diagnostic) ->
                if Uri_set.mem dirty_uris uri then
                  let diagnostic = set_dune_source dune.pid diagnostic in
                  add_dune_diagnostic pending uri diagnostic));
        t.dirty_uris <-
          (match which with
          | `All -> Uri_set.empty
          | `One uri -> Uri_set.remove t.dirty_uris uri);
        Table.foldi pending ~init:[] ~f:(fun uri diagnostics acc ->
            let diagnostics = List.flatten (Range_map.values diagnostics) in
            (* we don't include a version because some of the diagnostics might
               come from dune which reads from the file system and not from the
               editor's view *)
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

(* this is not inlined in [tags_of_message] for reusability *)
let diagnostic_tags_unnecessary = Some [ DiagnosticTag.Unnecessary ]

let tags_of_message ~src message =
  match src with
  | `Dune when String.is_prefix message ~prefix:"unused" ->
    diagnostic_tags_unnecessary
  | `Merlin when Diagnostic_util.is_unused_var_warning message ->
    diagnostic_tags_unnecessary
  | `Merlin when Diagnostic_util.is_deprecated_warning message ->
    Some [ DiagnosticTag.Deprecated ]
  | `Dune | `Merlin -> None

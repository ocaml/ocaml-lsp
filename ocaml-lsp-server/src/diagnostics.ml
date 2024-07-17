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
        if is_space ci && is_space cj
        then (
          eat_space m1 i;
          eat_space m2 j)
        else if Char.equal ci cj
        then (
          incr i;
          incr j)
        else raise_notrace Exit
      done;
      eat_space m1 i;
      eat_space m2 j;
      (* we make sure that everything is consumed *)
      !i = String.length m1 && !j = String.length m2
    with
    | Exit -> false
;;

type t =
  { dune : (Dune.t, (Drpc.Diagnostic.Id.t, Uri.t * Diagnostic.t) Table.t) Table.t
  ; merlin : (Uri.t, Diagnostic.t list) Table.t
  ; send : PublishDiagnosticsParams.t list -> unit Fiber.t
  ; mutable dirty_uris : Uri_set.t
  ; related_information : bool
  ; tags : DiagnosticTag.t list
  ; mutable report_dune_diagnostics : bool
  }

let create
  (capabilities : PublishDiagnosticsClientCapabilities.t option)
  send
  ~report_dune_diagnostics
  =
  let related_information, tags =
    match capabilities with
    | None -> false, []
    | Some c ->
      ( Option.value ~default:false c.relatedInformation
      , (match c.tagSupport with
         | None -> []
         | Some { valueSet } -> valueSet) )
  in
  { dune = Table.create (module Dune) 32
  ; merlin = Table.create (module Uri) 32
  ; dirty_uris = Uri_set.empty
  ; send
  ; related_information
  ; tags
  ; report_dune_diagnostics
  }
;;

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
               if List.exists diagnostics ~f:(fun (d : Diagnostic.t) ->
                    match d.source with
                    | None -> assert false
                    | Some source ->
                      String.equal ocamllsp_source source
                      &&
                        (match d.message, diagnostic.message with
                        | `String m1, `String m2 -> equal_message m1 m2
                        | `MarkupContent { kind; value }, `MarkupContent mc ->
                          Poly.equal kind mc.kind && equal_message value mc.value
                        | _, _ -> false))
               then diagnostics
               else diagnostic :: diagnostics))
    in
    Table.set pending uri value
  in
  let range_map_of_unduplicated_diagnostics diagnostics =
    List.rev_map diagnostics ~f:(fun (d : Diagnostic.t) -> d.range, d)
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
        Table.set pending uri (range_map_of_unduplicated_diagnostics diagnostics));
      let set_dune_source =
        let annotate_dune_pid = Table.length t.dune > 1 in
        if annotate_dune_pid
        then
          fun pid (d : Diagnostic.t) ->
          let source = Some (sprintf "dune (pid=%d)" (Pid.to_int pid)) in
          { d with source }
        else fun _pid x -> x
      in
      if t.report_dune_diagnostics
      then
        Table.foldi ~init:() t.dune ~f:(fun dune per_dune () ->
          Table.iter per_dune ~f:(fun (uri, diagnostic) ->
            if Uri_set.mem dirty_uris uri
            then (
              let diagnostic = set_dune_source dune.pid diagnostic in
              add_dune_diagnostic pending uri diagnostic)));
      t.dirty_uris
      <- (match which with
          | `All -> Uri_set.empty
          | `One uri -> Uri_set.remove t.dirty_uris uri);
      Table.foldi pending ~init:[] ~f:(fun uri diagnostics acc ->
        let diagnostics = List.flatten (Range_map.values diagnostics) in
        (* we don't include a version because some of the diagnostics might
           come from dune which reads from the file system and not from the
           editor's view *)
        PublishDiagnosticsParams.create ~uri ~diagnostics () :: acc)
      |> t.send)
;;

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
;;

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
;;

let disconnect t dune =
  Table.find t.dune dune
  |> Option.iter ~f:(fun dune_diagnostics ->
    Table.iter dune_diagnostics ~f:(fun (uri, _) ->
      t.dirty_uris <- Uri_set.add t.dirty_uris uri);
    Table.remove t.dune dune)
;;

let tags_of_message =
  let tags_of_message ~src message : DiagnosticTag.t option =
    match src with
    | `Dune when String.is_prefix message ~prefix:"unused" -> Some Unnecessary
    | `Merlin when Diagnostic_util.is_unused_var_warning message -> Some Unnecessary
    | `Merlin when Diagnostic_util.is_deprecated_warning message -> Some Deprecated
    | `Dune | `Merlin -> None
  in
  fun t ~src message ->
    match tags_of_message ~src message with
    | None -> None
    | Some tag -> Option.some_if (List.mem t.tags tag ~equal:Poly.equal) [ tag ]
;;

let extract_related_errors uri raw_message =
  match Ocamlc_loc.parse_raw raw_message with
  | `Message message :: related ->
    let string_of_message message = String.trim message in
    let related =
      let rec loop acc = function
        | `Loc loc :: `Message m :: xs -> loop ((loc, m) :: acc) xs
        | [] -> List.rev acc
        | _ ->
          (* give up when we see something unexpected *)
          Log.log ~section:"debug" (fun () ->
            Log.msg "unable to parse error" [ "error", `String raw_message ]);
          []
      in
      loop [] related
    in
    let related =
      match related with
      | [] -> None
      | related ->
        let make_related ({ Ocamlc_loc.path = _; lines; chars }, message) =
          let location =
            let start, end_ =
              let line_start, line_end =
                match lines with
                | Single i -> i, i
                | Range (i, j) -> i, j
              in
              let char_start, char_end =
                match chars with
                | None -> 1, 1
                | Some (x, y) -> x, y
              in
              ( Position.create ~line:line_start ~character:char_start
              , Position.create ~line:line_end ~character:char_end )
            in
            let range = Range.create ~start ~end_ in
            Location.create ~range ~uri
          in
          let message = string_of_message message in
          DiagnosticRelatedInformation.create ~location ~message
        in
        Some (List.map related ~f:make_related)
    in
    string_of_message message, related
  | _ -> raw_message, None
;;

let merlin_diagnostics diagnostics merlin =
  let doc = Document.Merlin.to_doc merlin in
  let uri = Document.uri doc in
  let create_diagnostic = Diagnostic.create ~source:ocamllsp_source in
  let open Fiber.O in
  let+ all_diagnostics =
    let command =
      Query_protocol.Errors { lexing = true; parsing = true; typing = true }
    in
    Document.Merlin.with_pipeline_exn ~name:"diagnostics" merlin (fun pipeline ->
      match Query_commands.dispatch pipeline command with
      | exception Merlin_extend.Extend_main.Handshake.Error error ->
        let message =
          `String
            (sprintf
               "%s.\nHint: install the following packages: merlin-extend, reason"
               error)
        in
        [ create_diagnostic ~range:Range.first_line ~message () ]
      | errors ->
        let merlin_diagnostics =
          List.rev_map errors ~f:(fun (error : Loc.error) ->
            let loc = Loc.loc_of_report error in
            let range = Range.of_loc loc in
            let severity =
              match error.source with
              | Warning -> DiagnosticSeverity.Warning
              | _ -> DiagnosticSeverity.Error
            in
            let make_message ppf m = String.trim (Format.asprintf "%a@." ppf m) in
            let message = make_message Loc.print_main error in
            let message, relatedInformation =
              match diagnostics.related_information with
              | false -> message, None
              | true ->
                (match error.sub with
                 | [] -> extract_related_errors uri message
                 | _ :: _ ->
                   ( message
                   , Some
                       (List.map error.sub ~f:(fun (sub : Loc.msg) ->
                          let location =
                            let range = Range.of_loc sub.loc in
                            Location.create ~range ~uri
                          in
                          let message = make_message Loc.print_sub_msg sub in
                          DiagnosticRelatedInformation.create ~location ~message)) ))
            in
            let tags = tags_of_message diagnostics ~src:`Merlin message in
            create_diagnostic
              ?tags
              ?relatedInformation
              ~range
              ~message:(`String message)
              ~severity
              ())
        in
        let holes_as_err_diags =
          Query_commands.dispatch pipeline Holes
          |> List.rev_map ~f:(fun (loc, typ) ->
            let range = Range.of_loc loc in
            let severity = DiagnosticSeverity.Error in
            let message =
              "This typed hole should be replaced with an expression of type " ^ typ
            in
            (* we set specific diagnostic code = "hole" to be able to
               filter through diagnostics easily *)
            create_diagnostic
              ~code:(`String "hole")
              ~range
              ~message:(`String message)
              ~severity
              ())
        in
        (* Can we use [List.merge] instead? *)
        List.rev_append holes_as_err_diags merlin_diagnostics
        |> List.sort ~compare:(fun (d1 : Diagnostic.t) (d2 : Diagnostic.t) ->
          Range.compare d1.range d2.range))
  in
  set diagnostics (`Merlin (uri, all_diagnostics))
;;

let set_report_dune_diagnostics t ~report_dune_diagnostics =
  let open Fiber.O in
  let* () = Fiber.return () in
  if t.report_dune_diagnostics = report_dune_diagnostics
  then Fiber.return ()
  else (
    t.report_dune_diagnostics <- report_dune_diagnostics;
    Table.iter t.dune ~f:(fun per_dune ->
      Table.iter per_dune ~f:(fun (uri, _diagnostic) ->
        t.dirty_uris <- Uri_set.add t.dirty_uris uri));
    send t `All)
;;

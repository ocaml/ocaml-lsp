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
  include Drpc.V1.Diagnostic.Id

  let equal x y = compare x y = Eq
  let to_dyn = Dyn.opaque
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
  let remove_errno msg =
    (* Sometimes errors (in my testing, specifically from Merlin) are prefixed with
       "Error (warning ..): ..." while Dune errors are not. Perhaps it may be better
       to investigate why this is, but it doesn't hurt to just filter that out. *)
    match Re.Str.bounded_split (Re.Str.regexp "^Error (warning [0-9]+): ") msg 2 with
    | [ rest ] -> rest
    | _ -> msg
  in
  fun m1 m2 ->
    (* Merlin errors that are warnings start with a message of the form Error (warning ..)
       while Dune errors omit that part - filter that out for the messages *)
    let m1 = remove_errno m1 in
    let m2 = remove_errno m2 in
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
  { dune : (Id.t, Uri.t * Diagnostic.t) Table.t
  ; merlin : (Uri.t, Diagnostic.t list) Table.t
  ; send : PublishDiagnosticsParams.t list -> unit Fiber.t
  ; mutable dirty_uris : Uri_set.t
  ; related_information : bool
  ; tags : DiagnosticTag.t list
  ; mutable display_merlin_diagnostics : bool
  ; mutable shorten_merlin_diagnostics : bool
  ; client_name : string
  }

let create
  (capabilities : PublishDiagnosticsClientCapabilities.t option)
  send
  ~display_merlin_diagnostics
  ~shorten_merlin_diagnostics
  ~client_name
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
  { dune = Table.create (module Id) 32
  ; merlin = Table.create (module Uri) 32
  ; dirty_uris = Uri_set.empty
  ; send
  ; related_information
  ; tags
  ; display_merlin_diagnostics
  ; shorten_merlin_diagnostics
  ; client_name = String.lowercase client_name
  }
;;

let send =
  let module Range_map = Map.Make (Range) in
  (* TODO deduplicate related errors as well *)
  (* Adds the provided diagnostic if and only if it's not one of the dune diagnostics at
     that location *)
  let add_merlin_diagnostic
    t
    ~dune_diagnostics_table
    ~merlin_diagnostics_table
    uri
    (diagnostic : Diagnostic.t)
    =
    let merlin_diagnostics_for_file =
      Table.find_or_add merlin_diagnostics_table uri ~f:(fun _ -> [])
    in
    let merlin_diagnostics_for_file =
      let dune_map =
        Table.find dune_diagnostics_table uri |> Option.value ~default:Range_map.empty
      in
      match Range_map.find dune_map diagnostic.range with
      | None -> diagnostic :: merlin_diagnostics_for_file
      | Some dune_diagnostics ->
        if String.starts_with ~prefix:"vscode" t.client_name
           && List.exists dune_diagnostics ~f:(fun (d : Diagnostic.t) ->
             match d.source with
             | None ->
               failwith "BUG: source of a diagnostic was not set, when it should be"
             | Some source ->
               String.equal dune_source source
               &&
                 (match d.message, diagnostic.message with
                 | `String m1, `String m2 -> equal_message m1 m2
                 | `MarkupContent { kind; value }, `MarkupContent mc ->
                   Poly.equal kind mc.kind && equal_message value mc.value
                 | _, _ -> false))
        then merlin_diagnostics_for_file
        else diagnostic :: merlin_diagnostics_for_file
    in
    Table.set merlin_diagnostics_table uri merlin_diagnostics_for_file
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
      let dune_diagnostics_table = Table.create (module Uri) 4 in
      let merlin_diagnostics_table = Table.create (module Uri) 4 in
      Uri_set.iter dirty_uris ~f:(fun uri ->
        let just_uri = Uri_set.singleton uri in
        let dune_diagnostics =
          Table.fold
            ~init:[]
            ~f:(fun (diagnostic_uri, diagnostic) acc ->
              if Uri_set.mem just_uri diagnostic_uri then diagnostic :: acc else acc)
            t.dune
        in
        Table.set
          dune_diagnostics_table
          uri
          (range_map_of_unduplicated_diagnostics dune_diagnostics);
        Table.set merlin_diagnostics_table uri []);
      Table.foldi ~init:() t.merlin ~f:(fun uri diagnostics () ->
        if Uri_set.mem dirty_uris uri
        then
          List.iter diagnostics ~f:(fun diagnostic ->
            add_merlin_diagnostic
              t
              ~dune_diagnostics_table
              ~merlin_diagnostics_table
              uri
              diagnostic));
      t.dirty_uris
      <- (match which with
          | `All -> Uri_set.empty
          | `One uri -> Uri_set.remove t.dirty_uris uri);
      Table.foldi merlin_diagnostics_table ~init:[] ~f:(fun uri diagnostics acc ->
        (* we don't include a version because some of the diagnostics might come from
           dune which reads from the file system and not from the editor's view *)
        PublishDiagnosticsParams.create ~uri ~diagnostics () :: acc)
      |> t.send)
;;

let set t what =
  let uri =
    match what with
    | `Dune (_, uri, _) -> uri
    | `Merlin (uri, _) -> uri
  in
  t.dirty_uris <- Uri_set.add t.dirty_uris uri;
  match what with
  | `Merlin (uri, diagnostics) -> Table.set t.merlin uri diagnostics
  | `Dune (id, uri, diagnostics) -> Table.set t.dune id (uri, diagnostics)
;;

let remove t = function
  | `Dune diagnostic ->
    Table.find t.dune diagnostic
    |> Option.iter ~f:(fun (uri, _) ->
      Table.remove t.dune diagnostic;
      t.dirty_uris <- Uri_set.add t.dirty_uris uri)
  | `Merlin uri ->
    t.dirty_uris <- Uri_set.add t.dirty_uris uri;
    Table.remove t.merlin uri
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

let first_n_lines_of_range (range : Range.t) n : Range.t =
  if range.end_.line - range.start.line < n
  then range
  else
    { start = { character = range.start.character; line = range.start.line }
    ; end_ = { character = 0; line = range.start.line + n }
    }
;;

let default_error_to_diagnostic ~diagnostics ~merlin ~error =
  let doc = Document.Merlin.to_doc merlin in
  let create_diagnostic = Diagnostic.create ~source:ocamllsp_source in
  let uri = Document.uri doc in
  let loc = Loc.loc_of_report error in
  let original_range = Range.of_loc loc in
  let range =
    if diagnostics.shorten_merlin_diagnostics
    then first_n_lines_of_range original_range 1
    else original_range
  in
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
  let maybe_extra_range_information =
    match diagnostics.shorten_merlin_diagnostics with
    | false -> None
    | true ->
      (* NOTE: in VSCode, the message displayed for related information currently shows
         only the start of the span. It's still possible to find the full span, so we've
         opted not to send a second related-information with the end of the range, but in
         the future we could look into changing how VSCode displays relatedInformation
         ranges. *)
      let start_location = Location.create ~range:original_range ~uri in
      Some
        [ DiagnosticRelatedInformation.create
            ~location:start_location
            ~message:"Original error span"
        ]
  in
  let relatedInformation =
    Option.merge maybe_extra_range_information relatedInformation ~f:( @ )
  in
  let tags = tags_of_message diagnostics ~src:`Merlin message in
  create_diagnostic
    ?tags
    ?relatedInformation
    ~range
    ~message:(`String message)
    ~severity
    ()
;;

let query_merlin_for_diagnostics ~log_info diagnostics merlin =
  let create_diagnostic ~message =
    Diagnostic.create ~source:ocamllsp_source ~message:(`String message)
  in
  let command = Query_protocol.Errors { lexing = true; parsing = true; typing = true } in
  Document.Merlin.with_pipeline_exn ~log_info merlin (fun pipeline ->
    match Query_commands.dispatch pipeline command with
    | exception Merlin_extend.Extend_main.Handshake.Error error ->
      let message =
        sprintf "%s.\nHint: install the following packages: merlin-extend, reason" error
      in
      [ create_diagnostic ~range:Range.first_line ~message () ]
    | all_errors ->
      let merlin_diagnostics =
        match (Mpipeline.final_config pipeline).merlin.failures with
        | [] ->
          let syntax_diagnostics, typer_diagnostics =
            Base.List.partition_map all_errors ~f:(fun (error : Loc.error) ->
              let diagnostic = default_error_to_diagnostic ~diagnostics ~merlin ~error in
              match error.source with
              | Lexer | Parser -> First diagnostic
              | _ -> Second diagnostic)
          in
          (* Only show syntax errors if there are any, otherwise show the other
             diagnostics *)
          (match syntax_diagnostics with
           | [] ->
             let holes_as_err_diags =
               Query_commands.dispatch pipeline Holes
               |> List.rev_map ~f:(fun (loc, typ) ->
                 let range = Range.of_loc loc in
                 let severity = DiagnosticSeverity.Error in
                 let message =
                   "This typed hole should be replaced with an expression of type " ^ typ
                 in
                 (* we set specific diagnostic code = "hole" to be able to filter
                    through diagnostics easily *)
                 create_diagnostic ~code:(`String "hole") ~range ~message ~severity ())
             in
             (* Can we use [List.merge] instead? *)
             List.rev_append holes_as_err_diags typer_diagnostics
           | _ -> syntax_diagnostics)
        | config_failures ->
          List.map config_failures ~f:(fun failure ->
            create_diagnostic ~range:Range.first_line ~message:failure ())
      in
      List.sort
        merlin_diagnostics
        ~compare:(fun (d1 : Diagnostic.t) (d2 : Diagnostic.t) ->
          Range.compare d1.range d2.range))
;;

let merlin_diagnostics ~log_info diagnostics merlin =
  let open Fiber.O in
  let doc = Document.Merlin.to_doc merlin in
  let uri = Document.uri doc in
  let+ diagnostics_to_display =
    match diagnostics.display_merlin_diagnostics with
    | true -> query_merlin_for_diagnostics ~log_info diagnostics merlin
    | false -> Fiber.return []
  in
  set diagnostics (`Merlin (uri, diagnostics_to_display))
;;

let has_cached_errors diagnostics merlin =
  let cached_diagnostics =
    Document.Merlin.to_doc merlin |> Document.uri |> Table.find diagnostics.merlin
  in
  match cached_diagnostics with
  | None | Some [] -> false
  | _ -> true
;;

let set_display_merlin_diagnostics t ~display_merlin_diagnostics =
  t.display_merlin_diagnostics <- display_merlin_diagnostics
;;

let set_shorten_merlin_diagnostics t ~shorten_merlin_diagnostics =
  (* The ocaml-lsp ChangeConfiguration command handles re-sending diagnostics on configuration
     changes *)
  t.shorten_merlin_diagnostics <- shorten_merlin_diagnostics
;;

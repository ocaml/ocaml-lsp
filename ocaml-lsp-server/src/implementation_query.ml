open Import
open Fiber.O

(* Merlin's locate query reports [At_origin] when invoked on a type declaration
   in an interface. Keep the declaration's path through nested modules so that
   we can resolve it with Merlin's shape-backed locator in the implementation's
   typing environment. *)
type type_path =
  { modules : string list
  ; name : string
  }

let type_path_at_position doc position =
  Document.Merlin.with_pipeline_exn ~name:"implementation" doc (fun pipeline ->
    let typer = Mpipeline.typer_result pipeline in
    let browse = Mtyper.get_typedtree typer |> Mbrowse.of_typedtree in
    let position = Mpipeline.get_lexing_pos pipeline (Position.logical position) in
    let enclosing = Mbrowse.enclosing position [ browse ] in
    let name =
      List.find_map enclosing ~f:(fun (_, node) ->
        match node with
        | Browse_raw.Type_declaration declaration
          when Loc.compare_pos position declaration.typ_name.loc = 0 ->
          Some declaration.typ_name.txt
        | _ -> None)
    in
    match name with
    | None -> None
    | Some _
      when List.exists enclosing ~f:(fun (_, node) ->
             match node with
             | Browse_raw.Module_type_declaration _ -> true
             | _ -> false) -> None
    | Some name ->
      let modules =
        List.filter_map enclosing ~f:(fun (_, node) ->
          match node with
          | Browse_raw.Module_declaration { md_name = { txt = Some name; _ }; _ } ->
            Some name
          | _ -> None)
        |> List.rev
      in
      Some { modules; name })
;;

let longident_of_type_path { modules; name } =
  Longident.unflatten (modules @ [ name ]) |> Option.value_exn
;;

let location_in_document doc path =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin doc ->
    Document.Merlin.with_pipeline_exn ~name:"implementation" doc (fun pipeline ->
      let typer = Mpipeline.typer_result pipeline in
      let local_defs = Mtyper.get_typedtree typer in
      let config : Merlin_analysis.Locate.config =
        { mconfig = Mpipeline.final_config pipeline
        ; ml_or_mli = `Smart
        ; traverse_aliases = true
        }
      in
      match
        Merlin_analysis.Env_lookup.by_longident
          [ `Type ]
          (longident_of_type_path path)
          (Mtyper.get_env typer)
      with
      | None -> None
      | Some (path, _) ->
        (match
           Merlin_analysis.Locate.from_path
             ~config
             ~env:(Mtyper.get_env typer)
             ~local_defs
             ~namespace:Type
             path
         with
         | `Found { file; location; _ } ->
           Some { Location.uri = Uri.of_path file; range = Range.of_loc location }
         | `Builtin _ | `File_not_found _ | `Not_found _ | `Not_in_env _ -> None))
;;

let with_document (state : State.t) uri f =
  match Document_store.get_opt state.store uri with
  | Some doc -> f doc
  | None ->
    let* doc =
      Document.make_from_file
        (State.wheel state)
        state.merlin_config
        state.merlin
        uri
        ~position_encoding:(State.position_encoding state)
    in
    (match doc with
     | None -> Fiber.return None
     | Some doc ->
       Fiber.finalize (fun () -> f doc) ~finally:(fun () -> Document.close doc))
;;

let run (state : State.t) uri position =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin when Document.Merlin.kind merlin = Impl -> Fiber.return None
  | `Merlin merlin ->
    let* path = type_path_at_position merlin position in
    (match path with
     | None -> Fiber.return None
     | Some path ->
       let implementation_uris = Document.get_impl_intf_counterparts (Some merlin) uri in
       let* locations =
         Fiber.parallel_map implementation_uris ~f:(fun implementation_uri ->
           with_document state implementation_uri (fun doc ->
             location_in_document doc path))
       in
       (match List.filter_opt locations with
        | [] -> Fiber.return None
        | locations -> Fiber.return (Some (`Location locations))))
;;

open Import
open Fiber.O

(* Merlin's locate query reports [At_origin] when invoked on a type declaration
   in an interface. Keep the declaration's path through nested modules so that
   we can resolve it with Merlin's shape-backed locator in the implementation's
   typing environment. *)
let type_path_at_position doc position =
  Document.Merlin.with_pipeline_exn ~name:"implementation" doc (fun pipeline ->
    let position = Mpipeline.get_lexing_pos pipeline (Position.logical position) in
    let enclosing =
      let browse =
        Mpipeline.typer_result pipeline |> Mtyper.get_typedtree |> Mbrowse.of_typedtree
      in
      Mbrowse.enclosing position [ browse ]
    in
    match
      List.find_map enclosing ~f:(fun (_, (node : Browse_raw.node)) ->
        match node with
        | Type_declaration declaration
          when Loc.compare_pos position declaration.typ_name.loc = 0 ->
          Some declaration.typ_name.txt
        | _ -> None)
    with
    | None -> None
    | Some _
      when List.exists enclosing ~f:(fun (_, (node : Browse_raw.node)) ->
             match node with
             | Module_type_declaration _ -> true
             | Module_type { mty_desc = Tmty_functor (Named (_, _, parameter), _); _ }
             | Module_expr { mod_desc = Tmod_functor (Named (_, _, parameter), _); _ } ->
               Loc.compare_pos position parameter.mty_loc = 0
             | _ -> false) -> None
    | Some name ->
      let modules =
        List.filter_map enclosing ~f:(fun (_, (node : Browse_raw.node)) ->
          match node with
          | Module_declaration { md_name = { txt = Some name; _ }; _ }
          | Module_binding { mb_name = { txt = Some name; _ }; _ } -> Some name
          | _ -> None)
        |> List.rev
      in
      Longident.unflatten (modules @ [ name ]))
;;

(* A functor result has no ordinary path such as [F.t]. Expose each result as a
   temporary module in the query environment, retaining its declaration UIDs
   for Locate. Keeping a module boundary avoids finding an outer [t] when the
   result itself does not export [t]. *)
let rec result_signature seen env mty =
  match Ocaml_typing.Mtype.scrape env mty with
  | Mty_signature _ as mty -> Some (env, mty)
  | Mty_functor (parameter, result) ->
    let env =
      match parameter with
      | Named (Some id, mty) -> Env.add_module id Mp_present mty env
      | Named (None, _) | Unit -> env
    in
    result_signature seen env result
  | Mty_alias path when not (List.exists seen ~f:(Path.same path)) ->
    (match Env.find_module path env with
     | declaration -> result_signature (path :: seen) env declaration.md_type
     | exception Not_found -> None)
  | Mty_alias _ | Mty_ident _ | Mty_for_hole -> None
;;

let rec module_environment env (lid : Longident.t) =
  let open Option.O in
  let* env, signature =
    let* env, lid =
      match lid with
      | Lident _ -> Some (env, lid)
      | Ldot (parent, name) ->
        let+ env, lid = module_environment env parent.txt in
        env, Longident.Ldot ({ parent with txt = lid }, name)
      | Lapply _ -> None
    in
    let* md_type =
      match Env.find_module_by_name lid env with
      | (_ : Path.t), declaration -> Some declaration.md_type
      | exception Not_found -> None
    in
    result_signature [] env md_type
  in
  let id = Ident.create_local "__ocamllsp_implementation" in
  let env = Env.add_module id Mp_present signature env in
  Some (env, Longident.Lident (Ident.name id))
;;

let lookup_type =
  let lookup env lid =
    Merlin_analysis.Env_lookup.by_longident [ `Type ] lid env
    |> Option.map ~f:(fun (path, _) -> env, path)
  in
  fun env lid ->
    match lookup env lid with
    | Some _ as result -> result
    | None ->
      (match lid with
       | Longident.Lident _ | Lapply _ -> None
       | Ldot (parent, name) ->
         let open Option.O in
         let* env, lid = module_environment env parent.txt in
         lookup env (Longident.Ldot ({ parent with txt = lid }, name)))
;;

let location_in_document doc path =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin doc ->
    Document.Merlin.with_pipeline_exn ~name:"implementation" doc (fun pipeline ->
      let typer = Mpipeline.typer_result pipeline in
      match lookup_type (Mtyper.get_env typer) path with
      | None -> None
      | Some (env, path) ->
        (match
           let config : Merlin_analysis.Locate.config =
             { mconfig = Mpipeline.final_config pipeline
             ; ml_or_mli = `Smart
             ; traverse_aliases = true
             }
           in
           let local_defs = Mtyper.get_typedtree typer in
           Merlin_analysis.Locate.from_path ~config ~env ~local_defs ~namespace:Type path
         with
         | `Found
             { uid = Ocaml_typing.Shape.Uid.Item { from = Impl; _ }; file; location; _ }
           -> Some { Location.uri = Uri.of_path file; range = Range.of_loc location }
         (* Smart locate can fall back to a declaration in an interface-only
            dependency. That is not an implementation. *)
         | `Found _ | `Builtin _ | `File_not_found _ | `Not_found _ | `Not_in_env _ ->
           None))
;;

let with_document (state : State.t) uri f =
  match Document_store.get_opt state.store uri with
  | Some doc -> f doc
  | None ->
    Document.make_from_file
      (State.wheel state)
      state.merlin_config
      state.merlin
      uri
      ~position_encoding:(State.position_encoding state)
    >>= (function
     | None -> Fiber.return None
     | Some doc ->
       Fiber.finalize (fun () -> f doc) ~finally:(fun () -> Document.close doc))
;;

let run (state : State.t) uri position =
  match Document.kind (Document_store.get state.store uri) with
  | `Other -> Fiber.return None
  | `Merlin merlin when Document.Merlin.kind merlin = Impl -> Fiber.return None
  | `Merlin merlin ->
    type_path_at_position merlin position
    >>= (function
     | None -> Fiber.return None
     | Some path ->
       Document.get_impl_intf_counterparts (Some merlin) uri
       |> Fiber.parallel_map ~f:(fun implementation_uri ->
         with_document state implementation_uri (fun doc -> location_in_document doc path))
       >>| List.filter_opt
       >>| (function
        | [] -> None
        | locations -> Some (`Location locations)))
;;

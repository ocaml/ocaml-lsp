open Import

module Fiber_option = struct
  let bind t ~f =
    Fiber.bind t ~f:(function
      | None -> Fiber.return None
      | Some x -> f x)
  ;;

  module Let_syntax = struct
    module Let_syntax = struct
      let bind = bind
    end
  end
end

let module_path_iter module_path_ref is_at_cursor =
  let module_binding (self : Ast_iterator.iterator) (mb : Parsetree.module_binding) =
    if is_at_cursor mb.pmb_loc
    then (
      (match mb.pmb_name with
       | { txt = Some name; _ } -> module_path_ref := name :: !module_path_ref
       | _ -> ());
      Ast_iterator.default_iterator.module_binding self mb)
  in
  module_binding
;;

let find_parent_function_of ~position parsetree =
  (* TODO: This is not finding top level definitions that deconstruct values like
     [let { foo } = some_fn ();], or [let () = command ... ;;] and more. *)
  let is_at_cursor = Util.is_at_cursor position in
  let last_fn = ref None in
  let module_path = ref [] in
  let value_binding (self : Ast_iterator.iterator) (vb : Parsetree.value_binding) =
    if is_at_cursor vb.pvb_expr.pexp_loc
    then (
      (match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
       (* E.g. [let foo a = a + 1], [let foo = fun a -> a + 1]*)
       | Ppat_var fn_name, Pexp_function _
       (* E.g. [let foo a : int = a + 1] *)
       | Ppat_constraint ({ ppat_desc = Ppat_var fn_name; _ }, _, _), Pexp_function _
       (* E.g. [let foo : type t. t -> int = fun x -> x + 1]*)
       | ( Ppat_constraint ({ ppat_desc = Ppat_var fn_name; _ }, _, _)
         , Pexp_newtype
             ( _
             , _
             , { pexp_desc = Pexp_constraint ({ pexp_desc = Pexp_function _; _ }, _, _)
               ; _
               } ) ) -> last_fn := Some fn_name
       (* The following are only relevant for when [last_fn] is [None] this allows top
          level call resolving e.g. [let () = main () ;;]*)
       | Ppat_var name, _ when Option.is_none !last_fn -> last_fn := Some name
       (* TODO: Add cases for deconstructed tuples, records, array and other top level
          value bindings. *)
       | _ -> ());
      Ast_iterator.default_iterator.value_binding self vb)
  in
  let module_binding = module_path_iter module_path is_at_cursor in
  let iterator = { Ast_iterator.default_iterator with value_binding; module_binding } in
  let () =
    match parsetree with
    | `Interface signature -> iterator.signature iterator signature
    | `Implementation structure -> iterator.structure iterator structure
  in
  let%map.Option last_fn = !last_fn in
  last_fn, List.rev !module_path
;;

module Function_type = struct
  type t =
    | Not_a_function_or_identifier
    | Maybe_fn_call_or_reference of Longident.t Asttypes.loc
    | Maybe_fn_alias of string Asttypes.loc * Longident.t Asttypes.loc
    | Fn_definition of string Asttypes.loc
end

let function_type_at ~position parsetree =
  let is_at_cursor = Util.is_at_cursor position in
  let ret = ref Function_type.Not_a_function_or_identifier in
  let module_path = ref [] in
  let module_binding = module_path_iter module_path is_at_cursor in
  let value_binding (self : Ast_iterator.iterator) (vb : Parsetree.value_binding) =
    if is_at_cursor vb.pvb_loc
    then (
      (match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
       | Ppat_var fn_name, Pexp_function _ when is_at_cursor fn_name.loc ->
         ret := Fn_definition fn_name
       | Ppat_constraint ({ ppat_desc = Ppat_var fn_name; _ }, _, _), _
         when is_at_cursor fn_name.loc -> ret := Fn_definition fn_name
       | Ppat_var fn_name, Pexp_ident id when is_at_cursor fn_name.loc ->
         ret := Maybe_fn_alias (fn_name, id)
       | _ -> ());
      Ast_iterator.default_iterator.value_binding self vb)
  in
  let expr (self : Ast_iterator.iterator) (expr : Parsetree.expression) =
    if is_at_cursor expr.pexp_loc
    then (
      match expr.pexp_desc with
      | Pexp_ident id -> ret := Maybe_fn_call_or_reference id
      | _ -> ());
    Ast_iterator.default_iterator.expr self expr
  in
  let iterator =
    { Ast_iterator.default_iterator with value_binding; expr; module_binding }
  in
  let () =
    match parsetree with
    | `Interface signature -> iterator.signature iterator signature
    | `Implementation structure -> iterator.structure iterator structure
  in
  !ret, List.rev !module_path
;;

let detail_str ~uri ~(state : State.t) =
  let workspace_root =
    (match state.init with
     | Initialized { params = { rootUri; _ }; _ } ->
       rootUri |> Option.map ~f:(fun p -> Uri.to_string p ^ "/")
     | Uninitialized -> None)
    |> Option.value ~default:(Uri.of_path "/" |> Uri.to_string)
  in
  String.chop_prefix_if_exists (Uri.to_string uri) ~prefix:workspace_root
;;

let get_merlin_doc (state : State.t) uri =
  let open Fiber.O in
  let%bind.Fiber_option doc =
    match Document_store.get_opt state.store uri with
    | Some doc -> Fiber.return (Some doc)
    | None ->
      let%bind.Fiber_option doc = Util.open_document_from_file state uri in
      let+ () = Document_store.open_document state.store doc in
      Some doc
  in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin -> Some merlin |> Fiber.return
;;

let get_parsetree merlin_doc ~log_info =
  Document.Merlin.with_pipeline_exn ~log_info merlin_doc (fun pipeline ->
    Mpipeline.reader_parsetree pipeline)
;;

let handle_prepare ~log_info (server : State.t Server.t) params =
  let open Fiber.O in
  let state = Server.state server in
  let { CallHierarchyPrepareParams.textDocument = { uri }; position; _ } = params in
  let first_alias = Core.Set_once.create () in
  let rec get_prepare_item ~position ~uri =
    let%bind.Fiber_option merlin_doc = get_merlin_doc state uri in
    let* parsetree = get_parsetree merlin_doc ~log_info in
    let resolve_definition (rhs : Longident.t Asttypes.loc) =
      (* Important to use loc_end for the position, e.g. for [Foo.foo] we want to go to
         [foo] not [Foo] *)
      let position = Position.of_lexical_position rhs.loc.loc_end in
      (* Definition is None whenever the cursor is on an identifier that is not a
         function. Then we also don't want to to call hierachy, so fine to bind here. *)
      let%bind.Fiber_option (`Location locs) =
        Option.value_map position ~default:(Fiber.return None) ~f:(fun position ->
          Definition_query.run ~log_info `Definition state uri position)
      in
      (* Definition should only return exactly one location. *)
      let%bind.Fiber_option { range = { start; _ }; uri } =
        List.hd locs |> Fiber.return
      in
      get_prepare_item ~position:start ~uri
    in
    match function_type_at ~position parsetree with
    | Not_a_function_or_identifier, _ -> Fiber.return None
    | Maybe_fn_call_or_reference rhs, _ -> resolve_definition rhs
    | Maybe_fn_alias (lhs, rhs), module_path ->
      Core.Set_once.set_if_none first_alias (uri, lhs, module_path);
      resolve_definition rhs
    | Fn_definition id, module_path ->
      let item =
        let uri, { Loc.txt; loc }, module_path =
          Base.Option.value (Core.Set_once.get first_alias) ~default:(uri, id, module_path)
        in
        let name = String.concat (module_path @ [ txt ]) ~sep:"." in
        let range = Range.of_loc loc in
        CallHierarchyItem.create
          ~detail:(detail_str ~state ~uri)
          ~kind:SymbolKind.Function
          ~name
          ~range
          ~selectionRange:range
          ~uri
          ()
      in
      Some [ item ] |> Fiber.return
  in
  get_prepare_item ~position ~uri
;;

(** Batched mapping for [Fiber.t], would be better if we had some sort of job queue, but
    too much effort right now, and this should be good enough for now. *)
let batched_parallel fibers ~f ~batch_size =
  let grouped = Base.List.chunks_of fibers ~length:batch_size in
  Fiber.sequential_map grouped ~f:(Fiber.parallel_map ~f) |> Fiber.map ~f:List.concat
;;

let handle_incoming ~log_info (server : State.t Server.t) params =
  let open Fiber.O in
  let state = Server.state server in
  let { CallHierarchyIncomingCallsParams.item; _ } = params in
  let { CallHierarchyItem.uri; range; _ } = item in
  let%bind.Fiber_option merlin_doc = get_merlin_doc state uri in
  let* locs, _synced =
    Document.Merlin.dispatch_exn
      ~log_info
      merlin_doc
      (Occurrences (`Ident_at (Position.logical range.end_), `Project))
  in
  List.map locs ~f:(fun (loc : Loc.t) -> loc.loc_start.pos_fname, loc)
  |> Base.List.Assoc.sort_and_group ~compare:Base.String.compare
  |> batched_parallel ~batch_size:40 ~f:(fun (fn_name, locs) ->
    (* Using Map to only open one doc and create one parsetree per file name. *)
    let uri = DocumentUri.of_path fn_name in
    let%bind.Fiber_option merlin_doc = get_merlin_doc state uri in
    let+ parsetree = get_parsetree merlin_doc ~log_info in
    List.filter_map locs ~f:(fun loc ->
      let range = Range.of_loc loc in
      let%map.Option { txt; loc }, module_path =
        find_parent_function_of ~position:range.start parsetree
      in
      let name = String.concat (module_path @ [ txt ]) ~sep:"." in
      loc, (name, range))
    |> Base.List.Assoc.sort_and_group ~compare:Loc.compare
    |> List.map ~f:(fun (loc, ranges) ->
      let name, _ = List.hd_exn ranges in
      (* by construction every loc has at least one range associated with it *)
      let ranges = List.map ranges ~f:snd in
      let from =
        CallHierarchyItem.create
          ~detail:(detail_str ~state ~uri)
          ~kind:SymbolKind.Function
          ~name
          ~range:(Range.of_loc loc)
          ~selectionRange:(Range.of_loc loc)
          ~uri
          ()
      in
      CallHierarchyIncomingCall.create ~from ~fromRanges:ranges)
    |> Option.some)
  >>| List.filter_opt
  >>| List.concat
  >>| Option.some
;;

let handle_outgoing ~log_info (server : State.t Server.t) params =
  ignore log_info;
  ignore server;
  ignore params;
  Fiber.return None
;;

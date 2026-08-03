open Import
open Fiber.O

module Code_action_error = struct
  type t =
    | Initial
    | Need_merlin_extend of string
    | Exn of Exn_with_backtrace.t

  let empty = Initial

  let combine x y =
    match x, y with
    | Initial, _ -> y (* [Initial] cedes to any *)
    | _, Initial -> x
    | Exn _, _ -> x (* [Exn] takes over any *)
    | _, Exn _ -> y
    | Need_merlin_extend _, Need_merlin_extend _ -> y
  ;;
end

module Code_action_error_monoid = struct
  type t = Code_action_error.t

  include Monoid.Make (Code_action_error)
end

let client_can_resolve_edits (state : State.t) =
  let capabilities = State.client_capabilities state in
  Capabilities.code_action_data_support capabilities
  &&
  match Capabilities.code_action_resolve_properties capabilities with
  | Some properties -> List.mem properties "edit" ~equal:String.equal
  | None -> false
;;

let compute_ocaml_code_actions (params : CodeActionParams.t) state doc =
  let destruct_dispatch = Document.merlin_exn doc |> Action_destruct.cached_dispatch in
  let enabled_actions =
    List.filter
      ~f:(fun (action : Code_action.t) ->
        Lsp.Code_action.kind_is_requested params.context.only action.kind)
      [ Action_destruct_line.t ~dispatch:destruct_dispatch state
      ; Action_destruct.t ~dispatch:destruct_dispatch state
      ; Action_update_signature.t state
      ; Action_combine_cases.t
      ; Action_inferred_intf.t state
      ; Action_type_annotate.t
      ; Action_remove_type_annotation.t
      ; Action_construct.t
      ; Action_refactor_open.unqualify
      ; Action_refactor_open.qualify
      ; Action_add_rec.t
      ; Action_mark_remove_unused.mark
      ; Action_mark_remove_unused.remove
      ; (if client_can_resolve_edits state
         then Action_inline.unresolved
         else Action_inline.t)
      ; Action_extract.local
      ; Action_extract.function_
      ]
  in
  let batchable, non_batchable =
    List.partition_map enabled_actions ~f:(fun ca ->
      match ca.run with
      | `Batchable f -> Either.First f
      | `Non_batchable f -> Second f)
  in
  let* batch_results =
    if List.is_empty batchable
    then Fiber.return []
    else
      Document.Merlin.with_pipeline_exn
        ~name:"batched-code-actions"
        (Document.merlin_exn doc)
        (fun pipeline ->
           List.filter_map batchable ~f:(fun ca ->
             try ca pipeline doc params with
             | Merlin_extend.Extend_main.Handshake.Error _ -> None))
  in
  let code_action ca =
    let+ res =
      Fiber.map_reduce_errors
        ~on_error:(fun (exn : Exn_with_backtrace.t) ->
          match exn.exn with
          | Merlin_extend.Extend_main.Handshake.Error error ->
            Fiber.return (Code_action_error.Need_merlin_extend error)
          | _ -> Fiber.return (Code_action_error.Exn exn))
        (module Code_action_error_monoid)
        (fun () -> ca doc params)
    in
    match res with
    | Ok res -> res
    | Error Initial -> assert false
    | Error (Need_merlin_extend _) -> None
    | Error (Exn exn) -> Exn_with_backtrace.reraise exn
  in
  let+ non_batch_results =
    Fiber.parallel_map non_batchable ~f:code_action |> Fiber.map ~f:List.filter_opt
  in
  batch_results @ non_batch_results
;;

let compute server (params : CodeActionParams.t) =
  let state : State.t = Server.state server in
  let uri = params.textDocument.uri in
  let doc =
    let store = state.store in
    Document_store.get_opt store uri
  in
  let kind_is_requested = Lsp.Code_action.kind_is_requested params.context.only in
  let dune_actions =
    if kind_is_requested CodeActionKind.QuickFix
    then Dune.code_actions (State.dune state) params.textDocument.uri
    else []
  in
  let actions xs =
    let xs =
      match params.context.only with
      | None -> xs
      | Some _ ->
        List.filter xs ~f:(fun (action : CodeAction.t) ->
          Option.exists action.kind ~f:kind_is_requested)
    in
    match xs with
    | [] -> None
    | xs -> Some (List.map ~f:(fun a -> `CodeAction a) xs)
  in
  match doc with
  | None -> Fiber.return (Reply.now (actions dune_actions), state)
  | Some doc ->
    let capabilities = Capabilities.show_document (State.client_capabilities state) in
    let open_related =
      if kind_is_requested Action_open_related.kind
      then Action_open_related.for_uri capabilities doc
      else []
    in
    let open_dune =
      if kind_is_requested Action_open_dune.kind
      then Action_open_dune.for_uri capabilities uri
      else []
    in
    (match Document.syntax doc with
     | Ocamllex | Menhir | Cram | Dune ->
       Fiber.return (Reply.now (actions (dune_actions @ open_related @ open_dune)), state)
     | Ocaml | Reason | Mlx ->
       let* merlin_jumps =
         match state.configuration.data.merlin_jump_code_actions with
         | Some { enable = true } -> Action_jump.code_actions doc params capabilities
         | Some { enable = false } | None -> Fiber.return []
       in
       let reply () =
         let+ code_action_results = compute_ocaml_code_actions params state doc in
         List.concat
           [ code_action_results; dune_actions; open_related; open_dune; merlin_jumps ]
         |> actions
       in
       let later f =
         Fiber.return
           ( Reply.later (fun k ->
               let* resp = f () in
               k resp)
           , state )
       in
       later reply)
;;

let resolve state action =
  match Action_inline.resolve state action with
  | Some resolved -> resolved
  | None -> Fiber.return action
;;

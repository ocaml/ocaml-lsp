open Import
open Fiber.O

module Code_action_error = struct
  type t =
    | Initial
    | Need_merlin_extend of string
    | Exn of Exn_with_backtrace.t

  let empty = Initial

  let combine x y =
    match (x, y) with
    | Initial, _ -> y (* [Initial] cedes to any *)
    | _, Initial -> x
    | Exn _, _ -> x (* [Exn] takes over any *)
    | _, Exn _ -> y
    | Need_merlin_extend _, Need_merlin_extend _ -> y
end

module Code_action_error_monoid = struct
  type t = Code_action_error.t

  include Stdune.Monoid.Make (Code_action_error)
end

let compute server (params : CodeActionParams.t) =
  let state : State.t = Server.state server in
  let doc =
    let uri = params.textDocument.uri in
    let store = state.store in
    Document_store.get_opt store uri
  in
  let dune_actions =
    Dune.code_actions (State.dune state) params.textDocument.uri
  in
  let actions = function
    | [] -> None
    | xs -> Some (List.map ~f:(fun a -> `CodeAction a) xs)
  in
  match doc with
  | None -> Fiber.return (Reply.now (actions dune_actions), state)
  | Some doc -> (
    match Document.syntax doc with
    | Ocamllex | Menhir | Cram | Dune ->
      let state : State.t = Server.state server in
      Fiber.return (Reply.now (actions dune_actions), state)
    | Ocaml | Reason ->
      let reply () =
        let code_action (ca : Code_action.t) =
          match params.context.only with
          | Some set when not (List.mem set ca.kind ~equal:Poly.equal) ->
            Fiber.return None
          | Some _ | None -> (
            let+ res =
              Fiber.map_reduce_errors
                ~on_error:(fun (exn : Exn_with_backtrace.t) ->
                  match exn.exn with
                  | Merlin_extend.Extend_main.Handshake.Error error ->
                    Fiber.return (Code_action_error.Need_merlin_extend error)
                  | _ -> Fiber.return (Code_action_error.Exn exn))
                (module Code_action_error_monoid)
                (fun () -> ca.run doc params)
            in
            match res with
            | Ok res -> res
            | Error Initial -> assert false
            | Error (Need_merlin_extend _) -> None
            | Error (Exn exn) -> Exn_with_backtrace.reraise exn)
        in
        let+ code_action_results =
          (* XXX this is a really bad use of resources. we should be batching
             all the merlin related work *)
          Fiber.parallel_map ~f:code_action
            [ Action_destruct.t state
            ; Action_inferred_intf.t state
            ; Action_type_annotate.t
            ; Action_construct.t
            ; Action_refactor_open.unqualify
            ; Action_refactor_open.qualify
            ; Action_add_rec.t
            ; Action_mark_remove_unused.mark
            ; Action_mark_remove_unused.remove
            ]
        in
        List.filter_opt code_action_results
        |> List.append dune_actions |> actions
      in
      let later f =
        Fiber.return
          ( Reply.later (fun k ->
                let* resp = f () in
                k resp)
          , state )
      in
      later reply)

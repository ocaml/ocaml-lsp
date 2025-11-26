open Import
open Fiber.O

let outline_type typ =
  typ
  |> Format.asprintf "@[<h>: %s@]"
  |> String.extract_words ~is_word_char:(function
    | ' ' | '\t' | '\n' -> false
    | _ -> true)
  |> String.concat ~sep:" "
;;

let compute (state : State.t) { InlayHintParams.range; textDocument = { uri }; _ } =
  let doc =
    let store = state.store in
    Document_store.get store uri
  in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin m when Document.Merlin.kind m = Intf -> Fiber.return None
  | `Merlin doc ->
    let+ hints =
      let hint_let_bindings =
        Option.map state.configuration.data.inlay_hints ~f:(fun c -> c.hint_let_bindings)
        |> Option.value ~default:false
      in
      let hint_pattern_variables =
        Option.map state.configuration.data.inlay_hints ~f:(fun c ->
          c.hint_pattern_variables)
        |> Option.value ~default:false
      in
      Document.Merlin.with_pipeline_exn ~name:"inlay-hints" doc (fun pipeline ->
        let start = range.start |> Position.logical
        and stop = range.end_ |> Position.logical in
        let command =
          Query_protocol.Inlay_hints
            (start, stop, hint_let_bindings, hint_pattern_variables, not inside_test)
        in
        let hints = Query_commands.dispatch pipeline command in
        List.filter_map
          ~f:(fun (pos, label) ->
            let open Option.O in
            let+ position = Position.of_lexical_position pos in
            let label = `String (outline_type label) in
            InlayHint.create
              ~kind:Type
              ~position
              ~label
              ~paddingLeft:false
              ~paddingRight:false
              ())
          hints)
    in
    Some hints
;;

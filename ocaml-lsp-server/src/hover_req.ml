open Import
open Fiber.O

type mode =
  | Default
  | Extended_fixed of int
  | Extended_variable

(* possibly overwrite the default mode using an environment variable *)
let environment_mode =
  match Env_vars._IS_HOVER_EXTENDED () with
  | Some true -> Extended_variable
  | Some false | None -> Default

let format_contents ~syntax ~markdown ~typ ~doc =
  (* TODO for vscode, we should just use the language id. But that will not work
     for all editors *)
  `MarkupContent
    (if markdown then
       let value =
         let markdown_name = Document.Syntax.markdown_name syntax in
         match doc with
         | None -> sprintf "```%s\n%s\n```" markdown_name typ
         | Some s ->
           let doc =
             match Doc_to_md.translate s with
             | Raw d -> sprintf "(** %s *)" d
             | Markdown d -> d
           in
           sprintf "```%s\n%s\n```\n---\n%s" markdown_name typ doc
       in
       { MarkupContent.value; kind = MarkupKind.Markdown }
     else
       let value =
         match doc with
         | None -> sprintf "%s" typ
         | Some d -> sprintf "%s\n%s" typ d
       in
       { MarkupContent.value; kind = MarkupKind.PlainText })

let handle server { HoverParams.textDocument = { uri }; position; _ } mode =
  Fiber.of_thunk (fun () ->
      let state : State.t = Server.state server in
      let doc =
        let store = state.store in
        Document_store.get store uri
      in
      match Document.kind doc with
      | `Other -> Fiber.return None
      | `Merlin merlin -> (
        let verbosity =
          let mode =
            match (mode, environment_mode) with
            | Default, Extended_variable -> Extended_variable
            | x, _ -> x
          in
          match mode with
          | Default -> 0
          | Extended_fixed v ->
            state.hover_extended.history <- None;
            v
          | Extended_variable ->
            let v =
              match state.hover_extended.history with
              | None -> 0
              | Some (h_uri, h_position, h_verbosity) ->
                if
                  Uri.equal uri h_uri
                  && Ordering.is_eq (Position.compare position h_position)
                then succ h_verbosity
                else 0
            in
            state.hover_extended.history <- Some (uri, position, v);
            v
        in
        let pos = Position.logical position in
        let* type_enclosing =
          Document.Merlin.type_enclosing merlin pos verbosity
        in
        match type_enclosing with
        | None -> Fiber.return None
        | Some { Document.Merlin.loc; typ; doc = documentation } ->
          let syntax = Document.syntax doc in
          let+ typ =
            (* We ask Ocamlformat to format this type *)
            let* result =
              Ocamlformat_rpc.format_type state.ocamlformat_rpc ~typ
            in
            match result with
            | Ok v ->
              (* OCamlformat adds an unnecessay newline at the end of the
                 type *)
              Fiber.return (String.trim v)
            | Error `No_process -> Fiber.return typ
            | Error (`Msg message) ->
              (* We log OCamlformat errors and display the unformated type *)
              let+ () =
                let message =
                  sprintf
                    "An error occured while querying ocamlformat:\n\
                     Input type: %s\n\n\
                     Answer: %s"
                    typ
                    message
                in
                State.log_msg server ~type_:Warning ~message
              in
              typ
          in
          let contents =
            let markdown =
              let client_capabilities = State.client_capabilities state in
              ClientCapabilities.markdown_support
                client_capabilities
                ~field:(fun td ->
                  Option.map td.hover ~f:(fun h -> h.contentFormat))
            in
            format_contents ~syntax ~markdown ~typ ~doc:documentation
          in
          let range = Range.of_loc loc in
          Some (Hover.create ~contents ~range ())))

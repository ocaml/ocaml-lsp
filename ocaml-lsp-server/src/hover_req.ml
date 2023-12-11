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

let print_dividers sections = String.concat ~sep:"\n---\n" sections

let format_as_code_block ~highlighter strings =
  sprintf "```%s\n%s\n```" highlighter (String.concat ~sep:" " strings)

let format_contents ~syntax ~markdown ~typ ~doc
    ~(syntax_doc : Query_protocol.syntax_doc_result option) =
  (* TODO for vscode, we should just use the language id. But that will not work
     for all editors *)
  let syntax_doc =
    Option.map syntax_doc ~f:(fun syntax_doc ->
        sprintf
          "`syntax` %s: %s. See [Manual](%s)"
          syntax_doc.name
          syntax_doc.description
          syntax_doc.documentation)
  in
  `MarkupContent
    (if markdown then
       let value =
         let markdown_name = Document.Syntax.markdown_name syntax in
         let type_info = Some (format_as_code_block ~highlighter:markdown_name [ typ ]) in
         let doc =
           Option.map doc ~f:(fun doc ->
               match Doc_to_md.translate doc with
               | Raw d -> d
               | Markdown d -> d)
         in
         print_dividers (List.filter_opt [ type_info; syntax_doc; doc ])
       in
       { MarkupContent.value; kind = MarkupKind.Markdown }
     else
       let value =
         print_dividers (List.filter_opt [ Some typ; syntax_doc; doc ])
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
        let with_syntax_doc =
          match state.configuration.data.syntax_documentation with
          | Some { enable = true } -> true
          | Some _ | None -> false
        in
        let* type_enclosing =
          Document.Merlin.type_enclosing merlin pos verbosity ~with_syntax_doc
        in
        match type_enclosing with
        | None -> Fiber.return None
        | Some { Document.Merlin.loc; typ; doc = documentation; syntax_doc } ->
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
            format_contents
              ~syntax
              ~markdown
              ~typ
              ~doc:documentation
              ~syntax_doc
          in
          let range = Range.of_loc loc in
          Some (Hover.create ~contents ~range ())))

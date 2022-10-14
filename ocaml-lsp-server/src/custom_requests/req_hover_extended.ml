open Import
open Fiber.O

let capability = ("handleHoverExtended", `Bool true)

let meth = "ocamllsp/hoverExtended"

module Request_params = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; cursor_position : Position.t
    ; verbosity : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]

  let params_schema =
    `Assoc
      [ ("textDocument", `String "<TextDocumentIdentifier>")
      ; ("position", `String "<Position>")
      ; ("verbosity", `String "<integer?>")
      ]

  let of_jsonrpc_params params : t option =
    match params with
    | `Assoc
        [ ("textDocument", text_document)
        ; ("position", position)
        ; ("verbosity", verbosity)
        ] ->
      let text_document = TextDocumentIdentifier.t_of_yojson text_document in
      let cursor_position = Position.t_of_yojson position in
      let verbosity = Some (Yojson.Safe.Util.to_int verbosity) in
      Some { text_document; cursor_position; verbosity }
    | `Assoc [ ("textDocument", text_document); ("position", position) ] ->
      let text_document = TextDocumentIdentifier.t_of_yojson text_document in
      let cursor_position = Position.t_of_yojson position in
      let verbosity = None in
      Some { text_document; cursor_position; verbosity }
    | _ -> None

  let of_jsonrpc_params_exn params : t =
    let params_spec = { Custom_request.params_schema; of_jsonrpc_params } in
    Custom_request.of_jsonrpc_params_exn params_spec params
end

let client_capabilities (state : State.t) =
  (State.initialize_params state).capabilities

(* TODO: duplicated with Hover_req.format_contents *)
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

let hover server (state : State.t) text_document position verbosity =
  let doc =
    Document_store.get state.store text_document.TextDocumentIdentifier.uri
  in
  let pos = Position.logical position in
  let verbosity =
    match verbosity with
    | Some v -> v
    | None -> (
      match state.hover_extended.history with
      | None -> 0
      | Some (h_uri, h_position, h_verbosity) ->
        if
          Uri.equal text_document.uri h_uri
          && Ordering.is_eq (Position.compare position h_position)
        then succ h_verbosity
        else 0)
  in
  state.hover_extended.history <- Some (text_document.uri, position, verbosity);
  (* TODO we shouldn't be acquiring the merlin thread twice per request *)
  let* type_enclosing = Document.type_enclosing ~verbosity doc pos in
  match type_enclosing with
  | None -> Fiber.return None
  | Some { loc; typ; doc = documentation } ->
    let syntax = Document.syntax doc in
    let+ typ =
      (* We ask Ocamlformat to format this type *)
      let* result = Ocamlformat_rpc.format_type state.ocamlformat_rpc ~typ in
      match result with
      | Ok v ->
        (* OCamlformat adds an unnecessay newline at the end of the type *)
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
          ~field:(fun td -> Option.map td.hover ~f:(fun h -> h.contentFormat))
      in
      format_contents ~syntax ~markdown ~typ ~doc:documentation
    in
    let range = Range.of_loc loc in
    Some (Hover.create ~contents ~range ())

let on_request ~(params : Jsonrpc.Structured.t option)
    (server : State.t Server.t) (state : State.t) =
  let { Request_params.text_document; cursor_position; verbosity } =
    Request_params.of_jsonrpc_params_exn params
  in
  let+ hover = hover server state text_document cursor_position verbosity in
  match hover with
  | None -> `Null
  | Some hover -> Hover.yojson_of_t hover

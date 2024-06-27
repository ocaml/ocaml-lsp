open Import

let capability = ("handleTypeEnclosing", `Bool true)

let meth = "ocamllsp/typeEnclosing"

type params =
  { text_document_position : Lsp.Types.TextDocumentPositionParams.t
  ; index : int
  ; range_end : Position.t option
  ; verbosity : int
  }

let expected_params =
  `Assoc
    [ ("index", `String "uinteger")
    ; ("rangeEnd?", `String "<Position>")
    ; ("verbosity?", `String "uinteger")
    ; ("position", `String "<Position>")
    ; ("textDocument", `String "<TextDocumentIdentifier>")
    ]

let index_of_yojson params =
  match List.assoc_opt "index" params with
  | Some (`Int index) -> Some index
  | _ ->
    (* If the parameter is incorrectly formatted or missing, we refuse to build
       the parameter, [index] is mandatory. *)
    None

let verbosity_of_yojson params =
  match List.assoc_opt "verbosity" params with
  | Some (`Int verbosity) -> verbosity
  | _ ->
    (* If the parameter is incorrectly formatted or missing, it is assumed that
       the we ask for a verbosity level set to 0. *)
    0

let range_end_of_yojson params =
  match List.assoc_opt "rangeEnd" params with
  | Some range_end -> (
    try Some (Position.t_of_yojson range_end) with _ -> None)
  | _ ->
    (* If the parameter is incorrectly formatted or missing, it is assumed that
       the we do not provide rangeEnd parameter. *)
    None

let raise_invalid_params ?data ~message () =
  let open Jsonrpc.Response.Error in
  raise @@ make ?data ~code:Code.InvalidParams ~message ()

let of_yojson = function
  | `Assoc params as full_params ->
    let verbosity = verbosity_of_yojson params in
    let range_end = range_end_of_yojson params in
    let open Option.O in
    let* index = index_of_yojson params in
    let text_document_position =
      Lsp.Types.TextDocumentPositionParams.t_of_yojson full_params
    in
    Some { index; range_end; verbosity; text_document_position }
  | _ -> None

let of_yojson_exn = function
  | None -> raise_invalid_params ~message:"Expected params but received none" ()
  | Some params -> (
    match of_yojson params with
    | Some params -> params
    | None ->
      let data =
        `Assoc
          [ ("expectedParams", expected_params)
          ; ("receivedParams", (params :> Json.t))
          ]
      in
      raise_invalid_params ~data ~message:"Unexpected params format" ())

let render_result index result =
  let typ, enclosings =
    match result with
    | None -> ("<no information>", [])
    | Some (typ, enclosings) ->
      (typ, List.map ~f:Lsp.Types.Range.yojson_of_t enclosings)
  in
  `Assoc
    [ ("index", `Int index)
    ; ("enclosings", `List enclosings)
    ; ("type", `String typ)
    ]

let config_with_given_verbosity config verbosity =
  let open Mconfig in
  { config with query = { config.query with verbosity } }

let with_pipeline state uri verbosity with_pipeline =
  let doc = Document_store.get state.State.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let open Fiber.O in
    let* config = Document.Merlin.mconfig merlin in
    Document.Merlin.with_configurable_pipeline_exn
      ~config:(config_with_given_verbosity config verbosity)
      merlin
      with_pipeline

let make_enclosing_command position index =
  Query_protocol.Type_enclosing (None, position, Some index)

let get_first_enclosing_index range_end enclosings =
  let rec aux i = function
    | (loc, _, _) :: xs -> (
      let range = Range.of_loc loc in
      match
        ( Position.compare range_end range.start
        , Position.compare range_end range.end_ )
      with
      | Ordering.(Gt, Gt)
      | Ordering.(Eq, Lt)
      | Ordering.(Gt, Eq)
      | Ordering.(Eq, Eq)
      | Ordering.(Gt, Lt) -> Some i
      | _ -> aux (succ i) xs)
    | _ -> None
  in
  aux 0 enclosings

let dispatch_command pipeline command first_index index =
  let rec aux i acc = function
    | (_, `String typ, _) :: _ as enclosings when i = index ->
      Some
        ( typ
        , List.map
            ~f:(fun (loc, _, _) -> Range.of_loc loc)
            (List.rev_append acc enclosings) )
    | curr :: enclosings when i >= first_index ->
      aux (succ i) (curr :: acc) enclosings
    | _ :: enclosings -> aux (succ i) acc enclosings
    | [] -> None
  in
  aux 0 [] (Query_commands.dispatch pipeline command)

let dispatch_with_range_end pipeline position index range_end =
  (* merlin's `type-enclosing` command takes a position and returns a list of
     increasing enclosures around that position. If it is given the [index]
     parameter, it annotates the corresponding enclosing with its type.

     As the request would like to allow the target of an interval, we want to
     truncate the list of enclosures that include the interval. Something merlin
     cannot do.

     We use a little hack where we use the `type-enclosing` command (with a
     negative index, so as not to make unnecessary computations) to calculate
     the enclosings around the given position. Then, we look for the index
     corresponding to the first enclosing included in the range which will act
     as an offset to calculate the real index, relative to the range *)
  let dummy_command = make_enclosing_command position (-1) in
  let enclosings = Query_commands.dispatch pipeline dummy_command in
  Option.bind
    (get_first_enclosing_index range_end enclosings)
    ~f:(fun first_index ->
      let real_index = first_index + index in
      let command = make_enclosing_command position real_index in
      dispatch_command pipeline command first_index real_index)

let dispatch_without_range_end pipeline position index =
  let command = make_enclosing_command position index in
  dispatch_command pipeline command 0 index

let dispatch_type_enclosing text_document_position index range_end pipeline =
  let position =
    Position.logical
      text_document_position.Lsp.Types.TextDocumentPositionParams.position
  in
  match range_end with
  | None -> dispatch_without_range_end pipeline position index
  | Some range_end -> dispatch_with_range_end pipeline position index range_end

let on_request ~params state =
  Fiber.of_thunk (fun () ->
      let open Fiber.O in
      let { index; verbosity; text_document_position; range_end; _ } =
        of_yojson_exn params
      in
      let uri = text_document_position.textDocument.uri in
      let verbosity = Mconfig.Verbosity.Lvl verbosity in
      let+ result =
        with_pipeline state uri verbosity
        @@ dispatch_type_enclosing text_document_position index range_end
      in
      render_result index result)

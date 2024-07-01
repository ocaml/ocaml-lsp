open Import
module TextDocumentPositionParams = Lsp.Types.TextDocumentPositionParams

let capability = ("handleTypeEnclosing", `Bool true)

let meth = "ocamllsp/typeEnclosing"

module Request_params = struct
  type t =
    { text_document_position : TextDocumentPositionParams.t
    ; index : int
    ; range_end : Position.t option
    ; verbosity : int
    }

  let yojson_of_t { text_document_position; index; range_end; verbosity } =
    match TextDocumentPositionParams.yojson_of_t text_document_position with
    | `Assoc assoc ->
      let index = ("index", `Int index) in
      let range_end =
        ( "rangeEnd"
        , match range_end with
          | Some x -> Position.yojson_of_t x
          | None -> `Null )
      in
      let verbosity = ("verbosity", `Int verbosity) in
      `Assoc (index :: range_end :: verbosity :: assoc)
    | _ -> (* unreachable  *) assert false

  let create ?range_end ?(verbosity = 0) ~text_document_position ~index () =
    { text_document_position; index; range_end; verbosity }

  let json_error json =
    Json.error "invalid Req_type_enclosing.Request_params" json

  let index_of_yojson json params =
    match List.assoc_opt "index" params with
    | Some (`Int index) -> index
    | _ ->
      (* If the parameter is incorrectly formatted or missing, we refuse to build
         the parameter, [index] is mandatory. *)
      json_error json

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

  let t_of_yojson = function
    | `Assoc params as json ->
      let verbosity = verbosity_of_yojson params in
      let range_end = range_end_of_yojson params in
      let index = index_of_yojson json params in
      let text_document_position =
        TextDocumentPositionParams.t_of_yojson json
      in
      { index; range_end; verbosity; text_document_position }
    | json -> json_error json
end

type t =
  { index : int
  ; type_ : string
  ; enclosings : Range.t list
  }

let yojson_of_t { index; type_; enclosings } =
  `Assoc
    [ ("index", `Int index)
    ; ("enclosings", `List (List.map ~f:Range.yojson_of_t enclosings))
    ; ("type", `String type_)
    ]

let config_with_given_verbosity config verbosity =
  let open Mconfig in
  { config with query = { config.query with verbosity } }

let with_pipeline state uri verbosity with_pipeline =
  let doc = Document_store.get state.State.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return `Null
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
  List.find_mapi
    ~f:(fun i (loc, _, _) ->
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
      | Ordering.Lt, Ordering.Lt
      | Ordering.Lt, Ordering.Eq
      | Ordering.Lt, Ordering.Gt
      | Ordering.Eq, Ordering.Gt -> None)
    enclosings

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

let dispatch_type_enclosing
    (text_document_position : TextDocumentPositionParams.t) index range_end
    pipeline =
  let position = Position.logical text_document_position.position in
  let result =
    match range_end with
    | None -> dispatch_without_range_end pipeline position index
    | Some range_end ->
      dispatch_with_range_end pipeline position index range_end
  in
  let type_, enclosings =
    match result with
    | None -> ("<no information>", [])
    | Some (typ, enclosings) -> (typ, enclosings)
  in
  yojson_of_t { index; type_; enclosings }

let on_request ~params state =
  Fiber.of_thunk (fun () ->
      let params = (Option.value ~default:(`Assoc []) params :> Json.t) in
      let Request_params.
            { index; verbosity; text_document_position; range_end; _ } =
        Request_params.t_of_yojson params
      in
      let uri = text_document_position.textDocument.uri in
      let verbosity = Mconfig.Verbosity.Lvl verbosity in
      with_pipeline state uri verbosity
      @@ dispatch_type_enclosing text_document_position index range_end)

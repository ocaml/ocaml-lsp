open Import

let capability = "handleTypeEnclosing", `Bool true
let meth = "ocamllsp/typeEnclosing"

module Request_params = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; at : [ `Range of Range.t | `Position of Position.t ]
    ; index : int
    ; verbosity : int
    }

  let yojson_of_at = function
    | `Range r -> Range.yojson_of_t r
    | `Position p -> Position.yojson_of_t p
  ;;

  let yojson_of_t { text_document; index; at; verbosity } =
    match TextDocumentIdentifier.yojson_of_t text_document with
    | `Assoc assoc ->
      let index = "index", `Int index in
      let range_end = "at", yojson_of_at at in
      let verbosity = "verbosity", `Int verbosity in
      `Assoc (index :: range_end :: verbosity :: assoc)
    | _ -> (* unreachable *) assert false
  ;;

  let create ?(verbosity = 0) ~text_document ~at ~index () =
    { text_document; index; at; verbosity }
  ;;

  let at_of_yojson json =
    let open Yojson.Safe.Util in
    let at = json |> member "at" in
    try `Position (Position.t_of_yojson at) with
    | _ -> `Range (Range.t_of_yojson at)
  ;;

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let verbosity =
      json |> member "verbosity" |> to_int_option |> Option.value ~default:0
    in
    let at = at_of_yojson json in
    let index = json |> member "index" |> to_int in
    let text_document = TextDocumentIdentifier.t_of_yojson json in
    { index; at; verbosity; text_document }
  ;;
end

type t =
  { index : int
  ; type_ : string
  ; enclosings : Range.t list
  }

let t_of_yojson json =
  let open Yojson.Safe.Util in
  let index = json |> member "index" |> to_int in
  let type_ = json |> member "type" |> to_string in
  let enclosings =
    json |> member "enclosings" |> to_list |> List.map ~f:Range.t_of_yojson
  in
  { index; type_; enclosings }
;;

let yojson_of_t { index; type_; enclosings } =
  `Assoc
    [ "index", `Int index
    ; "enclosings", `List (List.map ~f:Range.yojson_of_t enclosings)
    ; "type", `String type_
    ]
;;

let config_with_given_verbosity config verbosity =
  let open Mconfig in
  { config with query = { config.query with verbosity } }
;;

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
;;

let make_enclosing_command position index =
  Query_protocol.Type_enclosing (None, position, Some index)
;;

let get_first_enclosing_index range_end enclosings =
  List.find_mapi enclosings ~f:(fun i (loc, _, _) ->
    let range = Range.of_loc loc in
    match Position.compare range_end range.end_ with
    | Ordering.Lt | Ordering.Eq -> Some i
    | Ordering.Gt -> None)
;;

let dispatch_command pipeline command first_index index =
  let rec aux i acc = function
    | (_, `String typ, _) :: _ as enclosings when i = index ->
      Some
        ( typ
        , List.map
            ~f:(fun (loc, _, _) -> Range.of_loc loc)
            (List.rev_append acc enclosings) )
    | curr :: enclosings -> aux (succ i) (curr :: acc) enclosings
    | [] -> None
  in
  let result = List.drop (Query_commands.dispatch pipeline command) first_index in
  aux 0 [] result
;;

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
  Option.bind (get_first_enclosing_index range_end enclosings) ~f:(fun first_index ->
    let real_index = first_index + index in
    let command = make_enclosing_command position real_index in
    dispatch_command pipeline command first_index index)
;;

let dispatch_without_range_end pipeline position index =
  let command = make_enclosing_command position index in
  dispatch_command pipeline command 0 index
;;

let dispatch_type_enclosing position index range_end pipeline =
  let position = Position.logical position in
  let result =
    match range_end with
    | None -> dispatch_without_range_end pipeline position index
    | Some range_end -> dispatch_with_range_end pipeline position index range_end
  in
  let type_, enclosings =
    match result with
    | None -> "<no information>", []
    | Some (typ, enclosings) -> typ, enclosings
  in
  yojson_of_t { index; type_; enclosings }
;;

let on_request ~params state =
  Fiber.of_thunk (fun () ->
    let params = (Option.value ~default:(`Assoc []) params :> Json.t) in
    let Request_params.{ index; verbosity; text_document; at } =
      Request_params.t_of_yojson params
    in
    let position, range_end =
      match at with
      | `Position p -> p, None
      | `Range r -> r.start, Some r.end_
    in
    let uri = text_document.uri in
    let verbosity = Mconfig.Verbosity.Lvl verbosity in
    with_pipeline state uri verbosity @@ dispatch_type_enclosing position index range_end)
;;

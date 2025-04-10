open Import

let capability = "handleJumpTypedHole", `Bool true
let meth = "ocamllsp/jumpTypedHole"

module Request_params = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; position : Position.t
    ; range : Range.t option
    ; direction : [ `Next | `Prev ]
    }

  let create ?(direction = `Next) ?range ~text_document ~position () =
    { text_document; position; direction; range }
  ;;

  let yojson_of_direction = function
    | `Next -> `String "next"
    | `Prev -> `String "prev"
  ;;

  let yojson_of_t { text_document; position; direction; range } =
    match TextDocumentIdentifier.yojson_of_t text_document with
    | `Assoc assoc ->
      let position = "position", Position.yojson_of_t position in
      let range =
        ( "range"
        , match range with
          | None -> `Null
          | Some r -> Range.yojson_of_t r )
      in
      let direction = "direction", yojson_of_direction direction in
      `Assoc (direction :: position :: range :: assoc)
    | _ -> (* unreachable *) assert false
  ;;

  let direction_of_yojson json =
    let open Yojson.Safe.Util in
    let dir = json |> member "direction" |> to_string in
    match String.lowercase_ascii dir with
    | "prev" -> `Prev
    | _ -> `Next
  ;;

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let text_document = TextDocumentIdentifier.t_of_yojson json in
    let position = json |> member "position" |> Position.t_of_yojson in
    let direction = direction_of_yojson json in
    let range = json |> member "range" |> to_option Range.t_of_yojson in
    { text_document; position; direction; range }
  ;;
end

type t = Range.t option

let t_of_yojson opt =
  let open Yojson.Safe.Util in
  to_option Range.t_of_yojson opt
;;

let yojson_of_t = function
  | None -> `Null
  | Some range -> Range.yojson_of_t range
;;

let holes_in_range range holes =
  match range with
  | None -> holes
  | Some range -> List.filter ~f:(Range.contains range) holes
;;

let find_prev_hole ~range ~position holes =
  let holes = holes_in_range range holes in
  Base.List.fold_until
    ~init:None
    ~f:(fun prev hole ->
      match Position.compare hole.end_ position with
      | Lt -> Continue (Some hole)
      | Gt | Eq -> Stop prev)
    ~finish:Fun.id
    holes
  |> function
  | None -> Base.List.last holes
  | hole -> hole
;;

let find_next_hole ~range ~position holes =
  let holes = holes_in_range range holes in
  List.find
    ~f:(fun hole ->
      match Position.compare hole.start position with
      | Gt -> true
      | Lt | Eq -> false)
    holes
  |> function
  | None -> Base.List.hd holes
  | hole -> hole
;;

let find_hole ~range ~position ~direction holes =
  match direction with
  | `Prev -> find_prev_hole ~range ~position holes
  | `Next -> find_next_hole ~range ~position holes
;;

let on_request ~(params : Jsonrpc.Structured.t option) (state : State.t) =
  Fiber.of_thunk (fun () ->
    let params = (Option.value ~default:(`Assoc []) params :> Json.t) in
    let Request_params.{ text_document = { uri }; position; direction; range } =
      Request_params.t_of_yojson params
    in
    match Document_store.get_opt state.store uri with
    | Some doc ->
      let open Fiber.O in
      let+ holes =
        Document.Merlin.dispatch_exn
          ~name:"jump-typed-hole"
          (Document.merlin_exn doc)
          Holes
      in
      holes
      |> List.map ~f:(fun (loc, _ty) -> Range.of_loc loc)
      |> find_hole ~position ~range ~direction
      |> yojson_of_t
    | None ->
      Jsonrpc.Response.Error.raise
      @@ Jsonrpc.Response.Error.make
           ~code:Jsonrpc.Response.Error.Code.InvalidParams
           ~message:
             (Printf.sprintf
                "Document %s wasn't found in the document store"
                (Uri.to_string uri))
           ())
;;

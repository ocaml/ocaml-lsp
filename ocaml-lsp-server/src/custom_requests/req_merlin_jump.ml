open Import
module TextDocumentPositionParams = Lsp.Types.TextDocumentPositionParams

let meth = "ocamllsp/jump"
let capability = "handleJump", `Bool true

module JumpParams = struct
  let targets =
    [ "fun"
    ; "match"
    ; "let"
    ; "module"
    ; "module-type"
    ; "match-next-case"
    ; "match-prev-case"
    ]
  ;;

  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    { textDocument = json |> member "textDocument" |> TextDocumentIdentifier.t_of_yojson
    ; position = json |> member "position" |> Position.t_of_yojson
    }
  ;;

  let yojson_of_t { textDocument; position } =
    `Assoc
      [ "textDocument", TextDocumentIdentifier.yojson_of_t textDocument
      ; "position", Position.yojson_of_t position
      ]
  ;;
end

module Jump = struct
  type t = (string * Position.t) list

  let yojson_of_t (lst : t) : Yojson.Safe.t option =
    if List.is_empty lst
    then None
    else
      Some
        (`Assoc
          [ ( "jumps"
            , `List
                (List.map
                   ~f:(fun (target, position) ->
                     `Assoc
                       [ "target", `String target
                       ; "position", Position.yojson_of_t position
                       ])
                   lst) )
          ])
  ;;
end

type t = Jump.t

module Request_params = struct
  type t = JumpParams.t

  let yojson_of_t t = JumpParams.yojson_of_t t

  let create ~uri ~position =
    { JumpParams.textDocument = TextDocumentIdentifier.create ~uri; position }
  ;;
end

let dispatch ~merlin ~position ~target =
  Document.Merlin.with_pipeline_exn merlin (fun pipeline ->
    let pposition = Position.logical position in
    let query = Query_protocol.Jump (target, pposition) in
    Query_commands.dispatch pipeline query)
;;

let on_request ~params state =
  Fiber.of_thunk (fun () ->
    let params = (Option.value ~default:(`Assoc []) params :> Yojson.Safe.t) in
    let params = JumpParams.t_of_yojson params in
    let uri = params.textDocument.uri in
    let position = params.position in
    let doc = Document_store.get state.State.store uri in
    let targets = JumpParams.targets in
    match Document.kind doc with
    | `Other -> Fiber.return `Null
    | `Merlin merlin ->
      Fiber.map
        (Fiber.parallel_map targets ~f:(fun target ->
           dispatch ~merlin ~position ~target
           |> Fiber.map ~f:(function
             | `Error _ -> None
             | `Found pos ->
               (match Position.of_lexical_position pos with
                | None -> None
                | Some position -> Some (target, position)))))
        ~f:(fun results ->
          match List.filter_map results ~f:Fun.id with
          | [] -> `Null
          | lst -> Jump.yojson_of_t lst |> Option.value ~default:`Null))
;;

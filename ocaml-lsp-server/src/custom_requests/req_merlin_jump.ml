open Import
module TextDocumentPositionParams = Lsp.Types.TextDocumentPositionParams

let meth = "ocamllsp/jump"
let capability = "handleJump", `Bool true

module JumpParams = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; position : Position.t
    ; target : string
    }

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let textDocumentPosition = TextDocumentPositionParams.t_of_yojson json in
    let target = json |> member "target" |> to_string in
    { position = textDocumentPosition.position
    ; text_document = textDocumentPosition.textDocument
    ; target
    }
  ;;

  let yojson_of_t { text_document; position; target } =
    `Assoc
      [ "textDocument", TextDocumentIdentifier.yojson_of_t text_document
      ; "position", Position.yojson_of_t position
      ; "target", `String target
      ]
  ;;
end

module Jump = struct
  type t = Lsp.Types.TextDocumentPositionParams.t

  let yojson_of_t t = TextDocumentPositionParams.yojson_of_t t
end

type t = Jump.t

module Request_params = struct
  type t = JumpParams.t

  let yojson_of_t t = JumpParams.yojson_of_t t
  let create ~text_document ~position ~target () : t = { text_document; position; target }
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
    let JumpParams.{ text_document; position; target } = JumpParams.t_of_yojson params in
    let uri = text_document.uri in
    let doc = Document_store.get state.State.store uri in
    match Document.kind doc with
    | `Other -> Fiber.return `Null
    | `Merlin merlin ->
      Fiber.map (dispatch ~merlin ~position ~target) ~f:(fun res ->
        match res with
        | `Error err -> `String err
        | `Found pos ->
          (match Position.of_lexical_position pos with
           | None -> `Null
           | Some position ->
             let loc =
               Lsp.Types.TextDocumentPositionParams.create
                 ~position
                 ~textDocument:(TextDocumentIdentifier.create ~uri)
             in
             Jump.yojson_of_t loc)))
;;

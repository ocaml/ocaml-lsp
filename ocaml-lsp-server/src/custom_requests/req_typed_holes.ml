open Import
open Fiber.O

let capability = "handleTypedHoles", `Bool true
let meth = "ocamllsp/typedHoles"

type t = Range.t list

let yojson_of_t holes =
  Json.yojson_of_list (fun (loc, _type) -> loc |> Range.of_loc |> Range.yojson_of_t) holes
;;

let t_of_yojson list =
  let open Yojson.Safe.Util in
  list |> to_list |> List.map ~f:(fun range -> range |> Range.t_of_yojson)
;;

let on_request ~(params : Jsonrpc.Structured.t option) (state : State.t) =
  Fiber.of_thunk (fun () ->
    let uri = Request_uri_params.parse_exn params in
    let store = state.store in
    let doc = Document_store.get_opt store uri in
    match doc with
    | None ->
      Jsonrpc.Response.Error.raise
      @@ Jsonrpc.Response.Error.make
           ~code:Jsonrpc.Response.Error.Code.InvalidParams
           ~message:
             (Printf.sprintf
                "Document %s wasn't found in the document store"
                (Uri.to_string uri))
           ()
    | Some doc ->
      let+ holes =
        Document.Merlin.dispatch_exn ~name:"typed-holes" (Document.merlin_exn doc) Holes
      in
      yojson_of_t holes)
;;

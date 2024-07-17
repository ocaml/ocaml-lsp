open Import
open Fiber.O

let capability = "handleTypedHoles", `Bool true
let meth = "ocamllsp/typedHoles"

module Request_params = struct
  type t = Uri.t

  (* Request params must have the form as in the given string. *)
  let expected_params = `Assoc [ "uri", `String "<DocumentUri>" ]
  let create uri = uri

  let t_of_structured_json params : t option =
    match params with
    | `Assoc [ ("uri", uri) ] ->
      let uri = Uri.t_of_yojson uri in
      Some uri
    | _ -> None
  ;;

  let parse_exn (params : Jsonrpc.Structured.t option) : t =
    let raise_invalid_params ?data ~message () =
      Jsonrpc.Response.Error.raise
      @@ Jsonrpc.Response.Error.make
           ?data
           ~code:Jsonrpc.Response.Error.Code.InvalidParams
           ~message
           ()
    in
    match params with
    | None -> raise_invalid_params ~message:"Expected params but received none" ()
    | Some params ->
      (match t_of_structured_json params with
       | Some uri -> uri
       | None ->
         let error_json =
           `Assoc
             [ "params_expected", expected_params; "params_received", (params :> Json.t) ]
         in
         raise_invalid_params ~message:"Unxpected parameter format" ~data:error_json ())
  ;;

  let yojson_of_t = Uri.yojson_of_t
end

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
    let uri = Request_params.parse_exn params in
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

open Import

type t = Uri.t

(* Request params must have the form as in the given string. *)
let expected_params = `Assoc [ "uri", `String "<DocumentUri>" ]

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
       raise_invalid_params ~message:"Unexpected parameter format" ~data:error_json ())
;;

let yojson_of_t = Uri.yojson_of_t

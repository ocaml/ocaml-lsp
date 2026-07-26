open Base
open Base_quickcheck

type text =
  | Empty
  | Ascii
  | Quote
  | Two_byte
  | Four_byte
  | Newline
[@@deriving quickcheck, sexp_of]

type scalar =
  | Null
  | Bool of bool
  | Int of int
  | String of text
[@@deriving quickcheck, sexp_of]

type id =
  | Int_id of int
  | String_id of text
[@@deriving quickcheck, sexp_of]

type parameters =
  | No_params
  | Array_params of scalar list
  | Object_params of (text * scalar) list
[@@deriving quickcheck, sexp_of]

type call =
  | Request of id * text * parameters
  | Notification of text * parameters
[@@deriving quickcheck, sexp_of]

type error_code =
  | Invalid_request
  | Invalid_params
  | Internal_error
  | Custom_error
[@@deriving quickcheck, sexp_of]

type response =
  | Result of id * scalar
  | Error of id * error_code * text * scalar option
[@@deriving quickcheck, sexp_of]

type packet =
  | Call of call
  | Response of response
  | Batch_call of call * call list
  | Batch_response of response * response list
[@@deriving quickcheck, sexp_of]

let string_of_text = function
  | Empty -> ""
  | Ascii -> "method/value"
  | Quote -> "quote: \" and slash: \\"
  | Two_byte -> "café"
  | Four_byte -> "😀"
  | Newline -> "first\nsecond"
;;

let json_of_scalar = function
  | Null -> `Null
  | Bool value -> `Bool value
  | Int value -> `Int value
  | String value -> `String (string_of_text value)
;;

let id = function
  | Int_id value -> `Int value
  | String_id value -> `String (string_of_text value)
;;

let parameters = function
  | No_params -> None
  | Array_params values -> Some (`List (List.map values ~f:json_of_scalar))
  | Object_params fields ->
    Some
      (`Assoc
          (List.map fields ~f:(fun (name, value) ->
             string_of_text name, json_of_scalar value)))
;;

let call = function
  | Request (request_id, method_, params) ->
    `Request
      (Jsonrpc.Request.create
         ~id:(id request_id)
         ~method_:(string_of_text method_)
         ?params:(parameters params)
         ())
  | Notification (method_, params) ->
    `Notification
      (Jsonrpc.Notification.create
         ~method_:(string_of_text method_)
         ?params:(parameters params)
         ())
;;

let error_code = function
  | Invalid_request -> Jsonrpc.Response.Error.Code.InvalidRequest
  | Invalid_params -> InvalidParams
  | Internal_error -> InternalError
  | Custom_error -> Other 42
;;

let response = function
  | Result (response_id, value) ->
    Jsonrpc.Response.ok (id response_id) (json_of_scalar value)
  | Error (response_id, code, message, data) ->
    let error =
      Jsonrpc.Response.Error.make
        ~code:(error_code code)
        ~message:(string_of_text message)
        ?data:(Option.map data ~f:json_of_scalar)
        ()
    in
    Jsonrpc.Response.error (id response_id) error
;;

let packet = function
  | Call call_model ->
    (match call call_model with
     | `Request request -> Jsonrpc.Packet.Request request
     | `Notification notification -> Notification notification)
  | Response response_model -> Jsonrpc.Packet.Response (response response_model)
  | Batch_call (head, tail) -> Jsonrpc.Packet.Batch_call (List.map (head :: tail) ~f:call)
  | Batch_response (head, tail) ->
    Jsonrpc.Packet.Batch_response (List.map (head :: tail) ~f:response)
;;

module Case = struct
  type t = packet [@@deriving quickcheck, sexp_of]
end

let%expect_test "generated JSON-RPC packets round trip through JSON" =
  Test.run_exn
    (module Case)
    ~f:(fun model ->
      let packet = packet model in
      let json = Jsonrpc.Packet.yojson_of_t packet in
      let reparsed = Jsonrpc.Packet.t_of_yojson json in
      if not (Poly.equal packet reparsed)
      then
        failwith
          (Printf.sprintf
             "packet did not round trip:\n%s\n%s"
             (Sexp.to_string_hum (Case.sexp_of_t model))
             (Yojson.Safe.pretty_to_string ~std:false json)));
  [%expect {| |}]
;;

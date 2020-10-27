open Import
open Json.Conv

module Id = struct
  type t = (string, int) Either.t

  let yojson_of_t = function
    | Either.Left s -> `String s
    | Right i -> `Int i

  let t_of_yojson = function
    | `String s -> Either.Left s
    | `Int i -> Right i
    | json -> Json.error "Id.t" json

  let to_dyn x = Dyn.Encoder.opaque x

  let hash x = Hashtbl.hash x

  let equal = ( = )
end

module Constant = struct
  let jsonrpc = "jsonrpc"

  let jsonrpcv = "2.0"

  let id = "id"

  let method_ = "method"

  let params = "params"

  let result = "result"

  let error = "error"
end

module Message = struct
  type 'id t =
    { id : 'id
    ; method_ : string
    ; params : Json.t option
    }

  let create ?params ~id ~method_ () = { id; method_; params }

  let yojson_of_t add_id { id; method_; params } =
    let json =
      [ (Constant.method_, `String method_)
      ; (Constant.jsonrpc, `String Constant.jsonrpcv)
      ]
    in
    let json =
      match params with
      | None -> json
      | Some params -> (Constant.params, params) :: json
    in
    let json =
      match add_id id with
      | None -> json
      | Some id -> (Constant.id, id) :: json
    in
    `Assoc json

  let either_of_yojson json =
    match json with
    | `Assoc fields ->
      let method_ =
        Json.field_exn fields Constant.method_ Json.Conv.string_of_yojson
      in
      let params = Json.field fields Constant.params (fun x -> x) in
      let id = Json.field fields Constant.id Id.t_of_yojson in
      let jsonrpc =
        Json.field_exn fields Constant.jsonrpc Json.Conv.string_of_yojson
      in
      if jsonrpc = Constant.jsonrpcv then
        { method_; params; id }
      else
        Json.error "invalid version" json
    | _ -> Json.error "invalid request" json

  let read_json_params f v =
    match f v with
    | r -> Ok r
    | exception Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (Failure msg, _)
      ->
      Error msg

  let require_params json =
    match json with
    | None -> Error "params are required"
    | Some params -> Ok params

  let params t f =
    match require_params t.params with
    | Error e -> Error e
    | Ok x -> read_json_params f x

  let yojson_of_either t : Json.t = yojson_of_t (Option.map ~f:Id.yojson_of_t) t

  type request = Id.t t

  type notification = unit t

  type either = Id.t option t

  let yojson_of_notification = yojson_of_t (fun () -> None)

  let yojson_of_request (t : request) : Json.t =
    yojson_of_t (fun id -> Some (Id.yojson_of_t id)) t
end

module Response = struct
  module Error = struct
    module Code = struct
      type t =
        | ParseError
        | InvalidRequest
        | MethodNotFound
        | InvalidParams
        | InternalError
        | ServerErrorStart
        | ServerErrorEnd
        | ServerNotInitialized
        | UnknownErrorCode
        | RequestCancelled
        | ContentModified

      let of_int = function
        | -32700 -> Some ParseError
        | -32600 -> Some InvalidRequest
        | -32601 -> Some MethodNotFound
        | -32602 -> Some InvalidParams
        | -32603 -> Some InternalError
        | -32099 -> Some ServerErrorStart
        | -32000 -> Some ServerErrorEnd
        | -32002 -> Some ServerNotInitialized
        | -32001 -> Some UnknownErrorCode
        | -32800 -> Some RequestCancelled
        | -32801 -> Some ContentModified
        | _ -> None

      let to_int = function
        | ParseError -> -32700
        | InvalidRequest -> -32600
        | MethodNotFound -> -32601
        | InvalidParams -> -32602
        | InternalError -> -32603
        | ServerErrorStart -> -32099
        | ServerErrorEnd -> -32000
        | ServerNotInitialized -> -32002
        | UnknownErrorCode -> -32001
        | RequestCancelled -> -32800
        | ContentModified -> -32801

      let t_of_yojson json =
        match json with
        | `Int i -> (
          match of_int i with
          | None -> Json.error "unknown code" json
          | Some i -> i )
        | _ -> Json.error "invalid code" json

      let yojson_of_t t = `Int (to_int t)
    end

    type t =
      { code : Code.t
      ; message : string
      ; data : Json.t option
      }

    let yojson_of_t { code; message; data } =
      let assoc =
        [ ("code", Code.yojson_of_t code); ("message", `String message) ]
      in
      let assoc =
        match data with
        | None -> assoc
        | Some data -> ("data", data) :: assoc
      in
      `Assoc assoc

    let t_of_yojson json =
      match json with
      | `Assoc fields ->
        let code = Json.field_exn fields "code" Code.t_of_yojson in
        let message = Json.field_exn fields "message" string_of_yojson in
        let data = Json.field fields "data" (fun x -> x) in
        { code; message; data }
      | _ -> Json.error "Jsonrpc.Response.t" json

    exception E of t

    let raise t = raise (E t)

    let make ?data ~code ~message () = { data; code; message }

    let of_exn exn =
      let message = Printexc.to_string exn in
      make ~code:InternalError ~message ()
  end

  type t =
    { id : Id.t
    ; result : (Json.t, Error.t) Result.t
    }

  let yojson_of_t { id; result } =
    let result =
      match result with
      | Ok json -> (Constant.result, json)
      | Error e -> (Constant.error, Error.yojson_of_t e)
    in
    `Assoc
      [ (Constant.id, Id.yojson_of_t id)
      ; (Constant.jsonrpc, `String Constant.jsonrpcv)
      ; result
      ]

  let t_of_yojson json =
    match json with
    | `Assoc fields -> (
      let id = Json.field_exn fields Constant.id Id.t_of_yojson in
      let jsonrpc =
        Json.field_exn fields Constant.jsonrpc Json.Conv.string_of_yojson
      in
      if jsonrpc <> Constant.jsonrpcv then
        Json.error "Invalid response" json
      else
        match Json.field fields Constant.result (fun x -> x) with
        | Some res -> { id; result = Ok res }
        | None ->
          let result =
            Error (Json.field_exn fields Constant.error Error.t_of_yojson)
          in
          { id; result } )
    | _ -> Json.error "Jsonrpc.Result.t" json

  let make ~id ~result = { id; result }

  let ok id result = make ~id ~result:(Ok result)

  let error id error = make ~id ~result:(Error error)
end

type packet =
  | Message of Id.t option Message.t
  | Response of Response.t

let yojson_of_packet = function
  | Message r -> Message.yojson_of_either r
  | Response r -> Response.yojson_of_t r

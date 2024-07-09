open Import
open Json.Conv
module Json = Json

module Id = struct
  type t =
    [ `String of string
    | `Int of int
    ]

  let yojson_of_t = function
    | `String s -> `String s
    | `Int i -> `Int i
  ;;

  let t_of_yojson = function
    | `String s -> `String s
    | `Int i -> `Int i
    | json -> Json.error "Id.t" json
  ;;

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

let assert_jsonrpc_version fields =
  let jsonrpc = Json.field_exn fields Constant.jsonrpc Json.Conv.string_of_yojson in
  if not (String.equal jsonrpc Constant.jsonrpcv)
  then
    Json.error
      ("invalid packet: jsonrpc version doesn't match " ^ jsonrpc)
      (`Assoc fields)
;;

module Structured = struct
  type t =
    [ `Assoc of (string * Json.t) list
    | `List of Json.t list
    ]

  let t_of_yojson = function
    | `Assoc xs -> `Assoc xs
    | `List xs -> `List xs
    | json -> Json.error "invalid structured value" json
  ;;

  let yojson_of_t t = (t :> Json.t)
end

module Notification = struct
  type t =
    { method_ : string
    ; params : Structured.t option
    }

  let fields ~method_ ~params =
    let json =
      [ Constant.method_, `String method_; Constant.jsonrpc, `String Constant.jsonrpcv ]
    in
    match params with
    | None -> json
    | Some params -> (Constant.params, (params :> Json.t)) :: json
  ;;

  let yojson_of_t { method_; params } = `Assoc (fields ~method_ ~params)
  let create ?params ~method_ () = { params; method_ }
end

module Request = struct
  type t =
    { id : Id.t
    ; method_ : string
    ; params : Structured.t option
    }

  let yojson_of_t { id; method_; params } =
    let fields = Notification.fields ~method_ ~params in
    `Assoc ((Constant.id, Id.yojson_of_t id) :: fields)
  ;;

  let create ?params ~id ~method_ () = { params; id; method_ }
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
        (* the codes below are LSP specific *)
        | ServerErrorStart
        | ServerErrorEnd
        | ServerNotInitialized
        | UnknownErrorCode
        | RequestFailed
        | ServerCancelled
        | ContentModified
        | RequestCancelled
        (* all other codes are custom *)
        | Other of int

      let of_int = function
        | -32700 -> ParseError
        | -32600 -> InvalidRequest
        | -32601 -> MethodNotFound
        | -32602 -> InvalidParams
        | -32603 -> InternalError
        | -32099 -> ServerErrorStart
        | -32000 -> ServerErrorEnd
        | -32002 -> ServerNotInitialized
        | -32001 -> UnknownErrorCode
        | -32800 -> RequestCancelled
        | -32801 -> ContentModified
        | -32802 -> ServerCancelled
        | -32803 -> RequestFailed
        | code -> Other code
      ;;

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
        | ServerCancelled -> -32802
        | RequestFailed -> -32803
        | Other code -> code
      ;;

      let t_of_yojson json =
        match json with
        | `Int i -> of_int i
        | _ -> Json.error "invalid code" json
      ;;

      let yojson_of_t t = `Int (to_int t)
    end

    type t =
      { code : Code.t
      ; message : string
      ; data : Json.t option
      }

    let yojson_of_t { code; message; data } =
      let assoc = [ "code", Code.yojson_of_t code; "message", `String message ] in
      let assoc =
        match data with
        | None -> assoc
        | Some data -> ("data", data) :: assoc
      in
      `Assoc assoc
    ;;

    let t_of_yojson json =
      match json with
      | `Assoc fields ->
        let code = Json.field_exn fields "code" Code.t_of_yojson in
        let message = Json.field_exn fields "message" string_of_yojson in
        let data = Json.field fields "data" (fun x -> x) in
        { code; message; data }
      | _ -> Json.error "Jsonrpc.Response.t" json
    ;;

    exception E of t

    let raise t = raise (E t)
    let make ?data ~code ~message () = { data; code; message }

    let of_exn exn =
      let message = Printexc.to_string exn in
      make ~code:InternalError ~message ()
    ;;
  end

  type t =
    { id : Id.t
    ; result : (Json.t, Error.t) Result.t
    }

  let yojson_of_t { id; result } =
    let result =
      match result with
      | Ok json -> Constant.result, json
      | Error e -> Constant.error, Error.yojson_of_t e
    in
    `Assoc
      [ Constant.id, Id.yojson_of_t id
      ; Constant.jsonrpc, `String Constant.jsonrpcv
      ; result
      ]
  ;;

  let t_of_yojson json =
    match json with
    | `Assoc fields ->
      let id = Json.field_exn fields Constant.id Id.t_of_yojson in
      let jsonrpc = Json.field_exn fields Constant.jsonrpc Json.Conv.string_of_yojson in
      if jsonrpc <> Constant.jsonrpcv
      then Json.error "Invalid response" json
      else (
        match Json.field fields Constant.result (fun x -> x) with
        | Some res -> { id; result = Ok res }
        | None ->
          let result = Error (Json.field_exn fields Constant.error Error.t_of_yojson) in
          { id; result })
    | _ -> Json.error "Jsonrpc.Result.t" json
  ;;

  let make ~id ~result = { id; result }
  let ok id result = make ~id ~result:(Ok result)
  let error id error = make ~id ~result:(Error error)
end

module Packet = struct
  type t =
    | Notification of Notification.t
    | Request of Request.t
    | Response of Response.t
    | Batch_response of Response.t list
    | Batch_call of [ `Request of Request.t | `Notification of Notification.t ] list

  let yojson_of_t = function
    | Notification r -> Notification.yojson_of_t r
    | Request r -> Request.yojson_of_t r
    | Response r -> Response.yojson_of_t r
    | Batch_response r -> `List (List.map r ~f:Response.yojson_of_t)
    | Batch_call r ->
      `List
        (List.map r ~f:(function
          | `Request r -> Request.yojson_of_t r
          | `Notification r -> Notification.yojson_of_t r))
  ;;

  let t_of_fields (fields : (string * Json.t) list) =
    assert_jsonrpc_version fields;
    match Json.field fields Constant.id Id.t_of_yojson with
    | None ->
      let method_ = Json.field_exn fields Constant.method_ Json.Conv.string_of_yojson in
      let params = Json.field fields Constant.params Structured.t_of_yojson in
      Notification { Notification.params; method_ }
    | Some id ->
      (match Json.field fields Constant.method_ Json.Conv.string_of_yojson with
       | Some method_ ->
         let params = Json.field fields Constant.params Structured.t_of_yojson in
         Request { Request.method_; params; id }
       | None ->
         Response
           (match Json.field fields Constant.result (fun x -> x) with
            | Some result -> { Response.id; result = Ok result }
            | None ->
              let error =
                Json.field_exn fields Constant.error Response.Error.t_of_yojson
              in
              { id; result = Error error }))
  ;;

  let t_of_yojson_single json =
    match json with
    | `Assoc fields -> t_of_fields fields
    | _ -> Json.error "invalid packet" json
  ;;

  let t_of_yojson (json : Json.t) =
    match json with
    | `List [] -> Json.error "invalid packet" json
    | `List (x :: xs) ->
      (* we inspect the first element to see what we're dealing with *)
      let x =
        match x with
        | `Assoc fields -> t_of_fields fields
        | _ -> Json.error "invalid packet" json
      in
      (match
         match x with
         | Notification x -> `Call (`Notification x)
         | Request x -> `Call (`Request x)
         | Response r -> `Response r
         | _ -> Json.error "invalid packet" json
       with
       | `Call x ->
         Batch_call
           (x
            :: List.map xs ~f:(fun call ->
              let x = t_of_yojson_single call in
              match x with
              | Notification n -> `Notification n
              | Request n -> `Request n
              | _ -> Json.error "invalid packet" json))
       | `Response x ->
         Batch_response
           (x
            :: List.map xs ~f:(fun resp ->
              let resp = t_of_yojson_single resp in
              match resp with
              | Response n -> n
              | _ -> Json.error "invalid packet" json)))
    | _ -> t_of_yojson_single json
  ;;
end

open Import

module Id = struct
  type t = (string, int) Either.t

  let yojson_of_t = function
    | Either.Left s -> `String s
    | Right i -> `Int i

  let t_of_yojson = function
    | `String s -> Either.Left s
    | `Int i -> Right i
    | j -> yojson_error "Id.t" j
end

let field fields name conv = List.assoc_opt name fields |> Option.map ~f:conv

let field_exn fields name conv =
  match field fields name conv with
  | None -> yojson_error "Jsonrpc.Result.t: missing field" (`Assoc fields)
  | Some f -> f

module Constant = struct
  let jsonrpc = "jsonrpc"

  let jsonrpcv = "2.0"

  let id = "id"

  let method_ = "method"

  let params = "params"

  let result = "result"

  let error = "error"
end

module Request = struct
  type t =
    { id : Id.t option [@yojson.option]
    ; method_ : string [@key "method"]
    ; params : json option [@yojson.option]
    }

  let yojson_of_t { id; method_; params } =
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
      match id with
      | None -> json
      | Some id -> (Constant.id, Id.yojson_of_t id) :: json
    in
    `Assoc json

  let t_of_yojson json =
    match json with
    | `Assoc fields ->
      let method_ =
        field_exn fields Constant.method_ Yojson_conv.string_of_yojson
      in
      let params = field fields Constant.params json_of_yojson in
      let id = field fields Constant.id Id.t_of_yojson in
      let jsonrpc =
        field_exn fields Constant.jsonrpc Yojson_conv.string_of_yojson
      in
      if jsonrpc = Constant.jsonrpcv then
        { method_; params; id }
      else
        yojson_error "invalid version" json
    | _ -> yojson_error "invalid request" json

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
    let open Result.Infix in
    require_params t.params >>= read_json_params f
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
          | None -> yojson_error "unknown code" json
          | Some i -> i )
        | _ -> yojson_error "invalid code" json

      let yojson_of_t t = `Int (to_int t)
    end

    type t =
      { code : Code.t
      ; message : string
      ; data : json option [@yojson.option]
      }
    [@@deriving_inline yojson]

    let _ = fun (_ : t) -> ()

    let t_of_yojson =
      ( let _tp_loc = "lsp/src/jsonrpc.ml.Response.Error.t" in
        function
        | `Assoc field_yojsons as yojson -> (
          let code_field = ref None
          and message_field = ref None
          and data_field = ref None
          and duplicates = ref []
          and extra = ref [] in
          let rec iter = function
            | (field_name, _field_yojson) :: tail ->
              ( match field_name with
              | "code" -> (
                match Ppx_yojson_conv_lib.( ! ) code_field with
                | None ->
                  let fvalue = Code.t_of_yojson _field_yojson in
                  code_field := Some fvalue
                | Some _ ->
                  duplicates :=
                    field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
              | "message" -> (
                match Ppx_yojson_conv_lib.( ! ) message_field with
                | None ->
                  let fvalue = string_of_yojson _field_yojson in
                  message_field := Some fvalue
                | Some _ ->
                  duplicates :=
                    field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
              | "data" -> (
                match Ppx_yojson_conv_lib.( ! ) data_field with
                | None ->
                  let fvalue = json_of_yojson _field_yojson in
                  data_field := Some fvalue
                | Some _ ->
                  duplicates :=
                    field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
              | _ ->
                if
                  Ppx_yojson_conv_lib.( ! )
                    Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
                then
                  extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
                else
                  () );
              iter tail
            | [] -> ()
          in
          iter field_yojsons;
          match Ppx_yojson_conv_lib.( ! ) duplicates with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
              _tp_loc
              (Ppx_yojson_conv_lib.( ! ) duplicates)
              yojson
          | [] -> (
            match Ppx_yojson_conv_lib.( ! ) extra with
            | _ :: _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
                (Ppx_yojson_conv_lib.( ! ) extra)
                yojson
            | [] -> (
              match
                ( Ppx_yojson_conv_lib.( ! ) code_field
                , Ppx_yojson_conv_lib.( ! ) message_field
                , Ppx_yojson_conv_lib.( ! ) data_field )
              with
              | Some code_value, Some message_value, data_value ->
                { code = code_value
                ; message = message_value
                ; data = data_value
                }
              | _ ->
                Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                  _tp_loc yojson
                  [ ( Ppx_yojson_conv_lib.poly_equal
                        (Ppx_yojson_conv_lib.( ! ) code_field)
                        None
                    , "code" )
                  ; ( Ppx_yojson_conv_lib.poly_equal
                        (Ppx_yojson_conv_lib.( ! ) message_field)
                        None
                    , "message" )
                  ] ) ) )
        | _ as yojson ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
            yojson
        : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

    let _ = t_of_yojson

    let yojson_of_t =
      ( function
        | { code = v_code; message = v_message; data = v_data } ->
          let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
          let bnds =
            match v_data with
            | None -> bnds
            | Some v ->
              let arg = yojson_of_json v in
              let bnd = ("data", arg) in
              bnd :: bnds
          in
          let bnds =
            let arg = yojson_of_string v_message in
            ("message", arg) :: bnds
          in
          let bnds =
            let arg = Code.yojson_of_t v_code in
            ("code", arg) :: bnds
          in
          `Assoc bnds
        : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

    let _ = yojson_of_t

    [@@@end]

    let make ?data ~code ~message () = { data; code; message }
  end

  type t =
    { id : Id.t
    ; result : (json, Error.t) Result.t
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
      let id = field_exn fields Constant.id Id.t_of_yojson in
      let jsonrpc =
        field_exn fields Constant.jsonrpc Yojson_conv.string_of_yojson
      in
      if jsonrpc <> Constant.jsonrpcv then
        yojson_error "Invalid response" json
      else
        match field fields Constant.result json_of_yojson with
        | Some res -> { id; result = Ok res }
        | None ->
          let result =
            Error (field_exn fields Constant.error Error.t_of_yojson)
          in
          { id; result } )
    | _ -> yojson_error "Jsonrpc.Result.t" json

  let make ~id ~result = { id; result }

  let ok id result = make ~id ~result:(Ok result)

  let error id error = make ~id ~result:(Error error)
end

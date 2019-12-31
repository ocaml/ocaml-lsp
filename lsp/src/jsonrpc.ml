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

module Request = struct
  type t =
    { id : Id.t option [@default None]
    ; method_ : string [@key "method"]
    ; params : json
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/jsonrpc.ml.Request.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let id_field = ref None
        and method__field = ref None
        and params_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "id" -> (
              match Ppx_yojson_conv_lib.( ! ) id_field with
              | None ->
                let fvalue = option_of_yojson Id.t_of_yojson _field_yojson in
                id_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "method" -> (
              match Ppx_yojson_conv_lib.( ! ) method__field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                method__field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "params" -> (
              match Ppx_yojson_conv_lib.( ! ) params_field with
              | None ->
                let fvalue = json_of_yojson _field_yojson in
                params_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
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
              ( Ppx_yojson_conv_lib.( ! ) id_field
              , Ppx_yojson_conv_lib.( ! ) method__field
              , Ppx_yojson_conv_lib.( ! ) params_field )
            with
            | id_value, Some method__value, Some params_value ->
              { id =
                  ( match id_value with
                  | None -> None
                  | Some v -> v )
              ; method_ = method__value
              ; params = params_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) method__field)
                      None
                  , "method_" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) params_field)
                      None
                  , "params" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { id = v_id; method_ = v_method_; params = v_params } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_json v_params in
          ("params", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_method_ in
          ("method", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_option Id.yojson_of_t v_id in
          ("id", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module Response = struct
  type response =
    { id : Id.t
    ; jsonrpc : string
    ; result : json
    }
  [@@deriving_inline yojson]

  let _ = fun (_ : response) -> ()

  let response_of_yojson =
    ( let _tp_loc = "lsp/src/jsonrpc.ml.Response.response" in
      function
      | `Assoc field_yojsons as yojson -> (
        let id_field = ref None
        and jsonrpc_field = ref None
        and result_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "id" -> (
              match Ppx_yojson_conv_lib.( ! ) id_field with
              | None ->
                let fvalue = Id.t_of_yojson _field_yojson in
                id_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "jsonrpc" -> (
              match Ppx_yojson_conv_lib.( ! ) jsonrpc_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                jsonrpc_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "result" -> (
              match Ppx_yojson_conv_lib.( ! ) result_field with
              | None ->
                let fvalue = json_of_yojson _field_yojson in
                result_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
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
              ( Ppx_yojson_conv_lib.( ! ) id_field
              , Ppx_yojson_conv_lib.( ! ) jsonrpc_field
              , Ppx_yojson_conv_lib.( ! ) result_field )
            with
            | Some id_value, Some jsonrpc_value, Some result_value ->
              { id = id_value; jsonrpc = jsonrpc_value; result = result_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) id_field)
                      None
                  , "id" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) jsonrpc_field)
                      None
                  , "jsonrpc" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) result_field)
                      None
                  , "result" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> response )

  let _ = response_of_yojson

  let yojson_of_response =
    ( function
      | { id = v_id; jsonrpc = v_jsonrpc; result = v_result } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_json v_result in
          ("result", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_jsonrpc in
          ("jsonrpc", arg) :: bnds
        in
        let bnds =
          let arg = Id.yojson_of_t v_id in
          ("id", arg) :: bnds
        in
        `Assoc bnds
      : response -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_response

  [@@@end]

  type response_error =
    { id : Id.t
    ; jsonrpc : string
    ; error : error
    }

  and error =
    { code : int
    ; message : string
    }
  [@@deriving_inline yojson]

  let _ = fun (_ : response_error) -> ()

  let _ = fun (_ : error) -> ()

  let rec response_error_of_yojson =
    ( let _tp_loc = "lsp/src/jsonrpc.ml.Response.response_error" in
      function
      | `Assoc field_yojsons as yojson -> (
        let id_field = ref None
        and jsonrpc_field = ref None
        and error_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "id" -> (
              match Ppx_yojson_conv_lib.( ! ) id_field with
              | None ->
                let fvalue = Id.t_of_yojson _field_yojson in
                id_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "jsonrpc" -> (
              match Ppx_yojson_conv_lib.( ! ) jsonrpc_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                jsonrpc_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "error" -> (
              match Ppx_yojson_conv_lib.( ! ) error_field with
              | None ->
                let fvalue = error_of_yojson _field_yojson in
                error_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
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
              ( Ppx_yojson_conv_lib.( ! ) id_field
              , Ppx_yojson_conv_lib.( ! ) jsonrpc_field
              , Ppx_yojson_conv_lib.( ! ) error_field )
            with
            | Some id_value, Some jsonrpc_value, Some error_value ->
              { id = id_value; jsonrpc = jsonrpc_value; error = error_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) id_field)
                      None
                  , "id" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) jsonrpc_field)
                      None
                  , "jsonrpc" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) error_field)
                      None
                  , "error" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> response_error )

  and error_of_yojson =
    ( let _tp_loc = "lsp/src/jsonrpc.ml.Response.error" in
      function
      | `Assoc field_yojsons as yojson -> (
        let code_field = ref None
        and message_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "code" -> (
              match Ppx_yojson_conv_lib.( ! ) code_field with
              | None ->
                let fvalue = int_of_yojson _field_yojson in
                code_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "message" -> (
              match Ppx_yojson_conv_lib.( ! ) message_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                message_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
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
              , Ppx_yojson_conv_lib.( ! ) message_field )
            with
            | Some code_value, Some message_value ->
              { code = code_value; message = message_value }
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
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> error )

  let _ = response_error_of_yojson

  and _ = error_of_yojson

  let rec yojson_of_response_error =
    ( function
      | { id = v_id; jsonrpc = v_jsonrpc; error = v_error } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_error v_error in
          ("error", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_jsonrpc in
          ("jsonrpc", arg) :: bnds
        in
        let bnds =
          let arg = Id.yojson_of_t v_id in
          ("id", arg) :: bnds
        in
        `Assoc bnds
      : response_error -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  and yojson_of_error =
    ( function
      | { code = v_code; message = v_message } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_string v_message in
          ("message", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_int v_code in
          ("code", arg) :: bnds
        in
        `Assoc bnds
      : error -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_response_error

  and _ = yojson_of_error

  [@@@end]

  type t =
    | Response of response
    | Response_error of response_error

  let make id result = Response { id; result; jsonrpc = "2.0" }

  let make_error id code message =
    Response_error { id; error = { code; message }; jsonrpc = "2.0" }

  let yojson_of_t = function
    | Response v -> yojson_of_response v
    | Response_error v -> yojson_of_response_error v

  let t_of_yojson (json : json) =
    match json with
    | `Assoc xs -> (
      match List.assoc_opt "result" xs with
      | Some _ -> Response (response_of_yojson json)
      | None -> Response_error (response_error_of_yojson json) )
    | _ -> yojson_error "Jsonrpc.Response.t_of_yojson" json
end

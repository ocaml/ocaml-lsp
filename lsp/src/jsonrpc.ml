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
  module Error = struct
    type t =
      { code : int
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
                  let fvalue = int_of_yojson _field_yojson in
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
            let arg = yojson_of_int v_code in
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
    ; jsonrpc : string
    ; result : (json, Error.t) Result.t
    }

  let yojson_of_t { id; jsonrpc; result } =
    let result =
      match result with
      | Ok json -> ("result", json)
      | Error e -> ("error", Error.yojson_of_t e)
    in
    `Assoc [ ("id", Id.yojson_of_t id); ("jsonrpc", `String jsonrpc); result ]

  let t_of_yojson json =
    match json with
    | `Assoc fields -> (
      let field name conv = List.assoc_opt name fields |> Option.map ~f:conv in
      let field_exn name conv =
        match field name conv with
        | None -> yojson_error "Jsonrpc.Result.t: missing field" json
        | Some f -> f
      in
      let id = field_exn "id" Id.t_of_yojson in
      let jsonrpc = field_exn "jsonrpc" Yojson_conv.string_of_yojson in
      match field "result" json_of_yojson with
      | Some res -> { id; jsonrpc; result = Ok res }
      | None ->
        let result = Error (field_exn "error" Error.t_of_yojson) in
        { id; jsonrpc; result } )
    | _ -> yojson_error "Jsonrpc.Result.t" json

  let make ~id ~result = { id; result; jsonrpc = "2.0" }

  let ok id result = make ~id ~result:(Ok result)

  let error id error = make ~id ~result:(Error error)
end

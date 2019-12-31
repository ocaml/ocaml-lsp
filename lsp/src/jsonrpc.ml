open Import

module Request = struct
  type t =
    { id : Protocol.Id.t option [@default None]
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
                let fvalue =
                  option_of_yojson Protocol.Id.t_of_yojson _field_yojson
                in
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
          let arg = yojson_of_option Protocol.Id.yojson_of_t v_id in
          ("id", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

type 't req_params_spec =
  { params_schema : Jsonrpc.Message.Structured.t
        (** used to document the structure of the params; example:
            [`Assoc \[ "uri" , `String "<Uri>" \]]; *)
  ; of_jsonrpc_params : Jsonrpc.Message.Structured.t -> 't option
        (** parses given structured JSON if it's of the expected schema;
            otherwise, return [None] *)
  }

val of_jsonrpc_params_exn :
     'req_params req_params_spec
  -> Jsonrpc.Message.Structured.t option
  -> 'req_params

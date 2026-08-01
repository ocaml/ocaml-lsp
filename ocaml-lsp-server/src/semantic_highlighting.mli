open Import

type config

val create_config : SemanticTokensClientCapabilities.t -> config
val legend : config -> SemanticTokensLegend.t
val on_request_full : State.t -> SemanticTokensParams.t -> SemanticTokens.t option Fiber.t

module For_tests : sig
  type config

  val server_token_types : string list
  val server_token_modifiers : string list
  val create_config : token_types:string list -> token_modifiers:string list -> config
  val legend : config -> SemanticTokensLegend.t
  val token_type_index : int
  val token_modifiers_bitset : int

  val encode
    :  ?config:config
    -> ?token_type_index:int
    -> ?token_modifiers:int
    -> (Position.t * int) list
    -> int array

  val find_diff : old:int array -> new_:int array -> SemanticTokensEdit.t list
end

val on_request_full_delta
  :  State.t
  -> SemanticTokensDeltaParams.t
  -> [ `SemanticTokens of SemanticTokens.t
     | `SemanticTokensDelta of SemanticTokensDelta.t
     ]
       option
       Fiber.t

module Debug : sig
  val meth_request_full : string
  val on_request_full : params:Jsonrpc.Structured.t option -> State.t -> Json.t Fiber.t
end

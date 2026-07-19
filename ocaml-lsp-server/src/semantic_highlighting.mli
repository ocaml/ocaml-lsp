open Import

val legend : SemanticTokensLegend.t
val on_request_full : State.t -> SemanticTokensParams.t -> SemanticTokens.t option Fiber.t

module For_tests : sig
  val token_type_index : int
  val token_modifiers_bitset : int
  val encode : (Position.t * int) list -> int array
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

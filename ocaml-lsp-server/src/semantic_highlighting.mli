open Import

val legend : SemanticTokensLegend.t
val on_request_full : State.t -> SemanticTokensParams.t -> SemanticTokens.t option Fiber.t

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

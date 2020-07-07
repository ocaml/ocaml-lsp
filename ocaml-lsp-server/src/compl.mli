open Import

val complete :
     Document.t
  -> Position.t
  -> markdown:bool
  -> ([> `CompletionList of CompletionList.t ], Jsonrpc.Response.Error.t) result
     Fiber.t

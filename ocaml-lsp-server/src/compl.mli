open Import

val complete :
     Document.t
  -> Position.t
  -> has_markdown_support:bool
  -> ( [> `CompletionList of CompletionList.t ]
     , Lsp.Jsonrpc.Response.Error.t )
     result
     Fiber.t

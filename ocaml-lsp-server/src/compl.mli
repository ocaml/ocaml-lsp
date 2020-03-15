open Import

val complete :
     Document.t
  -> Position.t
  -> ( [> `CompletionList of CompletionList.t ]
     , Lsp.Jsonrpc.Response.Error.t )
     result

open Import

module type S = sig
  val action_kind : string

  val code_action :
       Document.t
    -> CodeActionParams.t
    -> (CodeAction.t option, Jsonrpc.Response.Error.t) Fiber.Result.t
end

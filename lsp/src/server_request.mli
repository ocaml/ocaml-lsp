open Import
open Types

type _ t =
  | WorkspaceApplyEdit : ApplyWorkspaceEditParams.t -> ApplyWorkspaceEditResult.t t
  | WorkspaceFolders : WorkspaceFolder.t list t
  | WorkspaceConfiguration : ConfigurationParams.t -> Json.t list t
  | ClientRegisterCapability : RegistrationParams.t -> unit t
  | ClientUnregisterCapability : UnregistrationParams.t -> unit t
  | ShowMessageRequest : ShowMessageRequestParams.t -> MessageActionItem.t option t
  | ShowDocumentRequest : ShowDocumentParams.t -> ShowDocumentResult.t t
  | WorkDoneProgressCreate : WorkDoneProgressCreateParams.t -> unit t
  | CodeLensRefresh : unit t
  | SemanticTokensRefresh : unit t
  | WorkspaceDiagnosticRefresh : unit t
  | WorkspaceFoldingRangeRefresh : unit t
  | WorkspaceInlayHintRefresh : unit t
  | WorkspaceInlineValueRefresh : unit t
  | UnknownRequest : string * Jsonrpc.Structured.t option -> Json.t t

type packed = E : 'r t -> packed

val yojson_of_result : 'a t -> 'a -> Json.t
val to_jsonrpc_request : _ t -> id:Jsonrpc.Id.t -> Jsonrpc.Request.t
val of_jsonrpc : Jsonrpc.Request.t -> (packed, string) Result.t
val response_of_json : 'a t -> Json.t -> 'a

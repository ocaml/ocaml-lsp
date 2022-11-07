open Import
open Types

type _ t =
  | WorkspaceApplyEdit :
      ApplyWorkspaceEditParams.t
      -> ApplyWorkspaceEditResult.t t
  | WorkspaceFolders : WorkspaceFolder.t list t
  | WorkspaceConfiguration : ConfigurationParams.t -> Json.t list t
  | ClientRegisterCapability : RegistrationParams.t -> unit t
  | ClientUnregisterCapability : UnregistrationParams.t -> unit t
  | ShowMessageRequest :
      ShowMessageRequestParams.t
      -> MessageActionItem.t option t
  | ShowDocumentRequest : ShowDocumentParams.t -> ShowDocumentResult.t t
  | WorkDoneProgressCreate : WorkDoneProgressCreateParams.t -> unit t
  | CodeLensRefresh : unit t
  | SemanticTokensRefresh : unit t
  | WorkspaceDiagnosticRefresh : unit t
  | UnknownRequest : string * Jsonrpc.Structured.t option -> Json.t t

type packed = E : 'r t -> packed

let method_ (type a) (t : a t) =
  match t with
  | WorkspaceConfiguration _ -> "workspace/configuration"
  | WorkspaceFolders -> "workspace/workspaceFolders"
  | WorkspaceApplyEdit _ -> "workspace/applyEdit"
  | ClientRegisterCapability _ -> "client/registerCapability"
  | ClientUnregisterCapability _ -> "client/unregisterCapability"
  | ShowMessageRequest _ -> "window/showMessageRequest"
  | ShowDocumentRequest _ -> "window/showDocument"
  | WorkDoneProgressCreate _ -> "window/workDoneProgress/create"
  | CodeLensRefresh -> "workspace/codeLens/refresh"
  | SemanticTokensRefresh -> "workspace/semanticTokens/refresh"
  | WorkspaceDiagnosticRefresh -> "workspace/diagnostic/refresh"
  | UnknownRequest (r, _) -> r

let params =
  let ret x = Some (Jsonrpc.Structured.t_of_yojson x) in
  fun (type a) (t : a t) ->
    match t with
    | WorkspaceApplyEdit params ->
      ret (ApplyWorkspaceEditParams.yojson_of_t params)
    | WorkspaceFolders -> None
    | WorkspaceConfiguration params ->
      ret (ConfigurationParams.yojson_of_t params)
    | ClientRegisterCapability params ->
      ret (RegistrationParams.yojson_of_t params)
    | ClientUnregisterCapability params ->
      ret (UnregistrationParams.yojson_of_t params)
    | ShowMessageRequest params ->
      ret (ShowMessageRequestParams.yojson_of_t params)
    | ShowDocumentRequest params -> ret (ShowDocumentParams.yojson_of_t params)
    | WorkDoneProgressCreate params ->
      ret (WorkDoneProgressCreateParams.yojson_of_t params)
    | CodeLensRefresh -> None
    | SemanticTokensRefresh -> None
    | WorkspaceDiagnosticRefresh -> None
    | UnknownRequest (_, params) -> params

let to_jsonrpc_request t ~id =
  let method_ = method_ t in
  let params = params t in
  Jsonrpc.Request.create ~id ~method_ ?params ()

let of_jsonrpc (r : Jsonrpc.Request.t) : (packed, string) Result.t =
  let open Result.O in
  let parse f = Json.message_params r.params f in
  match r.method_ with
  | "workspace/configuration" ->
    let+ params = parse ConfigurationParams.t_of_yojson in
    E (WorkspaceConfiguration params)
  | "workspace/workspaceFolders" -> Ok (E WorkspaceFolders)
  | "workspace/applyEdit" ->
    let+ params = parse ApplyWorkspaceEditParams.t_of_yojson in
    E (WorkspaceApplyEdit params)
  | "client/registerCapability" ->
    let+ params = parse RegistrationParams.t_of_yojson in
    E (ClientRegisterCapability params)
  | "client/unregisterCapability" ->
    let+ params = parse UnregistrationParams.t_of_yojson in
    E (ClientUnregisterCapability params)
  | "window/showMessageRequest" ->
    let+ params = parse ShowMessageRequestParams.t_of_yojson in
    E (ShowMessageRequest params)
  | "window/showDocument" ->
    let+ params = parse ShowDocumentParams.t_of_yojson in
    E (ShowDocumentRequest params)
  | "window/workDoneProgress/create" ->
    let+ params = parse WorkDoneProgressCreateParams.t_of_yojson in
    E (WorkDoneProgressCreate params)
  | "workspace/codeLens/refresh" -> Ok (E CodeLensRefresh)
  | "workspace/semanticTokens/refresh" -> Ok (E SemanticTokensRefresh)
  | m -> Ok (E (UnknownRequest (m, r.params)))

let yojson_of_result (type a) (t : a t) (r : a) : Json.t =
  match (t, r) with
  | WorkspaceApplyEdit _, r -> ApplyWorkspaceEditResult.yojson_of_t r
  | WorkspaceFolders, r ->
    Json.Conv.yojson_of_list WorkspaceFolder.yojson_of_t r
  | WorkspaceConfiguration _, r -> Json.Conv.yojson_of_list (fun x -> x) r
  | ClientRegisterCapability _, () -> `Null
  | ClientUnregisterCapability _, () -> `Null
  | WorkDoneProgressCreate _, () -> `Null
  | ShowMessageRequest _, r ->
    Json.Conv.yojson_of_option MessageActionItem.yojson_of_t r
  | ShowDocumentRequest _, r -> ShowDocumentResult.yojson_of_t r
  | CodeLensRefresh, _ -> `Null
  | SemanticTokensRefresh, _ -> `Null
  | WorkspaceDiagnosticRefresh, _ -> `Null
  | UnknownRequest (_, _), json -> json

let response_of_json (type a) (t : a t) (json : Json.t) : a =
  let open Json.Conv in
  match t with
  | WorkspaceApplyEdit _ -> ApplyWorkspaceEditResult.t_of_yojson json
  | WorkspaceFolders -> list_of_yojson WorkspaceFolder.t_of_yojson json
  | WorkspaceConfiguration _ -> list_of_yojson (fun x -> x) json
  | ClientRegisterCapability _ -> unit_of_yojson json
  | ClientUnregisterCapability _ -> unit_of_yojson json
  | ShowMessageRequest _ -> option_of_yojson MessageActionItem.t_of_yojson json
  | ShowDocumentRequest _ -> ShowDocumentResult.t_of_yojson json
  | WorkDoneProgressCreate _ -> unit_of_yojson json
  | CodeLensRefresh -> unit_of_yojson json
  | SemanticTokensRefresh -> unit_of_yojson json
  | WorkspaceDiagnosticRefresh -> unit_of_yojson json
  | UnknownRequest (_, _) -> json

open! Import
open Protocol

type t =
  | PublishDiagnostics of PublishDiagnostics.params
  | ShowMessage of ShowMessage.Params.t

let method_ = function
  | ShowMessage _ -> "window/showMessage"
  | PublishDiagnostics _ -> "textDocument/publishDiagnostics"

let yojson_of_t = function
  | ShowMessage params -> ShowMessage.Params.yojson_of_t params
  | PublishDiagnostics params -> PublishDiagnostics.yojson_of_params params

let to_jsonrpc_request t =
  let method_ = method_ t in
  let params = Some (yojson_of_t t) in
  { Jsonrpc.Request.id = None; params; method_ }

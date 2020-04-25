open Import
open Protocol

exception Not_initialized

class type t =
  object

    method clientCapabilities : Initialize.ClientCapabilities.t

    (* requests *)
    method shutdown : unit
    method textDocumentHover : Hover.params -> Hover.result
    method textDocumentDefinition : Definition.params -> Definition.result
    method textDocumentTypeDefinition : TypeDefinition.params -> TypeDefinition.result
    method textDocumentCompletion : Completion.params -> Completion.result
    method textDocumentCodeLens : CodeLens.params -> CodeLens.result
    method textDocumentRename : Rename.params -> Rename.result
    method documentSymbol : TextDocumentDocumentSymbol.params -> TextDocumentDocumentSymbol.result
    method debugEcho : DebugEcho.params -> DebugEcho.result
    method debugTextDocumentGet : DebugTextDocumentGet.params -> DebugTextDocumentGet.result
    method textDocumentReferences : References.params -> References.result
    method textDocumentHighlight : TextDocumentHighlight.params -> TextDocumentHighlight.result
    method textDocumentFoldingRange : FoldingRange.params -> FoldingRange.result
    method signatureHelp : TextDocumentPositionParams.t -> SignatureHelp.t
    method codeAction : CodeActionParams.t -> CodeAction.result
    method completionItemResolve : Completion.completionItem -> Completion.completionItem
    method initialize : Initialize.Params.t -> Initialize.Result.t

    (* notifications *)
    method textDocumentDidOpen : DidOpen.params -> unit
    method textDocumentDidChange : DidChangeTextDocumentParams.t -> unit
    method initialized : unit
    method exit : unit

    (** Handling of unknown requests/notifications *)
    method unknown_request : string -> json option -> unit
    method unknown_notification : string -> json option -> unit

    method register_exn_error : (exn -> Jsonrpc.Response.Error.t)
    method uncaught_exn : Jsonrpc.Request.t -> exn -> t
  end

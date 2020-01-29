open Import
open Protocol

module SignatureHelpOptions : sig
  type t = { triggerCharacters : string list }
end

module CodeActionOptions : sig
  type t = { codeActionsKinds : CodeAction.Kind.t list }
end

module CodeActionLiteralSupport : sig
  type codeActionKind = { valueSet : CodeAction.Kind.t list }

  type t = { codeActionKind : codeActionKind }
end

module Trace : sig
  type t =
    | Off
    | Messages
    | Verbose
end

module TextDocumentSyncKind : sig
  type t =
    | NoSync
    | FullSync
    | IncrementalSync
end

module Synchronization : sig
  type t =
    { willSave : bool
    ; willSaveWaitUntil : bool
    ; didSave : bool
    }
end

module CompletionItem : sig
  type t = { snippetSupport : bool }

  val empty : t
end

module Completion : sig
  type t = { completionItem : CompletionItem.t }

  val empty : t
end

module Hover : sig
  type t = { contentFormat : MarkupKind.t list }

  val empty : t
end

module CodeAction : sig
  type t =
    { codeActionLiteralSupport : CodeActionLiteralSupport.t option
    ; dynamicRegistration : bool option
    ; isPreferredSupport : bool option
    }

  val empty : t
end

module DocumentSymbol : sig
  type t = { hierarchicalDocumentSymbolSupport : bool }

  val empty : t
end

module TextDocumentClientCapabilities : sig
  type t =
    { synchronization : Synchronization.t
    ; completion : Completion.t
    ; documentSymbol : DocumentSymbol.t
    ; hover : Hover.t
    ; codeAction : CodeAction.t
    }

  val empty : t
end

module WorkspaceEdit : sig
  module ResourceOperationKind : sig
    type t =
      | Create
      | Rename
      | Delete
  end

  module FailureHandlingKind : sig
    type t =
      | Abort
      | Transactional
      | TextOnlyTransactional
      | Undo
  end

  type t =
    { documentChanges : bool
    ; resourceOperations : ResourceOperationKind.t list option
    ; failureHandlingKind : FailureHandlingKind.t option
    }

  val empty : t
end

module WorkspaceClientCapabilities : sig
  module Symbol : sig
    type t = { valueSet : SymbolKind.t list }
  end

  type t =
    { applyEdit : bool
    ; workspaceEdit : WorkspaceEdit.t
    ; symbol : Symbol.t
    ; workspaceFolders : bool
    ; configuration : bool
    }
end

module FoldingRangeClientCapabilities : sig
  type t =
    { rangeLimit : int option
    ; lineFoldingOnly : bool
    }

  val empty : t
end

module ClientCapabilities : sig
  type t =
    { workspace : WorkspaceClientCapabilities.t
    ; textDocument : TextDocumentClientCapabilities.t
    ; foldingRange : FoldingRangeClientCapabilities.t
    }

  val empty : t
end

module Info : sig
  type t =
    { name : string
    ; version : string option
    }
end

module Params : sig
  type t =
    { processId : int option
    ; rootPath : string option
    ; rootUri : documentUri option
    ; capabilities : ClientCapabilities.t
    ; trace : Trace.t
    ; workspaceFolders : WorkspaceFolder.t list
    ; initializationOptions : Json.t option
    ; clientInfo : Info.t option
    }

  val create :
       ?processId:int
    -> ?rootPath:string
    -> ?rootUri:documentUri
    -> ?capabilities:ClientCapabilities.t
    -> ?trace:Trace.t
    -> ?workspaceFolders:WorkspaceFolder.t list
    -> ?initializationOptions:Json.t
    -> ?clientInfo:Info.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module TextDocumentSyncOptions : sig
  type saveOptions = { includeText : bool }

  type t =
    { openClose : bool
    ; change : TextDocumentSyncKind.t
    ; willSave : bool
    ; willSaveWaitUntil : bool
    ; didSave : saveOptions option
    }
end

module CompletionOptions : sig
  type t =
    { resolveProvider : bool
    ; triggerCharacters : string list
    }
end

module CodeLensOptions : sig
  type t = { resolveProvider : bool }
end

module DocumentOnTypeFormattingOptions : sig
  type t =
    { firstTriggerCharacter : string
    ; moreTriggerCharacter : string list
    }
end

module DocumentLinkOptions : sig
  type t = { doclink_resolveProvider : bool }
end

module ExecuteCommandOptions : sig
  type t = { commands : string list }
end

module ServerCapabilities : sig
  type t =
    { textDocumentSync : TextDocumentSyncOptions.t
    ; hoverProvider : bool
    ; completionProvider : CompletionOptions.t option
    ; signatureHelpProvider : SignatureHelpOptions.t option
    ; definitionProvider : bool
    ; typeDefinitionProvider : bool
    ; referencesProvider : bool
    ; documentHighlightProvider : bool
    ; documentSymbolProvider : bool
    ; workspaceSymbolProvider : bool
    ; codeActionProvider : CodeActionOptions.t Or_bool.t
    ; codeLensProvider : CodeLensOptions.t option
    ; documentFormattingProvider : bool
    ; documentRangeFormattingProvider : bool
    ; documentOnTypeFormattingProvider :
        DocumentOnTypeFormattingOptions.t option
    ; renameProvider : bool
    ; documentLinkProvider : DocumentLinkOptions.t option
    ; executeCommandProvider : ExecuteCommandOptions.t option
    ; typeCoverageProvider : bool
    ; foldingRangeProvider : Void.t Or_bool.t
    }

  val default : t
end

module Result : sig
  type t =
    { capabilities : ServerCapabilities.t
    ; serverInfo : Info.t option
    }

  include Json.Jsonable.S with type t := t
end

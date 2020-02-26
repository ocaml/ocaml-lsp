open! Import

[@@@warning "-30"]

module DeleteFileOptions : sig
  type t =
    { recursive : bool option [@yojson.option]
    ; ignoreIfNotExists : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentUri : sig end

module DeleteFile : sig
  type t =
    { kind : unit
    ; uri : DocumentUri.t
    ; options : DeleteFileOptions.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameFileOptions : sig
  type t =
    { overwrite : bool option [@yojson.option]
    ; ignoreIfExists : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameFile : sig
  type t =
    { kind : unit
    ; oldUri : DocumentUri.t
    ; newUri : DocumentUri.t
    ; options : RenameFileOptions.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CreateFileOptions : sig
  type t =
    { overwrite : bool option [@yojson.option]
    ; ignoreIfExists : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CreateFile : sig
  type t =
    { kind : unit
    ; uri : DocumentUri.t
    ; options : CreateFileOptions.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Position : sig
  type t =
    { line : int
    ; character : int
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Range : sig
  type t =
    { start : Position.t
    ; end_ : Position.t [@key "end"]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextEdit : sig
  type t =
    { range : Range.t
    ; newText : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentIdentifier : sig
  type t = { uri : DocumentUri.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module VersionedTextDocumentIdentifier : sig
  type t = { version : unit } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentEdit : sig
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; edits : unit
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceEdit : sig
  type t =
    { changes : changes option [@yojson.option]
    ; documentChanges : unit option [@yojson.option]
    }

  and changes = { uri : unit } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ApplyWorkspaceEditParams : sig
  type t =
    { label : string option [@yojson.option]
    ; edit : WorkspaceEdit.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ApplyWorkspaceEditResponse : sig
  type t =
    { applied : bool
    ; failureReason : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CancelParams : sig
  type t = { id : unit } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRangeClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FoldingRangeClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; rangeLimit : int option [@yojson.option]
    ; lineFoldingOnly : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DiagnosticTag : sig
  type t =
    | Unnecessary
    | Deprecated

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module PublishDiagnosticsClientCapabilities : sig
  type t =
    { relatedInformation : bool option [@yojson.option]
    ; tagSupport : tagSupport option [@yojson.option]
    ; versionSupport : bool option [@yojson.option]
    }

  and tagSupport = { valueSet : unit }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; prepareSupport : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentOnTypeFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentRangeFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentColorClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; tooltipSupport : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionKind : sig
  type t =
    | Empty
    | QuickFix
    | Refactor
    | RefactorExtract
    | RefactorInline
    | RefactorRewrite
    | Source
    | SourceOrganizeImports

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module CodeActionClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; codeActionLiteralSupport : codeActionLiteralSupport option
          [@yojson.option]
    ; isPreferredSupport : bool option [@yojson.option]
    }

  and codeActionLiteralSupport = { codeActionKind : codeActionKind }

  and codeActionKind = { valueSet : unit }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SymbolKind : sig
  type t =
    | File
    | Module
    | Namespace
    | Package
    | Class
    | Method
    | Property
    | Field
    | Constructor
    | Enum
    | Interface
    | Function
    | Variable
    | Constant
    | String
    | Number
    | Boolean
    | Array
    | Object
    | Key
    | Null
    | EnumMember
    | Struct
    | Event
    | Operator
    | TypeParameter

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module DocumentSymbolClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; symbolKind : symbolKind option [@yojson.option]
    ; hierarchicalDocumentSymbolSupport : bool option [@yojson.option]
    }

  and symbolKind = { valueSet : unit option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlightClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ImplementationClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; linkSupport : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TypeDefinitionClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; linkSupport : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DefinitionClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; linkSupport : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DeclarationClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; linkSupport : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module MarkupKind : sig
  type t =
    | PlainText
    | Markdown

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module SignatureHelpClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; signatureInformation : signatureInformation option [@yojson.option]
    ; contextSupport : bool option [@yojson.option]
    }

  and signatureInformation =
    { documentationFormat : unit option [@yojson.option]
    ; parameterInformation : parameterInformation option [@yojson.option]
    }

  and parameterInformation =
    { labelOffsetSupport : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module HoverClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; contentFormat : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionItemKind : sig
  type t =
    | Text
    | Method
    | Function
    | Constructor
    | Field
    | Variable
    | Class
    | Interface
    | Module
    | Property
    | Unit
    | Value
    | Enum
    | Keyword
    | Snippet
    | Color
    | File
    | Reference
    | Folder
    | EnumMember
    | Constant
    | Struct
    | Event
    | Operator
    | TypeParameter

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module CompletionItemTag : sig
  type t = Deprecated

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module CompletionClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; completionItem : completionItem option [@yojson.option]
    ; completionItemKind : completionItemKind option [@yojson.option]
    ; contextSupport : bool option [@yojson.option]
    }

  and completionItem =
    { snippetSupport : bool option [@yojson.option]
    ; commitCharactersSupport : bool option [@yojson.option]
    ; documentationFormat : unit option [@yojson.option]
    ; deprecatedSupport : bool option [@yojson.option]
    ; preselectSupport : bool option [@yojson.option]
    ; tagSupport : tagSupport option [@yojson.option]
    }

  and tagSupport = { valueSet : unit }

  and completionItemKind = { valueSet : unit option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentSyncClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; willSave : bool option [@yojson.option]
    ; willSaveWaitUntil : bool option [@yojson.option]
    ; didSave : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentClientCapabilities : sig
  type t =
    { synchronization : TextDocumentSyncClientCapabilities.t option
          [@yojson.option]
    ; completion : CompletionClientCapabilities.t option [@yojson.option]
    ; hover : HoverClientCapabilities.t option [@yojson.option]
    ; signatureHelp : SignatureHelpClientCapabilities.t option [@yojson.option]
    ; declaration : DeclarationClientCapabilities.t option [@yojson.option]
    ; definition : DefinitionClientCapabilities.t option [@yojson.option]
    ; typeDefinition : TypeDefinitionClientCapabilities.t option
          [@yojson.option]
    ; implementation : ImplementationClientCapabilities.t option
          [@yojson.option]
    ; references : ReferenceClientCapabilities.t option [@yojson.option]
    ; documentHighlight : DocumentHighlightClientCapabilities.t option
          [@yojson.option]
    ; documentSymbol : DocumentSymbolClientCapabilities.t option
          [@yojson.option]
    ; codeAction : CodeActionClientCapabilities.t option [@yojson.option]
    ; codeLens : CodeLensClientCapabilities.t option [@yojson.option]
    ; documentLink : DocumentLinkClientCapabilities.t option [@yojson.option]
    ; colorProvider : DocumentColorClientCapabilities.t option [@yojson.option]
    ; formatting : DocumentFormattingClientCapabilities.t option
          [@yojson.option]
    ; rangeFormatting : DocumentRangeFormattingClientCapabilities.t option
          [@yojson.option]
    ; onTypeFormatting : DocumentOnTypeFormattingClientCapabilities.t option
          [@yojson.option]
    ; rename : RenameClientCapabilities.t option [@yojson.option]
    ; publishDiagnostics : PublishDiagnosticsClientCapabilities.t option
          [@yojson.option]
    ; foldingRange : FoldingRangeClientCapabilities.t option [@yojson.option]
    ; selectionRange : SelectionRangeClientCapabilities.t option
          [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ExecuteCommandClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; symbolKind : symbolKind option [@yojson.option]
    }

  and symbolKind = { valueSet : unit option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeWatchedFilesClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeConfigurationClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FailureHandlingKind : sig
  type t =
    | Abort
    | Transactional
    | TextOnlyTransactional
    | Undo

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module ResourceOperationKind : sig
  type t =
    | Create
    | Rename
    | Delete

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module WorkspaceEditClientCapabilities : sig
  type t =
    { documentChanges : bool option [@yojson.option]
    ; resourceOperations : unit option [@yojson.option]
    ; failureHandling : FailureHandlingKind.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ClientCapabilities : sig
  type t =
    { workspace : workspace option [@yojson.option]
    ; textDocument : TextDocumentClientCapabilities.t option [@yojson.option]
    ; window : window option [@yojson.option]
    ; experimental : Json.t option [@yojson.option]
    }

  and workspace =
    { applyEdit : bool option [@yojson.option]
    ; workspaceEdit : WorkspaceEditClientCapabilities.t option [@yojson.option]
    ; didChangeConfiguration : DidChangeConfigurationClientCapabilities.t option
          [@yojson.option]
    ; didChangeWatchedFiles : DidChangeWatchedFilesClientCapabilities.t option
          [@yojson.option]
    ; symbol : WorkspaceSymbolClientCapabilities.t option [@yojson.option]
    ; executeCommand : ExecuteCommandClientCapabilities.t option
          [@yojson.option]
    ; workspaceFolders : bool option [@yojson.option]
    ; configuration : bool option [@yojson.option]
    }

  and window = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Command : sig
  type t =
    { title : string
    ; command : string
    ; arguments : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Location : sig
  type t =
    { uri : DocumentUri.t
    ; range : Range.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DiagnosticRelatedInformation : sig
  type t =
    { location : Location.t
    ; message : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DiagnosticSeverity : sig
  type t =
    | Error
    | Warning
    | Information
    | Hint

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module Diagnostic : sig
  type t =
    { range : Range.t
    ; severity : DiagnosticSeverity.t option [@yojson.option]
    ; code : unit option [@yojson.option]
    ; source : string option [@yojson.option]
    ; message : string
    ; tags : unit option [@yojson.option]
    ; relatedInformation : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeAction : sig
  type t =
    { title : string
    ; kind : CodeActionKind.t option [@yojson.option]
    ; diagnostics : unit option [@yojson.option]
    ; isPreferred : bool option [@yojson.option]
    ; edit : WorkspaceEdit.t option [@yojson.option]
    ; command : Command.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionContext : sig
  type t =
    { diagnostics : unit
    ; only : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionOptions : sig
  type t = { codeActionKinds : unit option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ProgressToken : sig end

module PartialResultParams : sig
  type t = { partialResultToken : ProgressToken.t option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressParams : sig
  type t = { workDoneToken : ProgressToken.t option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; context : CodeActionContext.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFilter : sig
  type t =
    { language : string option [@yojson.option]
    ; scheme : string option [@yojson.option]
    ; pattern : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSelector : sig end

module TextDocumentRegistrationOptions : sig
  type t = { documentSelector : unit }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLens : sig
  type t =
    { range : Range.t
    ; command : Command.t option [@yojson.option]
    ; data : Json.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensOptions : sig
  type t = { resolveProvider : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensParams : sig
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Color : sig
  type t =
    { red : int
    ; green : int
    ; blue : int
    ; alpha : int
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ColorInformation : sig
  type t =
    { range : Range.t
    ; color : Color.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ColorPresentation : sig
  type t =
    { label : string
    ; textEdit : TextEdit.t option [@yojson.option]
    ; additionalTextEdits : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ColorPresentationParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; color : Color.t
    ; range : Range.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionTriggerKind : sig
  type t =
    | Invoked
    | TriggerCharacter
    | TriggerForIncompleteCompletions

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module CompletionContext : sig
  type t =
    { triggerKind : CompletionTriggerKind.t
    ; triggerCharacter : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module InsertTextFormat : sig
  type t =
    | PlainText
    | Snippet

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module MarkupContent : sig
  type t =
    { kind : MarkupKind.t
    ; value : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionItem : sig
  type t =
    { label : string
    ; kind : int option [@yojson.option]
    ; tags : unit option [@yojson.option]
    ; detail : string option [@yojson.option]
    ; documentation : unit option [@yojson.option]
    ; deprecated : bool option [@yojson.option]
    ; preselect : bool option [@yojson.option]
    ; sortText : string option [@yojson.option]
    ; filterText : string option [@yojson.option]
    ; insertText : string option [@yojson.option]
    ; insertTextFormat : InsertTextFormat.t option [@yojson.option]
    ; textEdit : TextEdit.t option [@yojson.option]
    ; additionalTextEdits : unit option [@yojson.option]
    ; commitCharacters : unit option [@yojson.option]
    ; command : Command.t option [@yojson.option]
    ; data : Json.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionList : sig
  type t =
    { isIncomplete : bool
    ; items : unit
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionOptions : sig
  type t =
    { triggerCharacters : unit option [@yojson.option]
    ; allCommitCharacters : unit option [@yojson.option]
    ; resolveProvider : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentPositionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionParams : sig
  type t = { context : CompletionContext.t option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ConfigurationItem : sig
  type t =
    { scopeUri : DocumentUri.t option [@yojson.option]
    ; section : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ConfigurationParams : sig
  type t = { items : unit } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module StaticRegistrationOptions : sig
  type t = { id : string option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeConfigurationParams : sig
  type t = { settings : Json.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentContentChangeEvent : sig end

module DidChangeTextDocumentParams : sig
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; contentChanges : unit
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FileEvent : sig
  type t =
    { uri : DocumentUri.t
    ; type_ : int [@key "type"]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeWatchedFilesParams : sig
  type t = { changes : unit } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FileSystemWatcher : sig
  type t =
    { globPattern : string
    ; kind : int option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeWatchedFilesRegistrationOptions : sig
  type t = { watchers : unit } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceFolder : sig
  type t =
    { uri : DocumentUri.t
    ; name : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceFoldersChangeEvent : sig
  type t =
    { added : unit
    ; removed : unit
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeWorkspaceFoldersParams : sig
  type t = { event : WorkspaceFoldersChangeEvent.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidCloseTextDocumentParams : sig
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentItem : sig
  type t =
    { uri : DocumentUri.t
    ; languageId : string
    ; version : int
    ; text : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidOpenTextDocumentParams : sig
  type t = { textDocument : TextDocumentItem.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidSaveTextDocumentParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; text : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentColorParams : sig
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FormattingOptions : sig
  type t =
    { tabSize : int
    ; insertSpaces : bool
    ; trimTrailingWhitespace : bool option [@yojson.option]
    ; insertFinalNewline : bool option [@yojson.option]
    ; trimFinalNewlines : bool option [@yojson.option]
    ; key : unit
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFormattingParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; options : FormattingOptions.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlight : sig
  type t =
    { range : Range.t
    ; kind : int option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlightKind : sig
  type t =
    | Text
    | Read
    | Write

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module DocumentLink : sig
  type t =
    { range : Range.t
    ; target : DocumentUri.t option [@yojson.option]
    ; tooltip : string option [@yojson.option]
    ; data : Json.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkOptions : sig
  type t = { resolveProvider : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkParams : sig
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentOnTypeFormattingOptions : sig
  type t =
    { firstTriggerCharacter : string
    ; moreTriggerCharacter : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentOnTypeFormattingParams : sig
  type t =
    { ch : string
    ; options : FormattingOptions.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentRangeFormattingParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; options : FormattingOptions.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSymbol : sig
  type t =
    { name : string
    ; detail : string option [@yojson.option]
    ; kind : SymbolKind.t
    ; deprecated : bool option [@yojson.option]
    ; range : Range.t
    ; selectionRange : Range.t
    ; children : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSymbolParams : sig
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ErrorCodes : sig
  type t =
    | ParseError
    | InvalidRequest
    | MethodNotFound
    | InvalidParams
    | InternalError
    | ServerErrorStart
    | ServerErrorEnd
    | ServerNotInitialized
    | UnknownErrorCode
    | RequestCancelled
    | ContentModified

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module ExecuteCommandOptions : sig
  type t = { commands : unit } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ExecuteCommandParams : sig
  type t =
    { command : string
    ; arguments : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FileChangeType : sig
  type t =
    | Created
    | Changed
    | Deleted

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module FoldingRange : sig
  type t =
    { startLine : int
    ; startCharacter : int option [@yojson.option]
    ; endLine : int
    ; endCharacter : int option [@yojson.option]
    ; kind : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FoldingRangeKind : sig
  type t =
    | Comment
    | Imports
    | Region

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module FoldingRangeParams : sig
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module MarkedString : sig end

module Hover : sig
  type t =
    { contents : unit
    ; range : Range.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module InitializeError : sig
  type t = UnknownProtocolVersion

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module InitializeParams : sig
  type t =
    { processId : unit
    ; clientInfo : clientInfo option [@yojson.option]
    ; rootPath : unit option [@yojson.option]
    ; rootUri : unit
    ; initializationOptions : Json.t option [@yojson.option]
    ; capabilities : ClientCapabilities.t
    ; trace : unit option [@yojson.option]
    ; workspaceFolders : unit option [@yojson.option]
    }

  and clientInfo =
    { name : string
    ; version : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceFoldersServerCapabilities : sig
  type t =
    { supported : bool option [@yojson.option]
    ; changeNotifications : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameOptions : sig
  type t = { prepareProvider : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelpOptions : sig
  type t =
    { triggerCharacters : unit option [@yojson.option]
    ; retriggerCharacters : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SaveOptions : sig
  type t = { includeText : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentSyncOptions : sig
  type t =
    { openClose : bool option [@yojson.option]
    ; change : int option [@yojson.option]
    ; willSave : bool option [@yojson.option]
    ; willSaveWaitUntil : bool option [@yojson.option]
    ; save : SaveOptions.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ServerCapabilities : sig
  type t =
    { textDocumentSync : unit option [@yojson.option]
    ; completionProvider : CompletionOptions.t option [@yojson.option]
    ; hoverProvider : unit option [@yojson.option]
    ; signatureHelpProvider : SignatureHelpOptions.t option [@yojson.option]
    ; declarationProvider : unit option [@yojson.option]
    ; definitionProvider : unit option [@yojson.option]
    ; typeDefinitionProvider : unit option [@yojson.option]
    ; implementationProvider : unit option [@yojson.option]
    ; referencesProvider : unit option [@yojson.option]
    ; documentHighlightProvider : unit option [@yojson.option]
    ; documentSymbolProvider : unit option [@yojson.option]
    ; codeActionProvider : unit option [@yojson.option]
    ; codeLensProvider : CodeLensOptions.t option [@yojson.option]
    ; documentLinkProvider : DocumentLinkOptions.t option [@yojson.option]
    ; colorProvider : unit option [@yojson.option]
    ; documentFormattingProvider : unit option [@yojson.option]
    ; documentRangeFormattingProvider : unit option [@yojson.option]
    ; documentOnTypeFormattingProvider :
        DocumentOnTypeFormattingOptions.t option
          [@yojson.option]
    ; renameProvider : unit option [@yojson.option]
    ; foldingRangeProvider : unit option [@yojson.option]
    ; executeCommandProvider : ExecuteCommandOptions.t option [@yojson.option]
    ; selectionRangeProvider : unit option [@yojson.option]
    ; workspaceSymbolProvider : bool option [@yojson.option]
    ; workspace : workspace option [@yojson.option]
    ; experimental : Json.t option [@yojson.option]
    }

  and workspace =
    { workspaceFolders : WorkspaceFoldersServerCapabilities.t option
          [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module InitializeResult : sig
  type t =
    { capabilities : ServerCapabilities.t
    ; serverInfo : serverInfo option [@yojson.option]
    }

  and serverInfo =
    { name : string
    ; version : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module LocationLink : sig
  type t =
    { originSelectionRange : Range.t option [@yojson.option]
    ; targetUri : DocumentUri.t
    ; targetRange : Range.t
    ; targetSelectionRange : Range.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module LogMessageParams : sig
  type t =
    { type_ : int [@key "type"]
    ; message : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Message : sig
  type t = { jsonrpc : string }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module MessageActionItem : sig
  type t = { title : string } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module MessageType : sig
  type t =
    | Error
    | Warning
    | Info
    | Log

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module NotificationMessage : sig
  type t =
    { method_ : string [@key "method"]
    ; params : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ParameterInformation : sig
  type t =
    { label : unit
    ; documentation : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ProgressParams : sig
  type t =
    { token : ProgressToken.t
    ; value : Json.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module PublishDiagnosticsParams : sig
  type t =
    { uri : DocumentUri.t
    ; version : int option [@yojson.option]
    ; diagnostics : unit
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceContext : sig
  type t = { includeDeclaration : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceParams : sig
  type t = { context : ReferenceContext.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Registration : sig
  type t =
    { id : string
    ; method_ : string [@key "method"]
    ; registerOptions : Json.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RegistrationParams : sig
  type t = { registrations : unit }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameParams : sig
  type t = { newName : string }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RequestMessage : sig
  type t =
    { id : unit
    ; method_ : string [@key "method"]
    ; params : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ResponseError : sig
  type t =
    { code : int
    ; message : string
    ; data : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ResponseMessage : sig
  type t =
    { id : unit
    ; result : unit option [@yojson.option]
    ; error : ResponseError.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRange : sig
  type t =
    { range : Range.t
    ; parent : t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRangeParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; positions : unit
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ShowMessageParams : sig
  type t =
    { type_ : int [@key "type"]
    ; message : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ShowMessageRequestParams : sig
  type t =
    { type_ : int [@key "type"]
    ; message : string
    ; actions : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureInformation : sig
  type t =
    { label : string
    ; documentation : unit option [@yojson.option]
    ; parameters : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelp : sig
  type t =
    { signatures : unit
    ; activeSignature : int option [@yojson.option]
    ; activeParameter : int option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelpTriggerKind : sig
  type t =
    | Invoked
    | TriggerCharacter
    | ContentChange

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module SignatureHelpContext : sig
  type t =
    { triggerKind : SignatureHelpTriggerKind.t
    ; triggerCharacter : string option [@yojson.option]
    ; isRetrigger : bool
    ; activeSignatureHelp : SignatureHelp.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelpParams : sig
  type t = { context : SignatureHelpContext.t option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SymbolInformation : sig
  type t =
    { name : string
    ; kind : SymbolKind.t
    ; deprecated : bool option [@yojson.option]
    ; location : Location.t
    ; containerName : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentSyncKind : sig
  type t =
    | None
    | Full
    | Incremental

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module TextDocumentChangeRegistrationOptions : sig
  type t = { syncKind : TextDocumentSyncKind.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentSaveReason : sig
  type t =
    | Manual
    | AfterDelay
    | FocusOut

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module TextDocumentSaveRegistrationOptions : sig
  type t = { includeText : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Unregistration : sig
  type t =
    { id : string
    ; method_ : string [@key "method"]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module UnregistrationParams : sig
  type t = { unregisterations : unit }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WatchKind : sig
  type t =
    | Create
    | Change
    | Delete

  val to_yojson : t -> Json.t

  val of_yojson : Json.t -> t
end

module WillSaveTextDocumentParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; reason : int
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressBegin : sig
  type t =
    { kind : unit
    ; title : string
    ; cancellable : bool option [@yojson.option]
    ; message : string option [@yojson.option]
    ; percentage : int option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressCancelParams : sig
  type t = { token : ProgressToken.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressCreateParams : sig
  type t = { token : ProgressToken.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressEnd : sig
  type t =
    { kind : unit
    ; message : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressReport : sig
  type t =
    { kind : unit
    ; cancellable : bool option [@yojson.option]
    ; message : string option [@yojson.option]
    ; percentage : int option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolParams : sig
  type t = { query : string } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

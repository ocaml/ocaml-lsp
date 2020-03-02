open! Import

[@@@warning "-30"]

module DeleteFileOptions = struct
  type t =
    { recursive : bool
    ; ignoreIfNotExists : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentUri = struct
  type t = string
end

module DeleteFile = struct
  type t =
    { kind : unit
    ; uri : DocumentUri.t
    ; options : DeleteFileOptions.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameFileOptions = struct
  type t =
    { overwrite : bool
    ; ignoreIfExists : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameFile = struct
  type t =
    { kind : unit
    ; oldUri : DocumentUri.t
    ; newUri : DocumentUri.t
    ; options : RenameFileOptions.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CreateFileOptions = struct
  type t =
    { overwrite : bool
    ; ignoreIfExists : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CreateFile = struct
  type t =
    { kind : unit
    ; uri : DocumentUri.t
    ; options : CreateFileOptions.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Position = struct
  type t =
    { line : int
    ; character : int
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Range = struct
  type t =
    { start : Position.t
    ; end_ : Position.t [@key "end"]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextEdit = struct
  type t =
    { range : Range.t
    ; newText : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentIdentifier = struct
  type t = { uri : DocumentUri.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module VersionedTextDocumentIdentifier = struct
  type t = { version : int option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentEdit = struct
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; edits : TextEdit.t list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceEdit = struct
  type changes = { uri : unit }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { changes : changes
    ; documentChanges : unit
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ApplyWorkspaceEditParams = struct
  type t =
    { label : string
    ; edit : WorkspaceEdit.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ApplyWorkspaceEditResponse = struct
  type t =
    { applied : bool
    ; failureReason : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CancelParams = struct
  type t = { id : Jsonrpc.Id.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRangeClientCapabilities = struct
  type t = { dynamicRegistration : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FoldingRangeClientCapabilities = struct
  type t =
    { dynamicRegistration : bool
    ; rangeLimit : int
    ; lineFoldingOnly : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DiagnosticTag = struct
  type t =
    | Unnecessary
    | Deprecated
end

module PublishDiagnosticsClientCapabilities = struct
  type tagSupport = { valueSet : DiagnosticTag.t list }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { relatedInformation : bool
    ; tagSupport : tagSupport
    ; versionSupport : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameClientCapabilities = struct
  type t =
    { dynamicRegistration : bool
    ; prepareSupport : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentOnTypeFormattingClientCapabilities = struct
  type t = { dynamicRegistration : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentRangeFormattingClientCapabilities = struct
  type t = { dynamicRegistration : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFormattingClientCapabilities = struct
  type t = { dynamicRegistration : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentColorClientCapabilities = struct
  type t = { dynamicRegistration : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkClientCapabilities = struct
  type t =
    { dynamicRegistration : bool
    ; tooltipSupport : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensClientCapabilities = struct
  type t = { dynamicRegistration : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionKind = struct
  type t =
    | Empty
    | QuickFix
    | Refactor
    | RefactorExtract
    | RefactorInline
    | RefactorRewrite
    | Source
    | SourceOrganizeImports
end

module CodeActionClientCapabilities = struct
  type codeActionKind = { valueSet : CodeActionKind.t list }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type codeActionLiteralSupport = { codeActionKind : codeActionKind }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool
    ; codeActionLiteralSupport : codeActionLiteralSupport
    ; isPreferredSupport : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SymbolKind = struct
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
end

module DocumentSymbolClientCapabilities = struct
  type symbolKind = { valueSet : SymbolKind.t list }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool
    ; symbolKind : symbolKind
    ; hierarchicalDocumentSymbolSupport : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlightClientCapabilities = struct
  type t = { dynamicRegistration : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceClientCapabilities = struct
  type t = { dynamicRegistration : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ImplementationClientCapabilities = struct
  type t =
    { dynamicRegistration : bool
    ; linkSupport : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TypeDefinitionClientCapabilities = struct
  type t =
    { dynamicRegistration : bool
    ; linkSupport : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DefinitionClientCapabilities = struct
  type t =
    { dynamicRegistration : bool
    ; linkSupport : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DeclarationClientCapabilities = struct
  type t =
    { dynamicRegistration : bool
    ; linkSupport : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module MarkupKind = struct
  type t =
    | PlainText
    | Markdown
end

module SignatureHelpClientCapabilities = struct
  type parameterInformation = { labelOffsetSupport : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type signatureInformation =
    { documentationFormat : MarkupKind.t list
    ; parameterInformation : parameterInformation
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool
    ; signatureInformation : signatureInformation
    ; contextSupport : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module HoverClientCapabilities = struct
  type t =
    { dynamicRegistration : bool
    ; contentFormat : MarkupKind.t list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionItemKind = struct
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
end

module CompletionItemTag = struct
  type t = Deprecated
end

module CompletionClientCapabilities = struct
  type completionItemKind = { valueSet : CompletionItemKind.t list }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type tagSupport = { valueSet : CompletionItemTag.t list }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type completionItem =
    { snippetSupport : bool
    ; commitCharactersSupport : bool
    ; documentationFormat : MarkupKind.t list
    ; deprecatedSupport : bool
    ; preselectSupport : bool
    ; tagSupport : tagSupport
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool
    ; completionItem : completionItem
    ; completionItemKind : completionItemKind
    ; contextSupport : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentSyncClientCapabilities = struct
  type t =
    { dynamicRegistration : bool
    ; willSave : bool
    ; willSaveWaitUntil : bool
    ; didSave : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentClientCapabilities = struct
  type t =
    { synchronization : TextDocumentSyncClientCapabilities.t
    ; completion : CompletionClientCapabilities.t
    ; hover : HoverClientCapabilities.t
    ; signatureHelp : SignatureHelpClientCapabilities.t
    ; declaration : DeclarationClientCapabilities.t
    ; definition : DefinitionClientCapabilities.t
    ; typeDefinition : TypeDefinitionClientCapabilities.t
    ; implementation : ImplementationClientCapabilities.t
    ; references : ReferenceClientCapabilities.t
    ; documentHighlight : DocumentHighlightClientCapabilities.t
    ; documentSymbol : DocumentSymbolClientCapabilities.t
    ; codeAction : CodeActionClientCapabilities.t
    ; codeLens : CodeLensClientCapabilities.t
    ; documentLink : DocumentLinkClientCapabilities.t
    ; colorProvider : DocumentColorClientCapabilities.t
    ; formatting : DocumentFormattingClientCapabilities.t
    ; rangeFormatting : DocumentRangeFormattingClientCapabilities.t
    ; onTypeFormatting : DocumentOnTypeFormattingClientCapabilities.t
    ; rename : RenameClientCapabilities.t
    ; publishDiagnostics : PublishDiagnosticsClientCapabilities.t
    ; foldingRange : FoldingRangeClientCapabilities.t
    ; selectionRange : SelectionRangeClientCapabilities.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ExecuteCommandClientCapabilities = struct
  type t = { dynamicRegistration : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolClientCapabilities = struct
  type symbolKind = { valueSet : SymbolKind.t list }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool
    ; symbolKind : symbolKind
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeWatchedFilesClientCapabilities = struct
  type t = { dynamicRegistration : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeConfigurationClientCapabilities = struct
  type t = { dynamicRegistration : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FailureHandlingKind = struct
  type t =
    | Abort
    | Transactional
    | TextOnlyTransactional
    | Undo
end

module ResourceOperationKind = struct
  type t =
    | Create
    | Rename
    | Delete
end

module WorkspaceEditClientCapabilities = struct
  type t =
    { documentChanges : bool
    ; resourceOperations : ResourceOperationKind.t list
    ; failureHandling : FailureHandlingKind.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ClientCapabilities = struct
  type window = { workDoneProgress : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type workspace =
    { applyEdit : bool
    ; workspaceEdit : WorkspaceEditClientCapabilities.t
    ; didChangeConfiguration : DidChangeConfigurationClientCapabilities.t
    ; didChangeWatchedFiles : DidChangeWatchedFilesClientCapabilities.t
    ; symbol : WorkspaceSymbolClientCapabilities.t
    ; executeCommand : ExecuteCommandClientCapabilities.t
    ; workspaceFolders : bool
    ; configuration : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { workspace : workspace
    ; textDocument : TextDocumentClientCapabilities.t
    ; window : window
    ; experimental : Json.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Command = struct
  type t =
    { title : string
    ; command : string
    ; arguments : Json.t list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Location = struct
  type t =
    { uri : DocumentUri.t
    ; range : Range.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DiagnosticRelatedInformation = struct
  type t =
    { location : Location.t
    ; message : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DiagnosticSeverity = struct
  type t =
    | Error
    | Warning
    | Information
    | Hint
end

module Diagnostic = struct
  type t =
    { range : Range.t
    ; severity : DiagnosticSeverity.t
    ; code : Jsonrpc.Id.t
    ; source : string
    ; message : string
    ; tags : DiagnosticTag.t list
    ; relatedInformation : DiagnosticRelatedInformation.t list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeAction = struct
  type t =
    { title : string
    ; kind : CodeActionKind.t
    ; diagnostics : Diagnostic.t list
    ; isPreferred : bool
    ; edit : WorkspaceEdit.t
    ; command : Command.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionContext = struct
  type t =
    { diagnostics : Diagnostic.t list
    ; only : CodeActionKind.t list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressOptions = struct
  type t = { workDoneProgress : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionOptions = struct
  type t = { codeActionKinds : CodeActionKind.t list }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ProgressToken = struct
  type t = Jsonrpc.Id.t
end

module PartialResultParams = struct
  type t = { partialResultToken : ProgressToken.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressParams = struct
  type t = { workDoneToken : ProgressToken.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; context : CodeActionContext.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFilter = struct
  type t =
    { language : string
    ; scheme : string
    ; pattern : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSelector = struct
  type t = DocumentFilter.t list
end

module TextDocumentRegistrationOptions = struct
  type t = { documentSelector : DocumentSelector.t option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionRegistrationOptions = struct end

module CodeLens = struct
  type t =
    { range : Range.t
    ; command : Command.t
    ; data : Json.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensOptions = struct
  type t = { resolveProvider : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensRegistrationOptions = struct end

module Color = struct
  type t =
    { red : int
    ; green : int
    ; blue : int
    ; alpha : int
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ColorInformation = struct
  type t =
    { range : Range.t
    ; color : Color.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ColorPresentation = struct
  type t =
    { label : string
    ; textEdit : TextEdit.t
    ; additionalTextEdits : TextEdit.t list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ColorPresentationParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; color : Color.t
    ; range : Range.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionTriggerKind = struct
  type t =
    | Invoked
    | TriggerCharacter
    | TriggerForIncompleteCompletions
end

module CompletionContext = struct
  type t =
    { triggerKind : CompletionTriggerKind.t
    ; triggerCharacter : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module InsertTextFormat = struct
  type t =
    | PlainText
    | Snippet
end

module MarkupContent = struct
  type t =
    { kind : MarkupKind.t
    ; value : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionItem = struct
  type t =
    { label : string
    ; kind : int
    ; tags : CompletionItemTag.t list
    ; detail : string
    ; documentation : unit
    ; deprecated : bool
    ; preselect : bool
    ; sortText : string
    ; filterText : string
    ; insertText : string
    ; insertTextFormat : InsertTextFormat.t
    ; textEdit : TextEdit.t
    ; additionalTextEdits : TextEdit.t list
    ; commitCharacters : string list
    ; command : Command.t
    ; data : Json.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionList = struct
  type t =
    { isIncomplete : bool
    ; items : CompletionItem.t list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionOptions = struct
  type t =
    { triggerCharacters : string list
    ; allCommitCharacters : string list
    ; resolveProvider : bool
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentPositionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionParams = struct
  type t = { context : CompletionContext.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionRegistrationOptions = struct end

module ConfigurationItem = struct
  type t =
    { scopeUri : DocumentUri.t
    ; section : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ConfigurationParams = struct
  type t = { items : ConfigurationItem.t list }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DeclarationOptions = struct end

module DeclarationParams = struct end

module StaticRegistrationOptions = struct
  type t = { id : string } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DeclarationRegistrationOptions = struct end

module DefinitionOptions = struct end

module DefinitionParams = struct end

module DefinitionRegistrationOptions = struct end

module DidChangeConfigurationParams = struct
  type t = { settings : Json.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentContentChangeEvent = struct
  type t = unit
end

module DidChangeTextDocumentParams = struct
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; contentChanges : TextDocumentContentChangeEvent.t list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FileEvent = struct
  type t =
    { uri : DocumentUri.t
    ; type_ : int [@key "type"]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeWatchedFilesParams = struct
  type t = { changes : FileEvent.t list }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FileSystemWatcher = struct
  type t =
    { globPattern : string
    ; kind : int
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeWatchedFilesRegistrationOptions = struct
  type t = { watchers : FileSystemWatcher.t list }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceFolder = struct
  type t =
    { uri : DocumentUri.t
    ; name : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceFoldersChangeEvent = struct
  type t =
    { added : WorkspaceFolder.t list
    ; removed : WorkspaceFolder.t list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeWorkspaceFoldersParams = struct
  type t = { event : WorkspaceFoldersChangeEvent.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidCloseTextDocumentParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentItem = struct
  type t =
    { uri : DocumentUri.t
    ; languageId : string
    ; version : int
    ; text : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidOpenTextDocumentParams = struct
  type t = { textDocument : TextDocumentItem.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidSaveTextDocumentParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; text : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentColorOptions = struct end

module DocumentColorParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentColorRegistrationOptions = struct end

module DocumentFormattingOptions = struct end

module FormattingOptions = struct
  type t =
    { tabSize : int
    ; insertSpaces : bool
    ; trimTrailingWhitespace : bool
    ; insertFinalNewline : bool
    ; trimFinalNewlines : bool
    ; key : unit
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFormattingParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; options : FormattingOptions.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFormattingRegistrationOptions = struct end

module DocumentHighlight = struct
  type t =
    { range : Range.t
    ; kind : int
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlightKind = struct
  type t =
    | Text
    | Read
    | Write
end

module DocumentHighlightOptions = struct end

module DocumentHighlightParams = struct end

module DocumentHighlightRegistrationOptions = struct end

module DocumentLink = struct
  type t =
    { range : Range.t
    ; target : DocumentUri.t
    ; tooltip : string
    ; data : Json.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkOptions = struct
  type t = { resolveProvider : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkRegistrationOptions = struct end

module DocumentOnTypeFormattingOptions = struct
  type t =
    { firstTriggerCharacter : string
    ; moreTriggerCharacter : string list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentOnTypeFormattingParams = struct
  type t =
    { ch : string
    ; options : FormattingOptions.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentOnTypeFormattingRegistrationOptions = struct end

module DocumentRangeFormattingOptions = struct end

module DocumentRangeFormattingParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; options : FormattingOptions.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentRangeFormattingRegistrationOptions = struct end

module DocumentSymbol = struct
  type t =
    { name : string
    ; detail : string
    ; kind : SymbolKind.t
    ; deprecated : bool
    ; range : Range.t
    ; selectionRange : Range.t
    ; children : t list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSymbolOptions = struct end

module DocumentSymbolParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSymbolRegistrationOptions = struct end

module ErrorCodes = struct
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
end

module ExecuteCommandOptions = struct
  type t = { commands : string list }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ExecuteCommandParams = struct
  type t =
    { command : string
    ; arguments : Json.t list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ExecuteCommandRegistrationOptions = struct end

module FileChangeType = struct
  type t =
    | Created
    | Changed
    | Deleted
end

module FoldingRange = struct
  type t =
    { startLine : int
    ; startCharacter : int
    ; endLine : int
    ; endCharacter : int
    ; kind : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FoldingRangeKind = struct
  type t =
    | Comment
    | Imports
    | Region
end

module FoldingRangeOptions = struct end

module FoldingRangeParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FoldingRangeRegistrationOptions = struct end

module MarkedString = struct
  type t = unit
end

module Hover = struct
  type t =
    { contents : unit
    ; range : Range.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module HoverOptions = struct end

module HoverParams = struct end

module HoverRegistrationOptions = struct end

module ImplementationOptions = struct end

module ImplementationParams = struct end

module ImplementationRegistrationOptions = struct end

module InitializeError = struct
  type t = UnknownProtocolVersion
end

module InitializeParams = struct
  type clientInfo =
    { name : string
    ; version : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { processId : int option [@yojson.option]
    ; clientInfo : clientInfo
    ; rootPath : string option [@yojson.option]
    ; rootUri : DocumentUri.t option [@yojson.option]
    ; initializationOptions : Json.t
    ; capabilities : ClientCapabilities.t
    ; trace : unit
    ; workspaceFolders : WorkspaceFolder.t list option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceFoldersServerCapabilities = struct
  type t =
    { supported : bool
    ; changeNotifications : unit
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRangeOptions = struct end

module SelectionRangeRegistrationOptions = struct end

module RenameOptions = struct
  type t = { prepareProvider : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceOptions = struct end

module TypeDefinitionOptions = struct end

module TypeDefinitionRegistrationOptions = struct end

module SignatureHelpOptions = struct
  type t =
    { triggerCharacters : string list
    ; retriggerCharacters : string list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SaveOptions = struct
  type t = { includeText : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentSyncOptions = struct
  type t =
    { openClose : bool
    ; change : int
    ; willSave : bool
    ; willSaveWaitUntil : bool
    ; save : SaveOptions.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ServerCapabilities = struct
  type workspace = { workspaceFolders : WorkspaceFoldersServerCapabilities.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { textDocumentSync : unit
    ; completionProvider : CompletionOptions.t
    ; hoverProvider : unit
    ; signatureHelpProvider : SignatureHelpOptions.t
    ; declarationProvider : unit
    ; definitionProvider : unit
    ; typeDefinitionProvider : unit
    ; implementationProvider : unit
    ; referencesProvider : unit
    ; documentHighlightProvider : unit
    ; documentSymbolProvider : unit
    ; codeActionProvider : unit
    ; codeLensProvider : CodeLensOptions.t
    ; documentLinkProvider : DocumentLinkOptions.t
    ; colorProvider : unit
    ; documentFormattingProvider : unit
    ; documentRangeFormattingProvider : unit
    ; documentOnTypeFormattingProvider : DocumentOnTypeFormattingOptions.t
    ; renameProvider : unit
    ; foldingRangeProvider : unit
    ; executeCommandProvider : ExecuteCommandOptions.t
    ; selectionRangeProvider : unit
    ; workspaceSymbolProvider : bool
    ; workspace : workspace
    ; experimental : Json.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module InitializeResult = struct
  type serverInfo =
    { name : string
    ; version : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { capabilities : ServerCapabilities.t
    ; serverInfo : serverInfo
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module InitializedParams = struct end

module LocationLink = struct
  type t =
    { originSelectionRange : Range.t
    ; targetUri : DocumentUri.t
    ; targetRange : Range.t
    ; targetSelectionRange : Range.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module LogMessageParams = struct
  type t =
    { type_ : int [@key "type"]
    ; message : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Message = struct
  type t = { jsonrpc : string }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module MessageActionItem = struct
  type t = { title : string } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module MessageType = struct
  type t =
    | Error
    | Warning
    | Info
    | Log
end

module NotificationMessage = struct
  type t =
    { method_ : string [@key "method"]
    ; params : unit
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ParameterInformation = struct
  type t =
    { label : unit
    ; documentation : unit
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module PrepareRenameParams = struct end

module ProgressParams = struct
  type t =
    { token : ProgressToken.t
    ; value : Json.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module PublishDiagnosticsParams = struct
  type t =
    { uri : DocumentUri.t
    ; version : int
    ; diagnostics : Diagnostic.t list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceContext = struct
  type t = { includeDeclaration : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceParams = struct
  type t = { context : ReferenceContext.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceRegistrationOptions = struct end

module Registration = struct
  type t =
    { id : string
    ; method_ : string [@key "method"]
    ; registerOptions : Json.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RegistrationParams = struct
  type t = { registrations : Registration.t list }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameParams = struct
  type t = { newName : string }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameRegistrationOptions = struct end

module RequestMessage = struct
  type t =
    { id : Jsonrpc.Id.t
    ; method_ : string [@key "method"]
    ; params : unit
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ResponseError = struct
  type t =
    { code : int
    ; message : string
    ; data : Json.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ResponseMessage = struct
  type t =
    { id : Jsonrpc.Id.t option [@yojson.option]
    ; result : unit option [@yojson.option]
    ; error : ResponseError.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRange = struct
  type t =
    { range : Range.t
    ; parent : t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRangeParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; positions : Position.t list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ShowMessageParams = struct
  type t =
    { type_ : int [@key "type"]
    ; message : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ShowMessageRequestParams = struct
  type t =
    { type_ : int [@key "type"]
    ; message : string
    ; actions : MessageActionItem.t list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureInformation = struct
  type t =
    { label : string
    ; documentation : unit
    ; parameters : ParameterInformation.t list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelp = struct
  type t =
    { signatures : SignatureInformation.t list
    ; activeSignature : int
    ; activeParameter : int
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelpTriggerKind = struct
  type t =
    | Invoked
    | TriggerCharacter
    | ContentChange
end

module SignatureHelpContext = struct
  type t =
    { triggerKind : SignatureHelpTriggerKind.t
    ; triggerCharacter : string
    ; isRetrigger : bool
    ; activeSignatureHelp : SignatureHelp.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelpParams = struct
  type t = { context : SignatureHelpContext.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelpRegistrationOptions = struct end

module SymbolInformation = struct
  type t =
    { name : string
    ; kind : SymbolKind.t
    ; deprecated : bool
    ; location : Location.t
    ; containerName : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentSyncKind = struct
  type t =
    | None
    | Full
    | Incremental
end

module TextDocumentChangeRegistrationOptions = struct
  type t = { syncKind : TextDocumentSyncKind.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentSaveReason = struct
  type t =
    | Manual
    | AfterDelay
    | FocusOut
end

module TextDocumentSaveRegistrationOptions = struct
  type t = { includeText : bool }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TypeDefinitionParams = struct end

module Unregistration = struct
  type t =
    { id : string
    ; method_ : string [@key "method"]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module UnregistrationParams = struct
  type t = { unregisterations : Unregistration.t list }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WatchKind = struct
  type t =
    | Create
    | Change
    | Delete
end

module WillSaveTextDocumentParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; reason : int
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressBegin = struct
  type t =
    { kind : unit
    ; title : string
    ; cancellable : bool
    ; message : string
    ; percentage : int
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressCancelParams = struct
  type t = { token : ProgressToken.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressCreateParams = struct
  type t = { token : ProgressToken.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressEnd = struct
  type t =
    { kind : unit
    ; message : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressReport = struct
  type t =
    { kind : unit
    ; cancellable : bool
    ; message : string
    ; percentage : int
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolOptions = struct end

module WorkspaceSymbolParams = struct
  type t = { query : string } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolRegistrationOptions = struct end

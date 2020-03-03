open! Import

[@@@warning "-30"]

module DeleteFileOptions : sig
  type t =
    { recursive : bool
    ; ignoreIfNotExists : bool
    }
end

module DocumentUri : sig
  type t = string
end

module DeleteFile : sig
  type t =
    { kind : unit
    ; uri : DocumentUri.t
    ; options : DeleteFileOptions.t
    }
end

module RenameFileOptions : sig
  type t =
    { overwrite : bool
    ; ignoreIfExists : bool
    }
end

module RenameFile : sig
  type t =
    { kind : unit
    ; oldUri : DocumentUri.t
    ; newUri : DocumentUri.t
    ; options : RenameFileOptions.t
    }
end

module CreateFileOptions : sig
  type t =
    { overwrite : bool
    ; ignoreIfExists : bool
    }
end

module CreateFile : sig
  type t =
    { kind : unit
    ; uri : DocumentUri.t
    ; options : CreateFileOptions.t
    }
end

module Position : sig
  type t =
    { line : int
    ; character : int
    }
end

module Range : sig
  type t =
    { start : Position.t
    ; end_ : Position.t [@key "end"]
    }
end

module TextEdit : sig
  type t =
    { range : Range.t
    ; newText : string
    }
end

module TextDocumentIdentifier : sig
  type t = { uri : DocumentUri.t }
end

module VersionedTextDocumentIdentifier : sig
  type t =
    { uri : DocumentUri.t
    ; version : int option [@yojson.option]
    }
end

module TextDocumentEdit : sig
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; edits : TextEdit.t list
    }
end

module WorkspaceEdit : sig
  type t =
    { changes : (DocumentUri.t * TextEdit.t list) list
    ; documentChanges :
        [ `TextDocumentEdit of TextDocumentEdit.t
        | `CreateFile of CreateFile.t
        | `RenameFile of RenameFile.t
        | `DeleteFile of DeleteFile.t
        ]
        list
    }
end

module ApplyWorkspaceEditParams : sig
  type t =
    { label : string
    ; edit : WorkspaceEdit.t
    }
end

module ApplyWorkspaceEditResponse : sig
  type t =
    { applied : bool
    ; failureReason : string
    }
end

module CancelParams : sig
  type t = { id : Jsonrpc.Id.t }
end

module SelectionRangeClientCapabilities : sig
  type t = { dynamicRegistration : bool }
end

module FoldingRangeClientCapabilities : sig
  type t =
    { dynamicRegistration : bool
    ; rangeLimit : int
    ; lineFoldingOnly : bool
    }
end

module DiagnosticTag : sig
  type t =
    | Unnecessary
    | Deprecated
end

module PublishDiagnosticsClientCapabilities : sig
  type tagSupport = { valueSet : DiagnosticTag.t list }

  type t =
    { relatedInformation : bool
    ; tagSupport : tagSupport
    ; versionSupport : bool
    }
end

module RenameClientCapabilities : sig
  type t =
    { dynamicRegistration : bool
    ; prepareSupport : bool
    }
end

module DocumentOnTypeFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool }
end

module DocumentRangeFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool }
end

module DocumentFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool }
end

module DocumentColorClientCapabilities : sig
  type t = { dynamicRegistration : bool }
end

module DocumentLinkClientCapabilities : sig
  type t =
    { dynamicRegistration : bool
    ; tooltipSupport : bool
    }
end

module CodeLensClientCapabilities : sig
  type t = { dynamicRegistration : bool }
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
end

module CodeActionClientCapabilities : sig
  type codeActionKind = { valueSet : CodeActionKind.t list }

  type codeActionLiteralSupport = { codeActionKind : codeActionKind }

  type t =
    { dynamicRegistration : bool
    ; codeActionLiteralSupport : codeActionLiteralSupport
    ; isPreferredSupport : bool
    }
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
end

module DocumentSymbolClientCapabilities : sig
  type symbolKind = { valueSet : SymbolKind.t list }

  type t =
    { dynamicRegistration : bool
    ; symbolKind : symbolKind
    ; hierarchicalDocumentSymbolSupport : bool
    }
end

module DocumentHighlightClientCapabilities : sig
  type t = { dynamicRegistration : bool }
end

module ReferenceClientCapabilities : sig
  type t = { dynamicRegistration : bool }
end

module ImplementationClientCapabilities : sig
  type t =
    { dynamicRegistration : bool
    ; linkSupport : bool
    }
end

module TypeDefinitionClientCapabilities : sig
  type t =
    { dynamicRegistration : bool
    ; linkSupport : bool
    }
end

module DefinitionClientCapabilities : sig
  type t =
    { dynamicRegistration : bool
    ; linkSupport : bool
    }
end

module DeclarationClientCapabilities : sig
  type t =
    { dynamicRegistration : bool
    ; linkSupport : bool
    }
end

module MarkupKind : sig
  type t =
    | PlainText
    | Markdown
end

module SignatureHelpClientCapabilities : sig
  type parameterInformation = { labelOffsetSupport : bool }

  type signatureInformation =
    { documentationFormat : MarkupKind.t list
    ; parameterInformation : parameterInformation
    }

  type t =
    { dynamicRegistration : bool
    ; signatureInformation : signatureInformation
    ; contextSupport : bool
    }
end

module HoverClientCapabilities : sig
  type t =
    { dynamicRegistration : bool
    ; contentFormat : MarkupKind.t list
    }
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
end

module CompletionItemTag : sig
  type t = Deprecated
end

module CompletionClientCapabilities : sig
  type completionItemKind = { valueSet : CompletionItemKind.t list }

  type tagSupport = { valueSet : CompletionItemTag.t list }

  type completionItem =
    { snippetSupport : bool
    ; commitCharactersSupport : bool
    ; documentationFormat : MarkupKind.t list
    ; deprecatedSupport : bool
    ; preselectSupport : bool
    ; tagSupport : tagSupport
    }

  type t =
    { dynamicRegistration : bool
    ; completionItem : completionItem
    ; completionItemKind : completionItemKind
    ; contextSupport : bool
    }
end

module TextDocumentSyncClientCapabilities : sig
  type t =
    { dynamicRegistration : bool
    ; willSave : bool
    ; willSaveWaitUntil : bool
    ; didSave : bool
    }
end

module TextDocumentClientCapabilities : sig
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
end

module ExecuteCommandClientCapabilities : sig
  type t = { dynamicRegistration : bool }
end

module WorkspaceSymbolClientCapabilities : sig
  type symbolKind = { valueSet : SymbolKind.t list }

  type t =
    { dynamicRegistration : bool
    ; symbolKind : symbolKind
    }
end

module DidChangeWatchedFilesClientCapabilities : sig
  type t = { dynamicRegistration : bool }
end

module DidChangeConfigurationClientCapabilities : sig
  type t = { dynamicRegistration : bool }
end

module FailureHandlingKind : sig
  type t =
    | Abort
    | Transactional
    | TextOnlyTransactional
    | Undo
end

module ResourceOperationKind : sig
  type t =
    | Create
    | Rename
    | Delete
end

module WorkspaceEditClientCapabilities : sig
  type t =
    { documentChanges : bool
    ; resourceOperations : ResourceOperationKind.t list
    ; failureHandling : FailureHandlingKind.t
    }
end

module ClientCapabilities : sig
  type window = { workDoneProgress : bool }

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

  type t =
    { workspace : workspace
    ; textDocument : TextDocumentClientCapabilities.t
    ; window : window
    ; experimental : Json.t
    }
end

module Command : sig
  type t =
    { title : string
    ; command : string
    ; arguments : Json.t list
    }
end

module Location : sig
  type t =
    { uri : DocumentUri.t
    ; range : Range.t
    }
end

module DiagnosticRelatedInformation : sig
  type t =
    { location : Location.t
    ; message : string
    }
end

module DiagnosticSeverity : sig
  type t =
    | Error
    | Warning
    | Information
    | Hint
end

module Diagnostic : sig
  type t =
    { range : Range.t
    ; severity : DiagnosticSeverity.t
    ; code : Jsonrpc.Id.t
    ; source : string
    ; message : string
    ; tags : DiagnosticTag.t list
    ; relatedInformation : DiagnosticRelatedInformation.t list
    }
end

module CodeAction : sig
  type t =
    { title : string
    ; kind : CodeActionKind.t
    ; diagnostics : Diagnostic.t list
    ; isPreferred : bool
    ; edit : WorkspaceEdit.t
    ; command : Command.t
    }
end

module CodeActionContext : sig
  type t =
    { diagnostics : Diagnostic.t list
    ; only : CodeActionKind.t list
    }
end

module WorkDoneProgressOptions : sig
  type t = { workDoneProgress : bool }
end

module CodeActionOptions : sig
  type t =
    { workDoneProgress : bool
    ; codeActionKinds : CodeActionKind.t list
    }
end

module ProgressToken : sig
  type t = Jsonrpc.Id.t
end

module PartialResultParams : sig
  type t = { partialResultToken : ProgressToken.t }
end

module WorkDoneProgressParams : sig
  type t = { workDoneToken : ProgressToken.t }
end

module CodeActionParams : sig
  type t =
    { workDoneToken : ProgressToken.t
    ; partialResultToken : ProgressToken.t
    ; textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; context : CodeActionContext.t
    }
end

module DocumentFilter : sig
  type t =
    { language : string
    ; scheme : string
    ; pattern : string
    }
end

module DocumentSelector : sig
  type t = DocumentFilter.t list
end

module TextDocumentRegistrationOptions : sig
  type t = { documentSelector : DocumentSelector.t option [@yojson.option] }
end

module CodeActionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool
    ; codeActionKinds : CodeActionKind.t list
    }
end

module CodeLens : sig
  type t =
    { range : Range.t
    ; command : Command.t
    ; data : Json.t
    }
end

module CodeLensOptions : sig
  type t =
    { workDoneProgress : bool
    ; resolveProvider : bool
    }
end

module CodeLensParams : sig
  type t =
    { workDoneToken : ProgressToken.t
    ; partialResultToken : ProgressToken.t
    ; textDocument : TextDocumentIdentifier.t
    }
end

module CodeLensRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool
    ; resolveProvider : bool
    }
end

module Color : sig
  type t =
    { red : int
    ; green : int
    ; blue : int
    ; alpha : int
    }
end

module ColorInformation : sig
  type t =
    { range : Range.t
    ; color : Color.t
    }
end

module ColorPresentation : sig
  type t =
    { label : string
    ; textEdit : TextEdit.t
    ; additionalTextEdits : TextEdit.t list
    }
end

module ColorPresentationParams : sig
  type t =
    { workDoneToken : ProgressToken.t
    ; partialResultToken : ProgressToken.t
    ; textDocument : TextDocumentIdentifier.t
    ; color : Color.t
    ; range : Range.t
    }
end

module CompletionTriggerKind : sig
  type t =
    | Invoked
    | TriggerCharacter
    | TriggerForIncompleteCompletions
end

module CompletionContext : sig
  type t =
    { triggerKind : CompletionTriggerKind.t
    ; triggerCharacter : string
    }
end

module InsertTextFormat : sig
  type t =
    | PlainText
    | Snippet
end

module MarkupContent : sig
  type t =
    { kind : MarkupKind.t
    ; value : string
    }
end

module CompletionItem : sig
  type t =
    { label : string
    ; kind : int
    ; tags : CompletionItemTag.t list
    ; detail : string
    ; documentation : [ `String of string | `MarkupContent of MarkupContent.t ]
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
end

module CompletionList : sig
  type t =
    { isIncomplete : bool
    ; items : CompletionItem.t list
    }
end

module CompletionOptions : sig
  type t =
    { workDoneProgress : bool
    ; triggerCharacters : string list
    ; allCommitCharacters : string list
    ; resolveProvider : bool
    }
end

module TextDocumentPositionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
end

module CompletionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t
    ; partialResultToken : ProgressToken.t
    ; context : CompletionContext.t
    }
end

module CompletionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool
    ; triggerCharacters : string list
    ; allCommitCharacters : string list
    ; resolveProvider : bool
    }
end

module ConfigurationItem : sig
  type t =
    { scopeUri : DocumentUri.t
    ; section : string
    }
end

module ConfigurationParams : sig
  type t = { items : ConfigurationItem.t list }
end

module DeclarationOptions : sig
  type t = { workDoneProgress : bool }
end

module DeclarationParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t
    ; partialResultToken : ProgressToken.t
    }
end

module StaticRegistrationOptions : sig
  type t = { id : string }
end

module DeclarationRegistrationOptions : sig
  type t =
    { workDoneProgress : bool
    ; documentSelector : DocumentSelector.t option [@yojson.option]
    ; id : string
    }
end

module DefinitionOptions : sig
  type t = { workDoneProgress : bool }
end

module DefinitionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t
    ; partialResultToken : ProgressToken.t
    }
end

module DefinitionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool
    }
end

module DidChangeConfigurationParams : sig
  type t = { settings : Json.t }
end

module TextDocumentContentChangeEvent : sig
  type t = unit
end

module DidChangeTextDocumentParams : sig
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; contentChanges : TextDocumentContentChangeEvent.t list
    }
end

module FileEvent : sig
  type t =
    { uri : DocumentUri.t
    ; type_ : int [@key "type"]
    }
end

module DidChangeWatchedFilesParams : sig
  type t = { changes : FileEvent.t list }
end

module FileSystemWatcher : sig
  type t =
    { globPattern : string
    ; kind : int
    }
end

module DidChangeWatchedFilesRegistrationOptions : sig
  type t = { watchers : FileSystemWatcher.t list }
end

module WorkspaceFolder : sig
  type t =
    { uri : DocumentUri.t
    ; name : string
    }
end

module WorkspaceFoldersChangeEvent : sig
  type t =
    { added : WorkspaceFolder.t list
    ; removed : WorkspaceFolder.t list
    }
end

module DidChangeWorkspaceFoldersParams : sig
  type t = { event : WorkspaceFoldersChangeEvent.t }
end

module DidCloseTextDocumentParams : sig
  type t = { textDocument : TextDocumentIdentifier.t }
end

module TextDocumentItem : sig
  type t =
    { uri : DocumentUri.t
    ; languageId : string
    ; version : int
    ; text : string
    }
end

module DidOpenTextDocumentParams : sig
  type t = { textDocument : TextDocumentItem.t }
end

module DidSaveTextDocumentParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; text : string
    }
end

module DocumentColorOptions : sig
  type t = { workDoneProgress : bool }
end

module DocumentColorParams : sig
  type t =
    { workDoneToken : ProgressToken.t
    ; partialResultToken : ProgressToken.t
    ; textDocument : TextDocumentIdentifier.t
    }
end

module DocumentColorRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; id : string
    ; workDoneProgress : bool
    }
end

module DocumentFormattingOptions : sig
  type t = { workDoneProgress : bool }
end

module FormattingOptions : sig
  type t =
    { tabSize : int
    ; insertSpaces : bool
    ; trimTrailingWhitespace : bool
    ; insertFinalNewline : bool
    ; trimFinalNewlines : bool
    ; key : (string * [ `Bool of bool | `Int of int | `String of string ]) list
    }
end

module DocumentFormattingParams : sig
  type t =
    { workDoneToken : ProgressToken.t
    ; textDocument : TextDocumentIdentifier.t
    ; options : FormattingOptions.t
    }
end

module DocumentFormattingRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool
    }
end

module DocumentHighlight : sig
  type t =
    { range : Range.t
    ; kind : int
    }
end

module DocumentHighlightKind : sig
  type t =
    | Text
    | Read
    | Write
end

module DocumentHighlightOptions : sig
  type t = { workDoneProgress : bool }
end

module DocumentHighlightParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t
    ; partialResultToken : ProgressToken.t
    }
end

module DocumentHighlightRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool
    }
end

module DocumentLink : sig
  type t =
    { range : Range.t
    ; target : DocumentUri.t
    ; tooltip : string
    ; data : Json.t
    }
end

module DocumentLinkOptions : sig
  type t =
    { workDoneProgress : bool
    ; resolveProvider : bool
    }
end

module DocumentLinkParams : sig
  type t =
    { workDoneToken : ProgressToken.t
    ; partialResultToken : ProgressToken.t
    ; textDocument : TextDocumentIdentifier.t
    }
end

module DocumentLinkRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool
    ; resolveProvider : bool
    }
end

module DocumentOnTypeFormattingOptions : sig
  type t =
    { firstTriggerCharacter : string
    ; moreTriggerCharacter : string list
    }
end

module DocumentOnTypeFormattingParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; ch : string
    ; options : FormattingOptions.t
    }
end

module DocumentOnTypeFormattingRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; firstTriggerCharacter : string
    ; moreTriggerCharacter : string list
    }
end

module DocumentRangeFormattingOptions : sig
  type t = { workDoneProgress : bool }
end

module DocumentRangeFormattingParams : sig
  type t =
    { workDoneToken : ProgressToken.t
    ; textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; options : FormattingOptions.t
    }
end

module DocumentRangeFormattingRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool
    }
end

module DocumentSymbol : sig
  type t =
    { name : string
    ; detail : string
    ; kind : SymbolKind.t
    ; deprecated : bool
    ; range : Range.t
    ; selectionRange : Range.t
    ; children : t list
    }
end

module DocumentSymbolOptions : sig
  type t = { workDoneProgress : bool }
end

module DocumentSymbolParams : sig
  type t =
    { workDoneToken : ProgressToken.t
    ; partialResultToken : ProgressToken.t
    ; textDocument : TextDocumentIdentifier.t
    }
end

module DocumentSymbolRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool
    }
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
end

module ExecuteCommandOptions : sig
  type t =
    { workDoneProgress : bool
    ; commands : string list
    }
end

module ExecuteCommandParams : sig
  type t =
    { workDoneToken : ProgressToken.t
    ; command : string
    ; arguments : Json.t list
    }
end

module ExecuteCommandRegistrationOptions : sig
  type t =
    { workDoneProgress : bool
    ; commands : string list
    }
end

module FileChangeType : sig
  type t =
    | Created
    | Changed
    | Deleted
end

module FoldingRange : sig
  type t =
    { startLine : int
    ; startCharacter : int
    ; endLine : int
    ; endCharacter : int
    ; kind : string
    }
end

module FoldingRangeKind : sig
  type t =
    | Comment
    | Imports
    | Region
end

module FoldingRangeOptions : sig
  type t = { workDoneProgress : bool }
end

module FoldingRangeParams : sig
  type t =
    { workDoneToken : ProgressToken.t
    ; partialResultToken : ProgressToken.t
    ; textDocument : TextDocumentIdentifier.t
    }
end

module FoldingRangeRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool
    ; id : string
    }
end

module MarkedString : sig
  type t = unit
end

module Hover : sig
  type t =
    { contents :
        [ `MarkedString of MarkedString.t
        | `List of MarkedString.t list
        | `MarkupContent of MarkupContent.t
        ]
    ; range : Range.t
    }
end

module HoverOptions : sig
  type t = { workDoneProgress : bool }
end

module HoverParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t
    }
end

module HoverRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool
    }
end

module ImplementationOptions : sig
  type t = { workDoneProgress : bool }
end

module ImplementationParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t
    ; partialResultToken : ProgressToken.t
    }
end

module ImplementationRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool
    ; id : string
    }
end

module InitializeError : sig
  type t = UnknownProtocolVersion
end

module InitializeParams : sig
  type clientInfo =
    { name : string
    ; version : string
    }

  type t =
    { workDoneToken : ProgressToken.t
    ; processId : int option [@yojson.option]
    ; clientInfo : clientInfo
    ; rootPath : string option [@yojson.option]
    ; rootUri : DocumentUri.t option [@yojson.option]
    ; initializationOptions : Json.t
    ; capabilities : ClientCapabilities.t
    ; trace : unit
    ; workspaceFolders : WorkspaceFolder.t list option [@yojson.option]
    }
end

module WorkspaceFoldersServerCapabilities : sig
  type t =
    { supported : bool
    ; changeNotifications : [ `String of string | `Bool of bool ]
    }
end

module SelectionRangeOptions : sig
  type t = { workDoneProgress : bool }
end

module SelectionRangeRegistrationOptions : sig
  type t =
    { workDoneProgress : bool
    ; documentSelector : DocumentSelector.t option [@yojson.option]
    ; id : string
    }
end

module RenameOptions : sig
  type t =
    { workDoneProgress : bool
    ; prepareProvider : bool
    }
end

module ReferenceOptions : sig
  type t = { workDoneProgress : bool }
end

module TypeDefinitionOptions : sig
  type t = { workDoneProgress : bool }
end

module TypeDefinitionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool
    ; id : string
    }
end

module SignatureHelpOptions : sig
  type t =
    { workDoneProgress : bool
    ; triggerCharacters : string list
    ; retriggerCharacters : string list
    }
end

module SaveOptions : sig
  type t = { includeText : bool }
end

module TextDocumentSyncOptions : sig
  type t =
    { openClose : bool
    ; change : int
    ; willSave : bool
    ; willSaveWaitUntil : bool
    ; save : SaveOptions.t
    }
end

module ServerCapabilities : sig
  type workspace = { workspaceFolders : WorkspaceFoldersServerCapabilities.t }

  type t =
    { textDocumentSync :
        [ `TextDocumentSyncOptions of TextDocumentSyncOptions.t | `Int of int ]
    ; completionProvider : CompletionOptions.t
    ; hoverProvider : [ `Bool of bool | `HoverOptions of HoverOptions.t ]
    ; signatureHelpProvider : SignatureHelpOptions.t
    ; declarationProvider :
        [ `Bool of bool
        | `DeclarationOptions of DeclarationOptions.t
        | `DeclarationRegistrationOptions of DeclarationRegistrationOptions.t
        ]
    ; definitionProvider :
        [ `Bool of bool | `DefinitionOptions of DefinitionOptions.t ]
    ; typeDefinitionProvider :
        [ `Bool of bool
        | `TypeDefinitionOptions of TypeDefinitionOptions.t
        | `TypeDefinitionRegistrationOptions of
          TypeDefinitionRegistrationOptions.t
        ]
    ; implementationProvider :
        [ `Bool of bool
        | `ImplementationOptions of ImplementationOptions.t
        | `ImplementationRegistrationOptions of
          ImplementationRegistrationOptions.t
        ]
    ; referencesProvider :
        [ `Bool of bool | `ReferenceOptions of ReferenceOptions.t ]
    ; documentHighlightProvider :
        [ `Bool of bool
        | `DocumentHighlightOptions of DocumentHighlightOptions.t
        ]
    ; documentSymbolProvider :
        [ `Bool of bool | `DocumentSymbolOptions of DocumentSymbolOptions.t ]
    ; codeActionProvider :
        [ `Bool of bool | `CodeActionOptions of CodeActionOptions.t ]
    ; codeLensProvider : CodeLensOptions.t
    ; documentLinkProvider : DocumentLinkOptions.t
    ; colorProvider :
        [ `Bool of bool
        | `DocumentColorOptions of DocumentColorOptions.t
        | `DocumentColorRegistrationOptions of
          DocumentColorRegistrationOptions.t
        ]
    ; documentFormattingProvider :
        [ `Bool of bool
        | `DocumentFormattingOptions of DocumentFormattingOptions.t
        ]
    ; documentRangeFormattingProvider :
        [ `Bool of bool
        | `DocumentRangeFormattingOptions of DocumentRangeFormattingOptions.t
        ]
    ; documentOnTypeFormattingProvider : DocumentOnTypeFormattingOptions.t
    ; renameProvider : [ `Bool of bool | `RenameOptions of RenameOptions.t ]
    ; foldingRangeProvider :
        [ `Bool of bool
        | `FoldingRangeOptions of FoldingRangeOptions.t
        | `FoldingRangeRegistrationOptions of FoldingRangeRegistrationOptions.t
        ]
    ; executeCommandProvider : ExecuteCommandOptions.t
    ; selectionRangeProvider :
        [ `Bool of bool
        | `SelectionRangeOptions of SelectionRangeOptions.t
        | `SelectionRangeRegistrationOptions of
          SelectionRangeRegistrationOptions.t
        ]
    ; workspaceSymbolProvider : bool
    ; workspace : workspace
    ; experimental : Json.t
    }
end

module InitializeResult : sig
  type serverInfo =
    { name : string
    ; version : string
    }

  type t =
    { capabilities : ServerCapabilities.t
    ; serverInfo : serverInfo
    }
end

module InitializedParams : sig end

module LocationLink : sig
  type t =
    { originSelectionRange : Range.t
    ; targetUri : DocumentUri.t
    ; targetRange : Range.t
    ; targetSelectionRange : Range.t
    }
end

module LogMessageParams : sig
  type t =
    { type_ : int [@key "type"]
    ; message : string
    }
end

module Message : sig
  type t = { jsonrpc : string }
end

module MessageActionItem : sig
  type t = { title : string }
end

module MessageType : sig
  type t =
    | Error
    | Warning
    | Info
    | Log
end

module NotificationMessage : sig
  type t =
    { jsonrpc : string
    ; method_ : string [@key "method"]
    ; params : [ `List of Json.t list | `Assoc of Json.t ]
    }
end

module ParameterInformation : sig
  type t =
    { label : [ `String of string | `Offset of int * int ]
    ; documentation : [ `String of string | `MarkupContent of MarkupContent.t ]
    }
end

module PrepareRenameParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
end

module ProgressParams : sig
  type t =
    { token : ProgressToken.t
    ; value : Json.t
    }
end

module PublishDiagnosticsParams : sig
  type t =
    { uri : DocumentUri.t
    ; version : int
    ; diagnostics : Diagnostic.t list
    }
end

module ReferenceContext : sig
  type t = { includeDeclaration : bool }
end

module ReferenceParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t
    ; partialResultToken : ProgressToken.t
    ; context : ReferenceContext.t
    }
end

module ReferenceRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool
    }
end

module Registration : sig
  type t =
    { id : string
    ; method_ : string [@key "method"]
    ; registerOptions : Json.t
    }
end

module RegistrationParams : sig
  type t = { registrations : Registration.t list }
end

module RenameParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t
    ; newName : string
    }
end

module RenameRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool
    ; prepareProvider : bool
    }
end

module RequestMessage : sig
  type t =
    { jsonrpc : string
    ; id : Jsonrpc.Id.t
    ; method_ : string [@key "method"]
    ; params : [ `List of Json.t list | `Assoc of Json.t ]
    }
end

module ResponseError : sig
  type t =
    { code : int
    ; message : string
    ; data : Json.t
    }
end

module ResponseMessage : sig
  type t =
    { jsonrpc : string
    ; id : Jsonrpc.Id.t option [@yojson.option]
    ; result :
        [ `String of string | `Int of int | `Bool of bool | `Assoc of Json.t ]
        option
          [@yojson.option]
    ; error : ResponseError.t
    }
end

module SelectionRange : sig
  type t =
    { range : Range.t
    ; parent : t
    }
end

module SelectionRangeParams : sig
  type t =
    { workDoneToken : ProgressToken.t
    ; partialResultToken : ProgressToken.t
    ; textDocument : TextDocumentIdentifier.t
    ; positions : Position.t list
    }
end

module ShowMessageParams : sig
  type t =
    { type_ : int [@key "type"]
    ; message : string
    }
end

module ShowMessageRequestParams : sig
  type t =
    { type_ : int [@key "type"]
    ; message : string
    ; actions : MessageActionItem.t list
    }
end

module SignatureInformation : sig
  type t =
    { label : string
    ; documentation : [ `String of string | `MarkupContent of MarkupContent.t ]
    ; parameters : ParameterInformation.t list
    }
end

module SignatureHelp : sig
  type t =
    { signatures : SignatureInformation.t list
    ; activeSignature : int
    ; activeParameter : int
    }
end

module SignatureHelpTriggerKind : sig
  type t =
    | Invoked
    | TriggerCharacter
    | ContentChange
end

module SignatureHelpContext : sig
  type t =
    { triggerKind : SignatureHelpTriggerKind.t
    ; triggerCharacter : string
    ; isRetrigger : bool
    ; activeSignatureHelp : SignatureHelp.t
    }
end

module SignatureHelpParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t
    ; context : SignatureHelpContext.t
    }
end

module SignatureHelpRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool
    ; triggerCharacters : string list
    ; retriggerCharacters : string list
    }
end

module SymbolInformation : sig
  type t =
    { name : string
    ; kind : SymbolKind.t
    ; deprecated : bool
    ; location : Location.t
    ; containerName : string
    }
end

module TextDocumentSyncKind : sig
  type t =
    | None
    | Full
    | Incremental
end

module TextDocumentChangeRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; syncKind : TextDocumentSyncKind.t
    }
end

module TextDocumentSaveReason : sig
  type t =
    | Manual
    | AfterDelay
    | FocusOut
end

module TextDocumentSaveRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; includeText : bool
    }
end

module TypeDefinitionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t
    ; partialResultToken : ProgressToken.t
    }
end

module Unregistration : sig
  type t =
    { id : string
    ; method_ : string [@key "method"]
    }
end

module UnregistrationParams : sig
  type t = { unregisterations : Unregistration.t list }
end

module WatchKind : sig
  type t =
    | Create
    | Change
    | Delete
end

module WillSaveTextDocumentParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; reason : int
    }
end

module WorkDoneProgressBegin : sig
  type t =
    { kind : unit
    ; title : string
    ; cancellable : bool
    ; message : string
    ; percentage : int
    }
end

module WorkDoneProgressCancelParams : sig
  type t = { token : ProgressToken.t }
end

module WorkDoneProgressCreateParams : sig
  type t = { token : ProgressToken.t }
end

module WorkDoneProgressEnd : sig
  type t =
    { kind : unit
    ; message : string
    }
end

module WorkDoneProgressReport : sig
  type t =
    { kind : unit
    ; cancellable : bool
    ; message : string
    ; percentage : int
    }
end

module WorkspaceSymbolOptions : sig
  type t = { workDoneProgress : bool }
end

module WorkspaceSymbolParams : sig
  type t =
    { workDoneToken : ProgressToken.t
    ; partialResultToken : ProgressToken.t
    ; query : string
    }
end

module WorkspaceSymbolRegistrationOptions : sig
  type t = { workDoneProgress : bool }
end

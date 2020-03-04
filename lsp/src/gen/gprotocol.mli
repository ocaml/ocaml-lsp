open! Import

[@@@warning "-30"]

module DeleteFileOptions : sig
  type t =
    { recursive : bool option
    ; ignoreIfNotExists : bool option
    }
end

module DocumentUri : sig
  type t = string
end

module DeleteFile : sig
  type t =
    { kind : unit
    ; uri : DocumentUri.t
    ; options : DeleteFileOptions.t option
    }
end

module RenameFileOptions : sig
  type t =
    { overwrite : bool option
    ; ignoreIfExists : bool option
    }
end

module RenameFile : sig
  type t =
    { kind : unit
    ; oldUri : DocumentUri.t
    ; newUri : DocumentUri.t
    ; options : RenameFileOptions.t option
    }
end

module CreateFileOptions : sig
  type t =
    { overwrite : bool option
    ; ignoreIfExists : bool option
    }
end

module CreateFile : sig
  type t =
    { kind : unit
    ; uri : DocumentUri.t
    ; options : CreateFileOptions.t option
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
    ; end_ : Position.t
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
    ; version : int option
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
    { changes : (DocumentUri.t * TextEdit.t list) list option
    ; documentChanges :
        [ `TextDocumentEdit of TextDocumentEdit.t
        | `CreateFile of CreateFile.t
        | `RenameFile of RenameFile.t
        | `DeleteFile of DeleteFile.t
        ]
        list
        option
    }
end

module ApplyWorkspaceEditParams : sig
  type t =
    { label : string option
    ; edit : WorkspaceEdit.t
    }
end

module ApplyWorkspaceEditResponse : sig
  type t =
    { applied : bool
    ; failureReason : string option
    }
end

module CancelParams : sig
  type t = { id : Jsonrpc.Id.t }
end

module SelectionRangeClientCapabilities : sig
  type t = { dynamicRegistration : bool option }
end

module FoldingRangeClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; rangeLimit : int option
    ; lineFoldingOnly : bool option
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
    { relatedInformation : bool option
    ; tagSupport : tagSupport option
    ; versionSupport : bool option
    }
end

module RenameClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; prepareSupport : bool option
    }
end

module DocumentOnTypeFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool option }
end

module DocumentRangeFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool option }
end

module DocumentFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool option }
end

module DocumentColorClientCapabilities : sig
  type t = { dynamicRegistration : bool option }
end

module DocumentLinkClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; tooltipSupport : bool option
    }
end

module CodeLensClientCapabilities : sig
  type t = { dynamicRegistration : bool option }
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
    { dynamicRegistration : bool option
    ; codeActionLiteralSupport : codeActionLiteralSupport option
    ; isPreferredSupport : bool option
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
  type symbolKind = { valueSet : SymbolKind.t list option }

  type t =
    { dynamicRegistration : bool option
    ; symbolKind : symbolKind option
    ; hierarchicalDocumentSymbolSupport : bool option
    }
end

module DocumentHighlightClientCapabilities : sig
  type t = { dynamicRegistration : bool option }
end

module ReferenceClientCapabilities : sig
  type t = { dynamicRegistration : bool option }
end

module ImplementationClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; linkSupport : bool option
    }
end

module TypeDefinitionClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; linkSupport : bool option
    }
end

module DefinitionClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; linkSupport : bool option
    }
end

module DeclarationClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; linkSupport : bool option
    }
end

module MarkupKind : sig
  type t =
    | PlainText
    | Markdown
end

module SignatureHelpClientCapabilities : sig
  type parameterInformation = { labelOffsetSupport : bool option }

  type signatureInformation =
    { documentationFormat : MarkupKind.t list option
    ; parameterInformation : parameterInformation option
    }

  type t =
    { dynamicRegistration : bool option
    ; signatureInformation : signatureInformation option
    ; contextSupport : bool option
    }
end

module HoverClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; contentFormat : MarkupKind.t list option
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
  type completionItemKind = { valueSet : CompletionItemKind.t list option }

  type tagSupport = { valueSet : CompletionItemTag.t list }

  type completionItem =
    { snippetSupport : bool option
    ; commitCharactersSupport : bool option
    ; documentationFormat : MarkupKind.t list option
    ; deprecatedSupport : bool option
    ; preselectSupport : bool option
    ; tagSupport : tagSupport option
    }

  type t =
    { dynamicRegistration : bool option
    ; completionItem : completionItem option
    ; completionItemKind : completionItemKind option
    ; contextSupport : bool option
    }
end

module TextDocumentSyncClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; willSave : bool option
    ; willSaveWaitUntil : bool option
    ; didSave : bool option
    }
end

module TextDocumentClientCapabilities : sig
  type t =
    { synchronization : TextDocumentSyncClientCapabilities.t option
    ; completion : CompletionClientCapabilities.t option
    ; hover : HoverClientCapabilities.t option
    ; signatureHelp : SignatureHelpClientCapabilities.t option
    ; declaration : DeclarationClientCapabilities.t option
    ; definition : DefinitionClientCapabilities.t option
    ; typeDefinition : TypeDefinitionClientCapabilities.t option
    ; implementation : ImplementationClientCapabilities.t option
    ; references : ReferenceClientCapabilities.t option
    ; documentHighlight : DocumentHighlightClientCapabilities.t option
    ; documentSymbol : DocumentSymbolClientCapabilities.t option
    ; codeAction : CodeActionClientCapabilities.t option
    ; codeLens : CodeLensClientCapabilities.t option
    ; documentLink : DocumentLinkClientCapabilities.t option
    ; colorProvider : DocumentColorClientCapabilities.t option
    ; formatting : DocumentFormattingClientCapabilities.t option
    ; rangeFormatting : DocumentRangeFormattingClientCapabilities.t option
    ; onTypeFormatting : DocumentOnTypeFormattingClientCapabilities.t option
    ; rename : RenameClientCapabilities.t option
    ; publishDiagnostics : PublishDiagnosticsClientCapabilities.t option
    ; foldingRange : FoldingRangeClientCapabilities.t option
    ; selectionRange : SelectionRangeClientCapabilities.t option
    }
end

module ExecuteCommandClientCapabilities : sig
  type t = { dynamicRegistration : bool option }
end

module WorkspaceSymbolClientCapabilities : sig
  type symbolKind = { valueSet : SymbolKind.t list option }

  type t =
    { dynamicRegistration : bool option
    ; symbolKind : symbolKind option
    }
end

module DidChangeWatchedFilesClientCapabilities : sig
  type t = { dynamicRegistration : bool option }
end

module DidChangeConfigurationClientCapabilities : sig
  type t = { dynamicRegistration : bool option }
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
    { documentChanges : bool option
    ; resourceOperations : ResourceOperationKind.t list option
    ; failureHandling : FailureHandlingKind.t option
    }
end

module ClientCapabilities : sig
  type window = { workDoneProgress : bool option }

  type workspace =
    { applyEdit : bool option
    ; workspaceEdit : WorkspaceEditClientCapabilities.t option
    ; didChangeConfiguration : DidChangeConfigurationClientCapabilities.t option
    ; didChangeWatchedFiles : DidChangeWatchedFilesClientCapabilities.t option
    ; symbol : WorkspaceSymbolClientCapabilities.t option
    ; executeCommand : ExecuteCommandClientCapabilities.t option
    ; workspaceFolders : bool option
    ; configuration : bool option
    }

  type t =
    { workspace : workspace option
    ; textDocument : TextDocumentClientCapabilities.t option
    ; window : window option
    ; experimental : Json.t option
    }
end

module Command : sig
  type t =
    { title : string
    ; command : string
    ; arguments : Json.t list option
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
    ; severity : DiagnosticSeverity.t option
    ; code : Jsonrpc.Id.t option
    ; source : string option
    ; message : string
    ; tags : DiagnosticTag.t list option
    ; relatedInformation : DiagnosticRelatedInformation.t list option
    }
end

module CodeAction : sig
  type t =
    { title : string
    ; kind : CodeActionKind.t option
    ; diagnostics : Diagnostic.t list option
    ; isPreferred : bool option
    ; edit : WorkspaceEdit.t option
    ; command : Command.t option
    }
end

module CodeActionContext : sig
  type t =
    { diagnostics : Diagnostic.t list
    ; only : CodeActionKind.t list option
    }
end

module WorkDoneProgressOptions : sig
  type t = { workDoneProgress : bool option }
end

module CodeActionOptions : sig
  type t =
    { workDoneProgress : bool option
    ; codeActionKinds : CodeActionKind.t list option
    }
end

module ProgressToken : sig
  type t = Jsonrpc.Id.t
end

module PartialResultParams : sig
  type t = { partialResultToken : ProgressToken.t option }
end

module WorkDoneProgressParams : sig
  type t = { workDoneToken : ProgressToken.t option }
end

module CodeActionParams : sig
  type t =
    { workDoneToken : ProgressToken.t option
    ; partialResultToken : ProgressToken.t option
    ; textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; context : CodeActionContext.t
    }
end

module DocumentFilter : sig
  type t =
    { language : string option
    ; scheme : string option
    ; pattern : string option
    }
end

module DocumentSelector : sig
  type t = DocumentFilter.t list
end

module TextDocumentRegistrationOptions : sig
  type t = { documentSelector : DocumentSelector.t option }
end

module CodeActionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; codeActionKinds : CodeActionKind.t list option
    }
end

module CodeLens : sig
  type t =
    { range : Range.t
    ; command : Command.t option
    ; data : Json.t option
    }
end

module CodeLensOptions : sig
  type t =
    { workDoneProgress : bool option
    ; resolveProvider : bool option
    }
end

module CodeLensParams : sig
  type t =
    { workDoneToken : ProgressToken.t option
    ; partialResultToken : ProgressToken.t option
    ; textDocument : TextDocumentIdentifier.t
    }
end

module CodeLensRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; resolveProvider : bool option
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
    ; textEdit : TextEdit.t option
    ; additionalTextEdits : TextEdit.t list option
    }
end

module ColorPresentationParams : sig
  type t =
    { workDoneToken : ProgressToken.t option
    ; partialResultToken : ProgressToken.t option
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
    ; triggerCharacter : string option
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
    ; kind : int option
    ; tags : CompletionItemTag.t list option
    ; detail : string option
    ; documentation :
        [ `String of string | `MarkupContent of MarkupContent.t ] option
    ; deprecated : bool option
    ; preselect : bool option
    ; sortText : string option
    ; filterText : string option
    ; insertText : string option
    ; insertTextFormat : InsertTextFormat.t option
    ; textEdit : TextEdit.t option
    ; additionalTextEdits : TextEdit.t list option
    ; commitCharacters : string list option
    ; command : Command.t option
    ; data : Json.t option
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
    { workDoneProgress : bool option
    ; triggerCharacters : string list option
    ; allCommitCharacters : string list option
    ; resolveProvider : bool option
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
    ; workDoneToken : ProgressToken.t option
    ; partialResultToken : ProgressToken.t option
    ; context : CompletionContext.t option
    }
end

module CompletionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; triggerCharacters : string list option
    ; allCommitCharacters : string list option
    ; resolveProvider : bool option
    }
end

module ConfigurationItem : sig
  type t =
    { scopeUri : DocumentUri.t option
    ; section : string option
    }
end

module ConfigurationParams : sig
  type t = { items : ConfigurationItem.t list }
end

module DeclarationOptions : sig
  type t = { workDoneProgress : bool option }
end

module DeclarationParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option
    ; partialResultToken : ProgressToken.t option
    }
end

module StaticRegistrationOptions : sig
  type t = { id : string option }
end

module DeclarationRegistrationOptions : sig
  type t =
    { workDoneProgress : bool option
    ; documentSelector : DocumentSelector.t option
    ; id : string option
    }
end

module DefinitionOptions : sig
  type t = { workDoneProgress : bool option }
end

module DefinitionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option
    ; partialResultToken : ProgressToken.t option
    }
end

module DefinitionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }
end

module DidChangeConfigurationParams : sig
  type t = { settings : Json.t }
end

module TextDocumentContentChangeEvent : sig
  type t =
    { range : Range.t
    ; rangeLength : int option
    ; text : string
    }
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
    ; type_ : int
    }
end

module DidChangeWatchedFilesParams : sig
  type t = { changes : FileEvent.t list }
end

module FileSystemWatcher : sig
  type t =
    { globPattern : string
    ; kind : int option
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
    ; text : string option
    }
end

module DocumentColorOptions : sig
  type t = { workDoneProgress : bool option }
end

module DocumentColorParams : sig
  type t =
    { workDoneToken : ProgressToken.t option
    ; partialResultToken : ProgressToken.t option
    ; textDocument : TextDocumentIdentifier.t
    }
end

module DocumentColorRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; id : string option
    ; workDoneProgress : bool option
    }
end

module DocumentFormattingOptions : sig
  type t = { workDoneProgress : bool option }
end

module FormattingOptions : sig
  type t =
    { tabSize : int
    ; insertSpaces : bool
    ; trimTrailingWhitespace : bool option
    ; insertFinalNewline : bool option
    ; trimFinalNewlines : bool option
    ; key : (string * [ `Bool of bool | `Int of int | `String of string ]) list
    }
end

module DocumentFormattingParams : sig
  type t =
    { workDoneToken : ProgressToken.t option
    ; textDocument : TextDocumentIdentifier.t
    ; options : FormattingOptions.t
    }
end

module DocumentFormattingRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }
end

module DocumentHighlight : sig
  type t =
    { range : Range.t
    ; kind : int option
    }
end

module DocumentHighlightKind : sig
  type t =
    | Text
    | Read
    | Write
end

module DocumentHighlightOptions : sig
  type t = { workDoneProgress : bool option }
end

module DocumentHighlightParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option
    ; partialResultToken : ProgressToken.t option
    }
end

module DocumentHighlightRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }
end

module DocumentLink : sig
  type t =
    { range : Range.t
    ; target : DocumentUri.t option
    ; tooltip : string option
    ; data : Json.t option
    }
end

module DocumentLinkOptions : sig
  type t =
    { workDoneProgress : bool option
    ; resolveProvider : bool option
    }
end

module DocumentLinkParams : sig
  type t =
    { workDoneToken : ProgressToken.t option
    ; partialResultToken : ProgressToken.t option
    ; textDocument : TextDocumentIdentifier.t
    }
end

module DocumentLinkRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; resolveProvider : bool option
    }
end

module DocumentOnTypeFormattingOptions : sig
  type t =
    { firstTriggerCharacter : string
    ; moreTriggerCharacter : string list option
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
    { documentSelector : DocumentSelector.t option
    ; firstTriggerCharacter : string
    ; moreTriggerCharacter : string list option
    }
end

module DocumentRangeFormattingOptions : sig
  type t = { workDoneProgress : bool option }
end

module DocumentRangeFormattingParams : sig
  type t =
    { workDoneToken : ProgressToken.t option
    ; textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; options : FormattingOptions.t
    }
end

module DocumentRangeFormattingRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }
end

module DocumentSymbol : sig
  type t =
    { name : string
    ; detail : string option
    ; kind : SymbolKind.t
    ; deprecated : bool option
    ; range : Range.t
    ; selectionRange : Range.t
    ; children : t list option
    }
end

module DocumentSymbolOptions : sig
  type t = { workDoneProgress : bool option }
end

module DocumentSymbolParams : sig
  type t =
    { workDoneToken : ProgressToken.t option
    ; partialResultToken : ProgressToken.t option
    ; textDocument : TextDocumentIdentifier.t
    }
end

module DocumentSymbolRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
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
    { workDoneProgress : bool option
    ; commands : string list
    }
end

module ExecuteCommandParams : sig
  type t =
    { workDoneToken : ProgressToken.t option
    ; command : string
    ; arguments : Json.t list option
    }
end

module ExecuteCommandRegistrationOptions : sig
  type t =
    { workDoneProgress : bool option
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
    ; startCharacter : int option
    ; endLine : int
    ; endCharacter : int option
    ; kind : string option
    }
end

module FoldingRangeKind : sig
  type t =
    | Comment
    | Imports
    | Region
end

module FoldingRangeOptions : sig
  type t = { workDoneProgress : bool option }
end

module FoldingRangeParams : sig
  type t =
    { workDoneToken : ProgressToken.t option
    ; partialResultToken : ProgressToken.t option
    ; textDocument : TextDocumentIdentifier.t
    }
end

module FoldingRangeRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; id : string option
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
    ; range : Range.t option
    }
end

module HoverOptions : sig
  type t = { workDoneProgress : bool option }
end

module HoverParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option
    }
end

module HoverRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }
end

module ImplementationOptions : sig
  type t = { workDoneProgress : bool option }
end

module ImplementationParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option
    ; partialResultToken : ProgressToken.t option
    }
end

module ImplementationRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; id : string option
    }
end

module InitializeError : sig
  type t = UnknownProtocolVersion
end

module InitializeParams : sig
  type clientInfo =
    { name : string
    ; version : string option
    }

  type t =
    { workDoneToken : ProgressToken.t option
    ; processId : int option
    ; clientInfo : clientInfo option
    ; rootPath : string option option
    ; rootUri : DocumentUri.t option
    ; initializationOptions : Json.t option
    ; capabilities : ClientCapabilities.t
    ; trace : [ `Off | `Messages | `Verbose ] option
    ; workspaceFolders : WorkspaceFolder.t list option option
    }
end

module WorkspaceFoldersServerCapabilities : sig
  type t =
    { supported : bool option
    ; changeNotifications : [ `String of string | `Bool of bool ] option
    }
end

module SelectionRangeOptions : sig
  type t = { workDoneProgress : bool option }
end

module SelectionRangeRegistrationOptions : sig
  type t =
    { workDoneProgress : bool option
    ; documentSelector : DocumentSelector.t option
    ; id : string option
    }
end

module RenameOptions : sig
  type t =
    { workDoneProgress : bool option
    ; prepareProvider : bool option
    }
end

module ReferenceOptions : sig
  type t = { workDoneProgress : bool option }
end

module TypeDefinitionOptions : sig
  type t = { workDoneProgress : bool option }
end

module TypeDefinitionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; id : string option
    }
end

module SignatureHelpOptions : sig
  type t =
    { workDoneProgress : bool option
    ; triggerCharacters : string list option
    ; retriggerCharacters : string list option
    }
end

module SaveOptions : sig
  type t = { includeText : bool option }
end

module TextDocumentSyncOptions : sig
  type t =
    { openClose : bool option
    ; change : int option
    ; willSave : bool option
    ; willSaveWaitUntil : bool option
    ; save : SaveOptions.t option
    }
end

module ServerCapabilities : sig
  type workspace =
    { workspaceFolders : WorkspaceFoldersServerCapabilities.t option }

  type t =
    { textDocumentSync :
        [ `TextDocumentSyncOptions of TextDocumentSyncOptions.t | `Int of int ]
        option
    ; completionProvider : CompletionOptions.t option
    ; hoverProvider : [ `Bool of bool | `HoverOptions of HoverOptions.t ] option
    ; signatureHelpProvider : SignatureHelpOptions.t option
    ; declarationProvider :
        [ `Bool of bool
        | `DeclarationOptions of DeclarationOptions.t
        | `DeclarationRegistrationOptions of DeclarationRegistrationOptions.t
        ]
        option
    ; definitionProvider :
        [ `Bool of bool | `DefinitionOptions of DefinitionOptions.t ] option
    ; typeDefinitionProvider :
        [ `Bool of bool
        | `TypeDefinitionOptions of TypeDefinitionOptions.t
        | `TypeDefinitionRegistrationOptions of
          TypeDefinitionRegistrationOptions.t
        ]
        option
    ; implementationProvider :
        [ `Bool of bool
        | `ImplementationOptions of ImplementationOptions.t
        | `ImplementationRegistrationOptions of
          ImplementationRegistrationOptions.t
        ]
        option
    ; referencesProvider :
        [ `Bool of bool | `ReferenceOptions of ReferenceOptions.t ] option
    ; documentHighlightProvider :
        [ `Bool of bool
        | `DocumentHighlightOptions of DocumentHighlightOptions.t
        ]
        option
    ; documentSymbolProvider :
        [ `Bool of bool | `DocumentSymbolOptions of DocumentSymbolOptions.t ]
        option
    ; codeActionProvider :
        [ `Bool of bool | `CodeActionOptions of CodeActionOptions.t ] option
    ; codeLensProvider : CodeLensOptions.t option
    ; documentLinkProvider : DocumentLinkOptions.t option
    ; colorProvider :
        [ `Bool of bool
        | `DocumentColorOptions of DocumentColorOptions.t
        | `DocumentColorRegistrationOptions of
          DocumentColorRegistrationOptions.t
        ]
        option
    ; documentFormattingProvider :
        [ `Bool of bool
        | `DocumentFormattingOptions of DocumentFormattingOptions.t
        ]
        option
    ; documentRangeFormattingProvider :
        [ `Bool of bool
        | `DocumentRangeFormattingOptions of DocumentRangeFormattingOptions.t
        ]
        option
    ; documentOnTypeFormattingProvider :
        DocumentOnTypeFormattingOptions.t option
    ; renameProvider :
        [ `Bool of bool | `RenameOptions of RenameOptions.t ] option
    ; foldingRangeProvider :
        [ `Bool of bool
        | `FoldingRangeOptions of FoldingRangeOptions.t
        | `FoldingRangeRegistrationOptions of FoldingRangeRegistrationOptions.t
        ]
        option
    ; executeCommandProvider : ExecuteCommandOptions.t option
    ; selectionRangeProvider :
        [ `Bool of bool
        | `SelectionRangeOptions of SelectionRangeOptions.t
        | `SelectionRangeRegistrationOptions of
          SelectionRangeRegistrationOptions.t
        ]
        option
    ; workspaceSymbolProvider : bool option
    ; workspace : workspace option
    ; experimental : Json.t option
    }
end

module InitializeResult : sig
  type serverInfo =
    { name : string
    ; version : string option
    }

  type t =
    { capabilities : ServerCapabilities.t
    ; serverInfo : serverInfo option
    }
end

module LocationLink : sig
  type t =
    { originSelectionRange : Range.t option
    ; targetUri : DocumentUri.t
    ; targetRange : Range.t
    ; targetSelectionRange : Range.t
    }
end

module LogMessageParams : sig
  type t =
    { type_ : int
    ; message : string
    }
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

module ParameterInformation : sig
  type t =
    { label : [ `String of string | `Offset of int * int ]
    ; documentation :
        [ `String of string | `MarkupContent of MarkupContent.t ] option
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
    ; version : int option
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
    ; workDoneToken : ProgressToken.t option
    ; partialResultToken : ProgressToken.t option
    ; context : ReferenceContext.t
    }
end

module ReferenceRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }
end

module Registration : sig
  type t =
    { id : string
    ; method_ : string
    ; registerOptions : Json.t option
    }
end

module RegistrationParams : sig
  type t = { registrations : Registration.t list }
end

module RenameParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option
    ; newName : string
    }
end

module RenameRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; prepareProvider : bool option
    }
end

module SelectionRange : sig
  type t =
    { range : Range.t
    ; parent : t option
    }
end

module SelectionRangeParams : sig
  type t =
    { workDoneToken : ProgressToken.t option
    ; partialResultToken : ProgressToken.t option
    ; textDocument : TextDocumentIdentifier.t
    ; positions : Position.t list
    }
end

module ShowMessageParams : sig
  type t =
    { type_ : int
    ; message : string
    }
end

module ShowMessageRequestParams : sig
  type t =
    { type_ : int
    ; message : string
    ; actions : MessageActionItem.t list option
    }
end

module SignatureInformation : sig
  type t =
    { label : string
    ; documentation :
        [ `String of string | `MarkupContent of MarkupContent.t ] option
    ; parameters : ParameterInformation.t list option
    }
end

module SignatureHelp : sig
  type t =
    { signatures : SignatureInformation.t list
    ; activeSignature : int option
    ; activeParameter : int option
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
    ; triggerCharacter : string option
    ; isRetrigger : bool
    ; activeSignatureHelp : SignatureHelp.t option
    }
end

module SignatureHelpParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option
    ; context : SignatureHelpContext.t option
    }
end

module SignatureHelpRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; triggerCharacters : string list option
    ; retriggerCharacters : string list option
    }
end

module SymbolInformation : sig
  type t =
    { name : string
    ; kind : SymbolKind.t
    ; deprecated : bool option
    ; location : Location.t
    ; containerName : string option
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
    { documentSelector : DocumentSelector.t option
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
    { documentSelector : DocumentSelector.t option
    ; includeText : bool option
    }
end

module TypeDefinitionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option
    ; partialResultToken : ProgressToken.t option
    }
end

module Unregistration : sig
  type t =
    { id : string
    ; method_ : string
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
    ; cancellable : bool option
    ; message : string option
    ; percentage : int option
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
    ; message : string option
    }
end

module WorkDoneProgressReport : sig
  type t =
    { kind : unit
    ; cancellable : bool option
    ; message : string option
    ; percentage : int option
    }
end

module WorkspaceSymbolOptions : sig
  type t = { workDoneProgress : bool option }
end

module WorkspaceSymbolParams : sig
  type t =
    { workDoneToken : ProgressToken.t option
    ; partialResultToken : ProgressToken.t option
    ; query : string
    }
end

module WorkspaceSymbolRegistrationOptions : sig
  type t = { workDoneProgress : bool option }
end

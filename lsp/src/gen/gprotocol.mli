open! Import

[@@@warning "-30"]

module DeleteFileOptions : sig
  type t =
    { recursive : bool option [@yojson.option]
    ; ignoreIfNotExists : bool option [@yojson.option]
    }
end

module DocumentUri : sig
  type t = string
end

module DeleteFile : sig
  type t =
    { kind : unit
    ; uri : DocumentUri.t
    ; options : DeleteFileOptions.t option [@yojson.option]
    }
end

module RenameFileOptions : sig
  type t =
    { overwrite : bool option [@yojson.option]
    ; ignoreIfExists : bool option [@yojson.option]
    }
end

module RenameFile : sig
  type t =
    { kind : unit
    ; oldUri : DocumentUri.t
    ; newUri : DocumentUri.t
    ; options : RenameFileOptions.t option [@yojson.option]
    }
end

module CreateFileOptions : sig
  type t =
    { overwrite : bool option [@yojson.option]
    ; ignoreIfExists : bool option [@yojson.option]
    }
end

module CreateFile : sig
  type t =
    { kind : unit
    ; uri : DocumentUri.t
    ; options : CreateFileOptions.t option [@yojson.option]
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
    { changes : (DocumentUri.t * TextEdit.t list) list option [@yojson.option]
    ; documentChanges :
        [ `TextDocumentEdit of TextDocumentEdit.t
        | `CreateFile of CreateFile.t
        | `RenameFile of RenameFile.t
        | `DeleteFile of DeleteFile.t
        ]
        list
        option
          [@yojson.option]
    }
end

module ApplyWorkspaceEditParams : sig
  type t =
    { label : string option [@yojson.option]
    ; edit : WorkspaceEdit.t
    }
end

module ApplyWorkspaceEditResponse : sig
  type t =
    { applied : bool
    ; failureReason : string option [@yojson.option]
    }
end

module CancelParams : sig
  type t = { id : Jsonrpc.Id.t }
end

module SelectionRangeClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
end

module FoldingRangeClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; rangeLimit : int option [@yojson.option]
    ; lineFoldingOnly : bool option [@yojson.option]
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
    { relatedInformation : bool option [@yojson.option]
    ; tagSupport : tagSupport option [@yojson.option]
    ; versionSupport : bool option [@yojson.option]
    }
end

module RenameClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; prepareSupport : bool option [@yojson.option]
    }
end

module DocumentOnTypeFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
end

module DocumentRangeFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
end

module DocumentFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
end

module DocumentColorClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
end

module DocumentLinkClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; tooltipSupport : bool option [@yojson.option]
    }
end

module CodeLensClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
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
    { dynamicRegistration : bool option [@yojson.option]
    ; codeActionLiteralSupport : codeActionLiteralSupport option
          [@yojson.option]
    ; isPreferredSupport : bool option [@yojson.option]
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
  type symbolKind = { valueSet : SymbolKind.t list option [@yojson.option] }

  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; symbolKind : symbolKind option [@yojson.option]
    ; hierarchicalDocumentSymbolSupport : bool option [@yojson.option]
    }
end

module DocumentHighlightClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
end

module ReferenceClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
end

module ImplementationClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; linkSupport : bool option [@yojson.option]
    }
end

module TypeDefinitionClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; linkSupport : bool option [@yojson.option]
    }
end

module DefinitionClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; linkSupport : bool option [@yojson.option]
    }
end

module DeclarationClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; linkSupport : bool option [@yojson.option]
    }
end

module MarkupKind : sig
  type t =
    | PlainText
    | Markdown
end

module SignatureHelpClientCapabilities : sig
  type parameterInformation =
    { labelOffsetSupport : bool option [@yojson.option] }

  type signatureInformation =
    { documentationFormat : MarkupKind.t list option [@yojson.option]
    ; parameterInformation : parameterInformation option [@yojson.option]
    }

  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; signatureInformation : signatureInformation option [@yojson.option]
    ; contextSupport : bool option [@yojson.option]
    }
end

module HoverClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; contentFormat : MarkupKind.t list option [@yojson.option]
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
  type completionItemKind =
    { valueSet : CompletionItemKind.t list option [@yojson.option] }

  type tagSupport = { valueSet : CompletionItemTag.t list }

  type completionItem =
    { snippetSupport : bool option [@yojson.option]
    ; commitCharactersSupport : bool option [@yojson.option]
    ; documentationFormat : MarkupKind.t list option [@yojson.option]
    ; deprecatedSupport : bool option [@yojson.option]
    ; preselectSupport : bool option [@yojson.option]
    ; tagSupport : tagSupport option [@yojson.option]
    }

  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; completionItem : completionItem option [@yojson.option]
    ; completionItemKind : completionItemKind option [@yojson.option]
    ; contextSupport : bool option [@yojson.option]
    }
end

module TextDocumentSyncClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; willSave : bool option [@yojson.option]
    ; willSaveWaitUntil : bool option [@yojson.option]
    ; didSave : bool option [@yojson.option]
    }
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
end

module ExecuteCommandClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
end

module WorkspaceSymbolClientCapabilities : sig
  type symbolKind = { valueSet : SymbolKind.t list option [@yojson.option] }

  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; symbolKind : symbolKind option [@yojson.option]
    }
end

module DidChangeWatchedFilesClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
end

module DidChangeConfigurationClientCapabilities : sig
  type t = { dynamicRegistration : bool option [@yojson.option] }
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
    { documentChanges : bool option [@yojson.option]
    ; resourceOperations : ResourceOperationKind.t list option [@yojson.option]
    ; failureHandling : FailureHandlingKind.t option [@yojson.option]
    }
end

module ClientCapabilities : sig
  type window = { workDoneProgress : bool option [@yojson.option] }

  type workspace =
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

  type t =
    { workspace : workspace option [@yojson.option]
    ; textDocument : TextDocumentClientCapabilities.t option [@yojson.option]
    ; window : window option [@yojson.option]
    ; experimental : Json.t option [@yojson.option]
    }
end

module Command : sig
  type t =
    { title : string
    ; command : string
    ; arguments : Json.t list option [@yojson.option]
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
    ; severity : DiagnosticSeverity.t option [@yojson.option]
    ; code : Jsonrpc.Id.t option [@yojson.option]
    ; source : string option [@yojson.option]
    ; message : string
    ; tags : DiagnosticTag.t list option [@yojson.option]
    ; relatedInformation : DiagnosticRelatedInformation.t list option
          [@yojson.option]
    }
end

module CodeAction : sig
  type t =
    { title : string
    ; kind : CodeActionKind.t option [@yojson.option]
    ; diagnostics : Diagnostic.t list option [@yojson.option]
    ; isPreferred : bool option [@yojson.option]
    ; edit : WorkspaceEdit.t option [@yojson.option]
    ; command : Command.t option [@yojson.option]
    }
end

module CodeActionContext : sig
  type t =
    { diagnostics : Diagnostic.t list
    ; only : CodeActionKind.t list option [@yojson.option]
    }
end

module WorkDoneProgressOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
end

module CodeActionOptions : sig
  type t =
    { workDoneProgress : bool option [@yojson.option]
    ; codeActionKinds : CodeActionKind.t list option [@yojson.option]
    }
end

module ProgressToken : sig
  type t = Jsonrpc.Id.t
end

module PartialResultParams : sig
  type t = { partialResultToken : ProgressToken.t option [@yojson.option] }
end

module WorkDoneProgressParams : sig
  type t = { workDoneToken : ProgressToken.t option [@yojson.option] }
end

module CodeActionParams : sig
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; context : CodeActionContext.t
    }
end

module DocumentFilter : sig
  type t =
    { language : string option [@yojson.option]
    ; scheme : string option [@yojson.option]
    ; pattern : string option [@yojson.option]
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
    ; workDoneProgress : bool option [@yojson.option]
    ; codeActionKinds : CodeActionKind.t list option [@yojson.option]
    }
end

module CodeLens : sig
  type t =
    { range : Range.t
    ; command : Command.t option [@yojson.option]
    ; data : Json.t option [@yojson.option]
    }
end

module CodeLensOptions : sig
  type t =
    { workDoneProgress : bool option [@yojson.option]
    ; resolveProvider : bool option [@yojson.option]
    }
end

module CodeLensParams : sig
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
    }
end

module CodeLensRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; resolveProvider : bool option [@yojson.option]
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
    ; textEdit : TextEdit.t option [@yojson.option]
    ; additionalTextEdits : TextEdit.t list option [@yojson.option]
    }
end

module ColorPresentationParams : sig
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
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
    ; triggerCharacter : string option [@yojson.option]
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
    ; kind : int option [@yojson.option]
    ; tags : CompletionItemTag.t list option [@yojson.option]
    ; detail : string option [@yojson.option]
    ; documentation :
        [ `String of string | `MarkupContent of MarkupContent.t ] option
          [@yojson.option]
    ; deprecated : bool option [@yojson.option]
    ; preselect : bool option [@yojson.option]
    ; sortText : string option [@yojson.option]
    ; filterText : string option [@yojson.option]
    ; insertText : string option [@yojson.option]
    ; insertTextFormat : InsertTextFormat.t option [@yojson.option]
    ; textEdit : TextEdit.t option [@yojson.option]
    ; additionalTextEdits : TextEdit.t list option [@yojson.option]
    ; commitCharacters : string list option [@yojson.option]
    ; command : Command.t option [@yojson.option]
    ; data : Json.t option [@yojson.option]
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
    { workDoneProgress : bool option [@yojson.option]
    ; triggerCharacters : string list option [@yojson.option]
    ; allCommitCharacters : string list option [@yojson.option]
    ; resolveProvider : bool option [@yojson.option]
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
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; context : CompletionContext.t option [@yojson.option]
    }
end

module CompletionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; triggerCharacters : string list option [@yojson.option]
    ; allCommitCharacters : string list option [@yojson.option]
    ; resolveProvider : bool option [@yojson.option]
    }
end

module ConfigurationItem : sig
  type t =
    { scopeUri : DocumentUri.t option [@yojson.option]
    ; section : string option [@yojson.option]
    }
end

module ConfigurationParams : sig
  type t = { items : ConfigurationItem.t list }
end

module DeclarationOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
end

module DeclarationParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    }
end

module StaticRegistrationOptions : sig
  type t = { id : string option [@yojson.option] }
end

module DeclarationRegistrationOptions : sig
  type t =
    { workDoneProgress : bool option [@yojson.option]
    ; documentSelector : DocumentSelector.t option [@yojson.option]
    ; id : string option [@yojson.option]
    }
end

module DefinitionOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
end

module DefinitionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    }
end

module DefinitionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
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
    ; kind : int option [@yojson.option]
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
    ; text : string option [@yojson.option]
    }
end

module DocumentColorOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
end

module DocumentColorParams : sig
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
    }
end

module DocumentColorRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; id : string option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    }
end

module DocumentFormattingOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
end

module FormattingOptions : sig
  type t =
    { tabSize : int
    ; insertSpaces : bool
    ; trimTrailingWhitespace : bool option [@yojson.option]
    ; insertFinalNewline : bool option [@yojson.option]
    ; trimFinalNewlines : bool option [@yojson.option]
    ; key : (string * [ `Bool of bool | `Int of int | `String of string ]) list
    }
end

module DocumentFormattingParams : sig
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
    ; options : FormattingOptions.t
    }
end

module DocumentFormattingRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    }
end

module DocumentHighlight : sig
  type t =
    { range : Range.t
    ; kind : int option [@yojson.option]
    }
end

module DocumentHighlightKind : sig
  type t =
    | Text
    | Read
    | Write
end

module DocumentHighlightOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
end

module DocumentHighlightParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    }
end

module DocumentHighlightRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    }
end

module DocumentLink : sig
  type t =
    { range : Range.t
    ; target : DocumentUri.t option [@yojson.option]
    ; tooltip : string option [@yojson.option]
    ; data : Json.t option [@yojson.option]
    }
end

module DocumentLinkOptions : sig
  type t =
    { workDoneProgress : bool option [@yojson.option]
    ; resolveProvider : bool option [@yojson.option]
    }
end

module DocumentLinkParams : sig
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
    }
end

module DocumentLinkRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; resolveProvider : bool option [@yojson.option]
    }
end

module DocumentOnTypeFormattingOptions : sig
  type t =
    { firstTriggerCharacter : string
    ; moreTriggerCharacter : string list option [@yojson.option]
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
    ; moreTriggerCharacter : string list option [@yojson.option]
    }
end

module DocumentRangeFormattingOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
end

module DocumentRangeFormattingParams : sig
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; options : FormattingOptions.t
    }
end

module DocumentRangeFormattingRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    }
end

module DocumentSymbol : sig
  type t =
    { name : string
    ; detail : string option [@yojson.option]
    ; kind : SymbolKind.t
    ; deprecated : bool option [@yojson.option]
    ; range : Range.t
    ; selectionRange : Range.t
    ; children : t list option [@yojson.option]
    }
end

module DocumentSymbolOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
end

module DocumentSymbolParams : sig
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
    }
end

module DocumentSymbolRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
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
    { workDoneProgress : bool option [@yojson.option]
    ; commands : string list
    }
end

module ExecuteCommandParams : sig
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; command : string
    ; arguments : Json.t list option [@yojson.option]
    }
end

module ExecuteCommandRegistrationOptions : sig
  type t =
    { workDoneProgress : bool option [@yojson.option]
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
    ; startCharacter : int option [@yojson.option]
    ; endLine : int
    ; endCharacter : int option [@yojson.option]
    ; kind : string option [@yojson.option]
    }
end

module FoldingRangeKind : sig
  type t =
    | Comment
    | Imports
    | Region
end

module FoldingRangeOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
end

module FoldingRangeParams : sig
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
    }
end

module FoldingRangeRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; id : string option [@yojson.option]
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
    ; range : Range.t option [@yojson.option]
    }
end

module HoverOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
end

module HoverParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    }
end

module HoverRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    }
end

module ImplementationOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
end

module ImplementationParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    }
end

module ImplementationRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; id : string option [@yojson.option]
    }
end

module InitializeError : sig
  type t = UnknownProtocolVersion
end

module InitializeParams : sig
  type clientInfo =
    { name : string
    ; version : string option [@yojson.option]
    }

  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; processId : int option [@yojson.option]
    ; clientInfo : clientInfo option [@yojson.option]
    ; rootPath : string option option [@yojson.option]
    ; rootUri : DocumentUri.t option [@yojson.option]
    ; initializationOptions : Json.t option [@yojson.option]
    ; capabilities : ClientCapabilities.t
    ; trace : [ `Off | `Messages | `Verbose ] option [@yojson.option]
    ; workspaceFolders : WorkspaceFolder.t list option option [@yojson.option]
    }
end

module WorkspaceFoldersServerCapabilities : sig
  type t =
    { supported : bool option [@yojson.option]
    ; changeNotifications : [ `String of string | `Bool of bool ] option
          [@yojson.option]
    }
end

module SelectionRangeOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
end

module SelectionRangeRegistrationOptions : sig
  type t =
    { workDoneProgress : bool option [@yojson.option]
    ; documentSelector : DocumentSelector.t option [@yojson.option]
    ; id : string option [@yojson.option]
    }
end

module RenameOptions : sig
  type t =
    { workDoneProgress : bool option [@yojson.option]
    ; prepareProvider : bool option [@yojson.option]
    }
end

module ReferenceOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
end

module TypeDefinitionOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
end

module TypeDefinitionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; id : string option [@yojson.option]
    }
end

module SignatureHelpOptions : sig
  type t =
    { workDoneProgress : bool option [@yojson.option]
    ; triggerCharacters : string list option [@yojson.option]
    ; retriggerCharacters : string list option [@yojson.option]
    }
end

module SaveOptions : sig
  type t = { includeText : bool option [@yojson.option] }
end

module TextDocumentSyncOptions : sig
  type t =
    { openClose : bool option [@yojson.option]
    ; change : int option [@yojson.option]
    ; willSave : bool option [@yojson.option]
    ; willSaveWaitUntil : bool option [@yojson.option]
    ; save : SaveOptions.t option [@yojson.option]
    }
end

module ServerCapabilities : sig
  type workspace =
    { workspaceFolders : WorkspaceFoldersServerCapabilities.t option
          [@yojson.option]
    }

  type t =
    { textDocumentSync :
        [ `TextDocumentSyncOptions of TextDocumentSyncOptions.t | `Int of int ]
        option
          [@yojson.option]
    ; completionProvider : CompletionOptions.t option [@yojson.option]
    ; hoverProvider : [ `Bool of bool | `HoverOptions of HoverOptions.t ] option
          [@yojson.option]
    ; signatureHelpProvider : SignatureHelpOptions.t option [@yojson.option]
    ; declarationProvider :
        [ `Bool of bool
        | `DeclarationOptions of DeclarationOptions.t
        | `DeclarationRegistrationOptions of DeclarationRegistrationOptions.t
        ]
        option
          [@yojson.option]
    ; definitionProvider :
        [ `Bool of bool | `DefinitionOptions of DefinitionOptions.t ] option
          [@yojson.option]
    ; typeDefinitionProvider :
        [ `Bool of bool
        | `TypeDefinitionOptions of TypeDefinitionOptions.t
        | `TypeDefinitionRegistrationOptions of
          TypeDefinitionRegistrationOptions.t
        ]
        option
          [@yojson.option]
    ; implementationProvider :
        [ `Bool of bool
        | `ImplementationOptions of ImplementationOptions.t
        | `ImplementationRegistrationOptions of
          ImplementationRegistrationOptions.t
        ]
        option
          [@yojson.option]
    ; referencesProvider :
        [ `Bool of bool | `ReferenceOptions of ReferenceOptions.t ] option
          [@yojson.option]
    ; documentHighlightProvider :
        [ `Bool of bool
        | `DocumentHighlightOptions of DocumentHighlightOptions.t
        ]
        option
          [@yojson.option]
    ; documentSymbolProvider :
        [ `Bool of bool | `DocumentSymbolOptions of DocumentSymbolOptions.t ]
        option
          [@yojson.option]
    ; codeActionProvider :
        [ `Bool of bool | `CodeActionOptions of CodeActionOptions.t ] option
          [@yojson.option]
    ; codeLensProvider : CodeLensOptions.t option [@yojson.option]
    ; documentLinkProvider : DocumentLinkOptions.t option [@yojson.option]
    ; colorProvider :
        [ `Bool of bool
        | `DocumentColorOptions of DocumentColorOptions.t
        | `DocumentColorRegistrationOptions of
          DocumentColorRegistrationOptions.t
        ]
        option
          [@yojson.option]
    ; documentFormattingProvider :
        [ `Bool of bool
        | `DocumentFormattingOptions of DocumentFormattingOptions.t
        ]
        option
          [@yojson.option]
    ; documentRangeFormattingProvider :
        [ `Bool of bool
        | `DocumentRangeFormattingOptions of DocumentRangeFormattingOptions.t
        ]
        option
          [@yojson.option]
    ; documentOnTypeFormattingProvider :
        DocumentOnTypeFormattingOptions.t option
          [@yojson.option]
    ; renameProvider :
        [ `Bool of bool | `RenameOptions of RenameOptions.t ] option
          [@yojson.option]
    ; foldingRangeProvider :
        [ `Bool of bool
        | `FoldingRangeOptions of FoldingRangeOptions.t
        | `FoldingRangeRegistrationOptions of FoldingRangeRegistrationOptions.t
        ]
        option
          [@yojson.option]
    ; executeCommandProvider : ExecuteCommandOptions.t option [@yojson.option]
    ; selectionRangeProvider :
        [ `Bool of bool
        | `SelectionRangeOptions of SelectionRangeOptions.t
        | `SelectionRangeRegistrationOptions of
          SelectionRangeRegistrationOptions.t
        ]
        option
          [@yojson.option]
    ; workspaceSymbolProvider : bool option [@yojson.option]
    ; workspace : workspace option [@yojson.option]
    ; experimental : Json.t option [@yojson.option]
    }
end

module InitializeResult : sig
  type serverInfo =
    { name : string
    ; version : string option [@yojson.option]
    }

  type t =
    { capabilities : ServerCapabilities.t
    ; serverInfo : serverInfo option [@yojson.option]
    }
end

module InitializedParams : sig end

module LocationLink : sig
  type t =
    { originSelectionRange : Range.t option [@yojson.option]
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
    ; params : [ `List of Json.t list | `Assoc of Json.t ] option
          [@yojson.option]
    }
end

module ParameterInformation : sig
  type t =
    { label : [ `String of string | `Offset of int * int ]
    ; documentation :
        [ `String of string | `MarkupContent of MarkupContent.t ] option
          [@yojson.option]
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
    ; version : int option [@yojson.option]
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
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; context : ReferenceContext.t
    }
end

module ReferenceRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    }
end

module Registration : sig
  type t =
    { id : string
    ; method_ : string [@key "method"]
    ; registerOptions : Json.t option [@yojson.option]
    }
end

module RegistrationParams : sig
  type t = { registrations : Registration.t list }
end

module RenameParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; newName : string
    }
end

module RenameRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; prepareProvider : bool option [@yojson.option]
    }
end

module RequestMessage : sig
  type t =
    { jsonrpc : string
    ; id : Jsonrpc.Id.t
    ; method_ : string [@key "method"]
    ; params : [ `List of Json.t list | `Assoc of Json.t ] option
          [@yojson.option]
    }
end

module ResponseError : sig
  type t =
    { code : int
    ; message : string
    ; data : Json.t option [@yojson.option]
    }
end

module ResponseMessage : sig
  type t =
    { jsonrpc : string
    ; id : Jsonrpc.Id.t option [@yojson.option]
    ; result :
        [ `String of string | `Int of int | `Bool of bool | `Assoc of Json.t ]
        option
        option
          [@yojson.option]
    ; error : ResponseError.t option [@yojson.option]
    }
end

module SelectionRange : sig
  type t =
    { range : Range.t
    ; parent : t option [@yojson.option]
    }
end

module SelectionRangeParams : sig
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
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
    ; actions : MessageActionItem.t list option [@yojson.option]
    }
end

module SignatureInformation : sig
  type t =
    { label : string
    ; documentation :
        [ `String of string | `MarkupContent of MarkupContent.t ] option
          [@yojson.option]
    ; parameters : ParameterInformation.t list option [@yojson.option]
    }
end

module SignatureHelp : sig
  type t =
    { signatures : SignatureInformation.t list
    ; activeSignature : int option [@yojson.option]
    ; activeParameter : int option [@yojson.option]
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
    ; triggerCharacter : string option [@yojson.option]
    ; isRetrigger : bool
    ; activeSignatureHelp : SignatureHelp.t option [@yojson.option]
    }
end

module SignatureHelpParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; context : SignatureHelpContext.t option [@yojson.option]
    }
end

module SignatureHelpRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; triggerCharacters : string list option [@yojson.option]
    ; retriggerCharacters : string list option [@yojson.option]
    }
end

module SymbolInformation : sig
  type t =
    { name : string
    ; kind : SymbolKind.t
    ; deprecated : bool option [@yojson.option]
    ; location : Location.t
    ; containerName : string option [@yojson.option]
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
    ; includeText : bool option [@yojson.option]
    }
end

module TypeDefinitionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
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
    ; cancellable : bool option [@yojson.option]
    ; message : string option [@yojson.option]
    ; percentage : int option [@yojson.option]
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
    ; message : string option [@yojson.option]
    }
end

module WorkDoneProgressReport : sig
  type t =
    { kind : unit
    ; cancellable : bool option [@yojson.option]
    ; message : string option [@yojson.option]
    ; percentage : int option [@yojson.option]
    }
end

module WorkspaceSymbolOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
end

module WorkspaceSymbolParams : sig
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; query : string
    }
end

module WorkspaceSymbolRegistrationOptions : sig
  type t = { workDoneProgress : bool option [@yojson.option] }
end

open! Import

[@@@warning "-30"]

module DeleteFileOptions = struct
  type t =
    { recursive : bool option [@yojson.option]
    ; ignoreIfNotExists : bool option [@yojson.option]
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
    ; options : DeleteFileOptions.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameFileOptions = struct
  type t =
    { overwrite : bool option [@yojson.option]
    ; ignoreIfExists : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameFile = struct
  type t =
    { kind : unit
    ; oldUri : DocumentUri.t
    ; newUri : DocumentUri.t
    ; options : RenameFileOptions.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CreateFileOptions = struct
  type t =
    { overwrite : bool option [@yojson.option]
    ; ignoreIfExists : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CreateFile = struct
  type t =
    { kind : unit
    ; uri : DocumentUri.t
    ; options : CreateFileOptions.t option [@yojson.option]
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
  type t =
    { uri : DocumentUri.t
    ; version : int option [@yojson.option]
    }
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
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ApplyWorkspaceEditParams = struct
  type t =
    { label : string option [@yojson.option]
    ; edit : WorkspaceEdit.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ApplyWorkspaceEditResponse = struct
  type t =
    { applied : bool
    ; failureReason : string option [@yojson.option]
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
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FoldingRangeClientCapabilities = struct
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; rangeLimit : int option [@yojson.option]
    ; lineFoldingOnly : bool option [@yojson.option]
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
    { relatedInformation : bool option [@yojson.option]
    ; tagSupport : tagSupport option [@yojson.option]
    ; versionSupport : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameClientCapabilities = struct
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; prepareSupport : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentOnTypeFormattingClientCapabilities = struct
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentRangeFormattingClientCapabilities = struct
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFormattingClientCapabilities = struct
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentColorClientCapabilities = struct
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkClientCapabilities = struct
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; tooltipSupport : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensClientCapabilities = struct
  type t = { dynamicRegistration : bool option [@yojson.option] }
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
    { dynamicRegistration : bool option [@yojson.option]
    ; codeActionLiteralSupport : codeActionLiteralSupport option
          [@yojson.option]
    ; isPreferredSupport : bool option [@yojson.option]
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
  type symbolKind = { valueSet : SymbolKind.t list option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; symbolKind : symbolKind option [@yojson.option]
    ; hierarchicalDocumentSymbolSupport : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlightClientCapabilities = struct
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceClientCapabilities = struct
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ImplementationClientCapabilities = struct
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; linkSupport : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TypeDefinitionClientCapabilities = struct
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; linkSupport : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DefinitionClientCapabilities = struct
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; linkSupport : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DeclarationClientCapabilities = struct
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; linkSupport : bool option [@yojson.option]
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
  type parameterInformation =
    { labelOffsetSupport : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type signatureInformation =
    { documentationFormat : MarkupKind.t list option [@yojson.option]
    ; parameterInformation : parameterInformation option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; signatureInformation : signatureInformation option [@yojson.option]
    ; contextSupport : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module HoverClientCapabilities = struct
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; contentFormat : MarkupKind.t list option [@yojson.option]
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
  type completionItemKind =
    { valueSet : CompletionItemKind.t list option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type tagSupport = { valueSet : CompletionItemTag.t list }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type completionItem =
    { snippetSupport : bool option [@yojson.option]
    ; commitCharactersSupport : bool option [@yojson.option]
    ; documentationFormat : MarkupKind.t list option [@yojson.option]
    ; deprecatedSupport : bool option [@yojson.option]
    ; preselectSupport : bool option [@yojson.option]
    ; tagSupport : tagSupport option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; completionItem : completionItem option [@yojson.option]
    ; completionItemKind : completionItemKind option [@yojson.option]
    ; contextSupport : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentSyncClientCapabilities = struct
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; willSave : bool option [@yojson.option]
    ; willSaveWaitUntil : bool option [@yojson.option]
    ; didSave : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentClientCapabilities = struct
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

module ExecuteCommandClientCapabilities = struct
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolClientCapabilities = struct
  type symbolKind = { valueSet : SymbolKind.t list option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; symbolKind : symbolKind option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeWatchedFilesClientCapabilities = struct
  type t = { dynamicRegistration : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeConfigurationClientCapabilities = struct
  type t = { dynamicRegistration : bool option [@yojson.option] }
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
    { documentChanges : bool option [@yojson.option]
    ; resourceOperations : ResourceOperationKind.t list option [@yojson.option]
    ; failureHandling : FailureHandlingKind.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ClientCapabilities = struct
  type window = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

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
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { workspace : workspace option [@yojson.option]
    ; textDocument : TextDocumentClientCapabilities.t option [@yojson.option]
    ; window : window option [@yojson.option]
    ; experimental : Json.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Command = struct
  type t =
    { title : string
    ; command : string
    ; arguments : Json.t list option [@yojson.option]
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
    ; severity : DiagnosticSeverity.t option [@yojson.option]
    ; code : Jsonrpc.Id.t option [@yojson.option]
    ; source : string option [@yojson.option]
    ; message : string
    ; tags : DiagnosticTag.t list option [@yojson.option]
    ; relatedInformation : DiagnosticRelatedInformation.t list option
          [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeAction = struct
  type t =
    { title : string
    ; kind : CodeActionKind.t option [@yojson.option]
    ; diagnostics : Diagnostic.t list option [@yojson.option]
    ; isPreferred : bool option [@yojson.option]
    ; edit : WorkspaceEdit.t option [@yojson.option]
    ; command : Command.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionContext = struct
  type t =
    { diagnostics : Diagnostic.t list
    ; only : CodeActionKind.t list option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressOptions = struct
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionOptions = struct
  type t =
    { workDoneProgress : bool option [@yojson.option]
    ; codeActionKinds : CodeActionKind.t list option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ProgressToken = struct
  type t = Jsonrpc.Id.t
end

module PartialResultParams = struct
  type t = { partialResultToken : ProgressToken.t option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressParams = struct
  type t = { workDoneToken : ProgressToken.t option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionParams = struct
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; context : CodeActionContext.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFilter = struct
  type t =
    { language : string option [@yojson.option]
    ; scheme : string option [@yojson.option]
    ; pattern : string option [@yojson.option]
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

module CodeActionRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; codeActionKinds : CodeActionKind.t list option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLens = struct
  type t =
    { range : Range.t
    ; command : Command.t option [@yojson.option]
    ; data : Json.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensOptions = struct
  type t =
    { workDoneProgress : bool option [@yojson.option]
    ; resolveProvider : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensParams = struct
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; resolveProvider : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

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
    ; textEdit : TextEdit.t option [@yojson.option]
    ; additionalTextEdits : TextEdit.t list option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ColorPresentationParams = struct
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
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
    ; triggerCharacter : string option [@yojson.option]
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
    { workDoneProgress : bool option [@yojson.option]
    ; triggerCharacters : string list option [@yojson.option]
    ; allCommitCharacters : string list option [@yojson.option]
    ; resolveProvider : bool option [@yojson.option]
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
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; context : CompletionContext.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; triggerCharacters : string list option [@yojson.option]
    ; allCommitCharacters : string list option [@yojson.option]
    ; resolveProvider : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ConfigurationItem = struct
  type t =
    { scopeUri : DocumentUri.t option [@yojson.option]
    ; section : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ConfigurationParams = struct
  type t = { items : ConfigurationItem.t list }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DeclarationOptions = struct
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DeclarationParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module StaticRegistrationOptions = struct
  type t = { id : string option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DeclarationRegistrationOptions = struct
  type t =
    { workDoneProgress : bool option [@yojson.option]
    ; documentSelector : DocumentSelector.t option [@yojson.option]
    ; id : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DefinitionOptions = struct
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DefinitionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DefinitionRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeConfigurationParams = struct
  type t = { settings : Json.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentContentChangeEvent = struct
  type t =
    { range : Range.t
    ; rangeLength : int option [@yojson.option]
    ; text : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
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
    ; kind : int option [@yojson.option]
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
    ; text : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentColorOptions = struct
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentColorParams = struct
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentColorRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; id : string option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFormattingOptions = struct
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FormattingOptions = struct
  type t =
    { tabSize : int
    ; insertSpaces : bool
    ; trimTrailingWhitespace : bool option [@yojson.option]
    ; insertFinalNewline : bool option [@yojson.option]
    ; trimFinalNewlines : bool option [@yojson.option]
    ; key : (string * [ `Bool of bool | `Int of int | `String of string ]) list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFormattingParams = struct
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
    ; options : FormattingOptions.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFormattingRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlight = struct
  type t =
    { range : Range.t
    ; kind : int option [@yojson.option]
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

module DocumentHighlightOptions = struct
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlightParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlightRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLink = struct
  type t =
    { range : Range.t
    ; target : DocumentUri.t option [@yojson.option]
    ; tooltip : string option [@yojson.option]
    ; data : Json.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkOptions = struct
  type t =
    { workDoneProgress : bool option [@yojson.option]
    ; resolveProvider : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkParams = struct
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; resolveProvider : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentOnTypeFormattingOptions = struct
  type t =
    { firstTriggerCharacter : string
    ; moreTriggerCharacter : string list option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentOnTypeFormattingParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; ch : string
    ; options : FormattingOptions.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentOnTypeFormattingRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; firstTriggerCharacter : string
    ; moreTriggerCharacter : string list option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentRangeFormattingOptions = struct
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentRangeFormattingParams = struct
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; options : FormattingOptions.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentRangeFormattingRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSymbol = struct
  type t =
    { name : string
    ; detail : string option [@yojson.option]
    ; kind : SymbolKind.t
    ; deprecated : bool option [@yojson.option]
    ; range : Range.t
    ; selectionRange : Range.t
    ; children : t list option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSymbolOptions = struct
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSymbolParams = struct
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSymbolRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

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
  type t =
    { workDoneProgress : bool option [@yojson.option]
    ; commands : string list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ExecuteCommandParams = struct
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; command : string
    ; arguments : Json.t list option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ExecuteCommandRegistrationOptions = struct
  type t =
    { workDoneProgress : bool option [@yojson.option]
    ; commands : string list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FileChangeType = struct
  type t =
    | Created
    | Changed
    | Deleted
end

module FoldingRange = struct
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

module FoldingRangeKind = struct
  type t =
    | Comment
    | Imports
    | Region
end

module FoldingRangeOptions = struct
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FoldingRangeParams = struct
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FoldingRangeRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; id : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module MarkedString = struct
  type t = unit
end

module Hover = struct
  type t =
    { contents :
        [ `MarkedString of MarkedString.t
        | `List of MarkedString.t list
        | `MarkupContent of MarkupContent.t
        ]
    ; range : Range.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module HoverOptions = struct
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module HoverParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module HoverRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ImplementationOptions = struct
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ImplementationParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ImplementationRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; id : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module InitializeError = struct
  type t = UnknownProtocolVersion
end

module InitializeParams = struct
  type clientInfo =
    { name : string
    ; version : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

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
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceFoldersServerCapabilities = struct
  type t =
    { supported : bool option [@yojson.option]
    ; changeNotifications : [ `String of string | `Bool of bool ] option
          [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRangeOptions = struct
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRangeRegistrationOptions = struct
  type t =
    { workDoneProgress : bool option [@yojson.option]
    ; documentSelector : DocumentSelector.t option [@yojson.option]
    ; id : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameOptions = struct
  type t =
    { workDoneProgress : bool option [@yojson.option]
    ; prepareProvider : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceOptions = struct
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TypeDefinitionOptions = struct
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TypeDefinitionRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; id : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelpOptions = struct
  type t =
    { workDoneProgress : bool option [@yojson.option]
    ; triggerCharacters : string list option [@yojson.option]
    ; retriggerCharacters : string list option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SaveOptions = struct
  type t = { includeText : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentSyncOptions = struct
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

module ServerCapabilities = struct
  type workspace =
    { workspaceFolders : WorkspaceFoldersServerCapabilities.t option
          [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

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
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module InitializeResult = struct
  type serverInfo =
    { name : string
    ; version : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { capabilities : ServerCapabilities.t
    ; serverInfo : serverInfo option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module InitializedParams = struct end

module LocationLink = struct
  type t =
    { originSelectionRange : Range.t option [@yojson.option]
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
    { jsonrpc : string
    ; method_ : string [@key "method"]
    ; params : [ `List of Json.t list | `Assoc of Json.t ] option
          [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ParameterInformation = struct
  type t =
    { label : [ `String of string | `Offset of int * int ]
    ; documentation :
        [ `String of string | `MarkupContent of MarkupContent.t ] option
          [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module PrepareRenameParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

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
    ; version : int option [@yojson.option]
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
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; context : ReferenceContext.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Registration = struct
  type t =
    { id : string
    ; method_ : string [@key "method"]
    ; registerOptions : Json.t option [@yojson.option]
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
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; newName : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; prepareProvider : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RequestMessage = struct
  type t =
    { jsonrpc : string
    ; id : Jsonrpc.Id.t
    ; method_ : string [@key "method"]
    ; params : [ `List of Json.t list | `Assoc of Json.t ] option
          [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ResponseError = struct
  type t =
    { code : int
    ; message : string
    ; data : Json.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ResponseMessage = struct
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
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRange = struct
  type t =
    { range : Range.t
    ; parent : t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRangeParams = struct
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; textDocument : TextDocumentIdentifier.t
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
    ; actions : MessageActionItem.t list option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureInformation = struct
  type t =
    { label : string
    ; documentation :
        [ `String of string | `MarkupContent of MarkupContent.t ] option
          [@yojson.option]
    ; parameters : ParameterInformation.t list option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelp = struct
  type t =
    { signatures : SignatureInformation.t list
    ; activeSignature : int option [@yojson.option]
    ; activeParameter : int option [@yojson.option]
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
    ; triggerCharacter : string option [@yojson.option]
    ; isRetrigger : bool
    ; activeSignatureHelp : SignatureHelp.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelpParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; context : SignatureHelpContext.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelpRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; workDoneProgress : bool option [@yojson.option]
    ; triggerCharacters : string list option [@yojson.option]
    ; retriggerCharacters : string list option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SymbolInformation = struct
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

module TextDocumentSyncKind = struct
  type t =
    | None
    | Full
    | Incremental
end

module TextDocumentChangeRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; syncKind : TextDocumentSyncKind.t
    }
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
  type t =
    { documentSelector : DocumentSelector.t option [@yojson.option]
    ; includeText : bool option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TypeDefinitionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

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
    ; cancellable : bool option [@yojson.option]
    ; message : string option [@yojson.option]
    ; percentage : int option [@yojson.option]
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
    ; message : string option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressReport = struct
  type t =
    { kind : unit
    ; cancellable : bool option [@yojson.option]
    ; message : string option [@yojson.option]
    ; percentage : int option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolOptions = struct
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolParams = struct
  type t =
    { workDoneToken : ProgressToken.t option [@yojson.option]
    ; partialResultToken : ProgressToken.t option [@yojson.option]
    ; query : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolRegistrationOptions = struct
  type t = { workDoneProgress : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

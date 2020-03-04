open! Import

[@@@warning "-30"]

module DeleteFileOptions = struct
  type t =
    { recursive : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; ignoreIfNotExists : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; options : DeleteFileOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameFileOptions = struct
  type t =
    { overwrite : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; ignoreIfExists : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameFile = struct
  type t =
    { kind : unit
    ; oldUri : DocumentUri.t
    ; newUri : DocumentUri.t
    ; options : RenameFileOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CreateFileOptions = struct
  type t =
    { overwrite : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; ignoreIfExists : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CreateFile = struct
  type t =
    { kind : unit
    ; uri : DocumentUri.t
    ; options : CreateFileOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; version : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    { changes : (DocumentUri.t * TextEdit.t list) list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentChanges :
        [ `TextDocumentEdit of TextDocumentEdit.t
        | `CreateFile of CreateFile.t
        | `RenameFile of RenameFile.t
        | `DeleteFile of DeleteFile.t
        ]
        list
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ApplyWorkspaceEditParams = struct
  type t =
    { label : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; edit : WorkspaceEdit.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ApplyWorkspaceEditResponse = struct
  type t =
    { applied : bool
    ; failureReason : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FoldingRangeClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; rangeLimit : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; lineFoldingOnly : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    { relatedInformation : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; tagSupport : tagSupport Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; versionSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; prepareSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentOnTypeFormattingClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentRangeFormattingClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFormattingClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentColorClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; tooltipSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
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
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeActionLiteralSupport : codeActionLiteralSupport Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; isPreferredSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
  type symbolKind =
    { valueSet : SymbolKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; symbolKind : symbolKind Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; hierarchicalDocumentSymbolSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlightClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ImplementationClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; linkSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TypeDefinitionClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; linkSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DefinitionClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; linkSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DeclarationClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; linkSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    { labelOffsetSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type signatureInformation =
    { documentationFormat : MarkupKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; parameterInformation : parameterInformation Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; signatureInformation : signatureInformation Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; contextSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module HoverClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; contentFormat : MarkupKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    { valueSet : CompletionItemKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type tagSupport = { valueSet : CompletionItemTag.t list }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type completionItem =
    { snippetSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; commitCharactersSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentationFormat : MarkupKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; deprecatedSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; preselectSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; tagSupport : tagSupport Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; completionItem : completionItem Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; completionItemKind : completionItemKind Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; contextSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentSyncClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; willSave : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; willSaveWaitUntil : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; didSave : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentClientCapabilities = struct
  type t =
    { synchronization :
        TextDocumentSyncClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; completion : CompletionClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; hover : HoverClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; signatureHelp : SignatureHelpClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; declaration : DeclarationClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; definition : DefinitionClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; typeDefinition : TypeDefinitionClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; implementation : ImplementationClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; references : ReferenceClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentHighlight :
        DocumentHighlightClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentSymbol : DocumentSymbolClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeAction : CodeActionClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeLens : CodeLensClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentLink : DocumentLinkClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; colorProvider : DocumentColorClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; formatting : DocumentFormattingClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; rangeFormatting :
        DocumentRangeFormattingClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; onTypeFormatting :
        DocumentOnTypeFormattingClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; rename : RenameClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; publishDiagnostics :
        PublishDiagnosticsClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; foldingRange : FoldingRangeClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; selectionRange : SelectionRangeClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ExecuteCommandClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolClientCapabilities = struct
  type symbolKind =
    { valueSet : SymbolKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; symbolKind : symbolKind Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeWatchedFilesClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeConfigurationClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
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
    { documentChanges : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; resourceOperations : ResourceOperationKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; failureHandling : FailureHandlingKind.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ClientCapabilities = struct
  type window =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type workspace =
    { applyEdit : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workspaceEdit : WorkspaceEditClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; didChangeConfiguration :
        DidChangeConfigurationClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; didChangeWatchedFiles :
        DidChangeWatchedFilesClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; symbol : WorkspaceSymbolClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; executeCommand : ExecuteCommandClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workspaceFolders : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; configuration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { workspace : workspace Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; textDocument : TextDocumentClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; window : window Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; experimental : Json.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Command = struct
  type t =
    { title : string
    ; command : string
    ; arguments : Json.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; severity : DiagnosticSeverity.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; code : Jsonrpc.Id.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; source : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; message : string
    ; tags : DiagnosticTag.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; relatedInformation :
        DiagnosticRelatedInformation.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeAction = struct
  type t =
    { title : string
    ; kind : CodeActionKind.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; diagnostics : Diagnostic.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; isPreferred : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; edit : WorkspaceEdit.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; command : Command.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionContext = struct
  type t =
    { diagnostics : Diagnostic.t list
    ; only : CodeActionKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeActionKinds : CodeActionKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ProgressToken = struct
  type t = Jsonrpc.Id.t
end

module PartialResultParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressParams = struct
  type t =
    { workDoneToken : ProgressToken.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
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
    { language : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; scheme : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; pattern : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSelector = struct
  type t = DocumentFilter.t list
end

module TextDocumentRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeActionKinds : CodeActionKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLens = struct
  type t =
    { range : Range.t
    ; command : Command.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; data : Json.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; textEdit : TextEdit.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; additionalTextEdits : TextEdit.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; triggerCharacter : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; kind : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; tags : CompletionItemTag.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; detail : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentation :
        [ `String of string | `MarkupContent of MarkupContent.t ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; deprecated : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; preselect : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; sortText : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; filterText : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; insertText : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; insertTextFormat : InsertTextFormat.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; textEdit : TextEdit.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; additionalTextEdits : TextEdit.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; commitCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; command : Command.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; triggerCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; allCommitCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; context : CompletionContext.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; triggerCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; allCommitCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ConfigurationItem = struct
  type t =
    { scopeUri : DocumentUri.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; section : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DeclarationParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module StaticRegistrationOptions = struct
  type t =
    { id : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DeclarationRegistrationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DefinitionOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DefinitionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DefinitionRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; rangeLength : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; kind : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; text : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentColorOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentColorParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentColorRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFormattingOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FormattingOptions = struct
  type t =
    { tabSize : int
    ; insertSpaces : bool
    ; trimTrailingWhitespace : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; insertFinalNewline : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; trimFinalNewlines : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; key : (string * [ `Bool of bool | `Int of int | `String of string ]) list
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

module DocumentFormattingRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlight = struct
  type t =
    { range : Range.t
    ; kind : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlightParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlightRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLink = struct
  type t =
    { range : Range.t
    ; target : DocumentUri.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; tooltip : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; data : Json.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentOnTypeFormattingOptions = struct
  type t =
    { firstTriggerCharacter : string
    ; moreTriggerCharacter : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; firstTriggerCharacter : string
    ; moreTriggerCharacter : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentRangeFormattingOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentRangeFormattingParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; options : FormattingOptions.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentRangeFormattingRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSymbol = struct
  type t =
    { name : string
    ; detail : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; kind : SymbolKind.t
    ; deprecated : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; range : Range.t
    ; selectionRange : Range.t
    ; children : t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSymbolOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSymbolParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSymbolRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; commands : string list
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ExecuteCommandParams = struct
  type t =
    { command : string
    ; arguments : Json.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ExecuteCommandRegistrationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; startCharacter : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; endLine : int
    ; endCharacter : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; kind : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FoldingRangeParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FoldingRangeRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; range : Range.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module HoverOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module HoverParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module HoverRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ImplementationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ImplementationParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ImplementationRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; version : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { processId : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; clientInfo : clientInfo Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; rootPath : string Json.Nullable_option.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; rootUri : DocumentUri.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; initializationOptions : Json.t option [@yojson.option]
    ; capabilities : ClientCapabilities.t
    ; trace : [ `Off | `Messages | `Verbose ] Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workspaceFolders :
        WorkspaceFolder.t list Json.Nullable_option.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceFoldersServerCapabilities = struct
  type t =
    { supported : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; changeNotifications :
        [ `String of string | `Bool of bool ] Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRangeOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRangeRegistrationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; prepareProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TypeDefinitionOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TypeDefinitionRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelpOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; triggerCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; retriggerCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SaveOptions = struct
  type t =
    { includeText : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentSyncOptions = struct
  type t =
    { openClose : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; change : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; willSave : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; willSaveWaitUntil : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; save : SaveOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ServerCapabilities = struct
  type workspace =
    { workspaceFolders :
        WorkspaceFoldersServerCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { textDocumentSync :
        [ `TextDocumentSyncOptions of TextDocumentSyncOptions.t | `Int of int ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; completionProvider : CompletionOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; hoverProvider :
        [ `Bool of bool | `HoverOptions of HoverOptions.t ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; signatureHelpProvider : SignatureHelpOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; declarationProvider :
        [ `Bool of bool
        | `DeclarationOptions of DeclarationOptions.t
        | `DeclarationRegistrationOptions of DeclarationRegistrationOptions.t
        ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; definitionProvider :
        [ `Bool of bool | `DefinitionOptions of DefinitionOptions.t ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; typeDefinitionProvider :
        [ `Bool of bool
        | `TypeDefinitionOptions of TypeDefinitionOptions.t
        | `TypeDefinitionRegistrationOptions of
          TypeDefinitionRegistrationOptions.t
        ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; implementationProvider :
        [ `Bool of bool
        | `ImplementationOptions of ImplementationOptions.t
        | `ImplementationRegistrationOptions of
          ImplementationRegistrationOptions.t
        ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; referencesProvider :
        [ `Bool of bool | `ReferenceOptions of ReferenceOptions.t ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentHighlightProvider :
        [ `Bool of bool
        | `DocumentHighlightOptions of DocumentHighlightOptions.t
        ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentSymbolProvider :
        [ `Bool of bool | `DocumentSymbolOptions of DocumentSymbolOptions.t ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeActionProvider :
        [ `Bool of bool | `CodeActionOptions of CodeActionOptions.t ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeLensProvider : CodeLensOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentLinkProvider : DocumentLinkOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; colorProvider :
        [ `Bool of bool
        | `DocumentColorOptions of DocumentColorOptions.t
        | `DocumentColorRegistrationOptions of
          DocumentColorRegistrationOptions.t
        ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentFormattingProvider :
        [ `Bool of bool
        | `DocumentFormattingOptions of DocumentFormattingOptions.t
        ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentRangeFormattingProvider :
        [ `Bool of bool
        | `DocumentRangeFormattingOptions of DocumentRangeFormattingOptions.t
        ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentOnTypeFormattingProvider :
        DocumentOnTypeFormattingOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; renameProvider :
        [ `Bool of bool | `RenameOptions of RenameOptions.t ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; foldingRangeProvider :
        [ `Bool of bool
        | `FoldingRangeOptions of FoldingRangeOptions.t
        | `FoldingRangeRegistrationOptions of FoldingRangeRegistrationOptions.t
        ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; executeCommandProvider : ExecuteCommandOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; selectionRangeProvider :
        [ `Bool of bool
        | `SelectionRangeOptions of SelectionRangeOptions.t
        | `SelectionRangeRegistrationOptions of
          SelectionRangeRegistrationOptions.t
        ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workspaceSymbolProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workspace : workspace Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; experimental : Json.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module InitializeResult = struct
  type serverInfo =
    { name : string
    ; version : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { capabilities : ServerCapabilities.t
    ; serverInfo : serverInfo Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module LocationLink = struct
  type t =
    { originSelectionRange : Range.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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

module ParameterInformation = struct
  type t =
    { label : [ `String of string | `Offset of int * int ]
    ; documentation :
        [ `String of string | `MarkupContent of MarkupContent.t ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; version : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; context : ReferenceContext.t
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; newName : string
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; prepareProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRange = struct
  type t =
    { range : Range.t
    ; parent : t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; actions : MessageActionItem.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureInformation = struct
  type t =
    { label : string
    ; documentation :
        [ `String of string | `MarkupContent of MarkupContent.t ]
        Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; parameters : ParameterInformation.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelp = struct
  type t =
    { signatures : SignatureInformation.t list
    ; activeSignature : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; activeParameter : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; triggerCharacter : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; isRetrigger : bool
    ; activeSignatureHelp : SignatureHelp.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelpParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; context : SignatureHelpContext.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelpRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; triggerCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; retriggerCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SymbolInformation = struct
  type t =
    { name : string
    ; kind : SymbolKind.t
    ; deprecated : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; location : Location.t
    ; containerName : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; includeText : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TypeDefinitionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
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
    ; cancellable : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; message : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; percentage : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
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
    ; message : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressReport = struct
  type t =
    { kind : unit
    ; cancellable : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; message : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; percentage : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolParams = struct
  type t = { query : string } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolRegistrationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

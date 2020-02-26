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

module DocumentUri = struct end

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
  type t = { version : unit } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentEdit = struct
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; edits : unit
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceEdit = struct
  type t =
    { changes : changes option [@yojson.option]
    ; documentChanges : unit option [@yojson.option]
    }

  and changes = { uri : unit } [@@deriving_inline] [@@yojson.allow_extra_fields]

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
  type t = { id : unit } [@@deriving_inline] [@@yojson.allow_extra_fields]

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

  let to_yojson t =
    match t with
    | Unnecessary -> `Int 1
    | Deprecated -> `Int 2

  let of_yojson json =
    match json with
    | `Int 1 -> Unnecessary
    | `Int 2 -> Deprecated
    | _ -> assert false
end

module PublishDiagnosticsClientCapabilities = struct
  type t =
    { relatedInformation : bool option [@yojson.option]
    ; tagSupport : tagSupport option [@yojson.option]
    ; versionSupport : bool option [@yojson.option]
    }

  and tagSupport = { valueSet : unit }
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

  let to_yojson t =
    match t with
    | Empty -> `String ""
    | QuickFix -> `String "quickfix"
    | Refactor -> `String "refactor"
    | RefactorExtract -> `String "refactor.extract"
    | RefactorInline -> `String "refactor.inline"
    | RefactorRewrite -> `String "refactor.rewrite"
    | Source -> `String "source"
    | SourceOrganizeImports -> `String "source.organizeImports"

  let of_yojson json =
    match json with
    | `String "" -> Empty
    | `String "quickfix" -> QuickFix
    | `String "refactor" -> Refactor
    | `String "refactor.extract" -> RefactorExtract
    | `String "refactor.inline" -> RefactorInline
    | `String "refactor.rewrite" -> RefactorRewrite
    | `String "source" -> Source
    | `String "source.organizeImports" -> SourceOrganizeImports
    | _ -> assert false
end

module CodeActionClientCapabilities = struct
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

  let to_yojson t =
    match t with
    | File -> `Int 1
    | Module -> `Int 2
    | Namespace -> `Int 3
    | Package -> `Int 4
    | Class -> `Int 5
    | Method -> `Int 6
    | Property -> `Int 7
    | Field -> `Int 8
    | Constructor -> `Int 9
    | Enum -> `Int 10
    | Interface -> `Int 11
    | Function -> `Int 12
    | Variable -> `Int 13
    | Constant -> `Int 14
    | String -> `Int 15
    | Number -> `Int 16
    | Boolean -> `Int 17
    | Array -> `Int 18
    | Object -> `Int 19
    | Key -> `Int 20
    | Null -> `Int 21
    | EnumMember -> `Int 22
    | Struct -> `Int 23
    | Event -> `Int 24
    | Operator -> `Int 25
    | TypeParameter -> `Int 26

  let of_yojson json =
    match json with
    | `Int 1 -> File
    | `Int 2 -> Module
    | `Int 3 -> Namespace
    | `Int 4 -> Package
    | `Int 5 -> Class
    | `Int 6 -> Method
    | `Int 7 -> Property
    | `Int 8 -> Field
    | `Int 9 -> Constructor
    | `Int 10 -> Enum
    | `Int 11 -> Interface
    | `Int 12 -> Function
    | `Int 13 -> Variable
    | `Int 14 -> Constant
    | `Int 15 -> String
    | `Int 16 -> Number
    | `Int 17 -> Boolean
    | `Int 18 -> Array
    | `Int 19 -> Object
    | `Int 20 -> Key
    | `Int 21 -> Null
    | `Int 22 -> EnumMember
    | `Int 23 -> Struct
    | `Int 24 -> Event
    | `Int 25 -> Operator
    | `Int 26 -> TypeParameter
    | _ -> assert false
end

module DocumentSymbolClientCapabilities = struct
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; symbolKind : symbolKind option [@yojson.option]
    ; hierarchicalDocumentSymbolSupport : bool option [@yojson.option]
    }

  and symbolKind = { valueSet : unit option [@yojson.option] }
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

  let to_yojson t =
    match t with
    | PlainText -> `String "plaintext"
    | Markdown -> `String "markdown"

  let of_yojson json =
    match json with
    | `String "plaintext" -> PlainText
    | `String "markdown" -> Markdown
    | _ -> assert false
end

module SignatureHelpClientCapabilities = struct
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

module HoverClientCapabilities = struct
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; contentFormat : unit option [@yojson.option]
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

  let to_yojson t =
    match t with
    | Text -> `Int 1
    | Method -> `Int 2
    | Function -> `Int 3
    | Constructor -> `Int 4
    | Field -> `Int 5
    | Variable -> `Int 6
    | Class -> `Int 7
    | Interface -> `Int 8
    | Module -> `Int 9
    | Property -> `Int 10
    | Unit -> `Int 11
    | Value -> `Int 12
    | Enum -> `Int 13
    | Keyword -> `Int 14
    | Snippet -> `Int 15
    | Color -> `Int 16
    | File -> `Int 17
    | Reference -> `Int 18
    | Folder -> `Int 19
    | EnumMember -> `Int 20
    | Constant -> `Int 21
    | Struct -> `Int 22
    | Event -> `Int 23
    | Operator -> `Int 24
    | TypeParameter -> `Int 25

  let of_yojson json =
    match json with
    | `Int 1 -> Text
    | `Int 2 -> Method
    | `Int 3 -> Function
    | `Int 4 -> Constructor
    | `Int 5 -> Field
    | `Int 6 -> Variable
    | `Int 7 -> Class
    | `Int 8 -> Interface
    | `Int 9 -> Module
    | `Int 10 -> Property
    | `Int 11 -> Unit
    | `Int 12 -> Value
    | `Int 13 -> Enum
    | `Int 14 -> Keyword
    | `Int 15 -> Snippet
    | `Int 16 -> Color
    | `Int 17 -> File
    | `Int 18 -> Reference
    | `Int 19 -> Folder
    | `Int 20 -> EnumMember
    | `Int 21 -> Constant
    | `Int 22 -> Struct
    | `Int 23 -> Event
    | `Int 24 -> Operator
    | `Int 25 -> TypeParameter
    | _ -> assert false
end

module CompletionItemTag = struct
  type t = Deprecated

  let to_yojson t =
    match t with
    | Deprecated -> `Int 1

  let of_yojson json =
    match json with
    | `Int 1 -> Deprecated
    | _ -> assert false
end

module CompletionClientCapabilities = struct
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
  type t =
    { dynamicRegistration : bool option [@yojson.option]
    ; symbolKind : symbolKind option [@yojson.option]
    }

  and symbolKind = { valueSet : unit option [@yojson.option] }
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

  let to_yojson t =
    match t with
    | Abort -> `String "abort"
    | Transactional -> `String "transactional"
    | TextOnlyTransactional -> `String "textOnlyTransactional"
    | Undo -> `String "undo"

  let of_yojson json =
    match json with
    | `String "abort" -> Abort
    | `String "transactional" -> Transactional
    | `String "textOnlyTransactional" -> TextOnlyTransactional
    | `String "undo" -> Undo
    | _ -> assert false
end

module ResourceOperationKind = struct
  type t =
    | Create
    | Rename
    | Delete

  let to_yojson t =
    match t with
    | Create -> `String "create"
    | Rename -> `String "rename"
    | Delete -> `String "delete"

  let of_yojson json =
    match json with
    | `String "create" -> Create
    | `String "rename" -> Rename
    | `String "delete" -> Delete
    | _ -> assert false
end

module WorkspaceEditClientCapabilities = struct
  type t =
    { documentChanges : bool option [@yojson.option]
    ; resourceOperations : unit option [@yojson.option]
    ; failureHandling : FailureHandlingKind.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ClientCapabilities = struct
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

module Command = struct
  type t =
    { title : string
    ; command : string
    ; arguments : unit option [@yojson.option]
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

  let to_yojson t =
    match t with
    | Error -> `Int 1
    | Warning -> `Int 2
    | Information -> `Int 3
    | Hint -> `Int 4

  let of_yojson json =
    match json with
    | `Int 1 -> Error
    | `Int 2 -> Warning
    | `Int 3 -> Information
    | `Int 4 -> Hint
    | _ -> assert false
end

module Diagnostic = struct
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

module CodeAction = struct
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

module CodeActionContext = struct
  type t =
    { diagnostics : unit
    ; only : unit option [@yojson.option]
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
  type t = { codeActionKinds : unit option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ProgressToken = struct end

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
    { textDocument : TextDocumentIdentifier.t
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

module DocumentSelector = struct end

module TextDocumentRegistrationOptions = struct
  type t = { documentSelector : unit }
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
  type t = { resolveProvider : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
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
    ; additionalTextEdits : unit option [@yojson.option]
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

  let to_yojson t =
    match t with
    | Invoked -> `Int 1
    | TriggerCharacter -> `Int 2
    | TriggerForIncompleteCompletions -> `Int 3

  let of_yojson json =
    match json with
    | `Int 1 -> Invoked
    | `Int 2 -> TriggerCharacter
    | `Int 3 -> TriggerForIncompleteCompletions
    | _ -> assert false
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

  let to_yojson t =
    match t with
    | PlainText -> `Int 1
    | Snippet -> `Int 2

  let of_yojson json =
    match json with
    | `Int 1 -> PlainText
    | `Int 2 -> Snippet
    | _ -> assert false
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

module CompletionList = struct
  type t =
    { isIncomplete : bool
    ; items : unit
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionOptions = struct
  type t =
    { triggerCharacters : unit option [@yojson.option]
    ; allCommitCharacters : unit option [@yojson.option]
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
  type t = { context : CompletionContext.t option [@yojson.option] }
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
  type t = { items : unit } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module StaticRegistrationOptions = struct
  type t = { id : string option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeConfigurationParams = struct
  type t = { settings : Json.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentContentChangeEvent = struct end

module DidChangeTextDocumentParams = struct
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; contentChanges : unit
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
  type t = { changes : unit } [@@deriving_inline] [@@yojson.allow_extra_fields]

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
  type t = { watchers : unit } [@@deriving_inline] [@@yojson.allow_extra_fields]

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
    { added : unit
    ; removed : unit
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

module DocumentColorParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
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

  let to_yojson t =
    match t with
    | Text -> `Int 1
    | Read -> `Int 2
    | Write -> `Int 3

  let of_yojson json =
    match json with
    | `Int 1 -> Text
    | `Int 2 -> Read
    | `Int 3 -> Write
    | _ -> assert false
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
  type t = { resolveProvider : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentOnTypeFormattingOptions = struct
  type t =
    { firstTriggerCharacter : string
    ; moreTriggerCharacter : unit option [@yojson.option]
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

module DocumentRangeFormattingParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; options : FormattingOptions.t
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
    ; children : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSymbolParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
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

  let to_yojson t =
    match t with
    | ParseError -> `Int (-32700)
    | InvalidRequest -> `Int (-32600)
    | MethodNotFound -> `Int (-32601)
    | InvalidParams -> `Int (-32602)
    | InternalError -> `Int (-32603)
    | ServerErrorStart -> `Int (-32099)
    | ServerErrorEnd -> `Int (-32000)
    | ServerNotInitialized -> `Int (-32002)
    | UnknownErrorCode -> `Int (-32001)
    | RequestCancelled -> `Int (-32800)
    | ContentModified -> `Int (-32801)

  let of_yojson json =
    match json with
    | `Int -32700 -> ParseError
    | `Int -32600 -> InvalidRequest
    | `Int -32601 -> MethodNotFound
    | `Int -32602 -> InvalidParams
    | `Int -32603 -> InternalError
    | `Int -32099 -> ServerErrorStart
    | `Int -32000 -> ServerErrorEnd
    | `Int -32002 -> ServerNotInitialized
    | `Int -32001 -> UnknownErrorCode
    | `Int -32800 -> RequestCancelled
    | `Int -32801 -> ContentModified
    | _ -> assert false
end

module ExecuteCommandOptions = struct
  type t = { commands : unit } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ExecuteCommandParams = struct
  type t =
    { command : string
    ; arguments : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FileChangeType = struct
  type t =
    | Created
    | Changed
    | Deleted

  let to_yojson t =
    match t with
    | Created -> `Int 1
    | Changed -> `Int 2
    | Deleted -> `Int 3

  let of_yojson json =
    match json with
    | `Int 1 -> Created
    | `Int 2 -> Changed
    | `Int 3 -> Deleted
    | _ -> assert false
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

  let to_yojson t =
    match t with
    | Comment -> `String "comment"
    | Imports -> `String "imports"
    | Region -> `String "region"

  let of_yojson json =
    match json with
    | `String "comment" -> Comment
    | `String "imports" -> Imports
    | `String "region" -> Region
    | _ -> assert false
end

module FoldingRangeParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module MarkedString = struct end

module Hover = struct
  type t =
    { contents : unit
    ; range : Range.t option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module InitializeError = struct
  type t = UnknownProtocolVersion

  let to_yojson t =
    match t with
    | UnknownProtocolVersion -> `Int 1

  let of_yojson json =
    match json with
    | `Int 1 -> UnknownProtocolVersion
    | _ -> assert false
end

module InitializeParams = struct
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

module WorkspaceFoldersServerCapabilities = struct
  type t =
    { supported : bool option [@yojson.option]
    ; changeNotifications : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameOptions = struct
  type t = { prepareProvider : bool option [@yojson.option] }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelpOptions = struct
  type t =
    { triggerCharacters : unit option [@yojson.option]
    ; retriggerCharacters : unit option [@yojson.option]
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

module InitializeResult = struct
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

  let to_yojson t =
    match t with
    | Error -> `Int 1
    | Warning -> `Int 2
    | Info -> `Int 3
    | Log -> `Int 4

  let of_yojson json =
    match json with
    | `Int 1 -> Error
    | `Int 2 -> Warning
    | `Int 3 -> Info
    | `Int 4 -> Log
    | _ -> assert false
end

module NotificationMessage = struct
  type t =
    { method_ : string [@key "method"]
    ; params : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ParameterInformation = struct
  type t =
    { label : unit
    ; documentation : unit option [@yojson.option]
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
    ; diagnostics : unit
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
  type t = { registrations : unit }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameParams = struct
  type t = { newName : string }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RequestMessage = struct
  type t =
    { id : unit
    ; method_ : string [@key "method"]
    ; params : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ResponseError = struct
  type t =
    { code : int
    ; message : string
    ; data : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ResponseMessage = struct
  type t =
    { id : unit
    ; result : unit option [@yojson.option]
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
    { textDocument : TextDocumentIdentifier.t
    ; positions : unit
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
    ; actions : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureInformation = struct
  type t =
    { label : string
    ; documentation : unit option [@yojson.option]
    ; parameters : unit option [@yojson.option]
    }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelp = struct
  type t =
    { signatures : unit
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

  let to_yojson t =
    match t with
    | Invoked -> `Int 1
    | TriggerCharacter -> `Int 2
    | ContentChange -> `Int 3

  let of_yojson json =
    match json with
    | `Int 1 -> Invoked
    | `Int 2 -> TriggerCharacter
    | `Int 3 -> ContentChange
    | _ -> assert false
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
  type t = { context : SignatureHelpContext.t option [@yojson.option] }
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

  let to_yojson t =
    match t with
    | None -> `Int 0
    | Full -> `Int 1
    | Incremental -> `Int 2

  let of_yojson json =
    match json with
    | `Int 0 -> None
    | `Int 1 -> Full
    | `Int 2 -> Incremental
    | _ -> assert false
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

  let to_yojson t =
    match t with
    | Manual -> `Int 1
    | AfterDelay -> `Int 2
    | FocusOut -> `Int 3

  let of_yojson json =
    match json with
    | `Int 1 -> Manual
    | `Int 2 -> AfterDelay
    | `Int 3 -> FocusOut
    | _ -> assert false
end

module TextDocumentSaveRegistrationOptions = struct
  type t = { includeText : bool option [@yojson.option] }
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
  type t = { unregisterations : unit }
  [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WatchKind = struct
  type t =
    | Create
    | Change
    | Delete

  let to_yojson t =
    match t with
    | Create -> `Int 1
    | Change -> `Int 2
    | Delete -> `Int 4

  let of_yojson json =
    match json with
    | `Int 1 -> Create
    | `Int 2 -> Change
    | `Int 4 -> Delete
    | _ -> assert false
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

module WorkspaceSymbolParams = struct
  type t = { query : string } [@@deriving_inline] [@@yojson.allow_extra_fields]

  [@@@end]
end

open! Import

module MarkedString = struct
  type t =
    { value : string
    ; language : string option
    }

  let yojson_of_t { value; language } =
    match language with
    | None -> `String value
    | Some language ->
      `Assoc [ ("value", `String value); ("language", `String language) ]

  let t_of_yojson json =
    match json with
    | `String value -> { value; language = None }
    | `Assoc fields ->
      let value = Json.field_exn fields "value" Yojson_conv.string_of_yojson in
      let language =
        Json.field_exn fields "language" Yojson_conv.string_of_yojson
      in
      { value; language = Some language }
    | _ -> Json.error "invalid MarkedString" json
end

(*$ Lsp_gen.print_ml () *)

module DeleteFileOptions = struct
  type t =
    { recursive : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; ignoreIfNotExists : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentUri = struct
  type t = string [@@deriving_inline yojson]

  [@@@end]
end

module DeleteFile = struct
  type t =
    { uri : DocumentUri.t
    ; options : DeleteFileOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameFileOptions = struct
  type t =
    { overwrite : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; ignoreIfExists : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameFile = struct
  type t =
    { oldUri : DocumentUri.t
    ; newUri : DocumentUri.t
    ; options : RenameFileOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CreateFileOptions = struct
  type t =
    { overwrite : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; ignoreIfExists : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CreateFile = struct
  type t =
    { uri : DocumentUri.t
    ; options : CreateFileOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Position = struct
  type t =
    { line : int
    ; character : int
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Range = struct
  type t =
    { start : Position.t
    ; end_ : Position.t [@key "end"]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextEdit = struct
  type t =
    { range : Range.t
    ; newText : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentIdentifier = struct
  type t = { uri : DocumentUri.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module VersionedTextDocumentIdentifier = struct
  type t =
    { uri : DocumentUri.t
    ; version : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentEdit = struct
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; edits : TextEdit.t list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceEdit = struct
  type documentChanges =
    [ `TextDocumentEdit of TextDocumentEdit.t
    | `CreateFile of CreateFile.t
    | `RenameFile of RenameFile.t
    | `DeleteFile of DeleteFile.t
    ]

  let documentChanges_of_yojson (json : Json.t) : documentChanges =
    Json.Of.untagged_union "documentChanges"
      [ (fun json -> `TextDocumentEdit (TextDocumentEdit.t_of_yojson json))
      ; (fun json -> `CreateFile (CreateFile.t_of_yojson json))
      ; (fun json -> `RenameFile (RenameFile.t_of_yojson json))
      ; (fun json -> `DeleteFile (DeleteFile.t_of_yojson json))
      ]
      json

  let yojson_of_documentChanges (documentChanges : documentChanges) : Json.t =
    match documentChanges with
    | `TextDocumentEdit s -> TextDocumentEdit.yojson_of_t s
    | `CreateFile s -> CreateFile.yojson_of_t s
    | `RenameFile s -> RenameFile.yojson_of_t s
    | `DeleteFile s -> DeleteFile.yojson_of_t s

  type t =
    { changes :
        (DocumentUri.t, TextEdit.t list) Json.Assoc.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentChanges : documentChanges list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ApplyWorkspaceEditParams = struct
  type t =
    { label : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; edit : WorkspaceEdit.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ApplyWorkspaceEditResponse = struct
  type t =
    { applied : bool
    ; failureReason : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CancelParams = struct
  type t = { id : Jsonrpc.Id.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRangeClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DiagnosticTag = struct
  type t =
    | Unnecessary
    | Deprecated

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Unnecessary -> `Int 1
    | Deprecated -> `Int 2

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Unnecessary
    | `Int 2 -> Deprecated
    | _ -> Json.error "t" json
end

module PublishDiagnosticsClientCapabilities = struct
  type tagSupport = { valueSet : DiagnosticTag.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { relatedInformation : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; tagSupport : tagSupport Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; versionSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; prepareSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentOnTypeFormattingClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentRangeFormattingClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFormattingClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentColorClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; tooltipSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Empty -> `String ""
    | QuickFix -> `String "quickfix"
    | Refactor -> `String "refactor"
    | RefactorExtract -> `String "refactor.extract"
    | RefactorInline -> `String "refactor.inline"
    | RefactorRewrite -> `String "refactor.rewrite"
    | Source -> `String "source"
    | SourceOrganizeImports -> `String "source.organizeImports"

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "" -> Empty
    | `String "quickfix" -> QuickFix
    | `String "refactor" -> Refactor
    | `String "refactor.extract" -> RefactorExtract
    | `String "refactor.inline" -> RefactorInline
    | `String "refactor.rewrite" -> RefactorRewrite
    | `String "source" -> Source
    | `String "source.organizeImports" -> SourceOrganizeImports
    | _ -> Json.error "t" json
end

module CodeActionClientCapabilities = struct
  type codeActionKind = { valueSet : CodeActionKind.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]

  type codeActionLiteralSupport = { codeActionKind : codeActionKind }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeActionLiteralSupport : codeActionLiteralSupport Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; isPreferredSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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

  let yojson_of_t (t : t) : Json.t =
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

  let t_of_yojson (json : Json.t) : t =
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
    | _ -> Json.error "t" json
end

module DocumentSymbolClientCapabilities = struct
  type symbolKind =
    { valueSet : SymbolKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; symbolKind : symbolKind Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; hierarchicalDocumentSymbolSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlightClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ImplementationClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; linkSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TypeDefinitionClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; linkSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DefinitionClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; linkSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DeclarationClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; linkSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module MarkupKind = struct
  type t =
    | PlainText
    | Markdown

  let yojson_of_t (t : t) : Json.t =
    match t with
    | PlainText -> `String "plaintext"
    | Markdown -> `String "markdown"

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "plaintext" -> PlainText
    | `String "markdown" -> Markdown
    | _ -> Json.error "t" json
end

module SignatureHelpClientCapabilities = struct
  type parameterInformation =
    { labelOffsetSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]

  type signatureInformation =
    { documentationFormat : MarkupKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; parameterInformation : parameterInformation Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; signatureInformation : signatureInformation Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; contextSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module HoverClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; contentFormat : MarkupKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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

  let yojson_of_t (t : t) : Json.t =
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

  let t_of_yojson (json : Json.t) : t =
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
    | _ -> Json.error "t" json
end

module CompletionItemTag = struct
  type t = Deprecated

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Deprecated -> `Int 1

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Deprecated
    | _ -> Json.error "t" json
end

module CompletionClientCapabilities = struct
  type completionItemKind =
    { valueSet : CompletionItemKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]

  type tagSupport = { valueSet : CompletionItemTag.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ExecuteCommandClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolClientCapabilities = struct
  type symbolKind =
    { valueSet : SymbolKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; symbolKind : symbolKind Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeWatchedFilesClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeConfigurationClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FailureHandlingKind = struct
  type t =
    | Abort
    | Transactional
    | TextOnlyTransactional
    | Undo

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Abort -> `String "abort"
    | Transactional -> `String "transactional"
    | TextOnlyTransactional -> `String "textOnlyTransactional"
    | Undo -> `String "undo"

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "abort" -> Abort
    | `String "transactional" -> Transactional
    | `String "textOnlyTransactional" -> TextOnlyTransactional
    | `String "undo" -> Undo
    | _ -> Json.error "t" json
end

module ResourceOperationKind = struct
  type t =
    | Create
    | Rename
    | Delete

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Create -> `String "create"
    | Rename -> `String "rename"
    | Delete -> `String "delete"

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "create" -> Create
    | `String "rename" -> Rename
    | `String "delete" -> Delete
    | _ -> Json.error "t" json
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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ClientCapabilities = struct
  type window =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Command = struct
  type t =
    { title : string
    ; command : string
    ; arguments : Json.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Location = struct
  type t =
    { uri : DocumentUri.t
    ; range : Range.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DiagnosticRelatedInformation = struct
  type t =
    { location : Location.t
    ; message : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DiagnosticSeverity = struct
  type t =
    | Error
    | Warning
    | Information
    | Hint

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Error -> `Int 1
    | Warning -> `Int 2
    | Information -> `Int 3
    | Hint -> `Int 4

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Error
    | `Int 2 -> Warning
    | `Int 3 -> Information
    | `Int 4 -> Hint
    | _ -> Json.error "t" json
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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionContext = struct
  type t =
    { diagnostics : Diagnostic.t list
    ; only : CodeActionKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeActionKinds : CodeActionKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressParams = struct
  type t =
    { workDoneToken : ProgressToken.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeActionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; context : CodeActionContext.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLens = struct
  type t =
    { range : Range.t
    ; command : Command.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; data : Json.t option [@yojson.option]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CodeLensParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Color = struct
  type t =
    { red : int
    ; green : int
    ; blue : int
    ; alpha : int
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ColorInformation = struct
  type t =
    { range : Range.t
    ; color : Color.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ColorPresentationParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; color : Color.t
    ; range : Range.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionTriggerKind = struct
  type t =
    | Invoked
    | TriggerCharacter
    | TriggerForIncompleteCompletions

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Invoked -> `Int 1
    | TriggerCharacter -> `Int 2
    | TriggerForIncompleteCompletions -> `Int 3

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Invoked
    | `Int 2 -> TriggerCharacter
    | `Int 3 -> TriggerForIncompleteCompletions
    | _ -> Json.error "t" json
end

module CompletionContext = struct
  type t =
    { triggerKind : CompletionTriggerKind.t
    ; triggerCharacter : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module InsertTextFormat = struct
  type t =
    | PlainText
    | Snippet

  let yojson_of_t (t : t) : Json.t =
    match t with
    | PlainText -> `Int 1
    | Snippet -> `Int 2

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> PlainText
    | `Int 2 -> Snippet
    | _ -> Json.error "t" json
end

module MarkupContent = struct
  type t =
    { kind : MarkupKind.t
    ; value : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionItem = struct
  type documentation =
    [ `String of string
    | `MarkupContent of MarkupContent.t
    ]

  let documentation_of_yojson (json : Json.t) : documentation =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union "documentation"
        [ (fun json -> `MarkupContent (MarkupContent.t_of_yojson json)) ]
        json

  let yojson_of_documentation (documentation : documentation) : Json.t =
    match documentation with
    | `String j -> `String j
    | `MarkupContent s -> MarkupContent.yojson_of_t s

  type t =
    { label : string
    ; kind : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; tags : CompletionItemTag.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; detail : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentation : documentation Json.Nullable_option.t
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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionList = struct
  type t =
    { isIncomplete : bool
    ; items : CompletionItem.t list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentPositionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module CompletionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; context : CompletionContext.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ConfigurationItem = struct
  type t =
    { scopeUri : DocumentUri.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; section : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ConfigurationParams = struct
  type t = { items : ConfigurationItem.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DeclarationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DeclarationParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module StaticRegistrationOptions = struct
  type t =
    { id : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DefinitionOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DefinitionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DefinitionRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeConfigurationParams = struct
  type t = { settings : Json.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentContentChangeEvent = struct
  type t =
    { range : Range.t
    ; rangeLength : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; text : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeTextDocumentParams = struct
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; contentChanges : TextDocumentContentChangeEvent.t list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FileEvent = struct
  type t =
    { uri : DocumentUri.t
    ; type_ : int [@key "type"]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeWatchedFilesParams = struct
  type t = { changes : FileEvent.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FileSystemWatcher = struct
  type t =
    { globPattern : string
    ; kind : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeWatchedFilesRegistrationOptions = struct
  type t = { watchers : FileSystemWatcher.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceFolder = struct
  type t =
    { uri : DocumentUri.t
    ; name : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceFoldersChangeEvent = struct
  type t =
    { added : WorkspaceFolder.t list
    ; removed : WorkspaceFolder.t list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidChangeWorkspaceFoldersParams = struct
  type t = { event : WorkspaceFoldersChangeEvent.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidCloseTextDocumentParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentItem = struct
  type t =
    { uri : DocumentUri.t
    ; languageId : string
    ; version : int
    ; text : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidOpenTextDocumentParams = struct
  type t = { textDocument : TextDocumentItem.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DidSaveTextDocumentParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; text : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentColorOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentColorParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFormattingOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFormattingParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; options : FormattingOptions.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentFormattingRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlight = struct
  type t =
    { range : Range.t
    ; kind : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlightKind = struct
  type t =
    | Text
    | Read
    | Write

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Text -> `Int 1
    | Read -> `Int 2
    | Write -> `Int 3

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Text
    | `Int 2 -> Read
    | `Int 3 -> Write
    | _ -> Json.error "t" json
end

module DocumentHighlightOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlightParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentHighlightRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentLinkParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentOnTypeFormattingOptions = struct
  type t =
    { firstTriggerCharacter : string
    ; moreTriggerCharacter : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentOnTypeFormattingParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; ch : string
    ; options : FormattingOptions.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentRangeFormattingOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentRangeFormattingParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; options : FormattingOptions.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentRangeFormattingRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSymbolOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSymbolParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module DocumentSymbolRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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

  let yojson_of_t (t : t) : Json.t =
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

  let t_of_yojson (json : Json.t) : t =
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
    | _ -> Json.error "t" json
end

module ExecuteCommandOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; commands : string list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ExecuteCommandParams = struct
  type t =
    { command : string
    ; arguments : Json.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ExecuteCommandRegistrationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; commands : string list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FileChangeType = struct
  type t =
    | Created
    | Changed
    | Deleted

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Created -> `Int 1
    | Changed -> `Int 2
    | Deleted -> `Int 3

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Created
    | `Int 2 -> Changed
    | `Int 3 -> Deleted
    | _ -> Json.error "t" json
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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FoldingRangeKind = struct
  type t =
    | Comment
    | Imports
    | Region

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Comment -> `String "comment"
    | Imports -> `String "imports"
    | Region -> `String "region"

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "comment" -> Comment
    | `String "imports" -> Imports
    | `String "region" -> Region
    | _ -> Json.error "t" json
end

module FoldingRangeOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module FoldingRangeParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Hover = struct
  type contents =
    [ `MarkedString of MarkedString.t
    | `List of MarkedString.t list
    | `MarkupContent of MarkupContent.t
    ]

  let contents_of_yojson (json : Json.t) : contents =
    Json.Of.untagged_union "contents"
      [ (fun json -> `MarkedString (MarkedString.t_of_yojson json))
      ; (fun json -> `List (Json.Of.list MarkedString.t_of_yojson json))
      ; (fun json -> `MarkupContent (MarkupContent.t_of_yojson json))
      ]
      json

  let yojson_of_contents (contents : contents) : Json.t =
    match contents with
    | `MarkedString s -> MarkedString.yojson_of_t s
    | `List s -> Json.To.list MarkedString.yojson_of_t s
    | `MarkupContent s -> MarkupContent.yojson_of_t s

  type t =
    { contents : contents
    ; range : Range.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module HoverOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module HoverParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module HoverRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ImplementationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ImplementationParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module InitializeError = struct
  type t = UnknownProtocolVersion

  let yojson_of_t (t : t) : Json.t =
    match t with
    | UnknownProtocolVersion -> `Int 1

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> UnknownProtocolVersion
    | _ -> Json.error "t" json
end

module InitializeParams = struct
  type clientInfo =
    { name : string
    ; version : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]

  type trace =
    [ `Off
    | `Messages
    | `Verbose
    ]

  let yojson_of_trace (trace : trace) : Json.t =
    match trace with
    | `Off -> `String "off"
    | `Messages -> `String "messages"
    | `Verbose -> `String "verbose"

  let trace_of_yojson (json : Json.t) : trace =
    match json with
    | `String "off" -> `Off
    | `String "messages" -> `Messages
    | `String "verbose" -> `Verbose
    | _ -> Json.error "trace" json

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
    ; trace : trace Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workspaceFolders :
        WorkspaceFolder.t list Json.Nullable_option.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceFoldersServerCapabilities = struct
  type changeNotifications =
    [ `String of string
    | `Bool of bool
    ]

  let changeNotifications_of_yojson (json : Json.t) : changeNotifications =
    match json with
    | `String j -> `String j
    | `Bool j -> `Bool j
    | _ -> Json.error "changeNotifications" json

  let yojson_of_changeNotifications (changeNotifications : changeNotifications)
      : Json.t =
    match changeNotifications with
    | `String j -> `String j
    | `Bool j -> `Bool j

  type t =
    { supported : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; changeNotifications : changeNotifications Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRangeOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; prepareProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TypeDefinitionOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SaveOptions = struct
  type t =
    { includeText : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ServerCapabilities = struct
  type workspace =
    { workspaceFolders :
        WorkspaceFoldersServerCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]

  type textDocumentSync =
    [ `TextDocumentSyncOptions of TextDocumentSyncOptions.t
    | `Int of int
    ]

  let textDocumentSync_of_yojson (json : Json.t) : textDocumentSync =
    match json with
    | `Int j -> `Int j
    | _ ->
      Json.Of.untagged_union "textDocumentSync"
        [ (fun json ->
            `TextDocumentSyncOptions (TextDocumentSyncOptions.t_of_yojson json))
        ]
        json

  let yojson_of_textDocumentSync (textDocumentSync : textDocumentSync) : Json.t
      =
    match textDocumentSync with
    | `Int j -> `Int j
    | `TextDocumentSyncOptions s -> TextDocumentSyncOptions.yojson_of_t s

  type hoverProvider =
    [ `Bool of bool
    | `HoverOptions of HoverOptions.t
    ]

  let hoverProvider_of_yojson (json : Json.t) : hoverProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "hoverProvider"
        [ (fun json -> `HoverOptions (HoverOptions.t_of_yojson json)) ]
        json

  let yojson_of_hoverProvider (hoverProvider : hoverProvider) : Json.t =
    match hoverProvider with
    | `Bool j -> `Bool j
    | `HoverOptions s -> HoverOptions.yojson_of_t s

  type declarationProvider =
    [ `Bool of bool
    | `DeclarationOptions of DeclarationOptions.t
    | `DeclarationRegistrationOptions of DeclarationRegistrationOptions.t
    ]

  let declarationProvider_of_yojson (json : Json.t) : declarationProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "declarationProvider"
        [ (fun json ->
            `DeclarationOptions (DeclarationOptions.t_of_yojson json))
        ; (fun json ->
            `DeclarationRegistrationOptions
              (DeclarationRegistrationOptions.t_of_yojson json))
        ]
        json

  let yojson_of_declarationProvider (declarationProvider : declarationProvider)
      : Json.t =
    match declarationProvider with
    | `Bool j -> `Bool j
    | `DeclarationOptions s -> DeclarationOptions.yojson_of_t s
    | `DeclarationRegistrationOptions s ->
      DeclarationRegistrationOptions.yojson_of_t s

  type definitionProvider =
    [ `Bool of bool
    | `DefinitionOptions of DefinitionOptions.t
    ]

  let definitionProvider_of_yojson (json : Json.t) : definitionProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "definitionProvider"
        [ (fun json -> `DefinitionOptions (DefinitionOptions.t_of_yojson json))
        ]
        json

  let yojson_of_definitionProvider (definitionProvider : definitionProvider) :
      Json.t =
    match definitionProvider with
    | `Bool j -> `Bool j
    | `DefinitionOptions s -> DefinitionOptions.yojson_of_t s

  type typeDefinitionProvider =
    [ `Bool of bool
    | `TypeDefinitionOptions of TypeDefinitionOptions.t
    | `TypeDefinitionRegistrationOptions of TypeDefinitionRegistrationOptions.t
    ]

  let typeDefinitionProvider_of_yojson (json : Json.t) : typeDefinitionProvider
      =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "typeDefinitionProvider"
        [ (fun json ->
            `TypeDefinitionOptions (TypeDefinitionOptions.t_of_yojson json))
        ; (fun json ->
            `TypeDefinitionRegistrationOptions
              (TypeDefinitionRegistrationOptions.t_of_yojson json))
        ]
        json

  let yojson_of_typeDefinitionProvider
      (typeDefinitionProvider : typeDefinitionProvider) : Json.t =
    match typeDefinitionProvider with
    | `Bool j -> `Bool j
    | `TypeDefinitionOptions s -> TypeDefinitionOptions.yojson_of_t s
    | `TypeDefinitionRegistrationOptions s ->
      TypeDefinitionRegistrationOptions.yojson_of_t s

  type implementationProvider =
    [ `Bool of bool
    | `ImplementationOptions of ImplementationOptions.t
    | `ImplementationRegistrationOptions of ImplementationRegistrationOptions.t
    ]

  let implementationProvider_of_yojson (json : Json.t) : implementationProvider
      =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "implementationProvider"
        [ (fun json ->
            `ImplementationOptions (ImplementationOptions.t_of_yojson json))
        ; (fun json ->
            `ImplementationRegistrationOptions
              (ImplementationRegistrationOptions.t_of_yojson json))
        ]
        json

  let yojson_of_implementationProvider
      (implementationProvider : implementationProvider) : Json.t =
    match implementationProvider with
    | `Bool j -> `Bool j
    | `ImplementationOptions s -> ImplementationOptions.yojson_of_t s
    | `ImplementationRegistrationOptions s ->
      ImplementationRegistrationOptions.yojson_of_t s

  type referencesProvider =
    [ `Bool of bool
    | `ReferenceOptions of ReferenceOptions.t
    ]

  let referencesProvider_of_yojson (json : Json.t) : referencesProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "referencesProvider"
        [ (fun json -> `ReferenceOptions (ReferenceOptions.t_of_yojson json)) ]
        json

  let yojson_of_referencesProvider (referencesProvider : referencesProvider) :
      Json.t =
    match referencesProvider with
    | `Bool j -> `Bool j
    | `ReferenceOptions s -> ReferenceOptions.yojson_of_t s

  type documentHighlightProvider =
    [ `Bool of bool
    | `DocumentHighlightOptions of DocumentHighlightOptions.t
    ]

  let documentHighlightProvider_of_yojson (json : Json.t) :
      documentHighlightProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "documentHighlightProvider"
        [ (fun json ->
            `DocumentHighlightOptions
              (DocumentHighlightOptions.t_of_yojson json))
        ]
        json

  let yojson_of_documentHighlightProvider
      (documentHighlightProvider : documentHighlightProvider) : Json.t =
    match documentHighlightProvider with
    | `Bool j -> `Bool j
    | `DocumentHighlightOptions s -> DocumentHighlightOptions.yojson_of_t s

  type documentSymbolProvider =
    [ `Bool of bool
    | `DocumentSymbolOptions of DocumentSymbolOptions.t
    ]

  let documentSymbolProvider_of_yojson (json : Json.t) : documentSymbolProvider
      =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "documentSymbolProvider"
        [ (fun json ->
            `DocumentSymbolOptions (DocumentSymbolOptions.t_of_yojson json))
        ]
        json

  let yojson_of_documentSymbolProvider
      (documentSymbolProvider : documentSymbolProvider) : Json.t =
    match documentSymbolProvider with
    | `Bool j -> `Bool j
    | `DocumentSymbolOptions s -> DocumentSymbolOptions.yojson_of_t s

  type codeActionProvider =
    [ `Bool of bool
    | `CodeActionOptions of CodeActionOptions.t
    ]

  let codeActionProvider_of_yojson (json : Json.t) : codeActionProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "codeActionProvider"
        [ (fun json -> `CodeActionOptions (CodeActionOptions.t_of_yojson json))
        ]
        json

  let yojson_of_codeActionProvider (codeActionProvider : codeActionProvider) :
      Json.t =
    match codeActionProvider with
    | `Bool j -> `Bool j
    | `CodeActionOptions s -> CodeActionOptions.yojson_of_t s

  type colorProvider =
    [ `Bool of bool
    | `DocumentColorOptions of DocumentColorOptions.t
    | `DocumentColorRegistrationOptions of DocumentColorRegistrationOptions.t
    ]

  let colorProvider_of_yojson (json : Json.t) : colorProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "colorProvider"
        [ (fun json ->
            `DocumentColorOptions (DocumentColorOptions.t_of_yojson json))
        ; (fun json ->
            `DocumentColorRegistrationOptions
              (DocumentColorRegistrationOptions.t_of_yojson json))
        ]
        json

  let yojson_of_colorProvider (colorProvider : colorProvider) : Json.t =
    match colorProvider with
    | `Bool j -> `Bool j
    | `DocumentColorOptions s -> DocumentColorOptions.yojson_of_t s
    | `DocumentColorRegistrationOptions s ->
      DocumentColorRegistrationOptions.yojson_of_t s

  type documentFormattingProvider =
    [ `Bool of bool
    | `DocumentFormattingOptions of DocumentFormattingOptions.t
    ]

  let documentFormattingProvider_of_yojson (json : Json.t) :
      documentFormattingProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "documentFormattingProvider"
        [ (fun json ->
            `DocumentFormattingOptions
              (DocumentFormattingOptions.t_of_yojson json))
        ]
        json

  let yojson_of_documentFormattingProvider
      (documentFormattingProvider : documentFormattingProvider) : Json.t =
    match documentFormattingProvider with
    | `Bool j -> `Bool j
    | `DocumentFormattingOptions s -> DocumentFormattingOptions.yojson_of_t s

  type documentRangeFormattingProvider =
    [ `Bool of bool
    | `DocumentRangeFormattingOptions of DocumentRangeFormattingOptions.t
    ]

  let documentRangeFormattingProvider_of_yojson (json : Json.t) :
      documentRangeFormattingProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "documentRangeFormattingProvider"
        [ (fun json ->
            `DocumentRangeFormattingOptions
              (DocumentRangeFormattingOptions.t_of_yojson json))
        ]
        json

  let yojson_of_documentRangeFormattingProvider
      (documentRangeFormattingProvider : documentRangeFormattingProvider) :
      Json.t =
    match documentRangeFormattingProvider with
    | `Bool j -> `Bool j
    | `DocumentRangeFormattingOptions s ->
      DocumentRangeFormattingOptions.yojson_of_t s

  type renameProvider =
    [ `Bool of bool
    | `RenameOptions of RenameOptions.t
    ]

  let renameProvider_of_yojson (json : Json.t) : renameProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "renameProvider"
        [ (fun json -> `RenameOptions (RenameOptions.t_of_yojson json)) ]
        json

  let yojson_of_renameProvider (renameProvider : renameProvider) : Json.t =
    match renameProvider with
    | `Bool j -> `Bool j
    | `RenameOptions s -> RenameOptions.yojson_of_t s

  type foldingRangeProvider =
    [ `Bool of bool
    | `FoldingRangeOptions of FoldingRangeOptions.t
    | `FoldingRangeRegistrationOptions of FoldingRangeRegistrationOptions.t
    ]

  let foldingRangeProvider_of_yojson (json : Json.t) : foldingRangeProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "foldingRangeProvider"
        [ (fun json ->
            `FoldingRangeOptions (FoldingRangeOptions.t_of_yojson json))
        ; (fun json ->
            `FoldingRangeRegistrationOptions
              (FoldingRangeRegistrationOptions.t_of_yojson json))
        ]
        json

  let yojson_of_foldingRangeProvider
      (foldingRangeProvider : foldingRangeProvider) : Json.t =
    match foldingRangeProvider with
    | `Bool j -> `Bool j
    | `FoldingRangeOptions s -> FoldingRangeOptions.yojson_of_t s
    | `FoldingRangeRegistrationOptions s ->
      FoldingRangeRegistrationOptions.yojson_of_t s

  type selectionRangeProvider =
    [ `Bool of bool
    | `SelectionRangeOptions of SelectionRangeOptions.t
    | `SelectionRangeRegistrationOptions of SelectionRangeRegistrationOptions.t
    ]

  let selectionRangeProvider_of_yojson (json : Json.t) : selectionRangeProvider
      =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "selectionRangeProvider"
        [ (fun json ->
            `SelectionRangeOptions (SelectionRangeOptions.t_of_yojson json))
        ; (fun json ->
            `SelectionRangeRegistrationOptions
              (SelectionRangeRegistrationOptions.t_of_yojson json))
        ]
        json

  let yojson_of_selectionRangeProvider
      (selectionRangeProvider : selectionRangeProvider) : Json.t =
    match selectionRangeProvider with
    | `Bool j -> `Bool j
    | `SelectionRangeOptions s -> SelectionRangeOptions.yojson_of_t s
    | `SelectionRangeRegistrationOptions s ->
      SelectionRangeRegistrationOptions.yojson_of_t s

  type t =
    { textDocumentSync : textDocumentSync Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; completionProvider : CompletionOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; hoverProvider : hoverProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; signatureHelpProvider : SignatureHelpOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; declarationProvider : declarationProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; definitionProvider : definitionProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; typeDefinitionProvider : typeDefinitionProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; implementationProvider : implementationProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; referencesProvider : referencesProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentHighlightProvider :
        documentHighlightProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentSymbolProvider : documentSymbolProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeActionProvider : codeActionProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeLensProvider : CodeLensOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentLinkProvider : DocumentLinkOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; colorProvider : colorProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentFormattingProvider :
        documentFormattingProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentRangeFormattingProvider :
        documentRangeFormattingProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentOnTypeFormattingProvider :
        DocumentOnTypeFormattingOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; renameProvider : renameProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; foldingRangeProvider : foldingRangeProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; executeCommandProvider : ExecuteCommandOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; selectionRangeProvider : selectionRangeProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workspaceSymbolProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workspace : workspace Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; experimental : Json.t option [@yojson.option]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module InitializeResult = struct
  type serverInfo =
    { name : string
    ; version : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]

  type t =
    { capabilities : ServerCapabilities.t
    ; serverInfo : serverInfo Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module LogMessageParams = struct
  type t =
    { type_ : int [@key "type"]
    ; message : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module MessageActionItem = struct
  type t = { title : string }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module MessageType = struct
  type t =
    | Error
    | Warning
    | Info
    | Log

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Error -> `Int 1
    | Warning -> `Int 2
    | Info -> `Int 3
    | Log -> `Int 4

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Error
    | `Int 2 -> Warning
    | `Int 3 -> Info
    | `Int 4 -> Log
    | _ -> Json.error "t" json
end

module ParameterInformation = struct
  type label =
    [ `String of string
    | `Offset of int * int
    ]

  let label_of_yojson (json : Json.t) : label =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union "label"
        [ (fun json -> `Offset (Json.Of.int_pair json)) ]
        json

  let yojson_of_label (label : label) : Json.t =
    match label with
    | `String j -> `String j
    | `Offset s -> Json.To.int_pair s

  type documentation =
    [ `String of string
    | `MarkupContent of MarkupContent.t
    ]

  let documentation_of_yojson (json : Json.t) : documentation =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union "documentation"
        [ (fun json -> `MarkupContent (MarkupContent.t_of_yojson json)) ]
        json

  let yojson_of_documentation (documentation : documentation) : Json.t =
    match documentation with
    | `String j -> `String j
    | `MarkupContent s -> MarkupContent.yojson_of_t s

  type t =
    { label : label
    ; documentation : documentation Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module PrepareRenameParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ProgressParams = struct
  type t =
    { token : ProgressToken.t
    ; value : Json.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module PublishDiagnosticsParams = struct
  type t =
    { uri : DocumentUri.t
    ; version : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; diagnostics : Diagnostic.t list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceContext = struct
  type t = { includeDeclaration : bool }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; context : ReferenceContext.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ReferenceRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Registration = struct
  type t =
    { id : string
    ; method_ : string [@key "method"]
    ; registerOptions : Json.t option [@yojson.option]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RegistrationParams = struct
  type t = { registrations : Registration.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module RenameParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; newName : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRange = struct
  type t =
    { range : Range.t
    ; parent : t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SelectionRangeParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; positions : Position.t list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ShowMessageParams = struct
  type t =
    { type_ : int [@key "type"]
    ; message : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module ShowMessageRequestParams = struct
  type t =
    { type_ : int [@key "type"]
    ; message : string
    ; actions : MessageActionItem.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureInformation = struct
  type documentation =
    [ `String of string
    | `MarkupContent of MarkupContent.t
    ]

  let documentation_of_yojson (json : Json.t) : documentation =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union "documentation"
        [ (fun json -> `MarkupContent (MarkupContent.t_of_yojson json)) ]
        json

  let yojson_of_documentation (documentation : documentation) : Json.t =
    match documentation with
    | `String j -> `String j
    | `MarkupContent s -> MarkupContent.yojson_of_t s

  type t =
    { label : string
    ; documentation : documentation Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; parameters : ParameterInformation.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelpTriggerKind = struct
  type t =
    | Invoked
    | TriggerCharacter
    | ContentChange

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Invoked -> `Int 1
    | TriggerCharacter -> `Int 2
    | ContentChange -> `Int 3

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Invoked
    | `Int 2 -> TriggerCharacter
    | `Int 3 -> ContentChange
    | _ -> Json.error "t" json
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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module SignatureHelpParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; context : SignatureHelpContext.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentSyncKind = struct
  type t =
    | None
    | Full
    | Incremental

  let yojson_of_t (t : t) : Json.t =
    match t with
    | None -> `Int 0
    | Full -> `Int 1
    | Incremental -> `Int 2

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 0 -> None
    | `Int 1 -> Full
    | `Int 2 -> Incremental
    | _ -> Json.error "t" json
end

module TextDocumentChangeRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; syncKind : TextDocumentSyncKind.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TextDocumentSaveReason = struct
  type t =
    | Manual
    | AfterDelay
    | FocusOut

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Manual -> `Int 1
    | AfterDelay -> `Int 2
    | FocusOut -> `Int 3

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Manual
    | `Int 2 -> AfterDelay
    | `Int 3 -> FocusOut
    | _ -> Json.error "t" json
end

module TextDocumentSaveRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; includeText : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module TypeDefinitionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module Unregistration = struct
  type t =
    { id : string
    ; method_ : string [@key "method"]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module UnregistrationParams = struct
  type t = { unregisterations : Unregistration.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WatchKind = struct
  type t =
    | Create
    | Change
    | Delete

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Create -> `Int 1
    | Change -> `Int 2
    | Delete -> `Int 4

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Create
    | `Int 2 -> Change
    | `Int 4 -> Delete
    | _ -> Json.error "t" json
end

module WillSaveTextDocumentParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; reason : int
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressBegin = struct
  type t =
    { title : string
    ; cancellable : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; message : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; percentage : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressCancelParams = struct
  type t = { token : ProgressToken.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressCreateParams = struct
  type t = { token : ProgressToken.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressEnd = struct
  type t =
    { message : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkDoneProgressReport = struct
  type t =
    { cancellable : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; message : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; percentage : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolParams = struct
  type t = { query : string }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

module WorkspaceSymbolRegistrationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]
end

(*$*)

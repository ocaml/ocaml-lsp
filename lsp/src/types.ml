open! Import
open Json.Conv

module NotebookDocumentFilter = struct
  type t =
    { notebookType : string option [@yojson.option]
    ; scheme : string option [@yojson.option]
    ; pattern : string option [@yojson.option]
    }
  [@@deriving yojson]
end

module NotebookSelector = struct
  type notebook_pvar =
    [ `Notebook of string
    | `NotebookDocumentFilter of NotebookDocumentFilter.t
    ]

  let notebook_pvar_of_yojson (json : Json.t) : notebook_pvar =
    Json.Of.untagged_union
      "notebook_pvar"
      [ (fun json -> `Notebook (string_of_yojson json))
      ; (fun json -> `NotebookDocumentFilter (NotebookDocumentFilter.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_notebook_pvar (notebook_pvar : notebook_pvar) : Json.t =
    match notebook_pvar with
    | `Notebook j -> `String j
    | `NotebookDocumentFilter j -> NotebookDocumentFilter.yojson_of_t j
  ;;

  type cell = { language : string } [@@deriving yojson]

  type t =
    { notebook : notebook_pvar option [@yojson.option]
    ; cells : cell list option [@yojson.option]
    }
  [@@deriving yojson]
end

module NotebookDocumentSyncOptions = struct
  type notebookSelector_pvar =
    [ `NotebookSelector of NotebookSelector.t
    | `List of NotebookSelector.t list
    ]

  let notebookSelector_pvar_of_yojson (json : Json.t) : notebookSelector_pvar =
    Json.Of.untagged_union
      "notebookSelector"
      [ (fun json -> `NotebookSelector (NotebookSelector.t_of_yojson json))
      ; (fun json -> `List (Json.Of.list NotebookSelector.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_notebookSelector_pvar (notebookSelector_pvar : notebookSelector_pvar)
    : Json.t
    =
    match notebookSelector_pvar with
    | `NotebookSelector s -> NotebookSelector.yojson_of_t s
    | `List s -> Json.To.list NotebookSelector.yojson_of_t s
  ;;

  type t =
    { notebookSelector : notebookSelector_pvar
    ; save : bool option
    }
  [@@deriving yojson]
end

module NotebookDocumentSyncRegistrationOptions = struct
  type t = unit [@@deriving yojson]
end

module MarkedString = struct
  type t =
    { value : string
    ; language : string option
    }

  let yojson_of_t { value; language } =
    match language with
    | None -> `String value
    | Some language -> `Assoc [ "value", `String value; "language", `String language ]
  ;;

  let t_of_yojson json =
    match json with
    | `String value -> { value; language = None }
    | `Assoc fields ->
      let value = Json.field_exn fields "value" Json.Conv.string_of_yojson in
      let language = Json.field_exn fields "language" Json.Conv.string_of_yojson in
      { value; language = Some language }
    | _ -> Json.error "invalid MarkedString" json
  ;;
end

module DocumentUri = Uri0

module ProgressToken = struct
  type t =
    [ `Int of int
    | `String of string
    ]

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String j -> `String j
    | `Int i -> `Int i
    | _ -> Json.error "invalid ProgressToken" json
  ;;

  let yojson_of_t (t : t) : Json.t = (t :> Json.t)
end

module ProgressParams = struct
  type 'a t =
    { token : ProgressToken.t
    ; value : 'a
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(token : ProgressToken.t) ~value = { token; value }
end

module TextDocumentFilter = struct
  type t =
    { language : string option
    ; scheme : string option
    ; pattern : string option
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?language ?scheme ?pattern () = { language; scheme; pattern }
end

(*$ Lsp_gen.print_ml () *)
module SymbolTag = struct
  type t = Deprecated

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Deprecated -> `Int 1
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Deprecated
    | _ -> Json.error "Invalid value. Expected one of:\n1" json
  ;;
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
  ;;

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
    | _ ->
      Json.error
        "Invalid value.\n\
         Expected one of: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,\n\
         18, 19, 20, 21, 22, 23, 24, 25, 26"
        json
  ;;
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
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "create" -> Create
    | `String "rename" -> Rename
    | `String "delete" -> Delete
    | _ ->
      Json.error
        "Invalid value. Expected one of:\n\"create\", \"rename\", \"delete\""
        json
  ;;
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
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "abort" -> Abort
    | `String "transactional" -> Transactional
    | `String "textOnlyTransactional" -> TextOnlyTransactional
    | `String "undo" -> Undo
    | _ ->
      Json.error
        "Invalid value. Expected one of: \"abort\", \"transactional\",\n\
         \"textOnlyTransactional\", \"undo\""
        json
  ;;
end

module MarkupKind = struct
  type t =
    | PlainText
    | Markdown

  let yojson_of_t (t : t) : Json.t =
    match t with
    | PlainText -> `String "plaintext"
    | Markdown -> `String "markdown"
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "plaintext" -> PlainText
    | `String "markdown" -> Markdown
    | _ -> Json.error "Invalid value. Expected one of: \"plaintext\", \"markdown\"" json
  ;;
end

module TokenFormat = struct
  type t = Relative

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Relative -> `String "relative"
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "relative" -> Relative
    | _ -> Json.error "Invalid value.\nExpected one of: \"relative\"" json
  ;;
end

module PrepareSupportDefaultBehavior = struct
  type t = Identifier

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Identifier -> `Int 1
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Identifier
    | _ -> Json.error "Invalid value. Expected one of:\n1" json
  ;;
end

module DiagnosticTag = struct
  type t =
    | Unnecessary
    | Deprecated

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Unnecessary -> `Int 1
    | Deprecated -> `Int 2
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Unnecessary
    | `Int 2 -> Deprecated
    | _ -> Json.error "Invalid value. Expected one of: 1, 2" json
  ;;
end

module FoldingRangeKind = struct
  type t =
    | Comment
    | Imports
    | Region
    | Other of string

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Comment -> `String "comment"
    | Imports -> `String "imports"
    | Region -> `String "region"
    | Other s -> `String s
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "comment" -> Comment
    | `String "imports" -> Imports
    | `String "region" -> Region
    | `String s -> Other s
    | _ ->
      Json.error
        "Invalid\nvalue. Expected one of: \"comment\", \"imports\", \"region\""
        json
  ;;
end

module InsertTextMode = struct
  type t =
    | AsIs
    | AdjustIndentation

  let yojson_of_t (t : t) : Json.t =
    match t with
    | AsIs -> `Int 1
    | AdjustIndentation -> `Int 2
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> AsIs
    | `Int 2 -> AdjustIndentation
    | _ -> Json.error "Invalid value. Expected one of: 1, 2" json
  ;;
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
  ;;

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
    | _ ->
      Json.error
        "Invalid value. Expected one of: 1, 2, 3,\n\
         4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,\n\
         25"
        json
  ;;
end

module CompletionItemTag = struct
  type t = Deprecated

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Deprecated -> `Int 1
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Deprecated
    | _ -> Json.error "Invalid value. Expected one of:\n1" json
  ;;
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
    | SourceFixAll
    | Other of string

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
    | SourceFixAll -> `String "source.fixAll"
    | Other s -> `String s
  ;;

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
    | `String "source.fixAll" -> SourceFixAll
    | `String s -> Other s
    | _ ->
      Json.error
        "Invalid value. Expected one of: \"\",\n\
         \"quickfix\", \"refactor\", \"refactor.extract\", \"refactor.inline\",\n\
         \"refactor.rewrite\", \"source\", \"source.organizeImports\",\n\
         \"source.fixAll\""
        json
  ;;
end

module PositionEncodingKind = struct
  type t =
    | UTF8
    | UTF16
    | UTF32
    | Other of string

  let yojson_of_t (t : t) : Json.t =
    match t with
    | UTF8 -> `String "utf-8"
    | UTF16 -> `String "utf-16"
    | UTF32 -> `String "utf-32"
    | Other s -> `String s
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "utf-8" -> UTF8
    | `String "utf-16" -> UTF16
    | `String "utf-32" -> UTF32
    | `String s -> Other s
    | _ ->
      Json.error "Invalid\nvalue. Expected one of: \"utf-8\", \"utf-16\", \"utf-32\"" json
  ;;
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
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Error
    | `Int 2 -> Warning
    | `Int 3 -> Information
    | `Int 4 -> Hint
    | _ -> Json.error "Invalid value. Expected one of: 1, 2, 3, 4" json
  ;;
end

module CodeActionTriggerKind = struct
  type t =
    | Invoked
    | Automatic

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Invoked -> `Int 1
    | Automatic -> `Int 2
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Invoked
    | `Int 2 -> Automatic
    | _ -> Json.error "Invalid\nvalue. Expected one of: 1, 2" json
  ;;
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
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Invoked
    | `Int 2 -> TriggerCharacter
    | `Int 3 -> TriggerForIncompleteCompletions
    | _ -> Json.error "Invalid value. Expected\none of: 1, 2, 3" json
  ;;
end

module InsertTextFormat = struct
  type t =
    | PlainText
    | Snippet

  let yojson_of_t (t : t) : Json.t =
    match t with
    | PlainText -> `Int 1
    | Snippet -> `Int 2
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> PlainText
    | `Int 2 -> Snippet
    | _ -> Json.error "Invalid\nvalue. Expected one of: 1, 2" json
  ;;
end

module NotebookCellKind = struct
  type t =
    | Markup
    | Code

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Markup -> `Int 1
    | Code -> `Int 2
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Markup
    | `Int 2 -> Code
    | _ -> Json.error "Invalid value.\nExpected one of: 1, 2" json
  ;;
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
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Created
    | `Int 2 -> Changed
    | `Int 3 -> Deleted
    | _ -> Json.error "Invalid value. Expected one of: 1, 2, 3" json
  ;;
end

module WatchKind = struct
  type t =
    | Create
    | Change
    | Delete
    | Other of string

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Create -> `Int 1
    | Change -> `Int 2
    | Delete -> `Int 4
    | Other s -> `String s
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Create
    | `Int 2 -> Change
    | `Int 4 -> Delete
    | `String s -> Other s
    | _ -> Json.error "Invalid value. Expected one of: 1, 2,\n4" json
  ;;
end

module DocumentDiagnosticReportKind = struct
  type t =
    | Full
    | Unchanged

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Full -> `String "full"
    | Unchanged -> `String "unchanged"
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "full" -> Full
    | `String "unchanged" -> Unchanged
    | _ -> Json.error "Invalid value. Expected one of: \"full\", \"unchanged\"" json
  ;;
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
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Text
    | `Int 2 -> Read
    | `Int 3 -> Write
    | _ -> Json.error "Invalid value. Expected one of: 1, 2, 3" json
  ;;
end

module FileOperationPatternKind = struct
  type t =
    | File
    | Folder

  let yojson_of_t (t : t) : Json.t =
    match t with
    | File -> `String "file"
    | Folder -> `String "folder"
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "file" -> File
    | `String "folder" -> Folder
    | _ -> Json.error "Invalid value. Expected one of: \"file\", \"folder\"" json
  ;;
end

module TraceValues = struct
  type t =
    | Compact
    | Off
    | Messages
    | Verbose

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Compact -> `String "compact"
    | Off -> `String "off"
    | Messages -> `String "messages"
    | Verbose -> `String "verbose"
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "compact" -> Compact
    | `String "off" -> Off
    | `String "messages" -> Messages
    | `String "verbose" -> Verbose
    | _ ->
      Json.error
        "Invalid value. Expected one of: \"compact\", \"off\",\n\"messages\", \"verbose\""
        json
  ;;
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
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 0 -> None
    | `Int 1 -> Full
    | `Int 2 -> Incremental
    | _ -> Json.error "Invalid value. Expected one of: 0, 1, 2" json
  ;;
end

module InlayHintKind = struct
  type t =
    | Type
    | Parameter

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Type -> `Int 1
    | Parameter -> `Int 2
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Type
    | `Int 2 -> Parameter
    | _ -> Json.error "Invalid\nvalue. Expected one of: 1, 2" json
  ;;
end

module InlineCompletionTriggerKind = struct
  type t =
    | Invoked
    | Automatic

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Invoked -> `Int 0
    | Automatic -> `Int 1
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 0 -> Invoked
    | `Int 1 -> Automatic
    | _ -> Json.error "Invalid\nvalue. Expected one of: 0, 1" json
  ;;
end

module MessageType = struct
  type t =
    | Error
    | Warning
    | Info
    | Log
    | Debug

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Error -> `Int 1
    | Warning -> `Int 2
    | Info -> `Int 3
    | Log -> `Int 4
    | Debug -> `Int 5
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Error
    | `Int 2 -> Warning
    | `Int 3 -> Info
    | `Int 4 -> Log
    | `Int 5 -> Debug
    | _ -> Json.error "Invalid value. Expected one of:\n1, 2, 3, 4, 5" json
  ;;
end

module UniquenessLevel = struct
  type t =
    | Document
    | Project
    | Group
    | Scheme
    | Global

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Document -> `String "document"
    | Project -> `String "project"
    | Group -> `String "group"
    | Scheme -> `String "scheme"
    | Global -> `String "global"
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "document" -> Document
    | `String "project" -> Project
    | `String "group" -> Group
    | `String "scheme" -> Scheme
    | `String "global" -> Global
    | _ ->
      Json.error
        "Invalid value.\n\
         Expected one of: \"document\", \"project\", \"group\", \"scheme\",\n\
         \"global\""
        json
  ;;
end

module MonikerKind = struct
  type t =
    | Import
    | Export
    | Local

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Import -> `String "import"
    | Export -> `String "export"
    | Local -> `String "local"
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "import" -> Import
    | `String "export" -> Export
    | `String "local" -> Local
    | _ ->
      Json.error "Invalid value. Expected one of:\n\"import\", \"export\", \"local\"" json
  ;;
end

module SemanticTokenModifiers = struct
  type t =
    | Declaration
    | Definition
    | Readonly
    | Static
    | Deprecated
    | Abstract
    | Async
    | Modification
    | Documentation
    | DefaultLibrary

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Declaration -> `String "declaration"
    | Definition -> `String "definition"
    | Readonly -> `String "readonly"
    | Static -> `String "static"
    | Deprecated -> `String "deprecated"
    | Abstract -> `String "abstract"
    | Async -> `String "async"
    | Modification -> `String "modification"
    | Documentation -> `String "documentation"
    | DefaultLibrary -> `String "defaultLibrary"
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "declaration" -> Declaration
    | `String "definition" -> Definition
    | `String "readonly" -> Readonly
    | `String "static" -> Static
    | `String "deprecated" -> Deprecated
    | `String "abstract" -> Abstract
    | `String "async" -> Async
    | `String "modification" -> Modification
    | `String "documentation" -> Documentation
    | `String "defaultLibrary" -> DefaultLibrary
    | _ ->
      Json.error
        "Invalid value.\n\
         Expected one of: \"declaration\", \"definition\", \"readonly\", \"static\",\n\
         \"deprecated\", \"abstract\", \"async\", \"modification\", \"documentation\",\n\
         \"defaultLibrary\""
        json
  ;;
end

module SemanticTokenTypes = struct
  type t =
    | Namespace
    | Type
    | Class
    | Enum
    | Interface
    | Struct
    | TypeParameter
    | Parameter
    | Variable
    | Property
    | EnumMember
    | Event
    | Function
    | Method
    | Macro
    | Keyword
    | Modifier
    | Comment
    | String
    | Number
    | Regexp
    | Operator
    | Decorator

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Namespace -> `String "namespace"
    | Type -> `String "type"
    | Class -> `String "class"
    | Enum -> `String "enum"
    | Interface -> `String "interface"
    | Struct -> `String "struct"
    | TypeParameter -> `String "typeParameter"
    | Parameter -> `String "parameter"
    | Variable -> `String "variable"
    | Property -> `String "property"
    | EnumMember -> `String "enumMember"
    | Event -> `String "event"
    | Function -> `String "function"
    | Method -> `String "method"
    | Macro -> `String "macro"
    | Keyword -> `String "keyword"
    | Modifier -> `String "modifier"
    | Comment -> `String "comment"
    | String -> `String "string"
    | Number -> `String "number"
    | Regexp -> `String "regexp"
    | Operator -> `String "operator"
    | Decorator -> `String "decorator"
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "namespace" -> Namespace
    | `String "type" -> Type
    | `String "class" -> Class
    | `String "enum" -> Enum
    | `String "interface" -> Interface
    | `String "struct" -> Struct
    | `String "typeParameter" -> TypeParameter
    | `String "parameter" -> Parameter
    | `String "variable" -> Variable
    | `String "property" -> Property
    | `String "enumMember" -> EnumMember
    | `String "event" -> Event
    | `String "function" -> Function
    | `String "method" -> Method
    | `String "macro" -> Macro
    | `String "keyword" -> Keyword
    | `String "modifier" -> Modifier
    | `String "comment" -> Comment
    | `String "string" -> String
    | `String "number" -> Number
    | `String "regexp" -> Regexp
    | `String "operator" -> Operator
    | `String "decorator" -> Decorator
    | _ ->
      Json.error
        "Invalid value. Expected one of: \"namespace\",\n\
         \"type\", \"class\", \"enum\", \"interface\", \"struct\", \"typeParameter\",\n\
         \"parameter\", \"variable\", \"property\", \"enumMember\", \"event\",\n\
         \"function\", \"method\", \"macro\", \"keyword\", \"modifier\", \"comment\",\n\
         \"string\", \"number\", \"regexp\", \"operator\", \"decorator\""
        json
  ;;
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
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Invoked
    | `Int 2 -> TriggerCharacter
    | `Int 3 -> ContentChange
    | _ -> Json.error "Invalid value. Expected one of: 1, 2, 3" json
  ;;
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
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Manual
    | `Int 2 -> AfterDelay
    | `Int 3 -> FocusOut
    | _ -> Json.error "Invalid value. Expected one of: 1, 2, 3" json
  ;;
end

module Position = struct
  type t =
    { character : int
    ; line : int
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(character : int) ~(line : int) : t = { character; line }
end

module Range = struct
  type t =
    { end_ : Position.t [@key "end"]
    ; start : Position.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(end_ : Position.t) ~(start : Position.t) : t = { end_; start }
end

module ChangeAnnotationIdentifier = struct
  type t = string [@@deriving yojson]
end

module AnnotatedTextEdit = struct
  type t =
    { annotationId : ChangeAnnotationIdentifier.t
    ; newText : string
    ; range : Range.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(annotationId : ChangeAnnotationIdentifier.t)
    ~(newText : string)
    ~(range : Range.t)
    : t
    =
    { annotationId; newText; range }
  ;;
end

module DeleteFileOptions = struct
  type t =
    { ignoreIfNotExists : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; recursive : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(ignoreIfNotExists : bool option) ?(recursive : bool option) (() : unit) : t
    =
    { ignoreIfNotExists; recursive }
  ;;
end

module DeleteFile = struct
  type t =
    { annotationId : ChangeAnnotationIdentifier.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; options : DeleteFileOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; uri : DocumentUri.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(annotationId : ChangeAnnotationIdentifier.t option)
    ?(options : DeleteFileOptions.t option)
    ~(uri : DocumentUri.t)
    (() : unit)
    : t
    =
    { annotationId; options; uri }
  ;;

  let yojson_of_t (t : t) : Json.t = Json.To.literal_field "kind" "delete" yojson_of_t t

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "delete" t_of_yojson json
  ;;
end

module RenameFileOptions = struct
  type t =
    { ignoreIfExists : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; overwrite : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(ignoreIfExists : bool option) ?(overwrite : bool option) (() : unit) : t =
    { ignoreIfExists; overwrite }
  ;;
end

module RenameFile = struct
  type t =
    { annotationId : ChangeAnnotationIdentifier.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; newUri : DocumentUri.t
    ; oldUri : DocumentUri.t
    ; options : RenameFileOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(annotationId : ChangeAnnotationIdentifier.t option)
    ~(newUri : DocumentUri.t)
    ~(oldUri : DocumentUri.t)
    ?(options : RenameFileOptions.t option)
    (() : unit)
    : t
    =
    { annotationId; newUri; oldUri; options }
  ;;

  let yojson_of_t (t : t) : Json.t = Json.To.literal_field "kind" "rename" yojson_of_t t

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "rename" t_of_yojson json
  ;;
end

module CreateFileOptions = struct
  type t =
    { ignoreIfExists : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; overwrite : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(ignoreIfExists : bool option) ?(overwrite : bool option) (() : unit) : t =
    { ignoreIfExists; overwrite }
  ;;
end

module CreateFile = struct
  type t =
    { annotationId : ChangeAnnotationIdentifier.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; options : CreateFileOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; uri : DocumentUri.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(annotationId : ChangeAnnotationIdentifier.t option)
    ?(options : CreateFileOptions.t option)
    ~(uri : DocumentUri.t)
    (() : unit)
    : t
    =
    { annotationId; options; uri }
  ;;

  let yojson_of_t (t : t) : Json.t = Json.To.literal_field "kind" "create" yojson_of_t t

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "create" t_of_yojson json
  ;;
end

module OptionalVersionedTextDocumentIdentifier = struct
  type t =
    { uri : DocumentUri.t
    ; version : int Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(uri : DocumentUri.t) ?(version : int option) (() : unit) : t =
    { uri; version }
  ;;
end

module TextEdit = struct
  type t =
    { newText : string
    ; range : Range.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(newText : string) ~(range : Range.t) : t = { newText; range }
end

module TextDocumentEdit = struct
  type edits_pvar =
    [ `TextEdit of TextEdit.t
    | `AnnotatedTextEdit of AnnotatedTextEdit.t
    ]

  let edits_pvar_of_yojson (json : Json.t) : edits_pvar =
    Json.Of.untagged_union
      "edits_pvar"
      [ (fun json -> `TextEdit (TextEdit.t_of_yojson json))
      ; (fun json -> `AnnotatedTextEdit (AnnotatedTextEdit.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_edits_pvar (edits_pvar : edits_pvar) : Json.t =
    match edits_pvar with
    | `TextEdit s -> TextEdit.yojson_of_t s
    | `AnnotatedTextEdit s -> AnnotatedTextEdit.yojson_of_t s
  ;;

  type t =
    { edits : edits_pvar list
    ; textDocument : OptionalVersionedTextDocumentIdentifier.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(edits : edits_pvar list)
    ~(textDocument : OptionalVersionedTextDocumentIdentifier.t)
    : t
    =
    { edits; textDocument }
  ;;
end

module ChangeAnnotation = struct
  type t =
    { description : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; label : string
    ; needsConfirmation : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(description : string option)
    ~(label : string)
    ?(needsConfirmation : bool option)
    (() : unit)
    : t
    =
    { description; label; needsConfirmation }
  ;;
end

module WorkspaceEdit = struct
  type documentChanges_pvar =
    [ `TextDocumentEdit of TextDocumentEdit.t
    | `CreateFile of CreateFile.t
    | `RenameFile of RenameFile.t
    | `DeleteFile of DeleteFile.t
    ]

  let documentChanges_pvar_of_yojson (json : Json.t) : documentChanges_pvar =
    Json.Of.untagged_union
      "documentChanges_pvar"
      [ (fun json -> `TextDocumentEdit (TextDocumentEdit.t_of_yojson json))
      ; (fun json -> `CreateFile (CreateFile.t_of_yojson json))
      ; (fun json -> `RenameFile (RenameFile.t_of_yojson json))
      ; (fun json -> `DeleteFile (DeleteFile.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_documentChanges_pvar (documentChanges_pvar : documentChanges_pvar)
    : Json.t
    =
    match documentChanges_pvar with
    | `TextDocumentEdit s -> TextDocumentEdit.yojson_of_t s
    | `CreateFile s -> CreateFile.yojson_of_t s
    | `RenameFile s -> RenameFile.yojson_of_t s
    | `DeleteFile s -> DeleteFile.yojson_of_t s
  ;;

  type t =
    { changeAnnotations :
        (ChangeAnnotationIdentifier.t, ChangeAnnotation.t) Json.Assoc.t
          Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; changes : (DocumentUri.t, TextEdit.t list) Json.Assoc.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; documentChanges : documentChanges_pvar list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(changeAnnotations :
        (ChangeAnnotationIdentifier.t, ChangeAnnotation.t) Json.Assoc.t option)
    ?(changes : (DocumentUri.t, TextEdit.t list) Json.Assoc.t option)
    ?(documentChanges : documentChanges_pvar list option)
    (() : unit)
    : t
    =
    { changeAnnotations; changes; documentChanges }
  ;;
end

module ApplyWorkspaceEditParams = struct
  type t =
    { edit : WorkspaceEdit.t
    ; label : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(edit : WorkspaceEdit.t) ?(label : string option) (() : unit) : t =
    { edit; label }
  ;;
end

module ApplyWorkspaceEditResult = struct
  type t =
    { applied : bool
    ; failedChange : int Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; failureReason : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(applied : bool)
    ?(failedChange : int option)
    ?(failureReason : string option)
    (() : unit)
    : t
    =
    { applied; failedChange; failureReason }
  ;;
end

module BaseSymbolInformation = struct
  type t =
    { containerName : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; kind : SymbolKind.t
    ; name : string
    ; tags : SymbolTag.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(containerName : string option)
    ~(kind : SymbolKind.t)
    ~(name : string)
    ?(tags : SymbolTag.t list option)
    (() : unit)
    : t
    =
    { containerName; kind; name; tags }
  ;;
end

module CallHierarchyClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
  ;;
end

module CallHierarchyItem = struct
  type t =
    { data : Json.t option [@yojson.option]
    ; detail : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; kind : SymbolKind.t
    ; name : string
    ; range : Range.t
    ; selectionRange : Range.t
    ; tags : SymbolTag.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; uri : DocumentUri.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(data : Json.t option)
    ?(detail : string option)
    ~(kind : SymbolKind.t)
    ~(name : string)
    ~(range : Range.t)
    ~(selectionRange : Range.t)
    ?(tags : SymbolTag.t list option)
    ~(uri : DocumentUri.t)
    (() : unit)
    : t
    =
    { data; detail; kind; name; range; selectionRange; tags; uri }
  ;;
end

module CallHierarchyIncomingCall = struct
  type t =
    { from : CallHierarchyItem.t
    ; fromRanges : Range.t list
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(from : CallHierarchyItem.t) ~(fromRanges : Range.t list) : t =
    { from; fromRanges }
  ;;
end

module CallHierarchyIncomingCallsParams = struct
  type t =
    { item : CallHierarchyItem.t
    ; partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(item : CallHierarchyItem.t)
    ?(partialResultToken : ProgressToken.t option)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { item; partialResultToken; workDoneToken }
  ;;
end

module CallHierarchyOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module CallHierarchyOutgoingCall = struct
  type t =
    { fromRanges : Range.t list
    ; to_ : CallHierarchyItem.t [@key "to"]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(fromRanges : Range.t list) ~(to_ : CallHierarchyItem.t) : t =
    { fromRanges; to_ }
  ;;
end

module CallHierarchyOutgoingCallsParams = struct
  type t =
    { item : CallHierarchyItem.t
    ; partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(item : CallHierarchyItem.t)
    ?(partialResultToken : ProgressToken.t option)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { item; partialResultToken; workDoneToken }
  ;;
end

module TextDocumentIdentifier = struct
  type t = { uri : DocumentUri.t } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(uri : DocumentUri.t) : t = { uri }
end

module CallHierarchyPrepareParams = struct
  type t =
    { position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { position; textDocument; workDoneToken }
  ;;
end

module NotebookCellTextDocumentFilter = struct
  type notebook_pvar =
    [ `String of string
    | `NotebookDocumentFilter of NotebookDocumentFilter.t
    ]

  let notebook_pvar_of_yojson (json : Json.t) : notebook_pvar =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union
        "notebook_pvar"
        [ (fun json -> `NotebookDocumentFilter (NotebookDocumentFilter.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_notebook_pvar (notebook_pvar : notebook_pvar) : Json.t =
    match notebook_pvar with
    | `String j -> `String j
    | `NotebookDocumentFilter s -> NotebookDocumentFilter.yojson_of_t s
  ;;

  type t =
    { language : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; notebook : notebook_pvar
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(language : string option) ~(notebook : notebook_pvar) (() : unit) : t =
    { language; notebook }
  ;;
end

module DocumentFilter = struct
  type t =
    [ `TextDocumentFilter of TextDocumentFilter.t
    | `NotebookCellTextDocumentFilter of NotebookCellTextDocumentFilter.t
    ]

  let t_of_yojson (json : Json.t) : t =
    Json.Of.untagged_union
      "t"
      [ (fun json -> `TextDocumentFilter (TextDocumentFilter.t_of_yojson json))
      ; (fun json ->
          `NotebookCellTextDocumentFilter
            (NotebookCellTextDocumentFilter.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_t (t : t) : Json.t =
    match t with
    | `TextDocumentFilter s -> TextDocumentFilter.yojson_of_t s
    | `NotebookCellTextDocumentFilter s -> NotebookCellTextDocumentFilter.yojson_of_t s
  ;;
end

module DocumentSelector = struct
  type t = DocumentFilter.t list [@@deriving yojson]
end

module CallHierarchyRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(id : string option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; id; workDoneProgress }
  ;;
end

module CancelParams = struct
  type t = { id : Jsonrpc.Id.t } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(id : Jsonrpc.Id.t) : t = { id }
end

module WorkspaceEditClientCapabilities = struct
  type changeAnnotationSupport =
    { groupsOnLabel : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_changeAnnotationSupport ?(groupsOnLabel : bool option) (() : unit)
    : changeAnnotationSupport
    =
    { groupsOnLabel }
  ;;

  type t =
    { changeAnnotationSupport : changeAnnotationSupport Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; documentChanges : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; failureHandling : FailureHandlingKind.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; normalizesLineEndings : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; resourceOperations : ResourceOperationKind.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(changeAnnotationSupport : changeAnnotationSupport option)
    ?(documentChanges : bool option)
    ?(failureHandling : FailureHandlingKind.t option)
    ?(normalizesLineEndings : bool option)
    ?(resourceOperations : ResourceOperationKind.t list option)
    (() : unit)
    : t
    =
    { changeAnnotationSupport
    ; documentChanges
    ; failureHandling
    ; normalizesLineEndings
    ; resourceOperations
    }
  ;;
end

module WorkspaceSymbolClientCapabilities = struct
  type tagSupport = { valueSet : SymbolTag.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_tagSupport ~(valueSet : SymbolTag.t list) : tagSupport = { valueSet }

  type symbolKind =
    { valueSet : SymbolKind.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_symbolKind ?(valueSet : SymbolKind.t list option) (() : unit) : symbolKind =
    { valueSet }
  ;;

  type resolveSupport = { properties : string list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_resolveSupport ~(properties : string list) : resolveSupport = { properties }

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; resolveSupport : resolveSupport Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; symbolKind : symbolKind Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; tagSupport : tagSupport Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(dynamicRegistration : bool option)
    ?(resolveSupport : resolveSupport option)
    ?(symbolKind : symbolKind option)
    ?(tagSupport : tagSupport option)
    (() : unit)
    : t
    =
    { dynamicRegistration; resolveSupport; symbolKind; tagSupport }
  ;;
end

module SemanticTokensWorkspaceClientCapabilities = struct
  type t =
    { refreshSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(refreshSupport : bool option) (() : unit) : t = { refreshSupport }
end

module InlineValueWorkspaceClientCapabilities = struct
  type t =
    { refreshSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(refreshSupport : bool option) (() : unit) : t = { refreshSupport }
end

module InlayHintWorkspaceClientCapabilities = struct
  type t =
    { refreshSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(refreshSupport : bool option) (() : unit) : t = { refreshSupport }
end

module FoldingRangeWorkspaceClientCapabilities = struct
  type t =
    { refreshSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(refreshSupport : bool option) (() : unit) : t = { refreshSupport }
end

module FileOperationClientCapabilities = struct
  type t =
    { didCreate : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; didDelete : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; didRename : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; willCreate : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; willDelete : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; willRename : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(didCreate : bool option)
    ?(didDelete : bool option)
    ?(didRename : bool option)
    ?(dynamicRegistration : bool option)
    ?(willCreate : bool option)
    ?(willDelete : bool option)
    ?(willRename : bool option)
    (() : unit)
    : t
    =
    { didCreate
    ; didDelete
    ; didRename
    ; dynamicRegistration
    ; willCreate
    ; willDelete
    ; willRename
    }
  ;;
end

module ExecuteCommandClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
  ;;
end

module DidChangeWatchedFilesClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; relativePatternSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(dynamicRegistration : bool option)
    ?(relativePatternSupport : bool option)
    (() : unit)
    : t
    =
    { dynamicRegistration; relativePatternSupport }
  ;;
end

module DidChangeConfigurationClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
  ;;
end

module DiagnosticWorkspaceClientCapabilities = struct
  type t =
    { refreshSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(refreshSupport : bool option) (() : unit) : t = { refreshSupport }
end

module CodeLensWorkspaceClientCapabilities = struct
  type t =
    { refreshSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(refreshSupport : bool option) (() : unit) : t = { refreshSupport }
end

module WorkspaceClientCapabilities = struct
  type t =
    { applyEdit : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; codeLens : CodeLensWorkspaceClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; configuration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; diagnostics : DiagnosticWorkspaceClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; didChangeConfiguration :
        DidChangeConfigurationClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; didChangeWatchedFiles :
        DidChangeWatchedFilesClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; executeCommand : ExecuteCommandClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; fileOperations : FileOperationClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; foldingRange : FoldingRangeWorkspaceClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; inlayHint : InlayHintWorkspaceClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; inlineValue : InlineValueWorkspaceClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; semanticTokens : SemanticTokensWorkspaceClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; symbol : WorkspaceSymbolClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workspaceEdit : WorkspaceEditClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workspaceFolders : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(applyEdit : bool option)
    ?(codeLens : CodeLensWorkspaceClientCapabilities.t option)
    ?(configuration : bool option)
    ?(diagnostics : DiagnosticWorkspaceClientCapabilities.t option)
    ?(didChangeConfiguration : DidChangeConfigurationClientCapabilities.t option)
    ?(didChangeWatchedFiles : DidChangeWatchedFilesClientCapabilities.t option)
    ?(executeCommand : ExecuteCommandClientCapabilities.t option)
    ?(fileOperations : FileOperationClientCapabilities.t option)
    ?(foldingRange : FoldingRangeWorkspaceClientCapabilities.t option)
    ?(inlayHint : InlayHintWorkspaceClientCapabilities.t option)
    ?(inlineValue : InlineValueWorkspaceClientCapabilities.t option)
    ?(semanticTokens : SemanticTokensWorkspaceClientCapabilities.t option)
    ?(symbol : WorkspaceSymbolClientCapabilities.t option)
    ?(workspaceEdit : WorkspaceEditClientCapabilities.t option)
    ?(workspaceFolders : bool option)
    (() : unit)
    : t
    =
    { applyEdit
    ; codeLens
    ; configuration
    ; diagnostics
    ; didChangeConfiguration
    ; didChangeWatchedFiles
    ; executeCommand
    ; fileOperations
    ; foldingRange
    ; inlayHint
    ; inlineValue
    ; semanticTokens
    ; symbol
    ; workspaceEdit
    ; workspaceFolders
    }
  ;;
end

module ShowMessageRequestClientCapabilities = struct
  type messageActionItem =
    { additionalPropertiesSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_messageActionItem ?(additionalPropertiesSupport : bool option) (() : unit)
    : messageActionItem
    =
    { additionalPropertiesSupport }
  ;;

  type t =
    { messageActionItem : messageActionItem Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(messageActionItem : messageActionItem option) (() : unit) : t =
    { messageActionItem }
  ;;
end

module ShowDocumentClientCapabilities = struct
  type t = { support : bool } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(support : bool) : t = { support }
end

module WindowClientCapabilities = struct
  type t =
    { showDocument : ShowDocumentClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; showMessage : ShowMessageRequestClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(showDocument : ShowDocumentClientCapabilities.t option)
    ?(showMessage : ShowMessageRequestClientCapabilities.t option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { showDocument; showMessage; workDoneProgress }
  ;;
end

module TypeHierarchyClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
  ;;
end

module TypeDefinitionClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; linkSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) ?(linkSupport : bool option) (() : unit)
    : t
    =
    { dynamicRegistration; linkSupport }
  ;;
end

module TextDocumentSyncClientCapabilities = struct
  type t =
    { didSave : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; willSave : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; willSaveWaitUntil : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(didSave : bool option)
    ?(dynamicRegistration : bool option)
    ?(willSave : bool option)
    ?(willSaveWaitUntil : bool option)
    (() : unit)
    : t
    =
    { didSave; dynamicRegistration; willSave; willSaveWaitUntil }
  ;;
end

module SignatureHelpClientCapabilities = struct
  type parameterInformation =
    { labelOffsetSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_parameterInformation ?(labelOffsetSupport : bool option) (() : unit)
    : parameterInformation
    =
    { labelOffsetSupport }
  ;;

  type signatureInformation =
    { documentationFormat : MarkupKind.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; parameterInformation : parameterInformation Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; activeParameterSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; noActiveParameterSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_signatureInformation
    ?(documentationFormat : MarkupKind.t list option)
    ?(parameterInformation : parameterInformation option)
    ?(activeParameterSupport : bool option)
    ?(noActiveParameterSupport : bool option)
    (() : unit)
    : signatureInformation
    =
    { documentationFormat
    ; parameterInformation
    ; activeParameterSupport
    ; noActiveParameterSupport
    }
  ;;

  type t =
    { contextSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; signatureInformation : signatureInformation Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(contextSupport : bool option)
    ?(dynamicRegistration : bool option)
    ?(signatureInformation : signatureInformation option)
    (() : unit)
    : t
    =
    { contextSupport; dynamicRegistration; signatureInformation }
  ;;
end

module SemanticTokensClientCapabilities = struct
  type full =
    { delta : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_full ?(delta : bool option) (() : unit) : full = { delta }

  type full_pvar =
    [ `Bool of bool
    | `Full of full
    ]

  let full_pvar_of_yojson (json : Json.t) : full_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "full_pvar"
        [ (fun json -> `Full (full_of_yojson json)) ]
        json
  ;;

  let yojson_of_full_pvar (full_pvar : full_pvar) : Json.t =
    match full_pvar with
    | `Bool j -> `Bool j
    | `Full s -> yojson_of_full s
  ;;

  type requests =
    { range : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; full : full_pvar Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_requests ?(range : bool option) ?(full : full_pvar option) (() : unit)
    : requests
    =
    { range; full }
  ;;

  type t =
    { augmentsSyntaxTokens : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; formats : TokenFormat.t list
    ; multilineTokenSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; overlappingTokenSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; requests : requests
    ; serverCancelSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; tokenModifiers : string list
    ; tokenTypes : string list
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(augmentsSyntaxTokens : bool option)
    ?(dynamicRegistration : bool option)
    ~(formats : TokenFormat.t list)
    ?(multilineTokenSupport : bool option)
    ?(overlappingTokenSupport : bool option)
    ~(requests : requests)
    ?(serverCancelSupport : bool option)
    ~(tokenModifiers : string list)
    ~(tokenTypes : string list)
    (() : unit)
    : t
    =
    { augmentsSyntaxTokens
    ; dynamicRegistration
    ; formats
    ; multilineTokenSupport
    ; overlappingTokenSupport
    ; requests
    ; serverCancelSupport
    ; tokenModifiers
    ; tokenTypes
    }
  ;;
end

module SelectionRangeClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
  ;;
end

module RenameClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; honorsChangeAnnotations : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; prepareSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; prepareSupportDefaultBehavior :
        PrepareSupportDefaultBehavior.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(dynamicRegistration : bool option)
    ?(honorsChangeAnnotations : bool option)
    ?(prepareSupport : bool option)
    ?(prepareSupportDefaultBehavior : PrepareSupportDefaultBehavior.t option)
    (() : unit)
    : t
    =
    { dynamicRegistration
    ; honorsChangeAnnotations
    ; prepareSupport
    ; prepareSupportDefaultBehavior
    }
  ;;
end

module ReferenceClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
  ;;
end

module DocumentRangeFormattingClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; rangesSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(dynamicRegistration : bool option)
    ?(rangesSupport : bool option)
    (() : unit)
    : t
    =
    { dynamicRegistration; rangesSupport }
  ;;
end

module PublishDiagnosticsClientCapabilities = struct
  type tagSupport = { valueSet : DiagnosticTag.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_tagSupport ~(valueSet : DiagnosticTag.t list) : tagSupport = { valueSet }

  type t =
    { codeDescriptionSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; dataSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; relatedInformation : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; tagSupport : tagSupport Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; versionSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(codeDescriptionSupport : bool option)
    ?(dataSupport : bool option)
    ?(relatedInformation : bool option)
    ?(tagSupport : tagSupport option)
    ?(versionSupport : bool option)
    (() : unit)
    : t
    =
    { codeDescriptionSupport
    ; dataSupport
    ; relatedInformation
    ; tagSupport
    ; versionSupport
    }
  ;;
end

module DocumentOnTypeFormattingClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
  ;;
end

module MonikerClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
  ;;
end

module LinkedEditingRangeClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
  ;;
end

module InlineValueClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
  ;;
end

module InlineCompletionClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
  ;;
end

module InlayHintClientCapabilities = struct
  type resolveSupport = { properties : string list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_resolveSupport ~(properties : string list) : resolveSupport = { properties }

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; resolveSupport : resolveSupport Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(dynamicRegistration : bool option)
    ?(resolveSupport : resolveSupport option)
    (() : unit)
    : t
    =
    { dynamicRegistration; resolveSupport }
  ;;
end

module ImplementationClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; linkSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) ?(linkSupport : bool option) (() : unit)
    : t
    =
    { dynamicRegistration; linkSupport }
  ;;
end

module HoverClientCapabilities = struct
  type t =
    { contentFormat : MarkupKind.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(contentFormat : MarkupKind.t list option)
    ?(dynamicRegistration : bool option)
    (() : unit)
    : t
    =
    { contentFormat; dynamicRegistration }
  ;;
end

module DocumentFormattingClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
  ;;
end

module FoldingRangeClientCapabilities = struct
  type foldingRangeKind =
    { valueSet : FoldingRangeKind.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_foldingRangeKind ?(valueSet : FoldingRangeKind.t list option) (() : unit)
    : foldingRangeKind
    =
    { valueSet }
  ;;

  type foldingRange =
    { collapsedText : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_foldingRange ?(collapsedText : bool option) (() : unit) : foldingRange =
    { collapsedText }
  ;;

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; foldingRange : foldingRange Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; foldingRangeKind : foldingRangeKind Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; lineFoldingOnly : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; rangeLimit : int Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(dynamicRegistration : bool option)
    ?(foldingRange : foldingRange option)
    ?(foldingRangeKind : foldingRangeKind option)
    ?(lineFoldingOnly : bool option)
    ?(rangeLimit : int option)
    (() : unit)
    : t
    =
    { dynamicRegistration; foldingRange; foldingRangeKind; lineFoldingOnly; rangeLimit }
  ;;
end

module DocumentSymbolClientCapabilities = struct
  type tagSupport = { valueSet : SymbolTag.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_tagSupport ~(valueSet : SymbolTag.t list) : tagSupport = { valueSet }

  type symbolKind =
    { valueSet : SymbolKind.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_symbolKind ?(valueSet : SymbolKind.t list option) (() : unit) : symbolKind =
    { valueSet }
  ;;

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; hierarchicalDocumentSymbolSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; labelSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; symbolKind : symbolKind Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; tagSupport : tagSupport Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(dynamicRegistration : bool option)
    ?(hierarchicalDocumentSymbolSupport : bool option)
    ?(labelSupport : bool option)
    ?(symbolKind : symbolKind option)
    ?(tagSupport : tagSupport option)
    (() : unit)
    : t
    =
    { dynamicRegistration
    ; hierarchicalDocumentSymbolSupport
    ; labelSupport
    ; symbolKind
    ; tagSupport
    }
  ;;
end

module DocumentLinkClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; tooltipSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(dynamicRegistration : bool option)
    ?(tooltipSupport : bool option)
    (() : unit)
    : t
    =
    { dynamicRegistration; tooltipSupport }
  ;;
end

module DocumentHighlightClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
  ;;
end

module DiagnosticClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; markupMessageSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; relatedDocumentSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(dynamicRegistration : bool option)
    ?(markupMessageSupport : bool option)
    ?(relatedDocumentSupport : bool option)
    (() : unit)
    : t
    =
    { dynamicRegistration; markupMessageSupport; relatedDocumentSupport }
  ;;
end

module DefinitionClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; linkSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) ?(linkSupport : bool option) (() : unit)
    : t
    =
    { dynamicRegistration; linkSupport }
  ;;
end

module DeclarationClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; linkSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) ?(linkSupport : bool option) (() : unit)
    : t
    =
    { dynamicRegistration; linkSupport }
  ;;
end

module CompletionClientCapabilities = struct
  type completionList =
    { itemDefaults : string list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_completionList ?(itemDefaults : string list option) (() : unit)
    : completionList
    =
    { itemDefaults }
  ;;

  type completionItemKind =
    { valueSet : CompletionItemKind.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_completionItemKind ?(valueSet : CompletionItemKind.t list option) (() : unit)
    : completionItemKind
    =
    { valueSet }
  ;;

  type insertTextModeSupport = { valueSet : InsertTextMode.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_insertTextModeSupport ~(valueSet : InsertTextMode.t list)
    : insertTextModeSupport
    =
    { valueSet }
  ;;

  type resolveSupport = { properties : string list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_resolveSupport ~(properties : string list) : resolveSupport = { properties }

  type tagSupport = { valueSet : CompletionItemTag.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_tagSupport ~(valueSet : CompletionItemTag.t list) : tagSupport = { valueSet }

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
    ; insertReplaceSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; resolveSupport : resolveSupport Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; insertTextModeSupport : insertTextModeSupport Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; labelDetailsSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_completionItem
    ?(snippetSupport : bool option)
    ?(commitCharactersSupport : bool option)
    ?(documentationFormat : MarkupKind.t list option)
    ?(deprecatedSupport : bool option)
    ?(preselectSupport : bool option)
    ?(tagSupport : tagSupport option)
    ?(insertReplaceSupport : bool option)
    ?(resolveSupport : resolveSupport option)
    ?(insertTextModeSupport : insertTextModeSupport option)
    ?(labelDetailsSupport : bool option)
    (() : unit)
    : completionItem
    =
    { snippetSupport
    ; commitCharactersSupport
    ; documentationFormat
    ; deprecatedSupport
    ; preselectSupport
    ; tagSupport
    ; insertReplaceSupport
    ; resolveSupport
    ; insertTextModeSupport
    ; labelDetailsSupport
    }
  ;;

  type t =
    { completionItem : completionItem Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; completionItemKind : completionItemKind Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; completionList : completionList Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; contextSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; insertTextMode : InsertTextMode.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(completionItem : completionItem option)
    ?(completionItemKind : completionItemKind option)
    ?(completionList : completionList option)
    ?(contextSupport : bool option)
    ?(dynamicRegistration : bool option)
    ?(insertTextMode : InsertTextMode.t option)
    (() : unit)
    : t
    =
    { completionItem
    ; completionItemKind
    ; completionList
    ; contextSupport
    ; dynamicRegistration
    ; insertTextMode
    }
  ;;
end

module DocumentColorClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
  ;;
end

module CodeLensClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
  ;;
end

module CodeActionClientCapabilities = struct
  type resolveSupport = { properties : string list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_resolveSupport ~(properties : string list) : resolveSupport = { properties }

  type codeActionKind = { valueSet : CodeActionKind.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_codeActionKind ~(valueSet : CodeActionKind.t list) : codeActionKind =
    { valueSet }
  ;;

  type codeActionLiteralSupport = { codeActionKind : codeActionKind }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_codeActionLiteralSupport ~(codeActionKind : codeActionKind)
    : codeActionLiteralSupport
    =
    { codeActionKind }
  ;;

  type t =
    { codeActionLiteralSupport : codeActionLiteralSupport Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; dataSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; disabledSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; honorsChangeAnnotations : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; isPreferredSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; resolveSupport : resolveSupport Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(codeActionLiteralSupport : codeActionLiteralSupport option)
    ?(dataSupport : bool option)
    ?(disabledSupport : bool option)
    ?(dynamicRegistration : bool option)
    ?(honorsChangeAnnotations : bool option)
    ?(isPreferredSupport : bool option)
    ?(resolveSupport : resolveSupport option)
    (() : unit)
    : t
    =
    { codeActionLiteralSupport
    ; dataSupport
    ; disabledSupport
    ; dynamicRegistration
    ; honorsChangeAnnotations
    ; isPreferredSupport
    ; resolveSupport
    }
  ;;
end

module TextDocumentClientCapabilities = struct
  type t =
    { callHierarchy : CallHierarchyClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; codeAction : CodeActionClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; codeLens : CodeLensClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; colorProvider : DocumentColorClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; completion : CompletionClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; declaration : DeclarationClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; definition : DefinitionClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; diagnostic : DiagnosticClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; documentHighlight : DocumentHighlightClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; documentLink : DocumentLinkClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; documentSymbol : DocumentSymbolClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; foldingRange : FoldingRangeClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; formatting : DocumentFormattingClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; hover : HoverClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; implementation : ImplementationClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; inlayHint : InlayHintClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; inlineCompletion : InlineCompletionClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; inlineValue : InlineValueClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; linkedEditingRange : LinkedEditingRangeClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; moniker : MonikerClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; onTypeFormatting :
        DocumentOnTypeFormattingClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; publishDiagnostics : PublishDiagnosticsClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; rangeFormatting : DocumentRangeFormattingClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; references : ReferenceClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; rename : RenameClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; selectionRange : SelectionRangeClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; semanticTokens : SemanticTokensClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; signatureHelp : SignatureHelpClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; synchronization : TextDocumentSyncClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; typeDefinition : TypeDefinitionClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; typeHierarchy : TypeHierarchyClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(callHierarchy : CallHierarchyClientCapabilities.t option)
    ?(codeAction : CodeActionClientCapabilities.t option)
    ?(codeLens : CodeLensClientCapabilities.t option)
    ?(colorProvider : DocumentColorClientCapabilities.t option)
    ?(completion : CompletionClientCapabilities.t option)
    ?(declaration : DeclarationClientCapabilities.t option)
    ?(definition : DefinitionClientCapabilities.t option)
    ?(diagnostic : DiagnosticClientCapabilities.t option)
    ?(documentHighlight : DocumentHighlightClientCapabilities.t option)
    ?(documentLink : DocumentLinkClientCapabilities.t option)
    ?(documentSymbol : DocumentSymbolClientCapabilities.t option)
    ?(foldingRange : FoldingRangeClientCapabilities.t option)
    ?(formatting : DocumentFormattingClientCapabilities.t option)
    ?(hover : HoverClientCapabilities.t option)
    ?(implementation : ImplementationClientCapabilities.t option)
    ?(inlayHint : InlayHintClientCapabilities.t option)
    ?(inlineCompletion : InlineCompletionClientCapabilities.t option)
    ?(inlineValue : InlineValueClientCapabilities.t option)
    ?(linkedEditingRange : LinkedEditingRangeClientCapabilities.t option)
    ?(moniker : MonikerClientCapabilities.t option)
    ?(onTypeFormatting : DocumentOnTypeFormattingClientCapabilities.t option)
    ?(publishDiagnostics : PublishDiagnosticsClientCapabilities.t option)
    ?(rangeFormatting : DocumentRangeFormattingClientCapabilities.t option)
    ?(references : ReferenceClientCapabilities.t option)
    ?(rename : RenameClientCapabilities.t option)
    ?(selectionRange : SelectionRangeClientCapabilities.t option)
    ?(semanticTokens : SemanticTokensClientCapabilities.t option)
    ?(signatureHelp : SignatureHelpClientCapabilities.t option)
    ?(synchronization : TextDocumentSyncClientCapabilities.t option)
    ?(typeDefinition : TypeDefinitionClientCapabilities.t option)
    ?(typeHierarchy : TypeHierarchyClientCapabilities.t option)
    (() : unit)
    : t
    =
    { callHierarchy
    ; codeAction
    ; codeLens
    ; colorProvider
    ; completion
    ; declaration
    ; definition
    ; diagnostic
    ; documentHighlight
    ; documentLink
    ; documentSymbol
    ; foldingRange
    ; formatting
    ; hover
    ; implementation
    ; inlayHint
    ; inlineCompletion
    ; inlineValue
    ; linkedEditingRange
    ; moniker
    ; onTypeFormatting
    ; publishDiagnostics
    ; rangeFormatting
    ; references
    ; rename
    ; selectionRange
    ; semanticTokens
    ; signatureHelp
    ; synchronization
    ; typeDefinition
    ; typeHierarchy
    }
  ;;
end

module NotebookDocumentSyncClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; executionSummarySupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(dynamicRegistration : bool option)
    ?(executionSummarySupport : bool option)
    (() : unit)
    : t
    =
    { dynamicRegistration; executionSummarySupport }
  ;;
end

module NotebookDocumentClientCapabilities = struct
  type t = { synchronization : NotebookDocumentSyncClientCapabilities.t }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(synchronization : NotebookDocumentSyncClientCapabilities.t) : t =
    { synchronization }
  ;;
end

module RegularExpressionsClientCapabilities = struct
  type t =
    { engine : string
    ; version : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(engine : string) ?(version : string option) (() : unit) : t =
    { engine; version }
  ;;
end

module MarkdownClientCapabilities = struct
  type t =
    { allowedTags : string list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; parser : string
    ; version : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(allowedTags : string list option)
    ~(parser : string)
    ?(version : string option)
    (() : unit)
    : t
    =
    { allowedTags; parser; version }
  ;;
end

module GeneralClientCapabilities = struct
  type staleRequestSupport =
    { cancel : bool
    ; retryOnContentModified : string list
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_staleRequestSupport ~(cancel : bool) ~(retryOnContentModified : string list)
    : staleRequestSupport
    =
    { cancel; retryOnContentModified }
  ;;

  type t =
    { markdown : MarkdownClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; positionEncodings : PositionEncodingKind.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; regularExpressions : RegularExpressionsClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; staleRequestSupport : staleRequestSupport Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(markdown : MarkdownClientCapabilities.t option)
    ?(positionEncodings : PositionEncodingKind.t list option)
    ?(regularExpressions : RegularExpressionsClientCapabilities.t option)
    ?(staleRequestSupport : staleRequestSupport option)
    (() : unit)
    : t
    =
    { markdown; positionEncodings; regularExpressions; staleRequestSupport }
  ;;
end

module ClientCapabilities = struct
  type t =
    { experimental : Json.t option [@yojson.option]
    ; general : GeneralClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; notebookDocument : NotebookDocumentClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; textDocument : TextDocumentClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; window : WindowClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workspace : WorkspaceClientCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(experimental : Json.t option)
    ?(general : GeneralClientCapabilities.t option)
    ?(notebookDocument : NotebookDocumentClientCapabilities.t option)
    ?(textDocument : TextDocumentClientCapabilities.t option)
    ?(window : WindowClientCapabilities.t option)
    ?(workspace : WorkspaceClientCapabilities.t option)
    (() : unit)
    : t
    =
    { experimental; general; notebookDocument; textDocument; window; workspace }
  ;;
end

module Location = struct
  type t =
    { range : Range.t
    ; uri : DocumentUri.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(range : Range.t) ~(uri : DocumentUri.t) : t = { range; uri }
end

module DiagnosticRelatedInformation = struct
  type t =
    { location : Location.t
    ; message : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(location : Location.t) ~(message : string) : t = { location; message }
end

module MarkupContent = struct
  type t =
    { kind : MarkupKind.t
    ; value : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(kind : MarkupKind.t) ~(value : string) : t = { kind; value }
end

module CodeDescription = struct
  type t = { href : DocumentUri.t } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(href : DocumentUri.t) : t = { href }
end

module Diagnostic = struct
  type message_pvar =
    [ `String of string
    | `MarkupContent of MarkupContent.t
    ]

  let message_pvar_of_yojson (json : Json.t) : message_pvar =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union
        "message_pvar"
        [ (fun json -> `MarkupContent (MarkupContent.t_of_yojson json)) ]
        json
  ;;

  let yojson_of_message_pvar (message_pvar : message_pvar) : Json.t =
    match message_pvar with
    | `String j -> `String j
    | `MarkupContent s -> MarkupContent.yojson_of_t s
  ;;

  type t =
    { code : Jsonrpc.Id.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; codeDescription : CodeDescription.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; data : Json.t option [@yojson.option]
    ; message : message_pvar
    ; range : Range.t
    ; relatedInformation : DiagnosticRelatedInformation.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; severity : DiagnosticSeverity.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; source : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; tags : DiagnosticTag.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(code : Jsonrpc.Id.t option)
    ?(codeDescription : CodeDescription.t option)
    ?(data : Json.t option)
    ~(message : message_pvar)
    ~(range : Range.t)
    ?(relatedInformation : DiagnosticRelatedInformation.t list option)
    ?(severity : DiagnosticSeverity.t option)
    ?(source : string option)
    ?(tags : DiagnosticTag.t list option)
    (() : unit)
    : t
    =
    { code
    ; codeDescription
    ; data
    ; message
    ; range
    ; relatedInformation
    ; severity
    ; source
    ; tags
    }
  ;;
end

module Command = struct
  type t =
    { arguments : Json.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; command : string
    ; title : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(arguments : Json.t list option)
    ~(command : string)
    ~(title : string)
    (() : unit)
    : t
    =
    { arguments; command; title }
  ;;
end

module CodeAction = struct
  type disabled = { reason : string } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_disabled ~(reason : string) : disabled = { reason }

  type t =
    { command : Command.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; data : Json.t option [@yojson.option]
    ; diagnostics : Diagnostic.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; disabled : disabled Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; edit : WorkspaceEdit.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; isPreferred : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; kind : CodeActionKind.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; title : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(command : Command.t option)
    ?(data : Json.t option)
    ?(diagnostics : Diagnostic.t list option)
    ?(disabled : disabled option)
    ?(edit : WorkspaceEdit.t option)
    ?(isPreferred : bool option)
    ?(kind : CodeActionKind.t option)
    ~(title : string)
    (() : unit)
    : t
    =
    { command; data; diagnostics; disabled; edit; isPreferred; kind; title }
  ;;
end

module CodeActionContext = struct
  type t =
    { diagnostics : Diagnostic.t list
    ; only : CodeActionKind.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; triggerKind : CodeActionTriggerKind.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(diagnostics : Diagnostic.t list)
    ?(only : CodeActionKind.t list option)
    ?(triggerKind : CodeActionTriggerKind.t option)
    (() : unit)
    : t
    =
    { diagnostics; only; triggerKind }
  ;;
end

module CodeActionOptions = struct
  type t =
    { codeActionKinds : CodeActionKind.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(codeActionKinds : CodeActionKind.t list option)
    ?(resolveProvider : bool option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { codeActionKinds; resolveProvider; workDoneProgress }
  ;;
end

module CodeActionParams = struct
  type t =
    { context : CodeActionContext.t
    ; partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; range : Range.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(context : CodeActionContext.t)
    ?(partialResultToken : ProgressToken.t option)
    ~(range : Range.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { context; partialResultToken; range; textDocument; workDoneToken }
  ;;
end

module CodeActionRegistrationOptions = struct
  type t =
    { codeActionKinds : CodeActionKind.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(codeActionKinds : CodeActionKind.t list option)
    ?(documentSelector : DocumentSelector.t option)
    ?(resolveProvider : bool option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { codeActionKinds; documentSelector; resolveProvider; workDoneProgress }
  ;;
end

module CodeLens = struct
  type t =
    { command : Command.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; data : Json.t option [@yojson.option]
    ; range : Range.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(command : Command.t option)
    ?(data : Json.t option)
    ~(range : Range.t)
    (() : unit)
    : t
    =
    { command; data; range }
  ;;
end

module CodeLensOptions = struct
  type t =
    { resolveProvider : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(resolveProvider : bool option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { resolveProvider; workDoneProgress }
  ;;
end

module CodeLensParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(partialResultToken : ProgressToken.t option)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { partialResultToken; textDocument; workDoneToken }
  ;;
end

module CodeLensRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(resolveProvider : bool option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; resolveProvider; workDoneProgress }
  ;;
end

module Color = struct
  type t =
    { alpha : float
    ; blue : float
    ; green : float
    ; red : float
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(alpha : float) ~(blue : float) ~(green : float) ~(red : float) : t =
    { alpha; blue; green; red }
  ;;
end

module ColorInformation = struct
  type t =
    { color : Color.t
    ; range : Range.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(color : Color.t) ~(range : Range.t) : t = { color; range }
end

module ColorPresentation = struct
  type t =
    { additionalTextEdits : TextEdit.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; label : string
    ; textEdit : TextEdit.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(additionalTextEdits : TextEdit.t list option)
    ~(label : string)
    ?(textEdit : TextEdit.t option)
    (() : unit)
    : t
    =
    { additionalTextEdits; label; textEdit }
  ;;
end

module ColorPresentationParams = struct
  type t =
    { color : Color.t
    ; partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; range : Range.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(color : Color.t)
    ?(partialResultToken : ProgressToken.t option)
    ~(range : Range.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { color; partialResultToken; range; textDocument; workDoneToken }
  ;;
end

module CompletionContext = struct
  type t =
    { triggerCharacter : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; triggerKind : CompletionTriggerKind.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(triggerCharacter : string option)
    ~(triggerKind : CompletionTriggerKind.t)
    (() : unit)
    : t
    =
    { triggerCharacter; triggerKind }
  ;;
end

module InsertReplaceEdit = struct
  type t =
    { insert : Range.t
    ; newText : string
    ; replace : Range.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(insert : Range.t) ~(newText : string) ~(replace : Range.t) : t =
    { insert; newText; replace }
  ;;
end

module CompletionItemLabelDetails = struct
  type t =
    { description : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; detail : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(description : string option) ?(detail : string option) (() : unit) : t =
    { description; detail }
  ;;
end

module CompletionItem = struct
  type documentation_pvar =
    [ `String of string
    | `MarkupContent of MarkupContent.t
    ]

  let documentation_pvar_of_yojson (json : Json.t) : documentation_pvar =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union
        "documentation_pvar"
        [ (fun json -> `MarkupContent (MarkupContent.t_of_yojson json)) ]
        json
  ;;

  let yojson_of_documentation_pvar (documentation_pvar : documentation_pvar) : Json.t =
    match documentation_pvar with
    | `String j -> `String j
    | `MarkupContent s -> MarkupContent.yojson_of_t s
  ;;

  type textEdit_pvar =
    [ `TextEdit of TextEdit.t
    | `InsertReplaceEdit of InsertReplaceEdit.t
    ]

  let textEdit_pvar_of_yojson (json : Json.t) : textEdit_pvar =
    Json.Of.untagged_union
      "textEdit_pvar"
      [ (fun json -> `TextEdit (TextEdit.t_of_yojson json))
      ; (fun json -> `InsertReplaceEdit (InsertReplaceEdit.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_textEdit_pvar (textEdit_pvar : textEdit_pvar) : Json.t =
    match textEdit_pvar with
    | `TextEdit s -> TextEdit.yojson_of_t s
    | `InsertReplaceEdit s -> InsertReplaceEdit.yojson_of_t s
  ;;

  type t =
    { additionalTextEdits : TextEdit.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; command : Command.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; commitCharacters : string list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; data : Json.t option [@yojson.option]
    ; deprecated : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; detail : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; documentation : documentation_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; filterText : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; insertText : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; insertTextFormat : InsertTextFormat.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; insertTextMode : InsertTextMode.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; kind : CompletionItemKind.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; label : string
    ; labelDetails : CompletionItemLabelDetails.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; preselect : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; sortText : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; tags : CompletionItemTag.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; textEdit : textEdit_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; textEditText : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(additionalTextEdits : TextEdit.t list option)
    ?(command : Command.t option)
    ?(commitCharacters : string list option)
    ?(data : Json.t option)
    ?(deprecated : bool option)
    ?(detail : string option)
    ?(documentation : documentation_pvar option)
    ?(filterText : string option)
    ?(insertText : string option)
    ?(insertTextFormat : InsertTextFormat.t option)
    ?(insertTextMode : InsertTextMode.t option)
    ?(kind : CompletionItemKind.t option)
    ~(label : string)
    ?(labelDetails : CompletionItemLabelDetails.t option)
    ?(preselect : bool option)
    ?(sortText : string option)
    ?(tags : CompletionItemTag.t list option)
    ?(textEdit : textEdit_pvar option)
    ?(textEditText : string option)
    (() : unit)
    : t
    =
    { additionalTextEdits
    ; command
    ; commitCharacters
    ; data
    ; deprecated
    ; detail
    ; documentation
    ; filterText
    ; insertText
    ; insertTextFormat
    ; insertTextMode
    ; kind
    ; label
    ; labelDetails
    ; preselect
    ; sortText
    ; tags
    ; textEdit
    ; textEditText
    }
  ;;
end

module CompletionList = struct
  type editRange =
    { insert : Range.t
    ; replace : Range.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_editRange ~(insert : Range.t) ~(replace : Range.t) : editRange =
    { insert; replace }
  ;;

  type editRange_pvar =
    [ `Range of Range.t
    | `EditRange of editRange
    ]

  let editRange_pvar_of_yojson (json : Json.t) : editRange_pvar =
    Json.Of.untagged_union
      "editRange_pvar"
      [ (fun json -> `Range (Range.t_of_yojson json))
      ; (fun json -> `EditRange (editRange_of_yojson json))
      ]
      json
  ;;

  let yojson_of_editRange_pvar (editRange_pvar : editRange_pvar) : Json.t =
    match editRange_pvar with
    | `Range s -> Range.yojson_of_t s
    | `EditRange s -> yojson_of_editRange s
  ;;

  type itemDefaults =
    { commitCharacters : string list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; editRange : editRange_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; insertTextFormat : InsertTextFormat.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; insertTextMode : InsertTextMode.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; data : Json.t option [@yojson.option]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_itemDefaults
    ?(commitCharacters : string list option)
    ?(editRange : editRange_pvar option)
    ?(insertTextFormat : InsertTextFormat.t option)
    ?(insertTextMode : InsertTextMode.t option)
    ?(data : Json.t option)
    (() : unit)
    : itemDefaults
    =
    { commitCharacters; editRange; insertTextFormat; insertTextMode; data }
  ;;

  type t =
    { isIncomplete : bool
    ; itemDefaults : itemDefaults Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; items : CompletionItem.t list
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(isIncomplete : bool)
    ?(itemDefaults : itemDefaults option)
    ~(items : CompletionItem.t list)
    (() : unit)
    : t
    =
    { isIncomplete; itemDefaults; items }
  ;;
end

module CompletionOptions = struct
  type completionItem =
    { labelDetailsSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_completionItem ?(labelDetailsSupport : bool option) (() : unit)
    : completionItem
    =
    { labelDetailsSupport }
  ;;

  type t =
    { allCommitCharacters : string list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; completionItem : completionItem Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; triggerCharacters : string list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(allCommitCharacters : string list option)
    ?(completionItem : completionItem option)
    ?(resolveProvider : bool option)
    ?(triggerCharacters : string list option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { allCommitCharacters
    ; completionItem
    ; resolveProvider
    ; triggerCharacters
    ; workDoneProgress
    }
  ;;
end

module CompletionParams = struct
  type t =
    { context : CompletionContext.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(context : CompletionContext.t option)
    ?(partialResultToken : ProgressToken.t option)
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { context; partialResultToken; position; textDocument; workDoneToken }
  ;;
end

module CompletionRegistrationOptions = struct
  type completionItem =
    { labelDetailsSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_completionItem ?(labelDetailsSupport : bool option) (() : unit)
    : completionItem
    =
    { labelDetailsSupport }
  ;;

  type t =
    { allCommitCharacters : string list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; completionItem : completionItem Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; triggerCharacters : string list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(allCommitCharacters : string list option)
    ?(completionItem : completionItem option)
    ?(documentSelector : DocumentSelector.t option)
    ?(resolveProvider : bool option)
    ?(triggerCharacters : string list option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { allCommitCharacters
    ; completionItem
    ; documentSelector
    ; resolveProvider
    ; triggerCharacters
    ; workDoneProgress
    }
  ;;
end

module ConfigurationItem = struct
  type t =
    { scopeUri : DocumentUri.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; section : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(scopeUri : DocumentUri.t option) ?(section : string option) (() : unit) : t
    =
    { scopeUri; section }
  ;;
end

module ConfigurationParams = struct
  type t = { items : ConfigurationItem.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(items : ConfigurationItem.t list) : t = { items }
end

module FileCreate = struct
  type t = { uri : string } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(uri : string) : t = { uri }
end

module CreateFilesParams = struct
  type t = { files : FileCreate.t list } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(files : FileCreate.t list) : t = { files }
end

module Declaration = struct
  type t =
    [ `Location of Location.t
    | `List of Location.t list
    ]

  let t_of_yojson (json : Json.t) : t =
    Json.Of.untagged_union
      "t"
      [ (fun json -> `Location (Location.t_of_yojson json))
      ; (fun json -> `List (Json.Of.list Location.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_t (t : t) : Json.t =
    match t with
    | `Location s -> Location.yojson_of_t s
    | `List s -> Json.To.list Location.yojson_of_t s
  ;;
end

module LocationLink = struct
  type t =
    { originSelectionRange : Range.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; targetRange : Range.t
    ; targetSelectionRange : Range.t
    ; targetUri : DocumentUri.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(originSelectionRange : Range.t option)
    ~(targetRange : Range.t)
    ~(targetSelectionRange : Range.t)
    ~(targetUri : DocumentUri.t)
    (() : unit)
    : t
    =
    { originSelectionRange; targetRange; targetSelectionRange; targetUri }
  ;;
end

module DeclarationLink = struct
  type t = LocationLink.t [@@deriving yojson]
end

module DeclarationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module DeclarationParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(partialResultToken : ProgressToken.t option)
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { partialResultToken; position; textDocument; workDoneToken }
  ;;
end

module DeclarationRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(id : string option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; id; workDoneProgress }
  ;;
end

module Definition = struct
  type t =
    [ `Location of Location.t
    | `List of Location.t list
    ]

  let t_of_yojson (json : Json.t) : t =
    Json.Of.untagged_union
      "t"
      [ (fun json -> `Location (Location.t_of_yojson json))
      ; (fun json -> `List (Json.Of.list Location.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_t (t : t) : Json.t =
    match t with
    | `Location s -> Location.yojson_of_t s
    | `List s -> Json.To.list Location.yojson_of_t s
  ;;
end

module DefinitionLink = struct
  type t = LocationLink.t [@@deriving yojson]
end

module DefinitionOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module DefinitionParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(partialResultToken : ProgressToken.t option)
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { partialResultToken; position; textDocument; workDoneToken }
  ;;
end

module DefinitionRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; workDoneProgress }
  ;;
end

module FileDelete = struct
  type t = { uri : string } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(uri : string) : t = { uri }
end

module DeleteFilesParams = struct
  type t = { files : FileDelete.t list } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(files : FileDelete.t list) : t = { files }
end

module DiagnosticOptions = struct
  type t =
    { identifier : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; interFileDependencies : bool
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workspaceDiagnostics : bool
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(identifier : string option)
    ~(interFileDependencies : bool)
    ?(workDoneProgress : bool option)
    ~(workspaceDiagnostics : bool)
    (() : unit)
    : t
    =
    { identifier; interFileDependencies; workDoneProgress; workspaceDiagnostics }
  ;;
end

module DiagnosticRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; identifier : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; interFileDependencies : bool
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workspaceDiagnostics : bool
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(id : string option)
    ?(identifier : string option)
    ~(interFileDependencies : bool)
    ?(workDoneProgress : bool option)
    ~(workspaceDiagnostics : bool)
    (() : unit)
    : t
    =
    { documentSelector
    ; id
    ; identifier
    ; interFileDependencies
    ; workDoneProgress
    ; workspaceDiagnostics
    }
  ;;
end

module DiagnosticServerCancellationData = struct
  type t = { retriggerRequest : bool } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(retriggerRequest : bool) : t = { retriggerRequest }
end

module DidChangeConfigurationParams = struct
  type t = { settings : Json.t } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(settings : Json.t) : t = { settings }
end

module DidChangeConfigurationRegistrationOptions = struct
  type section_pvar =
    [ `String of string
    | `List of string list
    ]

  let section_pvar_of_yojson (json : Json.t) : section_pvar =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union
        "section_pvar"
        [ (fun json -> `List (Json.Of.list string_of_yojson json)) ]
        json
  ;;

  let yojson_of_section_pvar (section_pvar : section_pvar) : Json.t =
    match section_pvar with
    | `String j -> `String j
    | `List s -> Json.To.list yojson_of_string s
  ;;

  type t =
    { section : section_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(section : section_pvar option) (() : unit) : t = { section }
end

module VersionedNotebookDocumentIdentifier = struct
  type t =
    { uri : DocumentUri.t
    ; version : int
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(uri : DocumentUri.t) ~(version : int) : t = { uri; version }
end

module TextDocumentContentChangeEvent = struct
  type t =
    { range : Range.t Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; rangeLength : int Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; text : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(range : Range.t option)
    ?(rangeLength : int option)
    ~(text : string)
    (() : unit)
    : t
    =
    { range; rangeLength; text }
  ;;
end

module VersionedTextDocumentIdentifier = struct
  type t =
    { uri : DocumentUri.t
    ; version : int
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(uri : DocumentUri.t) ~(version : int) : t = { uri; version }
end

module ExecutionSummary = struct
  type t =
    { executionOrder : int
    ; success : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(executionOrder : int) ?(success : bool option) (() : unit) : t =
    { executionOrder; success }
  ;;
end

module NotebookCell = struct
  type t =
    { document : DocumentUri.t
    ; executionSummary : ExecutionSummary.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; kind : NotebookCellKind.t
    ; metadata : Json.Object.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(document : DocumentUri.t)
    ?(executionSummary : ExecutionSummary.t option)
    ~(kind : NotebookCellKind.t)
    ?(metadata : Json.Object.t option)
    (() : unit)
    : t
    =
    { document; executionSummary; kind; metadata }
  ;;
end

module TextDocumentItem = struct
  type t =
    { languageId : string
    ; text : string
    ; uri : DocumentUri.t
    ; version : int
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(languageId : string)
    ~(text : string)
    ~(uri : DocumentUri.t)
    ~(version : int)
    : t
    =
    { languageId; text; uri; version }
  ;;
end

module NotebookCellArrayChange = struct
  type t =
    { cells : NotebookCell.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; deleteCount : int
    ; start : int
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(cells : NotebookCell.t list option)
    ~(deleteCount : int)
    ~(start : int)
    (() : unit)
    : t
    =
    { cells; deleteCount; start }
  ;;
end

module NotebookDocumentChangeEvent = struct
  type textContent =
    { document : VersionedTextDocumentIdentifier.t
    ; changes : TextDocumentContentChangeEvent.t list
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_textContent
    ~(document : VersionedTextDocumentIdentifier.t)
    ~(changes : TextDocumentContentChangeEvent.t list)
    : textContent
    =
    { document; changes }
  ;;

  type structure =
    { array : NotebookCellArrayChange.t
    ; didOpen : TextDocumentItem.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; didClose : TextDocumentIdentifier.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_structure
    ~(array : NotebookCellArrayChange.t)
    ?(didOpen : TextDocumentItem.t list option)
    ?(didClose : TextDocumentIdentifier.t list option)
    (() : unit)
    : structure
    =
    { array; didOpen; didClose }
  ;;

  type cells =
    { structure : structure Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; data : NotebookCell.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; textContent : textContent list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_cells
    ?(structure : structure option)
    ?(data : NotebookCell.t list option)
    ?(textContent : textContent list option)
    (() : unit)
    : cells
    =
    { structure; data; textContent }
  ;;

  type t =
    { cells : cells Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; metadata : Json.Object.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(cells : cells option) ?(metadata : Json.Object.t option) (() : unit) : t =
    { cells; metadata }
  ;;
end

module DidChangeNotebookDocumentParams = struct
  type t =
    { change : NotebookDocumentChangeEvent.t
    ; notebookDocument : VersionedNotebookDocumentIdentifier.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(change : NotebookDocumentChangeEvent.t)
    ~(notebookDocument : VersionedNotebookDocumentIdentifier.t)
    : t
    =
    { change; notebookDocument }
  ;;
end

module DidChangeTextDocumentParams = struct
  type t =
    { contentChanges : TextDocumentContentChangeEvent.t list
    ; textDocument : VersionedTextDocumentIdentifier.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(contentChanges : TextDocumentContentChangeEvent.t list)
    ~(textDocument : VersionedTextDocumentIdentifier.t)
    : t
    =
    { contentChanges; textDocument }
  ;;
end

module FileEvent = struct
  type t =
    { type_ : FileChangeType.t [@key "type"]
    ; uri : DocumentUri.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(type_ : FileChangeType.t) ~(uri : DocumentUri.t) : t = { type_; uri }
end

module DidChangeWatchedFilesParams = struct
  type t = { changes : FileEvent.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(changes : FileEvent.t list) : t = { changes }
end

module Pattern = struct
  type t = string [@@deriving yojson]
end

module WorkspaceFolder = struct
  type t =
    { name : string
    ; uri : DocumentUri.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(name : string) ~(uri : DocumentUri.t) : t = { name; uri }
end

module RelativePattern = struct
  type t =
    { baseUri : unit
    ; pattern : Pattern.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(baseUri : unit) ~(pattern : Pattern.t) : t = { baseUri; pattern }
end

module GlobPattern = struct
  type t =
    [ `Pattern of Pattern.t
    | `RelativePattern of RelativePattern.t
    ]

  let t_of_yojson (json : Json.t) : t =
    Json.Of.untagged_union
      "t"
      [ (fun json -> `Pattern (Pattern.t_of_yojson json))
      ; (fun json -> `RelativePattern (RelativePattern.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_t (t : t) : Json.t =
    match t with
    | `Pattern s -> Pattern.yojson_of_t s
    | `RelativePattern s -> RelativePattern.yojson_of_t s
  ;;
end

module FileSystemWatcher = struct
  type t =
    { globPattern : GlobPattern.t
    ; kind : WatchKind.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(globPattern : GlobPattern.t) ?(kind : WatchKind.t option) (() : unit) : t =
    { globPattern; kind }
  ;;
end

module DidChangeWatchedFilesRegistrationOptions = struct
  type t = { watchers : FileSystemWatcher.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(watchers : FileSystemWatcher.t list) : t = { watchers }
end

module WorkspaceFoldersChangeEvent = struct
  type t =
    { added : WorkspaceFolder.t list
    ; removed : WorkspaceFolder.t list
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(added : WorkspaceFolder.t list) ~(removed : WorkspaceFolder.t list) : t =
    { added; removed }
  ;;
end

module DidChangeWorkspaceFoldersParams = struct
  type t = { event : WorkspaceFoldersChangeEvent.t }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(event : WorkspaceFoldersChangeEvent.t) : t = { event }
end

module NotebookDocumentIdentifier = struct
  type t = { uri : DocumentUri.t } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(uri : DocumentUri.t) : t = { uri }
end

module DidCloseNotebookDocumentParams = struct
  type t =
    { cellTextDocuments : TextDocumentIdentifier.t list
    ; notebookDocument : NotebookDocumentIdentifier.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(cellTextDocuments : TextDocumentIdentifier.t list)
    ~(notebookDocument : NotebookDocumentIdentifier.t)
    : t
    =
    { cellTextDocuments; notebookDocument }
  ;;
end

module DidCloseTextDocumentParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(textDocument : TextDocumentIdentifier.t) : t = { textDocument }
end

module NotebookDocument = struct
  type t =
    { cells : NotebookCell.t list
    ; metadata : Json.Object.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; notebookType : string
    ; uri : DocumentUri.t
    ; version : int
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(cells : NotebookCell.t list)
    ?(metadata : Json.Object.t option)
    ~(notebookType : string)
    ~(uri : DocumentUri.t)
    ~(version : int)
    (() : unit)
    : t
    =
    { cells; metadata; notebookType; uri; version }
  ;;
end

module DidOpenNotebookDocumentParams = struct
  type t =
    { cellTextDocuments : TextDocumentItem.t list
    ; notebookDocument : NotebookDocument.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(cellTextDocuments : TextDocumentItem.t list)
    ~(notebookDocument : NotebookDocument.t)
    : t
    =
    { cellTextDocuments; notebookDocument }
  ;;
end

module DidOpenTextDocumentParams = struct
  type t = { textDocument : TextDocumentItem.t }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(textDocument : TextDocumentItem.t) : t = { textDocument }
end

module DidSaveNotebookDocumentParams = struct
  type t = { notebookDocument : NotebookDocumentIdentifier.t }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(notebookDocument : NotebookDocumentIdentifier.t) : t = { notebookDocument }
end

module DidSaveTextDocumentParams = struct
  type t =
    { text : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; textDocument : TextDocumentIdentifier.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(text : string option)
    ~(textDocument : TextDocumentIdentifier.t)
    (() : unit)
    : t
    =
    { text; textDocument }
  ;;
end

module DocumentColorOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module DocumentColorParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(partialResultToken : ProgressToken.t option)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { partialResultToken; textDocument; workDoneToken }
  ;;
end

module DocumentColorRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(id : string option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; id; workDoneProgress }
  ;;
end

module DocumentDiagnosticParams = struct
  type t =
    { identifier : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; previousResultId : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(identifier : string option)
    ?(partialResultToken : ProgressToken.t option)
    ?(previousResultId : string option)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { identifier; partialResultToken; previousResultId; textDocument; workDoneToken }
  ;;
end

module UnchangedDocumentDiagnosticReport = struct
  type t = { resultId : string } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(resultId : string) : t = { resultId }

  let yojson_of_t (t : t) : Json.t =
    Json.To.literal_field "kind" "unchanged" yojson_of_t t
  ;;

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "unchanged" t_of_yojson json
  ;;
end

module FullDocumentDiagnosticReport = struct
  type t =
    { items : Diagnostic.t list
    ; resultId : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(items : Diagnostic.t list) ?(resultId : string option) (() : unit) : t =
    { items; resultId }
  ;;

  let yojson_of_t (t : t) : Json.t = Json.To.literal_field "kind" "full" yojson_of_t t

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "full" t_of_yojson json
  ;;
end

module RelatedUnchangedDocumentDiagnosticReport = struct
  type relatedDocuments_pvar =
    [ `FullDocumentDiagnosticReport of FullDocumentDiagnosticReport.t
    | `UnchangedDocumentDiagnosticReport of UnchangedDocumentDiagnosticReport.t
    ]

  let relatedDocuments_pvar_of_yojson (json : Json.t) : relatedDocuments_pvar =
    Json.Of.untagged_union
      "relatedDocuments_pvar"
      [ (fun json ->
          `FullDocumentDiagnosticReport (FullDocumentDiagnosticReport.t_of_yojson json))
      ; (fun json ->
          `UnchangedDocumentDiagnosticReport
            (UnchangedDocumentDiagnosticReport.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_relatedDocuments_pvar (relatedDocuments_pvar : relatedDocuments_pvar)
    : Json.t
    =
    match relatedDocuments_pvar with
    | `FullDocumentDiagnosticReport s -> FullDocumentDiagnosticReport.yojson_of_t s
    | `UnchangedDocumentDiagnosticReport s ->
      UnchangedDocumentDiagnosticReport.yojson_of_t s
  ;;

  type t =
    { relatedDocuments :
        (DocumentUri.t, relatedDocuments_pvar) Json.Assoc.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; resultId : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(relatedDocuments : (DocumentUri.t, relatedDocuments_pvar) Json.Assoc.t option)
    ~(resultId : string)
    (() : unit)
    : t
    =
    { relatedDocuments; resultId }
  ;;

  let yojson_of_t (t : t) : Json.t =
    Json.To.literal_field "kind" "unchanged" yojson_of_t t
  ;;

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "unchanged" t_of_yojson json
  ;;
end

module RelatedFullDocumentDiagnosticReport = struct
  type relatedDocuments_pvar =
    [ `FullDocumentDiagnosticReport of FullDocumentDiagnosticReport.t
    | `UnchangedDocumentDiagnosticReport of UnchangedDocumentDiagnosticReport.t
    ]

  let relatedDocuments_pvar_of_yojson (json : Json.t) : relatedDocuments_pvar =
    Json.Of.untagged_union
      "relatedDocuments_pvar"
      [ (fun json ->
          `FullDocumentDiagnosticReport (FullDocumentDiagnosticReport.t_of_yojson json))
      ; (fun json ->
          `UnchangedDocumentDiagnosticReport
            (UnchangedDocumentDiagnosticReport.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_relatedDocuments_pvar (relatedDocuments_pvar : relatedDocuments_pvar)
    : Json.t
    =
    match relatedDocuments_pvar with
    | `FullDocumentDiagnosticReport s -> FullDocumentDiagnosticReport.yojson_of_t s
    | `UnchangedDocumentDiagnosticReport s ->
      UnchangedDocumentDiagnosticReport.yojson_of_t s
  ;;

  type t =
    { items : Diagnostic.t list
    ; relatedDocuments :
        (DocumentUri.t, relatedDocuments_pvar) Json.Assoc.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; resultId : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(items : Diagnostic.t list)
    ?(relatedDocuments : (DocumentUri.t, relatedDocuments_pvar) Json.Assoc.t option)
    ?(resultId : string option)
    (() : unit)
    : t
    =
    { items; relatedDocuments; resultId }
  ;;

  let yojson_of_t (t : t) : Json.t = Json.To.literal_field "kind" "full" yojson_of_t t

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "full" t_of_yojson json
  ;;
end

module DocumentDiagnosticReport = struct
  type t =
    [ `RelatedFullDocumentDiagnosticReport of RelatedFullDocumentDiagnosticReport.t
    | `RelatedUnchangedDocumentDiagnosticReport of
      RelatedUnchangedDocumentDiagnosticReport.t
    ]

  let t_of_yojson (json : Json.t) : t =
    Json.Of.untagged_union
      "t"
      [ (fun json ->
          `RelatedFullDocumentDiagnosticReport
            (RelatedFullDocumentDiagnosticReport.t_of_yojson json))
      ; (fun json ->
          `RelatedUnchangedDocumentDiagnosticReport
            (RelatedUnchangedDocumentDiagnosticReport.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_t (t : t) : Json.t =
    match t with
    | `RelatedFullDocumentDiagnosticReport s ->
      RelatedFullDocumentDiagnosticReport.yojson_of_t s
    | `RelatedUnchangedDocumentDiagnosticReport s ->
      RelatedUnchangedDocumentDiagnosticReport.yojson_of_t s
  ;;
end

module DocumentDiagnosticReportPartialResult = struct
  type relatedDocuments_pvar =
    [ `FullDocumentDiagnosticReport of FullDocumentDiagnosticReport.t
    | `UnchangedDocumentDiagnosticReport of UnchangedDocumentDiagnosticReport.t
    ]

  let relatedDocuments_pvar_of_yojson (json : Json.t) : relatedDocuments_pvar =
    Json.Of.untagged_union
      "relatedDocuments_pvar"
      [ (fun json ->
          `FullDocumentDiagnosticReport (FullDocumentDiagnosticReport.t_of_yojson json))
      ; (fun json ->
          `UnchangedDocumentDiagnosticReport
            (UnchangedDocumentDiagnosticReport.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_relatedDocuments_pvar (relatedDocuments_pvar : relatedDocuments_pvar)
    : Json.t
    =
    match relatedDocuments_pvar with
    | `FullDocumentDiagnosticReport s -> FullDocumentDiagnosticReport.yojson_of_t s
    | `UnchangedDocumentDiagnosticReport s ->
      UnchangedDocumentDiagnosticReport.yojson_of_t s
  ;;

  type t = { relatedDocuments : (DocumentUri.t, relatedDocuments_pvar) Json.Assoc.t }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(relatedDocuments : (DocumentUri.t, relatedDocuments_pvar) Json.Assoc.t) : t
    =
    { relatedDocuments }
  ;;
end

module DocumentFormattingOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module FormattingOptions = struct
  type t =
    { insertFinalNewline : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; insertSpaces : bool
    ; tabSize : int
    ; trimFinalNewlines : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; trimTrailingWhitespace : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(insertFinalNewline : bool option)
    ~(insertSpaces : bool)
    ~(tabSize : int)
    ?(trimFinalNewlines : bool option)
    ?(trimTrailingWhitespace : bool option)
    (() : unit)
    : t
    =
    { insertFinalNewline
    ; insertSpaces
    ; tabSize
    ; trimFinalNewlines
    ; trimTrailingWhitespace
    }
  ;;
end

module DocumentFormattingParams = struct
  type t =
    { options : FormattingOptions.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(options : FormattingOptions.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { options; textDocument; workDoneToken }
  ;;
end

module DocumentFormattingRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; workDoneProgress }
  ;;
end

module DocumentHighlight = struct
  type t =
    { kind : DocumentHighlightKind.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; range : Range.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(kind : DocumentHighlightKind.t option) ~(range : Range.t) (() : unit) : t =
    { kind; range }
  ;;
end

module DocumentHighlightOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module DocumentHighlightParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(partialResultToken : ProgressToken.t option)
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { partialResultToken; position; textDocument; workDoneToken }
  ;;
end

module DocumentHighlightRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; workDoneProgress }
  ;;
end

module DocumentLink = struct
  type t =
    { data : Json.t option [@yojson.option]
    ; range : Range.t
    ; target : DocumentUri.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; tooltip : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(data : Json.t option)
    ~(range : Range.t)
    ?(target : DocumentUri.t option)
    ?(tooltip : string option)
    (() : unit)
    : t
    =
    { data; range; target; tooltip }
  ;;
end

module DocumentLinkOptions = struct
  type t =
    { resolveProvider : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(resolveProvider : bool option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { resolveProvider; workDoneProgress }
  ;;
end

module DocumentLinkParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(partialResultToken : ProgressToken.t option)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { partialResultToken; textDocument; workDoneToken }
  ;;
end

module DocumentLinkRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(resolveProvider : bool option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; resolveProvider; workDoneProgress }
  ;;
end

module DocumentOnTypeFormattingOptions = struct
  type t =
    { firstTriggerCharacter : string
    ; moreTriggerCharacter : string list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(firstTriggerCharacter : string)
    ?(moreTriggerCharacter : string list option)
    (() : unit)
    : t
    =
    { firstTriggerCharacter; moreTriggerCharacter }
  ;;
end

module DocumentOnTypeFormattingParams = struct
  type t =
    { ch : string
    ; options : FormattingOptions.t
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(ch : string)
    ~(options : FormattingOptions.t)
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    : t
    =
    { ch; options; position; textDocument }
  ;;
end

module DocumentOnTypeFormattingRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; firstTriggerCharacter : string
    ; moreTriggerCharacter : string list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ~(firstTriggerCharacter : string)
    ?(moreTriggerCharacter : string list option)
    (() : unit)
    : t
    =
    { documentSelector; firstTriggerCharacter; moreTriggerCharacter }
  ;;
end

module DocumentRangeFormattingOptions = struct
  type t =
    { rangesSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(rangesSupport : bool option) ?(workDoneProgress : bool option) (() : unit)
    : t
    =
    { rangesSupport; workDoneProgress }
  ;;
end

module DocumentRangeFormattingParams = struct
  type t =
    { options : FormattingOptions.t
    ; range : Range.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(options : FormattingOptions.t)
    ~(range : Range.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { options; range; textDocument; workDoneToken }
  ;;
end

module DocumentRangeFormattingRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; rangesSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(rangesSupport : bool option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; rangesSupport; workDoneProgress }
  ;;
end

module DocumentRangesFormattingParams = struct
  type t =
    { options : FormattingOptions.t
    ; ranges : Range.t list
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(options : FormattingOptions.t)
    ~(ranges : Range.t list)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { options; ranges; textDocument; workDoneToken }
  ;;
end

module DocumentSymbol = struct
  type t =
    { children : t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; deprecated : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; detail : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; kind : SymbolKind.t
    ; name : string
    ; range : Range.t
    ; selectionRange : Range.t
    ; tags : SymbolTag.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(children : t list option)
    ?(deprecated : bool option)
    ?(detail : string option)
    ~(kind : SymbolKind.t)
    ~(name : string)
    ~(range : Range.t)
    ~(selectionRange : Range.t)
    ?(tags : SymbolTag.t list option)
    (() : unit)
    : t
    =
    { children; deprecated; detail; kind; name; range; selectionRange; tags }
  ;;
end

module DocumentSymbolOptions = struct
  type t =
    { label : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(label : string option) ?(workDoneProgress : bool option) (() : unit) : t =
    { label; workDoneProgress }
  ;;
end

module DocumentSymbolParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(partialResultToken : ProgressToken.t option)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { partialResultToken; textDocument; workDoneToken }
  ;;
end

module DocumentSymbolRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; label : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(label : string option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; label; workDoneProgress }
  ;;
end

module ExecuteCommandOptions = struct
  type t =
    { commands : string list
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(commands : string list) ?(workDoneProgress : bool option) (() : unit) : t =
    { commands; workDoneProgress }
  ;;
end

module ExecuteCommandParams = struct
  type t =
    { arguments : Json.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; command : string
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(arguments : Json.t list option)
    ~(command : string)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { arguments; command; workDoneToken }
  ;;
end

module ExecuteCommandRegistrationOptions = struct
  type t =
    { commands : string list
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(commands : string list) ?(workDoneProgress : bool option) (() : unit) : t =
    { commands; workDoneProgress }
  ;;
end

module FileOperationPatternOptions = struct
  type t =
    { ignoreCase : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(ignoreCase : bool option) (() : unit) : t = { ignoreCase }
end

module FileOperationPattern = struct
  type t =
    { glob : string
    ; matches : FileOperationPatternKind.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; options : FileOperationPatternOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(glob : string)
    ?(matches : FileOperationPatternKind.t option)
    ?(options : FileOperationPatternOptions.t option)
    (() : unit)
    : t
    =
    { glob; matches; options }
  ;;
end

module FileOperationFilter = struct
  type t =
    { pattern : FileOperationPattern.t
    ; scheme : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(pattern : FileOperationPattern.t) ?(scheme : string option) (() : unit) : t
    =
    { pattern; scheme }
  ;;
end

module FileOperationRegistrationOptions = struct
  type t = { filters : FileOperationFilter.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(filters : FileOperationFilter.t list) : t = { filters }
end

module FileOperationOptions = struct
  type t =
    { didCreate : FileOperationRegistrationOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; didDelete : FileOperationRegistrationOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; didRename : FileOperationRegistrationOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; willCreate : FileOperationRegistrationOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; willDelete : FileOperationRegistrationOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; willRename : FileOperationRegistrationOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(didCreate : FileOperationRegistrationOptions.t option)
    ?(didDelete : FileOperationRegistrationOptions.t option)
    ?(didRename : FileOperationRegistrationOptions.t option)
    ?(willCreate : FileOperationRegistrationOptions.t option)
    ?(willDelete : FileOperationRegistrationOptions.t option)
    ?(willRename : FileOperationRegistrationOptions.t option)
    (() : unit)
    : t
    =
    { didCreate; didDelete; didRename; willCreate; willDelete; willRename }
  ;;
end

module FileRename = struct
  type t =
    { newUri : string
    ; oldUri : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(newUri : string) ~(oldUri : string) : t = { newUri; oldUri }
end

module FoldingRange = struct
  type t =
    { collapsedText : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; endCharacter : int Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; endLine : int
    ; kind : FoldingRangeKind.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; startCharacter : int Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; startLine : int
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(collapsedText : string option)
    ?(endCharacter : int option)
    ~(endLine : int)
    ?(kind : FoldingRangeKind.t option)
    ?(startCharacter : int option)
    ~(startLine : int)
    (() : unit)
    : t
    =
    { collapsedText; endCharacter; endLine; kind; startCharacter; startLine }
  ;;
end

module FoldingRangeOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module FoldingRangeParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(partialResultToken : ProgressToken.t option)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { partialResultToken; textDocument; workDoneToken }
  ;;
end

module FoldingRangeRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(id : string option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; id; workDoneProgress }
  ;;
end

module Hover = struct
  type contents_pvar =
    [ `MarkupContent of MarkupContent.t
    | `MarkedString of MarkedString.t
    | `List of MarkedString.t list
    ]

  let contents_pvar_of_yojson (json : Json.t) : contents_pvar =
    Json.Of.untagged_union
      "contents_pvar"
      [ (fun json -> `MarkupContent (MarkupContent.t_of_yojson json))
      ; (fun json -> `MarkedString (MarkedString.t_of_yojson json))
      ; (fun json -> `List (Json.Of.list MarkedString.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_contents_pvar (contents_pvar : contents_pvar) : Json.t =
    match contents_pvar with
    | `MarkupContent s -> MarkupContent.yojson_of_t s
    | `MarkedString s -> MarkedString.yojson_of_t s
    | `List s -> Json.To.list MarkedString.yojson_of_t s
  ;;

  type t =
    { contents : contents_pvar
    ; range : Range.t Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(contents : contents_pvar) ?(range : Range.t option) (() : unit) : t =
    { contents; range }
  ;;
end

module HoverOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module HoverParams = struct
  type t =
    { position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { position; textDocument; workDoneToken }
  ;;
end

module HoverRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; workDoneProgress }
  ;;
end

module ImplementationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module ImplementationParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(partialResultToken : ProgressToken.t option)
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { partialResultToken; position; textDocument; workDoneToken }
  ;;
end

module ImplementationRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(id : string option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; id; workDoneProgress }
  ;;
end

module InitializeError = struct
  type t = { retry : bool } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(retry : bool) : t = { retry }
end

module InitializeParams = struct
  type clientInfo =
    { name : string
    ; version : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_clientInfo ~(name : string) ?(version : string option) (() : unit)
    : clientInfo
    =
    { name; version }
  ;;

  let get_editor { name; version } =
    let editor =
      match Base.String.split ~on:' ' name with
      | [] -> ""
      | first_tok :: _ -> first_tok
    in
    match version with
    | None -> editor, "unknown"
    | Some v -> editor, v
  ;;

  type t =
    { capabilities : ClientCapabilities.t
    ; clientInfo : clientInfo Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; initializationOptions : Json.t option [@yojson.option]
    ; locale : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; processId : int Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; rootPath : string Json.Nullable_option.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; rootUri : DocumentUri.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; trace : TraceValues.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workspaceFolders :
        WorkspaceFolder.t list Json.Nullable_option.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(capabilities : ClientCapabilities.t)
    ?(clientInfo : clientInfo option)
    ?(initializationOptions : Json.t option)
    ?(locale : string option)
    ?(processId : int option)
    ?(rootPath : string option option)
    ?(rootUri : DocumentUri.t option)
    ?(trace : TraceValues.t option)
    ?(workDoneToken : ProgressToken.t option)
    ?(workspaceFolders : WorkspaceFolder.t list option option)
    (() : unit)
    : t
    =
    { capabilities
    ; clientInfo
    ; initializationOptions
    ; locale
    ; processId
    ; rootPath
    ; rootUri
    ; trace
    ; workDoneToken
    ; workspaceFolders
    }
  ;;
end

module WorkspaceSymbolOptions = struct
  type t =
    { resolveProvider : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(resolveProvider : bool option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { resolveProvider; workDoneProgress }
  ;;
end

module WorkspaceFoldersServerCapabilities = struct
  type changeNotifications_pvar =
    [ `String of string
    | `Bool of bool
    ]

  let changeNotifications_pvar_of_yojson (json : Json.t) : changeNotifications_pvar =
    match json with
    | `String j -> `String j
    | `Bool j -> `Bool j
    | _ -> Json.error "changeNotifications_pvar" json
  ;;

  let yojson_of_changeNotifications_pvar
    (changeNotifications_pvar : changeNotifications_pvar)
    : Json.t
    =
    match changeNotifications_pvar with
    | `String j -> `String j
    | `Bool j -> `Bool j
  ;;

  type t =
    { changeNotifications : changeNotifications_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; supported : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(changeNotifications : changeNotifications_pvar option)
    ?(supported : bool option)
    (() : unit)
    : t
    =
    { changeNotifications; supported }
  ;;
end

module TypeHierarchyRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(id : string option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; id; workDoneProgress }
  ;;
end

module TypeHierarchyOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module TypeDefinitionRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(id : string option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; id; workDoneProgress }
  ;;
end

module TypeDefinitionOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module SaveOptions = struct
  type t =
    { includeText : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(includeText : bool option) (() : unit) : t = { includeText }
end

module TextDocumentSyncOptions = struct
  type save_pvar =
    [ `Bool of bool
    | `SaveOptions of SaveOptions.t
    ]

  let save_pvar_of_yojson (json : Json.t) : save_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "save_pvar"
        [ (fun json -> `SaveOptions (SaveOptions.t_of_yojson json)) ]
        json
  ;;

  let yojson_of_save_pvar (save_pvar : save_pvar) : Json.t =
    match save_pvar with
    | `Bool j -> `Bool j
    | `SaveOptions s -> SaveOptions.yojson_of_t s
  ;;

  type t =
    { change : TextDocumentSyncKind.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; openClose : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; save : save_pvar Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; willSave : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; willSaveWaitUntil : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(change : TextDocumentSyncKind.t option)
    ?(openClose : bool option)
    ?(save : save_pvar option)
    ?(willSave : bool option)
    ?(willSaveWaitUntil : bool option)
    (() : unit)
    : t
    =
    { change; openClose; save; willSave; willSaveWaitUntil }
  ;;
end

module SignatureHelpOptions = struct
  type t =
    { retriggerCharacters : string list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; triggerCharacters : string list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(retriggerCharacters : string list option)
    ?(triggerCharacters : string list option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { retriggerCharacters; triggerCharacters; workDoneProgress }
  ;;
end

module SemanticTokensLegend = struct
  type t =
    { tokenModifiers : string list
    ; tokenTypes : string list
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(tokenModifiers : string list) ~(tokenTypes : string list) : t =
    { tokenModifiers; tokenTypes }
  ;;
end

module SemanticTokensRegistrationOptions = struct
  type full =
    { delta : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_full ?(delta : bool option) (() : unit) : full = { delta }

  type full_pvar =
    [ `Bool of bool
    | `Full of full
    ]

  let full_pvar_of_yojson (json : Json.t) : full_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "full_pvar"
        [ (fun json -> `Full (full_of_yojson json)) ]
        json
  ;;

  let yojson_of_full_pvar (full_pvar : full_pvar) : Json.t =
    match full_pvar with
    | `Bool j -> `Bool j
    | `Full s -> yojson_of_full s
  ;;

  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; full : full_pvar Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; legend : SemanticTokensLegend.t
    ; range : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(full : full_pvar option)
    ?(id : string option)
    ~(legend : SemanticTokensLegend.t)
    ?(range : bool option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; full; id; legend; range; workDoneProgress }
  ;;
end

module SemanticTokensOptions = struct
  type full =
    { delta : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_full ?(delta : bool option) (() : unit) : full = { delta }

  type full_pvar =
    [ `Bool of bool
    | `Full of full
    ]

  let full_pvar_of_yojson (json : Json.t) : full_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "full_pvar"
        [ (fun json -> `Full (full_of_yojson json)) ]
        json
  ;;

  let yojson_of_full_pvar (full_pvar : full_pvar) : Json.t =
    match full_pvar with
    | `Bool j -> `Bool j
    | `Full s -> yojson_of_full s
  ;;

  type t =
    { full : full_pvar Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; legend : SemanticTokensLegend.t
    ; range : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(full : full_pvar option)
    ~(legend : SemanticTokensLegend.t)
    ?(range : bool option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { full; legend; range; workDoneProgress }
  ;;
end

module SelectionRangeRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(id : string option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; id; workDoneProgress }
  ;;
end

module SelectionRangeOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module RenameOptions = struct
  type t =
    { prepareProvider : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(prepareProvider : bool option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { prepareProvider; workDoneProgress }
  ;;
end

module ReferenceOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module MonikerRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; workDoneProgress }
  ;;
end

module MonikerOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module LinkedEditingRangeRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(id : string option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; id; workDoneProgress }
  ;;
end

module LinkedEditingRangeOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module InlineValueRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(id : string option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; id; workDoneProgress }
  ;;
end

module InlineValueOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module InlineCompletionOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module InlayHintRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(id : string option)
    ?(resolveProvider : bool option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; id; resolveProvider; workDoneProgress }
  ;;
end

module InlayHintOptions = struct
  type t =
    { resolveProvider : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(resolveProvider : bool option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { resolveProvider; workDoneProgress }
  ;;
end

module ServerCapabilities = struct
  type workspace =
    { workspaceFolders : WorkspaceFoldersServerCapabilities.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; fileOperations : FileOperationOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_workspace
    ?(workspaceFolders : WorkspaceFoldersServerCapabilities.t option)
    ?(fileOperations : FileOperationOptions.t option)
    (() : unit)
    : workspace
    =
    { workspaceFolders; fileOperations }
  ;;

  type diagnostic =
    { markupMessageSupport : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_diagnostic ?(markupMessageSupport : bool option) (() : unit) : diagnostic =
    { markupMessageSupport }
  ;;

  type textDocument =
    { diagnostic : diagnostic Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_textDocument ?(diagnostic : diagnostic option) (() : unit) : textDocument =
    { diagnostic }
  ;;

  type callHierarchyProvider_pvar =
    [ `Bool of bool
    | `CallHierarchyOptions of CallHierarchyOptions.t
    | `CallHierarchyRegistrationOptions of CallHierarchyRegistrationOptions.t
    ]

  let callHierarchyProvider_pvar_of_yojson (json : Json.t) : callHierarchyProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "callHierarchyProvider_pvar"
        [ (fun json -> `CallHierarchyOptions (CallHierarchyOptions.t_of_yojson json))
        ; (fun json ->
            `CallHierarchyRegistrationOptions
              (CallHierarchyRegistrationOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_callHierarchyProvider_pvar
    (callHierarchyProvider_pvar : callHierarchyProvider_pvar)
    : Json.t
    =
    match callHierarchyProvider_pvar with
    | `Bool j -> `Bool j
    | `CallHierarchyOptions s -> CallHierarchyOptions.yojson_of_t s
    | `CallHierarchyRegistrationOptions s ->
      CallHierarchyRegistrationOptions.yojson_of_t s
  ;;

  type codeActionProvider_pvar =
    [ `Bool of bool
    | `CodeActionOptions of CodeActionOptions.t
    ]

  let codeActionProvider_pvar_of_yojson (json : Json.t) : codeActionProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "codeActionProvider_pvar"
        [ (fun json -> `CodeActionOptions (CodeActionOptions.t_of_yojson json)) ]
        json
  ;;

  let yojson_of_codeActionProvider_pvar
    (codeActionProvider_pvar : codeActionProvider_pvar)
    : Json.t
    =
    match codeActionProvider_pvar with
    | `Bool j -> `Bool j
    | `CodeActionOptions s -> CodeActionOptions.yojson_of_t s
  ;;

  type colorProvider_pvar =
    [ `Bool of bool
    | `DocumentColorOptions of DocumentColorOptions.t
    | `DocumentColorRegistrationOptions of DocumentColorRegistrationOptions.t
    ]

  let colorProvider_pvar_of_yojson (json : Json.t) : colorProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "colorProvider_pvar"
        [ (fun json -> `DocumentColorOptions (DocumentColorOptions.t_of_yojson json))
        ; (fun json ->
            `DocumentColorRegistrationOptions
              (DocumentColorRegistrationOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_colorProvider_pvar (colorProvider_pvar : colorProvider_pvar) : Json.t =
    match colorProvider_pvar with
    | `Bool j -> `Bool j
    | `DocumentColorOptions s -> DocumentColorOptions.yojson_of_t s
    | `DocumentColorRegistrationOptions s ->
      DocumentColorRegistrationOptions.yojson_of_t s
  ;;

  type declarationProvider_pvar =
    [ `Bool of bool
    | `DeclarationOptions of DeclarationOptions.t
    | `DeclarationRegistrationOptions of DeclarationRegistrationOptions.t
    ]

  let declarationProvider_pvar_of_yojson (json : Json.t) : declarationProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "declarationProvider_pvar"
        [ (fun json -> `DeclarationOptions (DeclarationOptions.t_of_yojson json))
        ; (fun json ->
            `DeclarationRegistrationOptions
              (DeclarationRegistrationOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_declarationProvider_pvar
    (declarationProvider_pvar : declarationProvider_pvar)
    : Json.t
    =
    match declarationProvider_pvar with
    | `Bool j -> `Bool j
    | `DeclarationOptions s -> DeclarationOptions.yojson_of_t s
    | `DeclarationRegistrationOptions s -> DeclarationRegistrationOptions.yojson_of_t s
  ;;

  type definitionProvider_pvar =
    [ `Bool of bool
    | `DefinitionOptions of DefinitionOptions.t
    ]

  let definitionProvider_pvar_of_yojson (json : Json.t) : definitionProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "definitionProvider_pvar"
        [ (fun json -> `DefinitionOptions (DefinitionOptions.t_of_yojson json)) ]
        json
  ;;

  let yojson_of_definitionProvider_pvar
    (definitionProvider_pvar : definitionProvider_pvar)
    : Json.t
    =
    match definitionProvider_pvar with
    | `Bool j -> `Bool j
    | `DefinitionOptions s -> DefinitionOptions.yojson_of_t s
  ;;

  type diagnosticProvider_pvar =
    [ `DiagnosticOptions of DiagnosticOptions.t
    | `DiagnosticRegistrationOptions of DiagnosticRegistrationOptions.t
    ]

  let diagnosticProvider_pvar_of_yojson (json : Json.t) : diagnosticProvider_pvar =
    Json.Of.untagged_union
      "diagnosticProvider_pvar"
      [ (fun json -> `DiagnosticOptions (DiagnosticOptions.t_of_yojson json))
      ; (fun json ->
          `DiagnosticRegistrationOptions (DiagnosticRegistrationOptions.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_diagnosticProvider_pvar
    (diagnosticProvider_pvar : diagnosticProvider_pvar)
    : Json.t
    =
    match diagnosticProvider_pvar with
    | `DiagnosticOptions s -> DiagnosticOptions.yojson_of_t s
    | `DiagnosticRegistrationOptions s -> DiagnosticRegistrationOptions.yojson_of_t s
  ;;

  type documentFormattingProvider_pvar =
    [ `Bool of bool
    | `DocumentFormattingOptions of DocumentFormattingOptions.t
    ]

  let documentFormattingProvider_pvar_of_yojson (json : Json.t)
    : documentFormattingProvider_pvar
    =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "documentFormattingProvider_pvar"
        [ (fun json ->
            `DocumentFormattingOptions (DocumentFormattingOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_documentFormattingProvider_pvar
    (documentFormattingProvider_pvar : documentFormattingProvider_pvar)
    : Json.t
    =
    match documentFormattingProvider_pvar with
    | `Bool j -> `Bool j
    | `DocumentFormattingOptions s -> DocumentFormattingOptions.yojson_of_t s
  ;;

  type documentHighlightProvider_pvar =
    [ `Bool of bool
    | `DocumentHighlightOptions of DocumentHighlightOptions.t
    ]

  let documentHighlightProvider_pvar_of_yojson (json : Json.t)
    : documentHighlightProvider_pvar
    =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "documentHighlightProvider_pvar"
        [ (fun json ->
            `DocumentHighlightOptions (DocumentHighlightOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_documentHighlightProvider_pvar
    (documentHighlightProvider_pvar : documentHighlightProvider_pvar)
    : Json.t
    =
    match documentHighlightProvider_pvar with
    | `Bool j -> `Bool j
    | `DocumentHighlightOptions s -> DocumentHighlightOptions.yojson_of_t s
  ;;

  type documentRangeFormattingProvider_pvar =
    [ `Bool of bool
    | `DocumentRangeFormattingOptions of DocumentRangeFormattingOptions.t
    ]

  let documentRangeFormattingProvider_pvar_of_yojson (json : Json.t)
    : documentRangeFormattingProvider_pvar
    =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "documentRangeFormattingProvider_pvar"
        [ (fun json ->
            `DocumentRangeFormattingOptions
              (DocumentRangeFormattingOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_documentRangeFormattingProvider_pvar
    (documentRangeFormattingProvider_pvar : documentRangeFormattingProvider_pvar)
    : Json.t
    =
    match documentRangeFormattingProvider_pvar with
    | `Bool j -> `Bool j
    | `DocumentRangeFormattingOptions s -> DocumentRangeFormattingOptions.yojson_of_t s
  ;;

  type documentSymbolProvider_pvar =
    [ `Bool of bool
    | `DocumentSymbolOptions of DocumentSymbolOptions.t
    ]

  let documentSymbolProvider_pvar_of_yojson (json : Json.t) : documentSymbolProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "documentSymbolProvider_pvar"
        [ (fun json -> `DocumentSymbolOptions (DocumentSymbolOptions.t_of_yojson json)) ]
        json
  ;;

  let yojson_of_documentSymbolProvider_pvar
    (documentSymbolProvider_pvar : documentSymbolProvider_pvar)
    : Json.t
    =
    match documentSymbolProvider_pvar with
    | `Bool j -> `Bool j
    | `DocumentSymbolOptions s -> DocumentSymbolOptions.yojson_of_t s
  ;;

  type foldingRangeProvider_pvar =
    [ `Bool of bool
    | `FoldingRangeOptions of FoldingRangeOptions.t
    | `FoldingRangeRegistrationOptions of FoldingRangeRegistrationOptions.t
    ]

  let foldingRangeProvider_pvar_of_yojson (json : Json.t) : foldingRangeProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "foldingRangeProvider_pvar"
        [ (fun json -> `FoldingRangeOptions (FoldingRangeOptions.t_of_yojson json))
        ; (fun json ->
            `FoldingRangeRegistrationOptions
              (FoldingRangeRegistrationOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_foldingRangeProvider_pvar
    (foldingRangeProvider_pvar : foldingRangeProvider_pvar)
    : Json.t
    =
    match foldingRangeProvider_pvar with
    | `Bool j -> `Bool j
    | `FoldingRangeOptions s -> FoldingRangeOptions.yojson_of_t s
    | `FoldingRangeRegistrationOptions s -> FoldingRangeRegistrationOptions.yojson_of_t s
  ;;

  type hoverProvider_pvar =
    [ `Bool of bool
    | `HoverOptions of HoverOptions.t
    ]

  let hoverProvider_pvar_of_yojson (json : Json.t) : hoverProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "hoverProvider_pvar"
        [ (fun json -> `HoverOptions (HoverOptions.t_of_yojson json)) ]
        json
  ;;

  let yojson_of_hoverProvider_pvar (hoverProvider_pvar : hoverProvider_pvar) : Json.t =
    match hoverProvider_pvar with
    | `Bool j -> `Bool j
    | `HoverOptions s -> HoverOptions.yojson_of_t s
  ;;

  type implementationProvider_pvar =
    [ `Bool of bool
    | `ImplementationOptions of ImplementationOptions.t
    | `ImplementationRegistrationOptions of ImplementationRegistrationOptions.t
    ]

  let implementationProvider_pvar_of_yojson (json : Json.t) : implementationProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "implementationProvider_pvar"
        [ (fun json -> `ImplementationOptions (ImplementationOptions.t_of_yojson json))
        ; (fun json ->
            `ImplementationRegistrationOptions
              (ImplementationRegistrationOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_implementationProvider_pvar
    (implementationProvider_pvar : implementationProvider_pvar)
    : Json.t
    =
    match implementationProvider_pvar with
    | `Bool j -> `Bool j
    | `ImplementationOptions s -> ImplementationOptions.yojson_of_t s
    | `ImplementationRegistrationOptions s ->
      ImplementationRegistrationOptions.yojson_of_t s
  ;;

  type inlayHintProvider_pvar =
    [ `Bool of bool
    | `InlayHintOptions of InlayHintOptions.t
    | `InlayHintRegistrationOptions of InlayHintRegistrationOptions.t
    ]

  let inlayHintProvider_pvar_of_yojson (json : Json.t) : inlayHintProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "inlayHintProvider_pvar"
        [ (fun json -> `InlayHintOptions (InlayHintOptions.t_of_yojson json))
        ; (fun json ->
            `InlayHintRegistrationOptions (InlayHintRegistrationOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_inlayHintProvider_pvar (inlayHintProvider_pvar : inlayHintProvider_pvar)
    : Json.t
    =
    match inlayHintProvider_pvar with
    | `Bool j -> `Bool j
    | `InlayHintOptions s -> InlayHintOptions.yojson_of_t s
    | `InlayHintRegistrationOptions s -> InlayHintRegistrationOptions.yojson_of_t s
  ;;

  type inlineCompletionProvider_pvar =
    [ `Bool of bool
    | `InlineCompletionOptions of InlineCompletionOptions.t
    ]

  let inlineCompletionProvider_pvar_of_yojson (json : Json.t)
    : inlineCompletionProvider_pvar
    =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "inlineCompletionProvider_pvar"
        [ (fun json ->
            `InlineCompletionOptions (InlineCompletionOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_inlineCompletionProvider_pvar
    (inlineCompletionProvider_pvar : inlineCompletionProvider_pvar)
    : Json.t
    =
    match inlineCompletionProvider_pvar with
    | `Bool j -> `Bool j
    | `InlineCompletionOptions s -> InlineCompletionOptions.yojson_of_t s
  ;;

  type inlineValueProvider_pvar =
    [ `Bool of bool
    | `InlineValueOptions of InlineValueOptions.t
    | `InlineValueRegistrationOptions of InlineValueRegistrationOptions.t
    ]

  let inlineValueProvider_pvar_of_yojson (json : Json.t) : inlineValueProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "inlineValueProvider_pvar"
        [ (fun json -> `InlineValueOptions (InlineValueOptions.t_of_yojson json))
        ; (fun json ->
            `InlineValueRegistrationOptions
              (InlineValueRegistrationOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_inlineValueProvider_pvar
    (inlineValueProvider_pvar : inlineValueProvider_pvar)
    : Json.t
    =
    match inlineValueProvider_pvar with
    | `Bool j -> `Bool j
    | `InlineValueOptions s -> InlineValueOptions.yojson_of_t s
    | `InlineValueRegistrationOptions s -> InlineValueRegistrationOptions.yojson_of_t s
  ;;

  type linkedEditingRangeProvider_pvar =
    [ `Bool of bool
    | `LinkedEditingRangeOptions of LinkedEditingRangeOptions.t
    | `LinkedEditingRangeRegistrationOptions of LinkedEditingRangeRegistrationOptions.t
    ]

  let linkedEditingRangeProvider_pvar_of_yojson (json : Json.t)
    : linkedEditingRangeProvider_pvar
    =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "linkedEditingRangeProvider_pvar"
        [ (fun json ->
            `LinkedEditingRangeOptions (LinkedEditingRangeOptions.t_of_yojson json))
        ; (fun json ->
            `LinkedEditingRangeRegistrationOptions
              (LinkedEditingRangeRegistrationOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_linkedEditingRangeProvider_pvar
    (linkedEditingRangeProvider_pvar : linkedEditingRangeProvider_pvar)
    : Json.t
    =
    match linkedEditingRangeProvider_pvar with
    | `Bool j -> `Bool j
    | `LinkedEditingRangeOptions s -> LinkedEditingRangeOptions.yojson_of_t s
    | `LinkedEditingRangeRegistrationOptions s ->
      LinkedEditingRangeRegistrationOptions.yojson_of_t s
  ;;

  type monikerProvider_pvar =
    [ `Bool of bool
    | `MonikerOptions of MonikerOptions.t
    | `MonikerRegistrationOptions of MonikerRegistrationOptions.t
    ]

  let monikerProvider_pvar_of_yojson (json : Json.t) : monikerProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "monikerProvider_pvar"
        [ (fun json -> `MonikerOptions (MonikerOptions.t_of_yojson json))
        ; (fun json ->
            `MonikerRegistrationOptions (MonikerRegistrationOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_monikerProvider_pvar (monikerProvider_pvar : monikerProvider_pvar)
    : Json.t
    =
    match monikerProvider_pvar with
    | `Bool j -> `Bool j
    | `MonikerOptions s -> MonikerOptions.yojson_of_t s
    | `MonikerRegistrationOptions s -> MonikerRegistrationOptions.yojson_of_t s
  ;;

  type notebookDocumentSync_pvar =
    [ `NotebookDocumentSyncOptions of NotebookDocumentSyncOptions.t
    | `NotebookDocumentSyncRegistrationOptions of
      NotebookDocumentSyncRegistrationOptions.t
    ]

  let notebookDocumentSync_pvar_of_yojson (json : Json.t) : notebookDocumentSync_pvar =
    Json.Of.untagged_union
      "notebookDocumentSync_pvar"
      [ (fun json ->
          `NotebookDocumentSyncOptions (NotebookDocumentSyncOptions.t_of_yojson json))
      ; (fun json ->
          `NotebookDocumentSyncRegistrationOptions
            (NotebookDocumentSyncRegistrationOptions.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_notebookDocumentSync_pvar
    (notebookDocumentSync_pvar : notebookDocumentSync_pvar)
    : Json.t
    =
    match notebookDocumentSync_pvar with
    | `NotebookDocumentSyncOptions s -> NotebookDocumentSyncOptions.yojson_of_t s
    | `NotebookDocumentSyncRegistrationOptions s ->
      NotebookDocumentSyncRegistrationOptions.yojson_of_t s
  ;;

  type referencesProvider_pvar =
    [ `Bool of bool
    | `ReferenceOptions of ReferenceOptions.t
    ]

  let referencesProvider_pvar_of_yojson (json : Json.t) : referencesProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "referencesProvider_pvar"
        [ (fun json -> `ReferenceOptions (ReferenceOptions.t_of_yojson json)) ]
        json
  ;;

  let yojson_of_referencesProvider_pvar
    (referencesProvider_pvar : referencesProvider_pvar)
    : Json.t
    =
    match referencesProvider_pvar with
    | `Bool j -> `Bool j
    | `ReferenceOptions s -> ReferenceOptions.yojson_of_t s
  ;;

  type renameProvider_pvar =
    [ `Bool of bool
    | `RenameOptions of RenameOptions.t
    ]

  let renameProvider_pvar_of_yojson (json : Json.t) : renameProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "renameProvider_pvar"
        [ (fun json -> `RenameOptions (RenameOptions.t_of_yojson json)) ]
        json
  ;;

  let yojson_of_renameProvider_pvar (renameProvider_pvar : renameProvider_pvar) : Json.t =
    match renameProvider_pvar with
    | `Bool j -> `Bool j
    | `RenameOptions s -> RenameOptions.yojson_of_t s
  ;;

  type selectionRangeProvider_pvar =
    [ `Bool of bool
    | `SelectionRangeOptions of SelectionRangeOptions.t
    | `SelectionRangeRegistrationOptions of SelectionRangeRegistrationOptions.t
    ]

  let selectionRangeProvider_pvar_of_yojson (json : Json.t) : selectionRangeProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "selectionRangeProvider_pvar"
        [ (fun json -> `SelectionRangeOptions (SelectionRangeOptions.t_of_yojson json))
        ; (fun json ->
            `SelectionRangeRegistrationOptions
              (SelectionRangeRegistrationOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_selectionRangeProvider_pvar
    (selectionRangeProvider_pvar : selectionRangeProvider_pvar)
    : Json.t
    =
    match selectionRangeProvider_pvar with
    | `Bool j -> `Bool j
    | `SelectionRangeOptions s -> SelectionRangeOptions.yojson_of_t s
    | `SelectionRangeRegistrationOptions s ->
      SelectionRangeRegistrationOptions.yojson_of_t s
  ;;

  type semanticTokensProvider_pvar =
    [ `SemanticTokensOptions of SemanticTokensOptions.t
    | `SemanticTokensRegistrationOptions of SemanticTokensRegistrationOptions.t
    ]

  let semanticTokensProvider_pvar_of_yojson (json : Json.t) : semanticTokensProvider_pvar =
    Json.Of.untagged_union
      "semanticTokensProvider_pvar"
      [ (fun json -> `SemanticTokensOptions (SemanticTokensOptions.t_of_yojson json))
      ; (fun json ->
          `SemanticTokensRegistrationOptions
            (SemanticTokensRegistrationOptions.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_semanticTokensProvider_pvar
    (semanticTokensProvider_pvar : semanticTokensProvider_pvar)
    : Json.t
    =
    match semanticTokensProvider_pvar with
    | `SemanticTokensOptions s -> SemanticTokensOptions.yojson_of_t s
    | `SemanticTokensRegistrationOptions s ->
      SemanticTokensRegistrationOptions.yojson_of_t s
  ;;

  type textDocumentSync_pvar =
    [ `TextDocumentSyncOptions of TextDocumentSyncOptions.t
    | `TextDocumentSyncKind of TextDocumentSyncKind.t
    ]

  let textDocumentSync_pvar_of_yojson (json : Json.t) : textDocumentSync_pvar =
    Json.Of.untagged_union
      "textDocumentSync_pvar"
      [ (fun json -> `TextDocumentSyncOptions (TextDocumentSyncOptions.t_of_yojson json))
      ; (fun json -> `TextDocumentSyncKind (TextDocumentSyncKind.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_textDocumentSync_pvar (textDocumentSync_pvar : textDocumentSync_pvar)
    : Json.t
    =
    match textDocumentSync_pvar with
    | `TextDocumentSyncOptions s -> TextDocumentSyncOptions.yojson_of_t s
    | `TextDocumentSyncKind s -> TextDocumentSyncKind.yojson_of_t s
  ;;

  type typeDefinitionProvider_pvar =
    [ `Bool of bool
    | `TypeDefinitionOptions of TypeDefinitionOptions.t
    | `TypeDefinitionRegistrationOptions of TypeDefinitionRegistrationOptions.t
    ]

  let typeDefinitionProvider_pvar_of_yojson (json : Json.t) : typeDefinitionProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "typeDefinitionProvider_pvar"
        [ (fun json -> `TypeDefinitionOptions (TypeDefinitionOptions.t_of_yojson json))
        ; (fun json ->
            `TypeDefinitionRegistrationOptions
              (TypeDefinitionRegistrationOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_typeDefinitionProvider_pvar
    (typeDefinitionProvider_pvar : typeDefinitionProvider_pvar)
    : Json.t
    =
    match typeDefinitionProvider_pvar with
    | `Bool j -> `Bool j
    | `TypeDefinitionOptions s -> TypeDefinitionOptions.yojson_of_t s
    | `TypeDefinitionRegistrationOptions s ->
      TypeDefinitionRegistrationOptions.yojson_of_t s
  ;;

  type typeHierarchyProvider_pvar =
    [ `Bool of bool
    | `TypeHierarchyOptions of TypeHierarchyOptions.t
    | `TypeHierarchyRegistrationOptions of TypeHierarchyRegistrationOptions.t
    ]

  let typeHierarchyProvider_pvar_of_yojson (json : Json.t) : typeHierarchyProvider_pvar =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "typeHierarchyProvider_pvar"
        [ (fun json -> `TypeHierarchyOptions (TypeHierarchyOptions.t_of_yojson json))
        ; (fun json ->
            `TypeHierarchyRegistrationOptions
              (TypeHierarchyRegistrationOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_typeHierarchyProvider_pvar
    (typeHierarchyProvider_pvar : typeHierarchyProvider_pvar)
    : Json.t
    =
    match typeHierarchyProvider_pvar with
    | `Bool j -> `Bool j
    | `TypeHierarchyOptions s -> TypeHierarchyOptions.yojson_of_t s
    | `TypeHierarchyRegistrationOptions s ->
      TypeHierarchyRegistrationOptions.yojson_of_t s
  ;;

  type workspaceSymbolProvider_pvar =
    [ `Bool of bool
    | `WorkspaceSymbolOptions of WorkspaceSymbolOptions.t
    ]

  let workspaceSymbolProvider_pvar_of_yojson (json : Json.t)
    : workspaceSymbolProvider_pvar
    =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union
        "workspaceSymbolProvider_pvar"
        [ (fun json -> `WorkspaceSymbolOptions (WorkspaceSymbolOptions.t_of_yojson json))
        ]
        json
  ;;

  let yojson_of_workspaceSymbolProvider_pvar
    (workspaceSymbolProvider_pvar : workspaceSymbolProvider_pvar)
    : Json.t
    =
    match workspaceSymbolProvider_pvar with
    | `Bool j -> `Bool j
    | `WorkspaceSymbolOptions s -> WorkspaceSymbolOptions.yojson_of_t s
  ;;

  type t =
    { callHierarchyProvider : callHierarchyProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; codeActionProvider : codeActionProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; codeLensProvider : CodeLensOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; colorProvider : colorProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; completionProvider : CompletionOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; declarationProvider : declarationProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; definitionProvider : definitionProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; diagnosticProvider : diagnosticProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; documentFormattingProvider : documentFormattingProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; documentHighlightProvider : documentHighlightProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; documentLinkProvider : DocumentLinkOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; documentOnTypeFormattingProvider :
        DocumentOnTypeFormattingOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; documentRangeFormattingProvider :
        documentRangeFormattingProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; documentSymbolProvider : documentSymbolProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; executeCommandProvider : ExecuteCommandOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; experimental : Json.t option [@yojson.option]
    ; foldingRangeProvider : foldingRangeProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; hoverProvider : hoverProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; implementationProvider : implementationProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; inlayHintProvider : inlayHintProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; inlineCompletionProvider : inlineCompletionProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; inlineValueProvider : inlineValueProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; linkedEditingRangeProvider : linkedEditingRangeProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; monikerProvider : monikerProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; notebookDocumentSync : notebookDocumentSync_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; positionEncoding : PositionEncodingKind.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; referencesProvider : referencesProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; renameProvider : renameProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; selectionRangeProvider : selectionRangeProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; semanticTokensProvider : semanticTokensProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; signatureHelpProvider : SignatureHelpOptions.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; textDocument : textDocument Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; textDocumentSync : textDocumentSync_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; typeDefinitionProvider : typeDefinitionProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; typeHierarchyProvider : typeHierarchyProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workspace : workspace Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workspaceSymbolProvider : workspaceSymbolProvider_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(callHierarchyProvider : callHierarchyProvider_pvar option)
    ?(codeActionProvider : codeActionProvider_pvar option)
    ?(codeLensProvider : CodeLensOptions.t option)
    ?(colorProvider : colorProvider_pvar option)
    ?(completionProvider : CompletionOptions.t option)
    ?(declarationProvider : declarationProvider_pvar option)
    ?(definitionProvider : definitionProvider_pvar option)
    ?(diagnosticProvider : diagnosticProvider_pvar option)
    ?(documentFormattingProvider : documentFormattingProvider_pvar option)
    ?(documentHighlightProvider : documentHighlightProvider_pvar option)
    ?(documentLinkProvider : DocumentLinkOptions.t option)
    ?(documentOnTypeFormattingProvider : DocumentOnTypeFormattingOptions.t option)
    ?(documentRangeFormattingProvider : documentRangeFormattingProvider_pvar option)
    ?(documentSymbolProvider : documentSymbolProvider_pvar option)
    ?(executeCommandProvider : ExecuteCommandOptions.t option)
    ?(experimental : Json.t option)
    ?(foldingRangeProvider : foldingRangeProvider_pvar option)
    ?(hoverProvider : hoverProvider_pvar option)
    ?(implementationProvider : implementationProvider_pvar option)
    ?(inlayHintProvider : inlayHintProvider_pvar option)
    ?(inlineCompletionProvider : inlineCompletionProvider_pvar option)
    ?(inlineValueProvider : inlineValueProvider_pvar option)
    ?(linkedEditingRangeProvider : linkedEditingRangeProvider_pvar option)
    ?(monikerProvider : monikerProvider_pvar option)
    ?(notebookDocumentSync : notebookDocumentSync_pvar option)
    ?(positionEncoding : PositionEncodingKind.t option)
    ?(referencesProvider : referencesProvider_pvar option)
    ?(renameProvider : renameProvider_pvar option)
    ?(selectionRangeProvider : selectionRangeProvider_pvar option)
    ?(semanticTokensProvider : semanticTokensProvider_pvar option)
    ?(signatureHelpProvider : SignatureHelpOptions.t option)
    ?(textDocument : textDocument option)
    ?(textDocumentSync : textDocumentSync_pvar option)
    ?(typeDefinitionProvider : typeDefinitionProvider_pvar option)
    ?(typeHierarchyProvider : typeHierarchyProvider_pvar option)
    ?(workspace : workspace option)
    ?(workspaceSymbolProvider : workspaceSymbolProvider_pvar option)
    (() : unit)
    : t
    =
    { callHierarchyProvider
    ; codeActionProvider
    ; codeLensProvider
    ; colorProvider
    ; completionProvider
    ; declarationProvider
    ; definitionProvider
    ; diagnosticProvider
    ; documentFormattingProvider
    ; documentHighlightProvider
    ; documentLinkProvider
    ; documentOnTypeFormattingProvider
    ; documentRangeFormattingProvider
    ; documentSymbolProvider
    ; executeCommandProvider
    ; experimental
    ; foldingRangeProvider
    ; hoverProvider
    ; implementationProvider
    ; inlayHintProvider
    ; inlineCompletionProvider
    ; inlineValueProvider
    ; linkedEditingRangeProvider
    ; monikerProvider
    ; notebookDocumentSync
    ; positionEncoding
    ; referencesProvider
    ; renameProvider
    ; selectionRangeProvider
    ; semanticTokensProvider
    ; signatureHelpProvider
    ; textDocument
    ; textDocumentSync
    ; typeDefinitionProvider
    ; typeHierarchyProvider
    ; workspace
    ; workspaceSymbolProvider
    }
  ;;
end

module InitializeResult = struct
  type serverInfo =
    { name : string
    ; version : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_serverInfo ~(name : string) ?(version : string option) (() : unit)
    : serverInfo
    =
    { name; version }
  ;;

  type t =
    { capabilities : ServerCapabilities.t
    ; serverInfo : serverInfo Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(capabilities : ServerCapabilities.t)
    ?(serverInfo : serverInfo option)
    (() : unit)
    : t
    =
    { capabilities; serverInfo }
  ;;
end

module InitializedParams_ = struct
  type clientInfo =
    { name : string
    ; version : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create_clientInfo ~(name : string) ?(version : string option) (() : unit)
    : clientInfo
    =
    { name; version }
  ;;

  type t =
    { capabilities : ClientCapabilities.t
    ; clientInfo : clientInfo Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; initializationOptions : Json.t option [@yojson.option]
    ; locale : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; processId : int Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; rootPath : string Json.Nullable_option.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; rootUri : DocumentUri.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; trace : TraceValues.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(capabilities : ClientCapabilities.t)
    ?(clientInfo : clientInfo option)
    ?(initializationOptions : Json.t option)
    ?(locale : string option)
    ?(processId : int option)
    ?(rootPath : string option option)
    ?(rootUri : DocumentUri.t option)
    ?(trace : TraceValues.t option)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { capabilities
    ; clientInfo
    ; initializationOptions
    ; locale
    ; processId
    ; rootPath
    ; rootUri
    ; trace
    ; workDoneToken
    }
  ;;
end

module InlayHintLabelPart = struct
  type tooltip_pvar =
    [ `String of string
    | `MarkupContent of MarkupContent.t
    ]

  let tooltip_pvar_of_yojson (json : Json.t) : tooltip_pvar =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union
        "tooltip_pvar"
        [ (fun json -> `MarkupContent (MarkupContent.t_of_yojson json)) ]
        json
  ;;

  let yojson_of_tooltip_pvar (tooltip_pvar : tooltip_pvar) : Json.t =
    match tooltip_pvar with
    | `String j -> `String j
    | `MarkupContent s -> MarkupContent.yojson_of_t s
  ;;

  type t =
    { command : Command.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; location : Location.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; tooltip : tooltip_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; value : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(command : Command.t option)
    ?(location : Location.t option)
    ?(tooltip : tooltip_pvar option)
    ~(value : string)
    (() : unit)
    : t
    =
    { command; location; tooltip; value }
  ;;
end

module InlayHint = struct
  type label_pvar =
    [ `String of string
    | `List of InlayHintLabelPart.t list
    ]

  let label_pvar_of_yojson (json : Json.t) : label_pvar =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union
        "label_pvar"
        [ (fun json -> `List (Json.Of.list InlayHintLabelPart.t_of_yojson json)) ]
        json
  ;;

  let yojson_of_label_pvar (label_pvar : label_pvar) : Json.t =
    match label_pvar with
    | `String j -> `String j
    | `List s -> Json.To.list InlayHintLabelPart.yojson_of_t s
  ;;

  type tooltip_pvar =
    [ `String of string
    | `MarkupContent of MarkupContent.t
    ]

  let tooltip_pvar_of_yojson (json : Json.t) : tooltip_pvar =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union
        "tooltip_pvar"
        [ (fun json -> `MarkupContent (MarkupContent.t_of_yojson json)) ]
        json
  ;;

  let yojson_of_tooltip_pvar (tooltip_pvar : tooltip_pvar) : Json.t =
    match tooltip_pvar with
    | `String j -> `String j
    | `MarkupContent s -> MarkupContent.yojson_of_t s
  ;;

  type t =
    { data : Json.t option [@yojson.option]
    ; kind : InlayHintKind.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; label : label_pvar
    ; paddingLeft : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; paddingRight : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; position : Position.t
    ; textEdits : TextEdit.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; tooltip : tooltip_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(data : Json.t option)
    ?(kind : InlayHintKind.t option)
    ~(label : label_pvar)
    ?(paddingLeft : bool option)
    ?(paddingRight : bool option)
    ~(position : Position.t)
    ?(textEdits : TextEdit.t list option)
    ?(tooltip : tooltip_pvar option)
    (() : unit)
    : t
    =
    { data; kind; label; paddingLeft; paddingRight; position; textEdits; tooltip }
  ;;
end

module InlayHintParams = struct
  type t =
    { range : Range.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(range : Range.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { range; textDocument; workDoneToken }
  ;;
end

module SelectedCompletionInfo = struct
  type t =
    { range : Range.t
    ; text : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(range : Range.t) ~(text : string) : t = { range; text }
end

module InlineCompletionContext = struct
  type t =
    { selectedCompletionInfo : SelectedCompletionInfo.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; triggerKind : InlineCompletionTriggerKind.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(selectedCompletionInfo : SelectedCompletionInfo.t option)
    ~(triggerKind : InlineCompletionTriggerKind.t)
    (() : unit)
    : t
    =
    { selectedCompletionInfo; triggerKind }
  ;;
end

module StringValue = struct
  type t = { value : string } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(value : string) : t = { value }
  let yojson_of_t (t : t) : Json.t = Json.To.literal_field "kind" "snippet" yojson_of_t t

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "snippet" t_of_yojson json
  ;;
end

module InlineCompletionItem = struct
  type insertText_pvar =
    [ `String of string
    | `StringValue of StringValue.t
    ]

  let insertText_pvar_of_yojson (json : Json.t) : insertText_pvar =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union
        "insertText_pvar"
        [ (fun json -> `StringValue (StringValue.t_of_yojson json)) ]
        json
  ;;

  let yojson_of_insertText_pvar (insertText_pvar : insertText_pvar) : Json.t =
    match insertText_pvar with
    | `String j -> `String j
    | `StringValue s -> StringValue.yojson_of_t s
  ;;

  type t =
    { command : Command.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; filterText : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; insertText : insertText_pvar
    ; range : Range.t Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(command : Command.t option)
    ?(filterText : string option)
    ~(insertText : insertText_pvar)
    ?(range : Range.t option)
    (() : unit)
    : t
    =
    { command; filterText; insertText; range }
  ;;
end

module InlineCompletionList = struct
  type t = { items : InlineCompletionItem.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(items : InlineCompletionItem.t list) : t = { items }
end

module InlineCompletionParams = struct
  type t =
    { context : InlineCompletionContext.t
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(context : InlineCompletionContext.t)
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { context; position; textDocument; workDoneToken }
  ;;
end

module InlineCompletionRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(id : string option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; id; workDoneProgress }
  ;;
end

module InlineValueEvaluatableExpression = struct
  type t =
    { expression : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; range : Range.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(expression : string option) ~(range : Range.t) (() : unit) : t =
    { expression; range }
  ;;
end

module InlineValueVariableLookup = struct
  type t =
    { caseSensitiveLookup : bool
    ; range : Range.t
    ; variableName : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(caseSensitiveLookup : bool)
    ~(range : Range.t)
    ?(variableName : string option)
    (() : unit)
    : t
    =
    { caseSensitiveLookup; range; variableName }
  ;;
end

module InlineValueText = struct
  type t =
    { range : Range.t
    ; text : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(range : Range.t) ~(text : string) : t = { range; text }
end

module InlineValue = struct
  type t =
    [ `InlineValueText of InlineValueText.t
    | `InlineValueVariableLookup of InlineValueVariableLookup.t
    | `InlineValueEvaluatableExpression of InlineValueEvaluatableExpression.t
    ]

  let t_of_yojson (json : Json.t) : t =
    Json.Of.untagged_union
      "t"
      [ (fun json -> `InlineValueText (InlineValueText.t_of_yojson json))
      ; (fun json ->
          `InlineValueVariableLookup (InlineValueVariableLookup.t_of_yojson json))
      ; (fun json ->
          `InlineValueEvaluatableExpression
            (InlineValueEvaluatableExpression.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_t (t : t) : Json.t =
    match t with
    | `InlineValueText s -> InlineValueText.yojson_of_t s
    | `InlineValueVariableLookup s -> InlineValueVariableLookup.yojson_of_t s
    | `InlineValueEvaluatableExpression s ->
      InlineValueEvaluatableExpression.yojson_of_t s
  ;;
end

module InlineValueContext = struct
  type t =
    { frameId : int
    ; stoppedLocation : Range.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(frameId : int) ~(stoppedLocation : Range.t) : t =
    { frameId; stoppedLocation }
  ;;
end

module InlineValueParams = struct
  type t =
    { context : InlineValueContext.t
    ; range : Range.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(context : InlineValueContext.t)
    ~(range : Range.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { context; range; textDocument; workDoneToken }
  ;;
end

module LinkedEditingRangeParams = struct
  type t =
    { position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { position; textDocument; workDoneToken }
  ;;
end

module LinkedEditingRanges = struct
  type t =
    { ranges : Range.t list
    ; wordPattern : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(ranges : Range.t list) ?(wordPattern : string option) (() : unit) : t =
    { ranges; wordPattern }
  ;;
end

module LogMessageParams = struct
  type t =
    { message : string
    ; type_ : MessageType.t [@key "type"]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(message : string) ~(type_ : MessageType.t) : t = { message; type_ }
end

module LogTraceParams = struct
  type t =
    { message : string
    ; verbose : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(message : string) ?(verbose : string option) (() : unit) : t =
    { message; verbose }
  ;;
end

module MessageActionItem = struct
  type t = { title : string } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(title : string) : t = { title }
end

module Moniker = struct
  type t =
    { identifier : string
    ; kind : MonikerKind.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; scheme : string
    ; unique : UniquenessLevel.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(identifier : string)
    ?(kind : MonikerKind.t option)
    ~(scheme : string)
    ~(unique : UniquenessLevel.t)
    (() : unit)
    : t
    =
    { identifier; kind; scheme; unique }
  ;;
end

module MonikerParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(partialResultToken : ProgressToken.t option)
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { partialResultToken; position; textDocument; workDoneToken }
  ;;
end

module ParameterInformation = struct
  type documentation_pvar =
    [ `String of string
    | `MarkupContent of MarkupContent.t
    ]

  let documentation_pvar_of_yojson (json : Json.t) : documentation_pvar =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union
        "documentation_pvar"
        [ (fun json -> `MarkupContent (MarkupContent.t_of_yojson json)) ]
        json
  ;;

  let yojson_of_documentation_pvar (documentation_pvar : documentation_pvar) : Json.t =
    match documentation_pvar with
    | `String j -> `String j
    | `MarkupContent s -> MarkupContent.yojson_of_t s
  ;;

  type label_pvar =
    [ `String of string
    | `Offset of int * int
    ]

  let label_pvar_of_yojson (json : Json.t) : label_pvar =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union
        "label_pvar"
        [ (fun json -> `Offset (Json.Of.int_pair json)) ]
        json
  ;;

  let yojson_of_label_pvar (label_pvar : label_pvar) : Json.t =
    match label_pvar with
    | `String j -> `String j
    | `Offset s -> Json.To.int_pair s
  ;;

  type t =
    { documentation : documentation_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; label : label_pvar
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentation : documentation_pvar option)
    ~(label : label_pvar)
    (() : unit)
    : t
    =
    { documentation; label }
  ;;
end

module PartialResultParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(partialResultToken : ProgressToken.t option) (() : unit) : t =
    { partialResultToken }
  ;;
end

module PrepareRenameParams = struct
  type t =
    { position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { position; textDocument; workDoneToken }
  ;;
end

module PreviousResultId = struct
  type t =
    { uri : DocumentUri.t
    ; value : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(uri : DocumentUri.t) ~(value : string) : t = { uri; value }
end

module PublishDiagnosticsParams = struct
  type t =
    { diagnostics : Diagnostic.t list
    ; uri : DocumentUri.t
    ; version : int Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(diagnostics : Diagnostic.t list)
    ~(uri : DocumentUri.t)
    ?(version : int option)
    (() : unit)
    : t
    =
    { diagnostics; uri; version }
  ;;
end

module ReferenceContext = struct
  type t = { includeDeclaration : bool } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(includeDeclaration : bool) : t = { includeDeclaration }
end

module ReferenceParams = struct
  type t =
    { context : ReferenceContext.t
    ; partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(context : ReferenceContext.t)
    ?(partialResultToken : ProgressToken.t option)
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { context; partialResultToken; position; textDocument; workDoneToken }
  ;;
end

module ReferenceRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; workDoneProgress }
  ;;
end

module Registration = struct
  type t =
    { id : string
    ; method_ : string [@key "method"]
    ; registerOptions : Json.t option [@yojson.option]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(id : string)
    ~(method_ : string)
    ?(registerOptions : Json.t option)
    (() : unit)
    : t
    =
    { id; method_; registerOptions }
  ;;
end

module RegistrationParams = struct
  type t = { registrations : Registration.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(registrations : Registration.t list) : t = { registrations }
end

module RenameFilesParams = struct
  type t = { files : FileRename.t list } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(files : FileRename.t list) : t = { files }
end

module RenameParams = struct
  type t =
    { newName : string
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(newName : string)
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { newName; position; textDocument; workDoneToken }
  ;;
end

module RenameRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; prepareProvider : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(prepareProvider : bool option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; prepareProvider; workDoneProgress }
  ;;
end

module ResourceOperation = struct
  type t =
    { annotationId : ChangeAnnotationIdentifier.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; kind : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(annotationId : ChangeAnnotationIdentifier.t option)
    ~(kind : string)
    (() : unit)
    : t
    =
    { annotationId; kind }
  ;;
end

module SelectionRange = struct
  type t =
    { parent : t Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; range : Range.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(parent : t option) ~(range : Range.t) (() : unit) : t = { parent; range }
end

module SelectionRangeParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; positions : Position.t list
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(partialResultToken : ProgressToken.t option)
    ~(positions : Position.t list)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { partialResultToken; positions; textDocument; workDoneToken }
  ;;
end

module SemanticTokens = struct
  type t =
    { data : int array
    ; resultId : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(data : int array) ?(resultId : string option) (() : unit) : t =
    { data; resultId }
  ;;
end

module SemanticTokensEdit = struct
  type t =
    { data : int array Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; deleteCount : int
    ; start : int
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(data : int array option) ~(deleteCount : int) ~(start : int) (() : unit)
    : t
    =
    { data; deleteCount; start }
  ;;
end

module SemanticTokensDelta = struct
  type t =
    { edits : SemanticTokensEdit.t list
    ; resultId : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(edits : SemanticTokensEdit.t list) ?(resultId : string option) (() : unit)
    : t
    =
    { edits; resultId }
  ;;
end

module SemanticTokensDeltaParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; previousResultId : string
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(partialResultToken : ProgressToken.t option)
    ~(previousResultId : string)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { partialResultToken; previousResultId; textDocument; workDoneToken }
  ;;
end

module SemanticTokensDeltaPartialResult = struct
  type t = { edits : SemanticTokensEdit.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(edits : SemanticTokensEdit.t list) : t = { edits }
end

module SemanticTokensParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(partialResultToken : ProgressToken.t option)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { partialResultToken; textDocument; workDoneToken }
  ;;
end

module SemanticTokensPartialResult = struct
  type t = { data : int array } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(data : int array) : t = { data }
end

module SemanticTokensRangeParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; range : Range.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(partialResultToken : ProgressToken.t option)
    ~(range : Range.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { partialResultToken; range; textDocument; workDoneToken }
  ;;
end

module SetTraceParams = struct
  type t = { value : TraceValues.t } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(value : TraceValues.t) : t = { value }
end

module ShowDocumentParams = struct
  type t =
    { external_ : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )] [@key "external"]
    ; selection : Range.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; takeFocus : bool Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; uri : DocumentUri.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(external_ : bool option)
    ?(selection : Range.t option)
    ?(takeFocus : bool option)
    ~(uri : DocumentUri.t)
    (() : unit)
    : t
    =
    { external_; selection; takeFocus; uri }
  ;;
end

module ShowDocumentResult = struct
  type t = { success : bool } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(success : bool) : t = { success }
end

module ShowMessageParams = struct
  type t =
    { message : string
    ; type_ : MessageType.t [@key "type"]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(message : string) ~(type_ : MessageType.t) : t = { message; type_ }
end

module ShowMessageRequestParams = struct
  type t =
    { actions : MessageActionItem.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; message : string
    ; type_ : MessageType.t [@key "type"]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(actions : MessageActionItem.t list option)
    ~(message : string)
    ~(type_ : MessageType.t)
    (() : unit)
    : t
    =
    { actions; message; type_ }
  ;;
end

module SignatureInformation = struct
  type documentation_pvar =
    [ `String of string
    | `MarkupContent of MarkupContent.t
    ]

  let documentation_pvar_of_yojson (json : Json.t) : documentation_pvar =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union
        "documentation_pvar"
        [ (fun json -> `MarkupContent (MarkupContent.t_of_yojson json)) ]
        json
  ;;

  let yojson_of_documentation_pvar (documentation_pvar : documentation_pvar) : Json.t =
    match documentation_pvar with
    | `String j -> `String j
    | `MarkupContent s -> MarkupContent.yojson_of_t s
  ;;

  type t =
    { activeParameter : int Json.Nullable_option.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; documentation : documentation_pvar Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; label : string
    ; parameters : ParameterInformation.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(activeParameter : int option option)
    ?(documentation : documentation_pvar option)
    ~(label : string)
    ?(parameters : ParameterInformation.t list option)
    (() : unit)
    : t
    =
    { activeParameter; documentation; label; parameters }
  ;;
end

module SignatureHelp = struct
  type t =
    { activeParameter : int Json.Nullable_option.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; activeSignature : int Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; signatures : SignatureInformation.t list
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(activeParameter : int option option)
    ?(activeSignature : int option)
    ~(signatures : SignatureInformation.t list)
    (() : unit)
    : t
    =
    { activeParameter; activeSignature; signatures }
  ;;
end

module SignatureHelpContext = struct
  type t =
    { activeSignatureHelp : SignatureHelp.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; isRetrigger : bool
    ; triggerCharacter : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; triggerKind : SignatureHelpTriggerKind.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(activeSignatureHelp : SignatureHelp.t option)
    ~(isRetrigger : bool)
    ?(triggerCharacter : string option)
    ~(triggerKind : SignatureHelpTriggerKind.t)
    (() : unit)
    : t
    =
    { activeSignatureHelp; isRetrigger; triggerCharacter; triggerKind }
  ;;
end

module SignatureHelpParams = struct
  type t =
    { context : SignatureHelpContext.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(context : SignatureHelpContext.t option)
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { context; position; textDocument; workDoneToken }
  ;;
end

module SignatureHelpRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; retriggerCharacters : string list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; triggerCharacters : string list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(retriggerCharacters : string list option)
    ?(triggerCharacters : string list option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { documentSelector; retriggerCharacters; triggerCharacters; workDoneProgress }
  ;;
end

module StaticRegistrationOptions = struct
  type t =
    { id : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(id : string option) (() : unit) : t = { id }
end

module SymbolInformation = struct
  type t =
    { containerName : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; deprecated : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; kind : SymbolKind.t
    ; location : Location.t
    ; name : string
    ; tags : SymbolTag.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(containerName : string option)
    ?(deprecated : bool option)
    ~(kind : SymbolKind.t)
    ~(location : Location.t)
    ~(name : string)
    ?(tags : SymbolTag.t list option)
    (() : unit)
    : t
    =
    { containerName; deprecated; kind; location; name; tags }
  ;;
end

module TextDocumentChangeRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; syncKind : TextDocumentSyncKind.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ~(syncKind : TextDocumentSyncKind.t)
    (() : unit)
    : t
    =
    { documentSelector; syncKind }
  ;;
end

module TextDocumentPositionParams = struct
  type t =
    { position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(position : Position.t) ~(textDocument : TextDocumentIdentifier.t) : t =
    { position; textDocument }
  ;;
end

module TextDocumentRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(documentSelector : DocumentSelector.t option) (() : unit) : t =
    { documentSelector }
  ;;
end

module TextDocumentSaveRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; includeText : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(documentSelector : DocumentSelector.t option)
    ?(includeText : bool option)
    (() : unit)
    : t
    =
    { documentSelector; includeText }
  ;;
end

module TypeDefinitionParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(partialResultToken : ProgressToken.t option)
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { partialResultToken; position; textDocument; workDoneToken }
  ;;
end

module TypeHierarchyItem = struct
  type t =
    { data : Json.t option [@yojson.option]
    ; detail : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; kind : SymbolKind.t
    ; name : string
    ; range : Range.t
    ; selectionRange : Range.t
    ; tags : SymbolTag.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; uri : DocumentUri.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(data : Json.t option)
    ?(detail : string option)
    ~(kind : SymbolKind.t)
    ~(name : string)
    ~(range : Range.t)
    ~(selectionRange : Range.t)
    ?(tags : SymbolTag.t list option)
    ~(uri : DocumentUri.t)
    (() : unit)
    : t
    =
    { data; detail; kind; name; range; selectionRange; tags; uri }
  ;;
end

module TypeHierarchyPrepareParams = struct
  type t =
    { position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(position : Position.t)
    ~(textDocument : TextDocumentIdentifier.t)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { position; textDocument; workDoneToken }
  ;;
end

module TypeHierarchySubtypesParams = struct
  type t =
    { item : TypeHierarchyItem.t
    ; partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(item : TypeHierarchyItem.t)
    ?(partialResultToken : ProgressToken.t option)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { item; partialResultToken; workDoneToken }
  ;;
end

module TypeHierarchySupertypesParams = struct
  type t =
    { item : TypeHierarchyItem.t
    ; partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(item : TypeHierarchyItem.t)
    ?(partialResultToken : ProgressToken.t option)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { item; partialResultToken; workDoneToken }
  ;;
end

module Unregistration = struct
  type t =
    { id : string
    ; method_ : string [@key "method"]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(id : string) ~(method_ : string) : t = { id; method_ }
end

module UnregistrationParams = struct
  type t = { unregisterations : Unregistration.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(unregisterations : Unregistration.t list) : t = { unregisterations }
end

module WillSaveTextDocumentParams = struct
  type t =
    { reason : TextDocumentSaveReason.t
    ; textDocument : TextDocumentIdentifier.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(reason : TextDocumentSaveReason.t)
    ~(textDocument : TextDocumentIdentifier.t)
    : t
    =
    { reason; textDocument }
  ;;
end

module WorkDoneProgressBegin = struct
  type t =
    { cancellable : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; message : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; percentage : int Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; title : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(cancellable : bool option)
    ?(message : string option)
    ?(percentage : int option)
    ~(title : string)
    (() : unit)
    : t
    =
    { cancellable; message; percentage; title }
  ;;

  let yojson_of_t (t : t) : Json.t = Json.To.literal_field "kind" "begin" yojson_of_t t

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "begin" t_of_yojson json
  ;;
end

module WorkDoneProgressCancelParams = struct
  type t = { token : ProgressToken.t } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(token : ProgressToken.t) : t = { token }
end

module WorkDoneProgressCreateParams = struct
  type t = { token : ProgressToken.t } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(token : ProgressToken.t) : t = { token }
end

module WorkDoneProgressEnd = struct
  type t =
    { message : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(message : string option) (() : unit) : t = { message }
  let yojson_of_t (t : t) : Json.t = Json.To.literal_field "kind" "end" yojson_of_t t

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "end" t_of_yojson json
  ;;
end

module WorkDoneProgressOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneProgress : bool option) (() : unit) : t = { workDoneProgress }
end

module WorkDoneProgressParams = struct
  type t =
    { workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workDoneToken : ProgressToken.t option) (() : unit) : t = { workDoneToken }
end

module WorkDoneProgressReport = struct
  type t =
    { cancellable : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; message : string Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    ; percentage : int Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(cancellable : bool option)
    ?(message : string option)
    ?(percentage : int option)
    (() : unit)
    : t
    =
    { cancellable; message; percentage }
  ;;

  let yojson_of_t (t : t) : Json.t = Json.To.literal_field "kind" "report" yojson_of_t t

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "report" t_of_yojson json
  ;;
end

module WorkspaceDiagnosticParams = struct
  type t =
    { identifier : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; previousResultIds : PreviousResultId.t list
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(identifier : string option)
    ?(partialResultToken : ProgressToken.t option)
    ~(previousResultIds : PreviousResultId.t list)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { identifier; partialResultToken; previousResultIds; workDoneToken }
  ;;
end

module WorkspaceUnchangedDocumentDiagnosticReport = struct
  type t =
    { resultId : string
    ; uri : DocumentUri.t
    ; version : int Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(resultId : string)
    ~(uri : DocumentUri.t)
    ?(version : int option)
    (() : unit)
    : t
    =
    { resultId; uri; version }
  ;;

  let yojson_of_t (t : t) : Json.t =
    Json.To.literal_field "kind" "unchanged" yojson_of_t t
  ;;

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "unchanged" t_of_yojson json
  ;;
end

module WorkspaceFullDocumentDiagnosticReport = struct
  type t =
    { items : Diagnostic.t list
    ; resultId : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; uri : DocumentUri.t
    ; version : int Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ~(items : Diagnostic.t list)
    ?(resultId : string option)
    ~(uri : DocumentUri.t)
    ?(version : int option)
    (() : unit)
    : t
    =
    { items; resultId; uri; version }
  ;;

  let yojson_of_t (t : t) : Json.t = Json.To.literal_field "kind" "full" yojson_of_t t

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "full" t_of_yojson json
  ;;
end

module WorkspaceDocumentDiagnosticReport = struct
  type t =
    [ `WorkspaceFullDocumentDiagnosticReport of WorkspaceFullDocumentDiagnosticReport.t
    | `WorkspaceUnchangedDocumentDiagnosticReport of
      WorkspaceUnchangedDocumentDiagnosticReport.t
    ]

  let t_of_yojson (json : Json.t) : t =
    Json.Of.untagged_union
      "t"
      [ (fun json ->
          `WorkspaceFullDocumentDiagnosticReport
            (WorkspaceFullDocumentDiagnosticReport.t_of_yojson json))
      ; (fun json ->
          `WorkspaceUnchangedDocumentDiagnosticReport
            (WorkspaceUnchangedDocumentDiagnosticReport.t_of_yojson json))
      ]
      json
  ;;

  let yojson_of_t (t : t) : Json.t =
    match t with
    | `WorkspaceFullDocumentDiagnosticReport s ->
      WorkspaceFullDocumentDiagnosticReport.yojson_of_t s
    | `WorkspaceUnchangedDocumentDiagnosticReport s ->
      WorkspaceUnchangedDocumentDiagnosticReport.yojson_of_t s
  ;;
end

module WorkspaceDiagnosticReport = struct
  type t = { items : WorkspaceDocumentDiagnosticReport.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(items : WorkspaceDocumentDiagnosticReport.t list) : t = { items }
end

module WorkspaceDiagnosticReportPartialResult = struct
  type t = { items : WorkspaceDocumentDiagnosticReport.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ~(items : WorkspaceDocumentDiagnosticReport.t list) : t = { items }
end

module WorkspaceFoldersInitializeParams = struct
  type t =
    { workspaceFolders :
        WorkspaceFolder.t list Json.Nullable_option.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?(workspaceFolders : WorkspaceFolder.t list option option) (() : unit) : t =
    { workspaceFolders }
  ;;
end

module WorkspaceSymbol = struct
  type t =
    { containerName : string Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; data : Json.t option [@yojson.option]
    ; kind : SymbolKind.t
    ; location : Location.t
    ; name : string
    ; tags : SymbolTag.t list Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(containerName : string option)
    ?(data : Json.t option)
    ~(kind : SymbolKind.t)
    ~(location : Location.t)
    ~(name : string)
    ?(tags : SymbolTag.t list option)
    (() : unit)
    : t
    =
    { containerName; data; kind; location; name; tags }
  ;;
end

module WorkspaceSymbolParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; query : string
    ; workDoneToken : ProgressToken.t Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(partialResultToken : ProgressToken.t option)
    ~(query : string)
    ?(workDoneToken : ProgressToken.t option)
    (() : unit)
    : t
    =
    { partialResultToken; query; workDoneToken }
  ;;
end

module WorkspaceSymbolRegistrationOptions = struct
  type t =
    { resolveProvider : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
         [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create
    ?(resolveProvider : bool option)
    ?(workDoneProgress : bool option)
    (() : unit)
    : t
    =
    { resolveProvider; workDoneProgress }
  ;;
end
(*$*)

module CodeActionResult = struct
  type t = [ `Command of Command.t | `CodeAction of CodeAction.t ] list option

  let yojson_of_t (t : t) : Json.t =
    match t with
    | None -> `Null
    | Some xs ->
      Json.To.list
        (function
          | `Command c -> Command.yojson_of_t c
          | `CodeAction a -> CodeAction.yojson_of_t a)
        xs
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Null -> None
    | `List _ ->
      Some
        (Json.Of.list
           (Json.Of.untagged_union
              "CodeActionResult"
              [ (fun j -> `Command (Command.t_of_yojson j))
              ; (fun j -> `CodeAction (CodeAction.t_of_yojson j))
              ])
           json)
    | _ -> Json.error "CodeActionResult" json
  ;;
end

module Locations = struct
  type t =
    [ `Location of Location.t list
    | `LocationLink of LocationLink.t list
    ]

  let yojson_of_t (t : t) : Json.t =
    match t with
    | `Location xs -> `List (List.map ~f:Location.yojson_of_t xs)
    | `LocationLink l -> `List (List.map ~f:LocationLink.yojson_of_t l)
  ;;

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Assoc _ -> `Location [ Location.t_of_yojson json ]
    | `List [] -> `Location []
    | `List (x :: xs) ->
      (match Location.t_of_yojson x with
       | loc -> `Location (loc :: List.map ~f:Location.t_of_yojson xs)
       | exception Of_yojson_error (_, _) ->
         `LocationLink (List.map ~f:LocationLink.t_of_yojson (x :: xs)))
    | _ -> Json.error "Locations.t" json
  ;;
end

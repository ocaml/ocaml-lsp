open! Import

module MarkedString : sig
  type t =
    { value : string
    ; language : string option
    }

  include Json.Jsonable.S with type t := t
end

module DocumentUri : module type of Uri0 with type t = Uri0.t

module ProgressToken : sig
  type t =
    [ `Int of int
    | `String of string
    ]

  include Json.Jsonable.S with type t := t
end

module ProgressParams : sig
  type 'a t =
    { token : ProgressToken.t
    ; value : 'a
    }

  val create : token:ProgressToken.t -> value:'a -> 'a t

  include Json.Jsonable.S1 with type 'a t := 'a t
end

module NotebookDocumentSyncOptions : sig
  type t = unit

  include Json.Jsonable.S with type t := t
end

module NotebookDocumentSyncRegistrationOptions : sig
  type t = unit

  include Json.Jsonable.S with type t := t
end

module NotebookDocumentFilter : sig
  type t = unit

  include Json.Jsonable.S with type t := t
end

module TextDocumentFilter : sig
  type t =
    { language : string option
    ; scheme : string option
    ; pattern : string option
    }

  val create : ?language:string -> ?scheme:string -> ?pattern:string -> unit -> t
end

(*$ Lsp_gen.print_mli () *)
module SymbolTag : sig
  type t = Deprecated

  include Json.Jsonable.S with type t := t
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

  include Json.Jsonable.S with type t := t
end

module ResourceOperationKind : sig
  type t =
    | Create
    | Rename
    | Delete

  include Json.Jsonable.S with type t := t
end

module FailureHandlingKind : sig
  type t =
    | Abort
    | Transactional
    | TextOnlyTransactional
    | Undo

  include Json.Jsonable.S with type t := t
end

module MarkupKind : sig
  type t =
    | PlainText
    | Markdown

  include Json.Jsonable.S with type t := t
end

module TokenFormat : sig
  type t = Relative

  include Json.Jsonable.S with type t := t
end

module PrepareSupportDefaultBehavior : sig
  type t = Identifier

  include Json.Jsonable.S with type t := t
end

module DiagnosticTag : sig
  type t =
    | Unnecessary
    | Deprecated

  include Json.Jsonable.S with type t := t
end

module FoldingRangeKind : sig
  type t =
    | Comment
    | Imports
    | Region
    | Other of string

  include Json.Jsonable.S with type t := t
end

module InsertTextMode : sig
  type t =
    | AsIs
    | AdjustIndentation

  include Json.Jsonable.S with type t := t
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

  include Json.Jsonable.S with type t := t
end

module CompletionItemTag : sig
  type t = Deprecated

  include Json.Jsonable.S with type t := t
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
    | SourceFixAll
    | Other of string

  include Json.Jsonable.S with type t := t
end

module PositionEncodingKind : sig
  type t =
    | UTF8
    | UTF16
    | UTF32
    | Other of string

  include Json.Jsonable.S with type t := t
end

module DiagnosticSeverity : sig
  type t =
    | Error
    | Warning
    | Information
    | Hint

  include Json.Jsonable.S with type t := t
end

module CodeActionTriggerKind : sig
  type t =
    | Invoked
    | Automatic

  include Json.Jsonable.S with type t := t
end

module CompletionTriggerKind : sig
  type t =
    | Invoked
    | TriggerCharacter
    | TriggerForIncompleteCompletions

  include Json.Jsonable.S with type t := t
end

module InsertTextFormat : sig
  type t =
    | PlainText
    | Snippet

  include Json.Jsonable.S with type t := t
end

module NotebookCellKind : sig
  type t =
    | Markup
    | Code

  include Json.Jsonable.S with type t := t
end

module FileChangeType : sig
  type t =
    | Created
    | Changed
    | Deleted

  include Json.Jsonable.S with type t := t
end

module WatchKind : sig
  type t =
    | Create
    | Change
    | Delete
    | Other of string

  include Json.Jsonable.S with type t := t
end

module DocumentDiagnosticReportKind : sig
  type t =
    | Full
    | Unchanged

  include Json.Jsonable.S with type t := t
end

module DocumentHighlightKind : sig
  type t =
    | Text
    | Read
    | Write

  include Json.Jsonable.S with type t := t
end

module FileOperationPatternKind : sig
  type t =
    | File
    | Folder

  include Json.Jsonable.S with type t := t
end

module TraceValues : sig
  type t =
    | Compact
    | Off
    | Messages
    | Verbose

  include Json.Jsonable.S with type t := t
end

module TextDocumentSyncKind : sig
  type t =
    | None
    | Full
    | Incremental

  include Json.Jsonable.S with type t := t
end

module InlayHintKind : sig
  type t =
    | Type
    | Parameter

  include Json.Jsonable.S with type t := t
end

module InlineCompletionTriggerKind : sig
  type t =
    | Invoked
    | Automatic

  include Json.Jsonable.S with type t := t
end

module MessageType : sig
  type t =
    | Error
    | Warning
    | Info
    | Log
    | Debug

  include Json.Jsonable.S with type t := t
end

module UniquenessLevel : sig
  type t =
    | Document
    | Project
    | Group
    | Scheme
    | Global

  include Json.Jsonable.S with type t := t
end

module MonikerKind : sig
  type t =
    | Import
    | Export
    | Local

  include Json.Jsonable.S with type t := t
end

module SemanticTokenModifiers : sig
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

  include Json.Jsonable.S with type t := t
end

module SemanticTokenTypes : sig
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

  include Json.Jsonable.S with type t := t
end

module SignatureHelpTriggerKind : sig
  type t =
    | Invoked
    | TriggerCharacter
    | ContentChange

  include Json.Jsonable.S with type t := t
end

module TextDocumentSaveReason : sig
  type t =
    | Manual
    | AfterDelay
    | FocusOut

  include Json.Jsonable.S with type t := t
end

module Position : sig
  type t =
    { character : int
    ; line : int
    }

  val create : character:int -> line:int -> t

  include Json.Jsonable.S with type t := t
end

module Range : sig
  type t =
    { end_ : Position.t
    ; start : Position.t
    }

  val create : end_:Position.t -> start:Position.t -> t

  include Json.Jsonable.S with type t := t
end

module ChangeAnnotationIdentifier : sig
  type t = string

  include Json.Jsonable.S with type t := t
end

module AnnotatedTextEdit : sig
  type t =
    { annotationId : ChangeAnnotationIdentifier.t
    ; newText : string
    ; range : Range.t
    }

  val create
    :  annotationId:ChangeAnnotationIdentifier.t
    -> newText:string
    -> range:Range.t
    -> t

  include Json.Jsonable.S with type t := t
end

module DeleteFileOptions : sig
  type t =
    { ignoreIfNotExists : bool option
    ; recursive : bool option
    }

  val create : ?ignoreIfNotExists:bool -> ?recursive:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DeleteFile : sig
  type t =
    { annotationId : ChangeAnnotationIdentifier.t option
    ; options : DeleteFileOptions.t option
    ; uri : DocumentUri.t
    }

  val create
    :  ?annotationId:ChangeAnnotationIdentifier.t
    -> ?options:DeleteFileOptions.t
    -> uri:DocumentUri.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module RenameFileOptions : sig
  type t =
    { ignoreIfExists : bool option
    ; overwrite : bool option
    }

  val create : ?ignoreIfExists:bool -> ?overwrite:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module RenameFile : sig
  type t =
    { annotationId : ChangeAnnotationIdentifier.t option
    ; newUri : DocumentUri.t
    ; oldUri : DocumentUri.t
    ; options : RenameFileOptions.t option
    }

  val create
    :  ?annotationId:ChangeAnnotationIdentifier.t
    -> newUri:DocumentUri.t
    -> oldUri:DocumentUri.t
    -> ?options:RenameFileOptions.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module CreateFileOptions : sig
  type t =
    { ignoreIfExists : bool option
    ; overwrite : bool option
    }

  val create : ?ignoreIfExists:bool -> ?overwrite:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module CreateFile : sig
  type t =
    { annotationId : ChangeAnnotationIdentifier.t option
    ; options : CreateFileOptions.t option
    ; uri : DocumentUri.t
    }

  val create
    :  ?annotationId:ChangeAnnotationIdentifier.t
    -> ?options:CreateFileOptions.t
    -> uri:DocumentUri.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module OptionalVersionedTextDocumentIdentifier : sig
  type t =
    { uri : DocumentUri.t
    ; version : int option
    }

  val create : uri:DocumentUri.t -> ?version:int -> unit -> t

  include Json.Jsonable.S with type t := t
end

module TextEdit : sig
  type t =
    { newText : string
    ; range : Range.t
    }

  val create : newText:string -> range:Range.t -> t

  include Json.Jsonable.S with type t := t
end

module TextDocumentEdit : sig
  type t =
    { edits : [ `TextEdit of TextEdit.t | `AnnotatedTextEdit of AnnotatedTextEdit.t ] list
    ; textDocument : OptionalVersionedTextDocumentIdentifier.t
    }

  val create
    :  edits:[ `TextEdit of TextEdit.t | `AnnotatedTextEdit of AnnotatedTextEdit.t ] list
    -> textDocument:OptionalVersionedTextDocumentIdentifier.t
    -> t

  include Json.Jsonable.S with type t := t
end

module ChangeAnnotation : sig
  type t =
    { description : string option
    ; label : string
    ; needsConfirmation : bool option
    }

  val create : ?description:string -> label:string -> ?needsConfirmation:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module WorkspaceEdit : sig
  type t =
    { changeAnnotations :
        (ChangeAnnotationIdentifier.t, ChangeAnnotation.t) Json.Assoc.t option
    ; changes : (DocumentUri.t, TextEdit.t list) Json.Assoc.t option
    ; documentChanges :
        [ `TextDocumentEdit of TextDocumentEdit.t
        | `CreateFile of CreateFile.t
        | `RenameFile of RenameFile.t
        | `DeleteFile of DeleteFile.t
        ]
          list
          option
    }

  val create
    :  ?changeAnnotations:(ChangeAnnotationIdentifier.t, ChangeAnnotation.t) Json.Assoc.t
    -> ?changes:(DocumentUri.t, TextEdit.t list) Json.Assoc.t
    -> ?documentChanges:
         [ `TextDocumentEdit of TextDocumentEdit.t
         | `CreateFile of CreateFile.t
         | `RenameFile of RenameFile.t
         | `DeleteFile of DeleteFile.t
         ]
           list
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module ApplyWorkspaceEditParams : sig
  type t =
    { edit : WorkspaceEdit.t
    ; label : string option
    }

  val create : edit:WorkspaceEdit.t -> ?label:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module ApplyWorkspaceEditResult : sig
  type t =
    { applied : bool
    ; failedChange : int option
    ; failureReason : string option
    }

  val create : applied:bool -> ?failedChange:int -> ?failureReason:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module BaseSymbolInformation : sig
  type t =
    { containerName : string option
    ; kind : SymbolKind.t
    ; name : string
    ; tags : SymbolTag.t list option
    }

  val create
    :  ?containerName:string
    -> kind:SymbolKind.t
    -> name:string
    -> ?tags:SymbolTag.t list
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module CallHierarchyClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  val create : ?dynamicRegistration:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module CallHierarchyItem : sig
  type t =
    { data : Json.t option
    ; detail : string option
    ; kind : SymbolKind.t
    ; name : string
    ; range : Range.t
    ; selectionRange : Range.t
    ; tags : SymbolTag.t list option
    ; uri : DocumentUri.t
    }

  val create
    :  ?data:Json.t
    -> ?detail:string
    -> kind:SymbolKind.t
    -> name:string
    -> range:Range.t
    -> selectionRange:Range.t
    -> ?tags:SymbolTag.t list
    -> uri:DocumentUri.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module CallHierarchyIncomingCall : sig
  type t =
    { from : CallHierarchyItem.t
    ; fromRanges : Range.t list
    }

  val create : from:CallHierarchyItem.t -> fromRanges:Range.t list -> t

  include Json.Jsonable.S with type t := t
end

module CallHierarchyIncomingCallsParams : sig
  type t =
    { item : CallHierarchyItem.t
    ; partialResultToken : ProgressToken.t option
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  item:CallHierarchyItem.t
    -> ?partialResultToken:ProgressToken.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module CallHierarchyOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module CallHierarchyOutgoingCall : sig
  type t =
    { fromRanges : Range.t list
    ; to_ : CallHierarchyItem.t
    }

  val create : fromRanges:Range.t list -> to_:CallHierarchyItem.t -> t

  include Json.Jsonable.S with type t := t
end

module CallHierarchyOutgoingCallsParams : sig
  type t =
    { item : CallHierarchyItem.t
    ; partialResultToken : ProgressToken.t option
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  item:CallHierarchyItem.t
    -> ?partialResultToken:ProgressToken.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module TextDocumentIdentifier : sig
  type t = { uri : DocumentUri.t }

  val create : uri:DocumentUri.t -> t

  include Json.Jsonable.S with type t := t
end

module CallHierarchyPrepareParams : sig
  type t =
    { position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module NotebookCellTextDocumentFilter : sig
  type t =
    { language : string option
    ; notebook :
        [ `String of string | `NotebookDocumentFilter of NotebookDocumentFilter.t ]
    }

  val create
    :  ?language:string
    -> notebook:
         [ `String of string | `NotebookDocumentFilter of NotebookDocumentFilter.t ]
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentFilter : sig
  type t =
    [ `TextDocumentFilter of TextDocumentFilter.t
    | `NotebookCellTextDocumentFilter of NotebookCellTextDocumentFilter.t
    ]

  include Json.Jsonable.S with type t := t
end

module DocumentSelector : sig
  type t = DocumentFilter.t list

  include Json.Jsonable.S with type t := t
end

module CallHierarchyRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; id : string option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?id:string
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module CancelParams : sig
  type t = { id : Jsonrpc.Id.t }

  val create : id:Jsonrpc.Id.t -> t

  include Json.Jsonable.S with type t := t
end

module WorkspaceEditClientCapabilities : sig
  type changeAnnotationSupport = { groupsOnLabel : bool option }

  val create_changeAnnotationSupport
    :  ?groupsOnLabel:bool
    -> unit
    -> changeAnnotationSupport

  type t =
    { changeAnnotationSupport : changeAnnotationSupport option
    ; documentChanges : bool option
    ; failureHandling : FailureHandlingKind.t option
    ; normalizesLineEndings : bool option
    ; resourceOperations : ResourceOperationKind.t list option
    }

  val create
    :  ?changeAnnotationSupport:changeAnnotationSupport
    -> ?documentChanges:bool
    -> ?failureHandling:FailureHandlingKind.t
    -> ?normalizesLineEndings:bool
    -> ?resourceOperations:ResourceOperationKind.t list
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module WorkspaceSymbolClientCapabilities : sig
  type tagSupport = { valueSet : SymbolTag.t list }

  val create_tagSupport : valueSet:SymbolTag.t list -> tagSupport

  type symbolKind = { valueSet : SymbolKind.t list option }

  val create_symbolKind : ?valueSet:SymbolKind.t list -> unit -> symbolKind

  type resolveSupport = { properties : string list }

  val create_resolveSupport : properties:string list -> resolveSupport

  type t =
    { dynamicRegistration : bool option
    ; resolveSupport : resolveSupport option
    ; symbolKind : symbolKind option
    ; tagSupport : tagSupport option
    }

  val create
    :  ?dynamicRegistration:bool
    -> ?resolveSupport:resolveSupport
    -> ?symbolKind:symbolKind
    -> ?tagSupport:tagSupport
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SemanticTokensWorkspaceClientCapabilities : sig
  type t = { refreshSupport : bool option }

  val create : ?refreshSupport:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module InlineValueWorkspaceClientCapabilities : sig
  type t = { refreshSupport : bool option }

  val create : ?refreshSupport:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module InlayHintWorkspaceClientCapabilities : sig
  type t = { refreshSupport : bool option }

  val create : ?refreshSupport:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module FoldingRangeWorkspaceClientCapabilities : sig
  type t = { refreshSupport : bool option }

  val create : ?refreshSupport:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module FileOperationClientCapabilities : sig
  type t =
    { didCreate : bool option
    ; didDelete : bool option
    ; didRename : bool option
    ; dynamicRegistration : bool option
    ; willCreate : bool option
    ; willDelete : bool option
    ; willRename : bool option
    }

  val create
    :  ?didCreate:bool
    -> ?didDelete:bool
    -> ?didRename:bool
    -> ?dynamicRegistration:bool
    -> ?willCreate:bool
    -> ?willDelete:bool
    -> ?willRename:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module ExecuteCommandClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  val create : ?dynamicRegistration:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DidChangeWatchedFilesClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; relativePatternSupport : bool option
    }

  val create : ?dynamicRegistration:bool -> ?relativePatternSupport:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DidChangeConfigurationClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  val create : ?dynamicRegistration:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DiagnosticWorkspaceClientCapabilities : sig
  type t = { refreshSupport : bool option }

  val create : ?refreshSupport:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module CodeLensWorkspaceClientCapabilities : sig
  type t = { refreshSupport : bool option }

  val create : ?refreshSupport:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module WorkspaceClientCapabilities : sig
  type t =
    { applyEdit : bool option
    ; codeLens : CodeLensWorkspaceClientCapabilities.t option
    ; configuration : bool option
    ; diagnostics : DiagnosticWorkspaceClientCapabilities.t option
    ; didChangeConfiguration : DidChangeConfigurationClientCapabilities.t option
    ; didChangeWatchedFiles : DidChangeWatchedFilesClientCapabilities.t option
    ; executeCommand : ExecuteCommandClientCapabilities.t option
    ; fileOperations : FileOperationClientCapabilities.t option
    ; foldingRange : FoldingRangeWorkspaceClientCapabilities.t option
    ; inlayHint : InlayHintWorkspaceClientCapabilities.t option
    ; inlineValue : InlineValueWorkspaceClientCapabilities.t option
    ; semanticTokens : SemanticTokensWorkspaceClientCapabilities.t option
    ; symbol : WorkspaceSymbolClientCapabilities.t option
    ; workspaceEdit : WorkspaceEditClientCapabilities.t option
    ; workspaceFolders : bool option
    }

  val create
    :  ?applyEdit:bool
    -> ?codeLens:CodeLensWorkspaceClientCapabilities.t
    -> ?configuration:bool
    -> ?diagnostics:DiagnosticWorkspaceClientCapabilities.t
    -> ?didChangeConfiguration:DidChangeConfigurationClientCapabilities.t
    -> ?didChangeWatchedFiles:DidChangeWatchedFilesClientCapabilities.t
    -> ?executeCommand:ExecuteCommandClientCapabilities.t
    -> ?fileOperations:FileOperationClientCapabilities.t
    -> ?foldingRange:FoldingRangeWorkspaceClientCapabilities.t
    -> ?inlayHint:InlayHintWorkspaceClientCapabilities.t
    -> ?inlineValue:InlineValueWorkspaceClientCapabilities.t
    -> ?semanticTokens:SemanticTokensWorkspaceClientCapabilities.t
    -> ?symbol:WorkspaceSymbolClientCapabilities.t
    -> ?workspaceEdit:WorkspaceEditClientCapabilities.t
    -> ?workspaceFolders:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module ShowMessageRequestClientCapabilities : sig
  type messageActionItem = { additionalPropertiesSupport : bool option }

  val create_messageActionItem
    :  ?additionalPropertiesSupport:bool
    -> unit
    -> messageActionItem

  type t = { messageActionItem : messageActionItem option }

  val create : ?messageActionItem:messageActionItem -> unit -> t

  include Json.Jsonable.S with type t := t
end

module ShowDocumentClientCapabilities : sig
  type t = { support : bool }

  val create : support:bool -> t

  include Json.Jsonable.S with type t := t
end

module WindowClientCapabilities : sig
  type t =
    { showDocument : ShowDocumentClientCapabilities.t option
    ; showMessage : ShowMessageRequestClientCapabilities.t option
    ; workDoneProgress : bool option
    }

  val create
    :  ?showDocument:ShowDocumentClientCapabilities.t
    -> ?showMessage:ShowMessageRequestClientCapabilities.t
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module TypeHierarchyClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  val create : ?dynamicRegistration:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module TypeDefinitionClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; linkSupport : bool option
    }

  val create : ?dynamicRegistration:bool -> ?linkSupport:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module TextDocumentSyncClientCapabilities : sig
  type t =
    { didSave : bool option
    ; dynamicRegistration : bool option
    ; willSave : bool option
    ; willSaveWaitUntil : bool option
    }

  val create
    :  ?didSave:bool
    -> ?dynamicRegistration:bool
    -> ?willSave:bool
    -> ?willSaveWaitUntil:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SignatureHelpClientCapabilities : sig
  type parameterInformation = { labelOffsetSupport : bool option }

  val create_parameterInformation
    :  ?labelOffsetSupport:bool
    -> unit
    -> parameterInformation

  type signatureInformation =
    { documentationFormat : MarkupKind.t list option
    ; parameterInformation : parameterInformation option
    ; activeParameterSupport : bool option
    ; noActiveParameterSupport : bool option
    }

  val create_signatureInformation
    :  ?documentationFormat:MarkupKind.t list
    -> ?parameterInformation:parameterInformation
    -> ?activeParameterSupport:bool
    -> ?noActiveParameterSupport:bool
    -> unit
    -> signatureInformation

  type t =
    { contextSupport : bool option
    ; dynamicRegistration : bool option
    ; signatureInformation : signatureInformation option
    }

  val create
    :  ?contextSupport:bool
    -> ?dynamicRegistration:bool
    -> ?signatureInformation:signatureInformation
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SemanticTokensClientCapabilities : sig
  type full = { delta : bool option }

  val create_full : ?delta:bool -> unit -> full

  type requests =
    { range : bool option
    ; full : [ `Bool of bool | `Full of full ] option
    }

  val create_requests
    :  ?range:bool
    -> ?full:[ `Bool of bool | `Full of full ]
    -> unit
    -> requests

  type t =
    { augmentsSyntaxTokens : bool option
    ; dynamicRegistration : bool option
    ; formats : TokenFormat.t list
    ; multilineTokenSupport : bool option
    ; overlappingTokenSupport : bool option
    ; requests : requests
    ; serverCancelSupport : bool option
    ; tokenModifiers : string list
    ; tokenTypes : string list
    }

  val create
    :  ?augmentsSyntaxTokens:bool
    -> ?dynamicRegistration:bool
    -> formats:TokenFormat.t list
    -> ?multilineTokenSupport:bool
    -> ?overlappingTokenSupport:bool
    -> requests:requests
    -> ?serverCancelSupport:bool
    -> tokenModifiers:string list
    -> tokenTypes:string list
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SelectionRangeClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  val create : ?dynamicRegistration:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module RenameClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; honorsChangeAnnotations : bool option
    ; prepareSupport : bool option
    ; prepareSupportDefaultBehavior : PrepareSupportDefaultBehavior.t option
    }

  val create
    :  ?dynamicRegistration:bool
    -> ?honorsChangeAnnotations:bool
    -> ?prepareSupport:bool
    -> ?prepareSupportDefaultBehavior:PrepareSupportDefaultBehavior.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module ReferenceClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  val create : ?dynamicRegistration:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DocumentRangeFormattingClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; rangesSupport : bool option
    }

  val create : ?dynamicRegistration:bool -> ?rangesSupport:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module PublishDiagnosticsClientCapabilities : sig
  type tagSupport = { valueSet : DiagnosticTag.t list }

  val create_tagSupport : valueSet:DiagnosticTag.t list -> tagSupport

  type t =
    { codeDescriptionSupport : bool option
    ; dataSupport : bool option
    ; relatedInformation : bool option
    ; tagSupport : tagSupport option
    ; versionSupport : bool option
    }

  val create
    :  ?codeDescriptionSupport:bool
    -> ?dataSupport:bool
    -> ?relatedInformation:bool
    -> ?tagSupport:tagSupport
    -> ?versionSupport:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentOnTypeFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  val create : ?dynamicRegistration:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module MonikerClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  val create : ?dynamicRegistration:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module LinkedEditingRangeClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  val create : ?dynamicRegistration:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module InlineValueClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  val create : ?dynamicRegistration:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module InlineCompletionClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  val create : ?dynamicRegistration:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module InlayHintClientCapabilities : sig
  type resolveSupport = { properties : string list }

  val create_resolveSupport : properties:string list -> resolveSupport

  type t =
    { dynamicRegistration : bool option
    ; resolveSupport : resolveSupport option
    }

  val create : ?dynamicRegistration:bool -> ?resolveSupport:resolveSupport -> unit -> t

  include Json.Jsonable.S with type t := t
end

module ImplementationClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; linkSupport : bool option
    }

  val create : ?dynamicRegistration:bool -> ?linkSupport:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module HoverClientCapabilities : sig
  type t =
    { contentFormat : MarkupKind.t list option
    ; dynamicRegistration : bool option
    }

  val create : ?contentFormat:MarkupKind.t list -> ?dynamicRegistration:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DocumentFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  val create : ?dynamicRegistration:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module FoldingRangeClientCapabilities : sig
  type foldingRangeKind = { valueSet : FoldingRangeKind.t list option }

  val create_foldingRangeKind
    :  ?valueSet:FoldingRangeKind.t list
    -> unit
    -> foldingRangeKind

  type foldingRange = { collapsedText : bool option }

  val create_foldingRange : ?collapsedText:bool -> unit -> foldingRange

  type t =
    { dynamicRegistration : bool option
    ; foldingRange : foldingRange option
    ; foldingRangeKind : foldingRangeKind option
    ; lineFoldingOnly : bool option
    ; rangeLimit : int option
    }

  val create
    :  ?dynamicRegistration:bool
    -> ?foldingRange:foldingRange
    -> ?foldingRangeKind:foldingRangeKind
    -> ?lineFoldingOnly:bool
    -> ?rangeLimit:int
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentSymbolClientCapabilities : sig
  type tagSupport = { valueSet : SymbolTag.t list }

  val create_tagSupport : valueSet:SymbolTag.t list -> tagSupport

  type symbolKind = { valueSet : SymbolKind.t list option }

  val create_symbolKind : ?valueSet:SymbolKind.t list -> unit -> symbolKind

  type t =
    { dynamicRegistration : bool option
    ; hierarchicalDocumentSymbolSupport : bool option
    ; labelSupport : bool option
    ; symbolKind : symbolKind option
    ; tagSupport : tagSupport option
    }

  val create
    :  ?dynamicRegistration:bool
    -> ?hierarchicalDocumentSymbolSupport:bool
    -> ?labelSupport:bool
    -> ?symbolKind:symbolKind
    -> ?tagSupport:tagSupport
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentLinkClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; tooltipSupport : bool option
    }

  val create : ?dynamicRegistration:bool -> ?tooltipSupport:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DocumentHighlightClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  val create : ?dynamicRegistration:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DiagnosticClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; markupMessageSupport : bool option
    ; relatedDocumentSupport : bool option
    }

  val create
    :  ?dynamicRegistration:bool
    -> ?markupMessageSupport:bool
    -> ?relatedDocumentSupport:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DefinitionClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; linkSupport : bool option
    }

  val create : ?dynamicRegistration:bool -> ?linkSupport:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DeclarationClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; linkSupport : bool option
    }

  val create : ?dynamicRegistration:bool -> ?linkSupport:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module CompletionClientCapabilities : sig
  type completionList = { itemDefaults : string list option }

  val create_completionList : ?itemDefaults:string list -> unit -> completionList

  type completionItemKind = { valueSet : CompletionItemKind.t list option }

  val create_completionItemKind
    :  ?valueSet:CompletionItemKind.t list
    -> unit
    -> completionItemKind

  type insertTextModeSupport = { valueSet : InsertTextMode.t list }

  val create_insertTextModeSupport
    :  valueSet:InsertTextMode.t list
    -> insertTextModeSupport

  type resolveSupport = { properties : string list }

  val create_resolveSupport : properties:string list -> resolveSupport

  type tagSupport = { valueSet : CompletionItemTag.t list }

  val create_tagSupport : valueSet:CompletionItemTag.t list -> tagSupport

  type completionItem =
    { snippetSupport : bool option
    ; commitCharactersSupport : bool option
    ; documentationFormat : MarkupKind.t list option
    ; deprecatedSupport : bool option
    ; preselectSupport : bool option
    ; tagSupport : tagSupport option
    ; insertReplaceSupport : bool option
    ; resolveSupport : resolveSupport option
    ; insertTextModeSupport : insertTextModeSupport option
    ; labelDetailsSupport : bool option
    }

  val create_completionItem
    :  ?snippetSupport:bool
    -> ?commitCharactersSupport:bool
    -> ?documentationFormat:MarkupKind.t list
    -> ?deprecatedSupport:bool
    -> ?preselectSupport:bool
    -> ?tagSupport:tagSupport
    -> ?insertReplaceSupport:bool
    -> ?resolveSupport:resolveSupport
    -> ?insertTextModeSupport:insertTextModeSupport
    -> ?labelDetailsSupport:bool
    -> unit
    -> completionItem

  type t =
    { completionItem : completionItem option
    ; completionItemKind : completionItemKind option
    ; completionList : completionList option
    ; contextSupport : bool option
    ; dynamicRegistration : bool option
    ; insertTextMode : InsertTextMode.t option
    }

  val create
    :  ?completionItem:completionItem
    -> ?completionItemKind:completionItemKind
    -> ?completionList:completionList
    -> ?contextSupport:bool
    -> ?dynamicRegistration:bool
    -> ?insertTextMode:InsertTextMode.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentColorClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  val create : ?dynamicRegistration:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module CodeLensClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  val create : ?dynamicRegistration:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module CodeActionClientCapabilities : sig
  type resolveSupport = { properties : string list }

  val create_resolveSupport : properties:string list -> resolveSupport

  type codeActionKind = { valueSet : CodeActionKind.t list }

  val create_codeActionKind : valueSet:CodeActionKind.t list -> codeActionKind

  type codeActionLiteralSupport = { codeActionKind : codeActionKind }

  val create_codeActionLiteralSupport
    :  codeActionKind:codeActionKind
    -> codeActionLiteralSupport

  type t =
    { codeActionLiteralSupport : codeActionLiteralSupport option
    ; dataSupport : bool option
    ; disabledSupport : bool option
    ; dynamicRegistration : bool option
    ; honorsChangeAnnotations : bool option
    ; isPreferredSupport : bool option
    ; resolveSupport : resolveSupport option
    }

  val create
    :  ?codeActionLiteralSupport:codeActionLiteralSupport
    -> ?dataSupport:bool
    -> ?disabledSupport:bool
    -> ?dynamicRegistration:bool
    -> ?honorsChangeAnnotations:bool
    -> ?isPreferredSupport:bool
    -> ?resolveSupport:resolveSupport
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module TextDocumentClientCapabilities : sig
  type t =
    { callHierarchy : CallHierarchyClientCapabilities.t option
    ; codeAction : CodeActionClientCapabilities.t option
    ; codeLens : CodeLensClientCapabilities.t option
    ; colorProvider : DocumentColorClientCapabilities.t option
    ; completion : CompletionClientCapabilities.t option
    ; declaration : DeclarationClientCapabilities.t option
    ; definition : DefinitionClientCapabilities.t option
    ; diagnostic : DiagnosticClientCapabilities.t option
    ; documentHighlight : DocumentHighlightClientCapabilities.t option
    ; documentLink : DocumentLinkClientCapabilities.t option
    ; documentSymbol : DocumentSymbolClientCapabilities.t option
    ; foldingRange : FoldingRangeClientCapabilities.t option
    ; formatting : DocumentFormattingClientCapabilities.t option
    ; hover : HoverClientCapabilities.t option
    ; implementation : ImplementationClientCapabilities.t option
    ; inlayHint : InlayHintClientCapabilities.t option
    ; inlineCompletion : InlineCompletionClientCapabilities.t option
    ; inlineValue : InlineValueClientCapabilities.t option
    ; linkedEditingRange : LinkedEditingRangeClientCapabilities.t option
    ; moniker : MonikerClientCapabilities.t option
    ; onTypeFormatting : DocumentOnTypeFormattingClientCapabilities.t option
    ; publishDiagnostics : PublishDiagnosticsClientCapabilities.t option
    ; rangeFormatting : DocumentRangeFormattingClientCapabilities.t option
    ; references : ReferenceClientCapabilities.t option
    ; rename : RenameClientCapabilities.t option
    ; selectionRange : SelectionRangeClientCapabilities.t option
    ; semanticTokens : SemanticTokensClientCapabilities.t option
    ; signatureHelp : SignatureHelpClientCapabilities.t option
    ; synchronization : TextDocumentSyncClientCapabilities.t option
    ; typeDefinition : TypeDefinitionClientCapabilities.t option
    ; typeHierarchy : TypeHierarchyClientCapabilities.t option
    }

  val create
    :  ?callHierarchy:CallHierarchyClientCapabilities.t
    -> ?codeAction:CodeActionClientCapabilities.t
    -> ?codeLens:CodeLensClientCapabilities.t
    -> ?colorProvider:DocumentColorClientCapabilities.t
    -> ?completion:CompletionClientCapabilities.t
    -> ?declaration:DeclarationClientCapabilities.t
    -> ?definition:DefinitionClientCapabilities.t
    -> ?diagnostic:DiagnosticClientCapabilities.t
    -> ?documentHighlight:DocumentHighlightClientCapabilities.t
    -> ?documentLink:DocumentLinkClientCapabilities.t
    -> ?documentSymbol:DocumentSymbolClientCapabilities.t
    -> ?foldingRange:FoldingRangeClientCapabilities.t
    -> ?formatting:DocumentFormattingClientCapabilities.t
    -> ?hover:HoverClientCapabilities.t
    -> ?implementation:ImplementationClientCapabilities.t
    -> ?inlayHint:InlayHintClientCapabilities.t
    -> ?inlineCompletion:InlineCompletionClientCapabilities.t
    -> ?inlineValue:InlineValueClientCapabilities.t
    -> ?linkedEditingRange:LinkedEditingRangeClientCapabilities.t
    -> ?moniker:MonikerClientCapabilities.t
    -> ?onTypeFormatting:DocumentOnTypeFormattingClientCapabilities.t
    -> ?publishDiagnostics:PublishDiagnosticsClientCapabilities.t
    -> ?rangeFormatting:DocumentRangeFormattingClientCapabilities.t
    -> ?references:ReferenceClientCapabilities.t
    -> ?rename:RenameClientCapabilities.t
    -> ?selectionRange:SelectionRangeClientCapabilities.t
    -> ?semanticTokens:SemanticTokensClientCapabilities.t
    -> ?signatureHelp:SignatureHelpClientCapabilities.t
    -> ?synchronization:TextDocumentSyncClientCapabilities.t
    -> ?typeDefinition:TypeDefinitionClientCapabilities.t
    -> ?typeHierarchy:TypeHierarchyClientCapabilities.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module NotebookDocumentSyncClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; executionSummarySupport : bool option
    }

  val create : ?dynamicRegistration:bool -> ?executionSummarySupport:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module NotebookDocumentClientCapabilities : sig
  type t = { synchronization : NotebookDocumentSyncClientCapabilities.t }

  val create : synchronization:NotebookDocumentSyncClientCapabilities.t -> t

  include Json.Jsonable.S with type t := t
end

module RegularExpressionsClientCapabilities : sig
  type t =
    { engine : string
    ; version : string option
    }

  val create : engine:string -> ?version:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module MarkdownClientCapabilities : sig
  type t =
    { allowedTags : string list option
    ; parser : string
    ; version : string option
    }

  val create : ?allowedTags:string list -> parser:string -> ?version:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module GeneralClientCapabilities : sig
  type staleRequestSupport =
    { cancel : bool
    ; retryOnContentModified : string list
    }

  val create_staleRequestSupport
    :  cancel:bool
    -> retryOnContentModified:string list
    -> staleRequestSupport

  type t =
    { markdown : MarkdownClientCapabilities.t option
    ; positionEncodings : PositionEncodingKind.t list option
    ; regularExpressions : RegularExpressionsClientCapabilities.t option
    ; staleRequestSupport : staleRequestSupport option
    }

  val create
    :  ?markdown:MarkdownClientCapabilities.t
    -> ?positionEncodings:PositionEncodingKind.t list
    -> ?regularExpressions:RegularExpressionsClientCapabilities.t
    -> ?staleRequestSupport:staleRequestSupport
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module ClientCapabilities : sig
  type t =
    { experimental : Json.t option
    ; general : GeneralClientCapabilities.t option
    ; notebookDocument : NotebookDocumentClientCapabilities.t option
    ; textDocument : TextDocumentClientCapabilities.t option
    ; window : WindowClientCapabilities.t option
    ; workspace : WorkspaceClientCapabilities.t option
    }

  val create
    :  ?experimental:Json.t
    -> ?general:GeneralClientCapabilities.t
    -> ?notebookDocument:NotebookDocumentClientCapabilities.t
    -> ?textDocument:TextDocumentClientCapabilities.t
    -> ?window:WindowClientCapabilities.t
    -> ?workspace:WorkspaceClientCapabilities.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module Location : sig
  type t =
    { range : Range.t
    ; uri : DocumentUri.t
    }

  val create : range:Range.t -> uri:DocumentUri.t -> t

  include Json.Jsonable.S with type t := t
end

module DiagnosticRelatedInformation : sig
  type t =
    { location : Location.t
    ; message : string
    }

  val create : location:Location.t -> message:string -> t

  include Json.Jsonable.S with type t := t
end

module MarkupContent : sig
  type t =
    { kind : MarkupKind.t
    ; value : string
    }

  val create : kind:MarkupKind.t -> value:string -> t

  include Json.Jsonable.S with type t := t
end

module CodeDescription : sig
  type t = { href : DocumentUri.t }

  val create : href:DocumentUri.t -> t

  include Json.Jsonable.S with type t := t
end

module Diagnostic : sig
  type t =
    { code : Jsonrpc.Id.t option
    ; codeDescription : CodeDescription.t option
    ; data : Json.t option
    ; message : [ `String of string | `MarkupContent of MarkupContent.t ]
    ; range : Range.t
    ; relatedInformation : DiagnosticRelatedInformation.t list option
    ; severity : DiagnosticSeverity.t option
    ; source : string option
    ; tags : DiagnosticTag.t list option
    }

  val create
    :  ?code:Jsonrpc.Id.t
    -> ?codeDescription:CodeDescription.t
    -> ?data:Json.t
    -> message:[ `String of string | `MarkupContent of MarkupContent.t ]
    -> range:Range.t
    -> ?relatedInformation:DiagnosticRelatedInformation.t list
    -> ?severity:DiagnosticSeverity.t
    -> ?source:string
    -> ?tags:DiagnosticTag.t list
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module Command : sig
  type t =
    { arguments : Json.t list option
    ; command : string
    ; title : string
    }

  val create : ?arguments:Json.t list -> command:string -> title:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module CodeAction : sig
  type disabled = { reason : string }

  val create_disabled : reason:string -> disabled

  type t =
    { command : Command.t option
    ; data : Json.t option
    ; diagnostics : Diagnostic.t list option
    ; disabled : disabled option
    ; edit : WorkspaceEdit.t option
    ; isPreferred : bool option
    ; kind : CodeActionKind.t option
    ; title : string
    }

  val create
    :  ?command:Command.t
    -> ?data:Json.t
    -> ?diagnostics:Diagnostic.t list
    -> ?disabled:disabled
    -> ?edit:WorkspaceEdit.t
    -> ?isPreferred:bool
    -> ?kind:CodeActionKind.t
    -> title:string
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module CodeActionContext : sig
  type t =
    { diagnostics : Diagnostic.t list
    ; only : CodeActionKind.t list option
    ; triggerKind : CodeActionTriggerKind.t option
    }

  val create
    :  diagnostics:Diagnostic.t list
    -> ?only:CodeActionKind.t list
    -> ?triggerKind:CodeActionTriggerKind.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module CodeActionOptions : sig
  type t =
    { codeActionKinds : CodeActionKind.t list option
    ; resolveProvider : bool option
    ; workDoneProgress : bool option
    }

  val create
    :  ?codeActionKinds:CodeActionKind.t list
    -> ?resolveProvider:bool
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module CodeActionParams : sig
  type t =
    { context : CodeActionContext.t
    ; partialResultToken : ProgressToken.t option
    ; range : Range.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  context:CodeActionContext.t
    -> ?partialResultToken:ProgressToken.t
    -> range:Range.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module CodeActionRegistrationOptions : sig
  type t =
    { codeActionKinds : CodeActionKind.t list option
    ; documentSelector : DocumentSelector.t option
    ; resolveProvider : bool option
    ; workDoneProgress : bool option
    }

  val create
    :  ?codeActionKinds:CodeActionKind.t list
    -> ?documentSelector:DocumentSelector.t
    -> ?resolveProvider:bool
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module CodeLens : sig
  type t =
    { command : Command.t option
    ; data : Json.t option
    ; range : Range.t
    }

  val create : ?command:Command.t -> ?data:Json.t -> range:Range.t -> unit -> t

  include Json.Jsonable.S with type t := t
end

module CodeLensOptions : sig
  type t =
    { resolveProvider : bool option
    ; workDoneProgress : bool option
    }

  val create : ?resolveProvider:bool -> ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module CodeLensParams : sig
  type t =
    { partialResultToken : ProgressToken.t option
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?partialResultToken:ProgressToken.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module CodeLensRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; resolveProvider : bool option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?resolveProvider:bool
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module Color : sig
  type t =
    { alpha : int
    ; blue : int
    ; green : int
    ; red : int
    }

  val create : alpha:int -> blue:int -> green:int -> red:int -> t

  include Json.Jsonable.S with type t := t
end

module ColorInformation : sig
  type t =
    { color : Color.t
    ; range : Range.t
    }

  val create : color:Color.t -> range:Range.t -> t

  include Json.Jsonable.S with type t := t
end

module ColorPresentation : sig
  type t =
    { additionalTextEdits : TextEdit.t list option
    ; label : string
    ; textEdit : TextEdit.t option
    }

  val create
    :  ?additionalTextEdits:TextEdit.t list
    -> label:string
    -> ?textEdit:TextEdit.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module ColorPresentationParams : sig
  type t =
    { color : Color.t
    ; partialResultToken : ProgressToken.t option
    ; range : Range.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  color:Color.t
    -> ?partialResultToken:ProgressToken.t
    -> range:Range.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module CompletionContext : sig
  type t =
    { triggerCharacter : string option
    ; triggerKind : CompletionTriggerKind.t
    }

  val create
    :  ?triggerCharacter:string
    -> triggerKind:CompletionTriggerKind.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module InsertReplaceEdit : sig
  type t =
    { insert : Range.t
    ; newText : string
    ; replace : Range.t
    }

  val create : insert:Range.t -> newText:string -> replace:Range.t -> t

  include Json.Jsonable.S with type t := t
end

module CompletionItemLabelDetails : sig
  type t =
    { description : string option
    ; detail : string option
    }

  val create : ?description:string -> ?detail:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module CompletionItem : sig
  type t =
    { additionalTextEdits : TextEdit.t list option
    ; command : Command.t option
    ; commitCharacters : string list option
    ; data : Json.t option
    ; deprecated : bool option
    ; detail : string option
    ; documentation : [ `String of string | `MarkupContent of MarkupContent.t ] option
    ; filterText : string option
    ; insertText : string option
    ; insertTextFormat : InsertTextFormat.t option
    ; insertTextMode : InsertTextMode.t option
    ; kind : CompletionItemKind.t option
    ; label : string
    ; labelDetails : CompletionItemLabelDetails.t option
    ; preselect : bool option
    ; sortText : string option
    ; tags : CompletionItemTag.t list option
    ; textEdit :
        [ `TextEdit of TextEdit.t | `InsertReplaceEdit of InsertReplaceEdit.t ] option
    ; textEditText : string option
    }

  val create
    :  ?additionalTextEdits:TextEdit.t list
    -> ?command:Command.t
    -> ?commitCharacters:string list
    -> ?data:Json.t
    -> ?deprecated:bool
    -> ?detail:string
    -> ?documentation:[ `String of string | `MarkupContent of MarkupContent.t ]
    -> ?filterText:string
    -> ?insertText:string
    -> ?insertTextFormat:InsertTextFormat.t
    -> ?insertTextMode:InsertTextMode.t
    -> ?kind:CompletionItemKind.t
    -> label:string
    -> ?labelDetails:CompletionItemLabelDetails.t
    -> ?preselect:bool
    -> ?sortText:string
    -> ?tags:CompletionItemTag.t list
    -> ?textEdit:[ `TextEdit of TextEdit.t | `InsertReplaceEdit of InsertReplaceEdit.t ]
    -> ?textEditText:string
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module CompletionList : sig
  type editRange =
    { insert : Range.t
    ; replace : Range.t
    }

  val create_editRange : insert:Range.t -> replace:Range.t -> editRange

  type itemDefaults =
    { commitCharacters : string list option
    ; editRange : [ `Range of Range.t | `EditRange of editRange ] option
    ; insertTextFormat : InsertTextFormat.t option
    ; insertTextMode : InsertTextMode.t option
    ; data : Json.t option
    }

  val create_itemDefaults
    :  ?commitCharacters:string list
    -> ?editRange:[ `Range of Range.t | `EditRange of editRange ]
    -> ?insertTextFormat:InsertTextFormat.t
    -> ?insertTextMode:InsertTextMode.t
    -> ?data:Json.t
    -> unit
    -> itemDefaults

  type t =
    { isIncomplete : bool
    ; itemDefaults : itemDefaults option
    ; items : CompletionItem.t list
    }

  val create
    :  isIncomplete:bool
    -> ?itemDefaults:itemDefaults
    -> items:CompletionItem.t list
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module CompletionOptions : sig
  type completionItem = { labelDetailsSupport : bool option }

  val create_completionItem : ?labelDetailsSupport:bool -> unit -> completionItem

  type t =
    { allCommitCharacters : string list option
    ; completionItem : completionItem option
    ; resolveProvider : bool option
    ; triggerCharacters : string list option
    ; workDoneProgress : bool option
    }

  val create
    :  ?allCommitCharacters:string list
    -> ?completionItem:completionItem
    -> ?resolveProvider:bool
    -> ?triggerCharacters:string list
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module CompletionParams : sig
  type t =
    { context : CompletionContext.t option
    ; partialResultToken : ProgressToken.t option
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?context:CompletionContext.t
    -> ?partialResultToken:ProgressToken.t
    -> position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module CompletionRegistrationOptions : sig
  type completionItem = { labelDetailsSupport : bool option }

  val create_completionItem : ?labelDetailsSupport:bool -> unit -> completionItem

  type t =
    { allCommitCharacters : string list option
    ; completionItem : completionItem option
    ; documentSelector : DocumentSelector.t option
    ; resolveProvider : bool option
    ; triggerCharacters : string list option
    ; workDoneProgress : bool option
    }

  val create
    :  ?allCommitCharacters:string list
    -> ?completionItem:completionItem
    -> ?documentSelector:DocumentSelector.t
    -> ?resolveProvider:bool
    -> ?triggerCharacters:string list
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module ConfigurationItem : sig
  type t =
    { scopeUri : DocumentUri.t option
    ; section : string option
    }

  val create : ?scopeUri:DocumentUri.t -> ?section:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module ConfigurationParams : sig
  type t = { items : ConfigurationItem.t list }

  val create : items:ConfigurationItem.t list -> t

  include Json.Jsonable.S with type t := t
end

module FileCreate : sig
  type t = { uri : string }

  val create : uri:string -> t

  include Json.Jsonable.S with type t := t
end

module CreateFilesParams : sig
  type t = { files : FileCreate.t list }

  val create : files:FileCreate.t list -> t

  include Json.Jsonable.S with type t := t
end

module Declaration : sig
  type t =
    [ `Location of Location.t
    | `List of Location.t list
    ]

  include Json.Jsonable.S with type t := t
end

module LocationLink : sig
  type t =
    { originSelectionRange : Range.t option
    ; targetRange : Range.t
    ; targetSelectionRange : Range.t
    ; targetUri : DocumentUri.t
    }

  val create
    :  ?originSelectionRange:Range.t
    -> targetRange:Range.t
    -> targetSelectionRange:Range.t
    -> targetUri:DocumentUri.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DeclarationLink : sig
  type t = LocationLink.t

  include Json.Jsonable.S with type t := t
end

module DeclarationOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DeclarationParams : sig
  type t =
    { partialResultToken : ProgressToken.t option
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?partialResultToken:ProgressToken.t
    -> position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DeclarationRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; id : string option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?id:string
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module Definition : sig
  type t =
    [ `Location of Location.t
    | `List of Location.t list
    ]

  include Json.Jsonable.S with type t := t
end

module DefinitionLink : sig
  type t = LocationLink.t

  include Json.Jsonable.S with type t := t
end

module DefinitionOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DefinitionParams : sig
  type t =
    { partialResultToken : ProgressToken.t option
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?partialResultToken:ProgressToken.t
    -> position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DefinitionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }

  val create : ?documentSelector:DocumentSelector.t -> ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module FileDelete : sig
  type t = { uri : string }

  val create : uri:string -> t

  include Json.Jsonable.S with type t := t
end

module DeleteFilesParams : sig
  type t = { files : FileDelete.t list }

  val create : files:FileDelete.t list -> t

  include Json.Jsonable.S with type t := t
end

module DiagnosticOptions : sig
  type t =
    { identifier : string option
    ; interFileDependencies : bool
    ; workDoneProgress : bool option
    ; workspaceDiagnostics : bool
    }

  val create
    :  ?identifier:string
    -> interFileDependencies:bool
    -> ?workDoneProgress:bool
    -> workspaceDiagnostics:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DiagnosticRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; id : string option
    ; identifier : string option
    ; interFileDependencies : bool
    ; workDoneProgress : bool option
    ; workspaceDiagnostics : bool
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?id:string
    -> ?identifier:string
    -> interFileDependencies:bool
    -> ?workDoneProgress:bool
    -> workspaceDiagnostics:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DiagnosticServerCancellationData : sig
  type t = { retriggerRequest : bool }

  val create : retriggerRequest:bool -> t

  include Json.Jsonable.S with type t := t
end

module DidChangeConfigurationParams : sig
  type t = { settings : Json.t }

  val create : settings:Json.t -> t

  include Json.Jsonable.S with type t := t
end

module DidChangeConfigurationRegistrationOptions : sig
  type t = { section : [ `String of string | `List of string list ] option }

  val create : ?section:[ `String of string | `List of string list ] -> unit -> t

  include Json.Jsonable.S with type t := t
end

module VersionedNotebookDocumentIdentifier : sig
  type t =
    { uri : DocumentUri.t
    ; version : int
    }

  val create : uri:DocumentUri.t -> version:int -> t

  include Json.Jsonable.S with type t := t
end

module TextDocumentContentChangeEvent : sig
  type t =
    { range : Range.t option
    ; rangeLength : int option
    ; text : string
    }

  val create : ?range:Range.t -> ?rangeLength:int -> text:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module VersionedTextDocumentIdentifier : sig
  type t =
    { uri : DocumentUri.t
    ; version : int
    }

  val create : uri:DocumentUri.t -> version:int -> t

  include Json.Jsonable.S with type t := t
end

module ExecutionSummary : sig
  type t =
    { executionOrder : int
    ; success : bool option
    }

  val create : executionOrder:int -> ?success:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module NotebookCell : sig
  type t =
    { document : DocumentUri.t
    ; executionSummary : ExecutionSummary.t option
    ; kind : NotebookCellKind.t
    ; metadata : Json.Object.t option
    }

  val create
    :  document:DocumentUri.t
    -> ?executionSummary:ExecutionSummary.t
    -> kind:NotebookCellKind.t
    -> ?metadata:Json.Object.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module TextDocumentItem : sig
  type t =
    { languageId : string
    ; text : string
    ; uri : DocumentUri.t
    ; version : int
    }

  val create : languageId:string -> text:string -> uri:DocumentUri.t -> version:int -> t

  include Json.Jsonable.S with type t := t
end

module NotebookCellArrayChange : sig
  type t =
    { cells : NotebookCell.t list option
    ; deleteCount : int
    ; start : int
    }

  val create : ?cells:NotebookCell.t list -> deleteCount:int -> start:int -> unit -> t

  include Json.Jsonable.S with type t := t
end

module NotebookDocumentChangeEvent : sig
  type textContent =
    { document : VersionedTextDocumentIdentifier.t
    ; changes : TextDocumentContentChangeEvent.t list
    }

  val create_textContent
    :  document:VersionedTextDocumentIdentifier.t
    -> changes:TextDocumentContentChangeEvent.t list
    -> textContent

  type structure =
    { array : NotebookCellArrayChange.t
    ; didOpen : TextDocumentItem.t list option
    ; didClose : TextDocumentIdentifier.t list option
    }

  val create_structure
    :  array:NotebookCellArrayChange.t
    -> ?didOpen:TextDocumentItem.t list
    -> ?didClose:TextDocumentIdentifier.t list
    -> unit
    -> structure

  type cells =
    { structure : structure option
    ; data : NotebookCell.t list option
    ; textContent : textContent list option
    }

  val create_cells
    :  ?structure:structure
    -> ?data:NotebookCell.t list
    -> ?textContent:textContent list
    -> unit
    -> cells

  type t =
    { cells : cells option
    ; metadata : Json.Object.t option
    }

  val create : ?cells:cells -> ?metadata:Json.Object.t -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DidChangeNotebookDocumentParams : sig
  type t =
    { change : NotebookDocumentChangeEvent.t
    ; notebookDocument : VersionedNotebookDocumentIdentifier.t
    }

  val create
    :  change:NotebookDocumentChangeEvent.t
    -> notebookDocument:VersionedNotebookDocumentIdentifier.t
    -> t

  include Json.Jsonable.S with type t := t
end

module DidChangeTextDocumentParams : sig
  type t =
    { contentChanges : TextDocumentContentChangeEvent.t list
    ; textDocument : VersionedTextDocumentIdentifier.t
    }

  val create
    :  contentChanges:TextDocumentContentChangeEvent.t list
    -> textDocument:VersionedTextDocumentIdentifier.t
    -> t

  include Json.Jsonable.S with type t := t
end

module FileEvent : sig
  type t =
    { type_ : FileChangeType.t
    ; uri : DocumentUri.t
    }

  val create : type_:FileChangeType.t -> uri:DocumentUri.t -> t

  include Json.Jsonable.S with type t := t
end

module DidChangeWatchedFilesParams : sig
  type t = { changes : FileEvent.t list }

  val create : changes:FileEvent.t list -> t

  include Json.Jsonable.S with type t := t
end

module Pattern : sig
  type t = string

  include Json.Jsonable.S with type t := t
end

module WorkspaceFolder : sig
  type t =
    { name : string
    ; uri : DocumentUri.t
    }

  val create : name:string -> uri:DocumentUri.t -> t

  include Json.Jsonable.S with type t := t
end

module RelativePattern : sig
  type t =
    { baseUri : unit
    ; pattern : Pattern.t
    }

  val create : baseUri:unit -> pattern:Pattern.t -> t

  include Json.Jsonable.S with type t := t
end

module GlobPattern : sig
  type t =
    [ `Pattern of Pattern.t
    | `RelativePattern of RelativePattern.t
    ]

  include Json.Jsonable.S with type t := t
end

module FileSystemWatcher : sig
  type t =
    { globPattern : GlobPattern.t
    ; kind : WatchKind.t option
    }

  val create : globPattern:GlobPattern.t -> ?kind:WatchKind.t -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DidChangeWatchedFilesRegistrationOptions : sig
  type t = { watchers : FileSystemWatcher.t list }

  val create : watchers:FileSystemWatcher.t list -> t

  include Json.Jsonable.S with type t := t
end

module WorkspaceFoldersChangeEvent : sig
  type t =
    { added : WorkspaceFolder.t list
    ; removed : WorkspaceFolder.t list
    }

  val create : added:WorkspaceFolder.t list -> removed:WorkspaceFolder.t list -> t

  include Json.Jsonable.S with type t := t
end

module DidChangeWorkspaceFoldersParams : sig
  type t = { event : WorkspaceFoldersChangeEvent.t }

  val create : event:WorkspaceFoldersChangeEvent.t -> t

  include Json.Jsonable.S with type t := t
end

module NotebookDocumentIdentifier : sig
  type t = { uri : DocumentUri.t }

  val create : uri:DocumentUri.t -> t

  include Json.Jsonable.S with type t := t
end

module DidCloseNotebookDocumentParams : sig
  type t =
    { cellTextDocuments : TextDocumentIdentifier.t list
    ; notebookDocument : NotebookDocumentIdentifier.t
    }

  val create
    :  cellTextDocuments:TextDocumentIdentifier.t list
    -> notebookDocument:NotebookDocumentIdentifier.t
    -> t

  include Json.Jsonable.S with type t := t
end

module DidCloseTextDocumentParams : sig
  type t = { textDocument : TextDocumentIdentifier.t }

  val create : textDocument:TextDocumentIdentifier.t -> t

  include Json.Jsonable.S with type t := t
end

module NotebookDocument : sig
  type t =
    { cells : NotebookCell.t list
    ; metadata : Json.Object.t option
    ; notebookType : string
    ; uri : DocumentUri.t
    ; version : int
    }

  val create
    :  cells:NotebookCell.t list
    -> ?metadata:Json.Object.t
    -> notebookType:string
    -> uri:DocumentUri.t
    -> version:int
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DidOpenNotebookDocumentParams : sig
  type t =
    { cellTextDocuments : TextDocumentItem.t list
    ; notebookDocument : NotebookDocument.t
    }

  val create
    :  cellTextDocuments:TextDocumentItem.t list
    -> notebookDocument:NotebookDocument.t
    -> t

  include Json.Jsonable.S with type t := t
end

module DidOpenTextDocumentParams : sig
  type t = { textDocument : TextDocumentItem.t }

  val create : textDocument:TextDocumentItem.t -> t

  include Json.Jsonable.S with type t := t
end

module DidSaveNotebookDocumentParams : sig
  type t = { notebookDocument : NotebookDocumentIdentifier.t }

  val create : notebookDocument:NotebookDocumentIdentifier.t -> t

  include Json.Jsonable.S with type t := t
end

module DidSaveTextDocumentParams : sig
  type t =
    { text : string option
    ; textDocument : TextDocumentIdentifier.t
    }

  val create : ?text:string -> textDocument:TextDocumentIdentifier.t -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DocumentColorOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DocumentColorParams : sig
  type t =
    { partialResultToken : ProgressToken.t option
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?partialResultToken:ProgressToken.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentColorRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; id : string option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?id:string
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentDiagnosticParams : sig
  type t =
    { identifier : string option
    ; partialResultToken : ProgressToken.t option
    ; previousResultId : string option
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?identifier:string
    -> ?partialResultToken:ProgressToken.t
    -> ?previousResultId:string
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module UnchangedDocumentDiagnosticReport : sig
  type t = { resultId : string }

  val create : resultId:string -> t

  include Json.Jsonable.S with type t := t
end

module FullDocumentDiagnosticReport : sig
  type t =
    { items : Diagnostic.t list
    ; resultId : string option
    }

  val create : items:Diagnostic.t list -> ?resultId:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module RelatedUnchangedDocumentDiagnosticReport : sig
  type t =
    { relatedDocuments :
        ( DocumentUri.t
          , [ `FullDocumentDiagnosticReport of FullDocumentDiagnosticReport.t
            | `UnchangedDocumentDiagnosticReport of UnchangedDocumentDiagnosticReport.t
            ] )
          Json.Assoc.t
          option
    ; resultId : string
    }

  val create
    :  ?relatedDocuments:
         ( DocumentUri.t
           , [ `FullDocumentDiagnosticReport of FullDocumentDiagnosticReport.t
             | `UnchangedDocumentDiagnosticReport of UnchangedDocumentDiagnosticReport.t
             ] )
           Json.Assoc.t
    -> resultId:string
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module RelatedFullDocumentDiagnosticReport : sig
  type t =
    { items : Diagnostic.t list
    ; relatedDocuments :
        ( DocumentUri.t
          , [ `FullDocumentDiagnosticReport of FullDocumentDiagnosticReport.t
            | `UnchangedDocumentDiagnosticReport of UnchangedDocumentDiagnosticReport.t
            ] )
          Json.Assoc.t
          option
    ; resultId : string option
    }

  val create
    :  items:Diagnostic.t list
    -> ?relatedDocuments:
         ( DocumentUri.t
           , [ `FullDocumentDiagnosticReport of FullDocumentDiagnosticReport.t
             | `UnchangedDocumentDiagnosticReport of UnchangedDocumentDiagnosticReport.t
             ] )
           Json.Assoc.t
    -> ?resultId:string
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentDiagnosticReport : sig
  type t =
    [ `RelatedFullDocumentDiagnosticReport of RelatedFullDocumentDiagnosticReport.t
    | `RelatedUnchangedDocumentDiagnosticReport of
      RelatedUnchangedDocumentDiagnosticReport.t
    ]

  include Json.Jsonable.S with type t := t
end

module DocumentDiagnosticReportPartialResult : sig
  type t =
    { relatedDocuments :
        ( DocumentUri.t
          , [ `FullDocumentDiagnosticReport of FullDocumentDiagnosticReport.t
            | `UnchangedDocumentDiagnosticReport of UnchangedDocumentDiagnosticReport.t
            ] )
          Json.Assoc.t
    }

  val create
    :  relatedDocuments:
         ( DocumentUri.t
           , [ `FullDocumentDiagnosticReport of FullDocumentDiagnosticReport.t
             | `UnchangedDocumentDiagnosticReport of UnchangedDocumentDiagnosticReport.t
             ] )
           Json.Assoc.t
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentFormattingOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module FormattingOptions : sig
  type t =
    { insertFinalNewline : bool option
    ; insertSpaces : bool
    ; tabSize : int
    ; trimFinalNewlines : bool option
    ; trimTrailingWhitespace : bool option
    }

  val create
    :  ?insertFinalNewline:bool
    -> insertSpaces:bool
    -> tabSize:int
    -> ?trimFinalNewlines:bool
    -> ?trimTrailingWhitespace:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentFormattingParams : sig
  type t =
    { options : FormattingOptions.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  options:FormattingOptions.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentFormattingRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }

  val create : ?documentSelector:DocumentSelector.t -> ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DocumentHighlight : sig
  type t =
    { kind : DocumentHighlightKind.t option
    ; range : Range.t
    }

  val create : ?kind:DocumentHighlightKind.t -> range:Range.t -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DocumentHighlightOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DocumentHighlightParams : sig
  type t =
    { partialResultToken : ProgressToken.t option
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?partialResultToken:ProgressToken.t
    -> position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentHighlightRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }

  val create : ?documentSelector:DocumentSelector.t -> ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DocumentLink : sig
  type t =
    { data : Json.t option
    ; range : Range.t
    ; target : DocumentUri.t option
    ; tooltip : string option
    }

  val create
    :  ?data:Json.t
    -> range:Range.t
    -> ?target:DocumentUri.t
    -> ?tooltip:string
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentLinkOptions : sig
  type t =
    { resolveProvider : bool option
    ; workDoneProgress : bool option
    }

  val create : ?resolveProvider:bool -> ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DocumentLinkParams : sig
  type t =
    { partialResultToken : ProgressToken.t option
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?partialResultToken:ProgressToken.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentLinkRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; resolveProvider : bool option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?resolveProvider:bool
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentOnTypeFormattingOptions : sig
  type t =
    { firstTriggerCharacter : string
    ; moreTriggerCharacter : string list option
    }

  val create
    :  firstTriggerCharacter:string
    -> ?moreTriggerCharacter:string list
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentOnTypeFormattingParams : sig
  type t =
    { ch : string
    ; options : FormattingOptions.t
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    }

  val create
    :  ch:string
    -> options:FormattingOptions.t
    -> position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentOnTypeFormattingRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; firstTriggerCharacter : string
    ; moreTriggerCharacter : string list option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> firstTriggerCharacter:string
    -> ?moreTriggerCharacter:string list
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentRangeFormattingOptions : sig
  type t =
    { rangesSupport : bool option
    ; workDoneProgress : bool option
    }

  val create : ?rangesSupport:bool -> ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DocumentRangeFormattingParams : sig
  type t =
    { options : FormattingOptions.t
    ; range : Range.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  options:FormattingOptions.t
    -> range:Range.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentRangeFormattingRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; rangesSupport : bool option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?rangesSupport:bool
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentRangesFormattingParams : sig
  type t =
    { options : FormattingOptions.t
    ; ranges : Range.t list
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  options:FormattingOptions.t
    -> ranges:Range.t list
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentSymbol : sig
  type t =
    { children : t list option
    ; deprecated : bool option
    ; detail : string option
    ; kind : SymbolKind.t
    ; name : string
    ; range : Range.t
    ; selectionRange : Range.t
    ; tags : SymbolTag.t list option
    }

  val create
    :  ?children:t list
    -> ?deprecated:bool
    -> ?detail:string
    -> kind:SymbolKind.t
    -> name:string
    -> range:Range.t
    -> selectionRange:Range.t
    -> ?tags:SymbolTag.t list
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentSymbolOptions : sig
  type t =
    { label : string option
    ; workDoneProgress : bool option
    }

  val create : ?label:string -> ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module DocumentSymbolParams : sig
  type t =
    { partialResultToken : ProgressToken.t option
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?partialResultToken:ProgressToken.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module DocumentSymbolRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; label : string option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?label:string
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module ExecuteCommandOptions : sig
  type t =
    { commands : string list
    ; workDoneProgress : bool option
    }

  val create : commands:string list -> ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module ExecuteCommandParams : sig
  type t =
    { arguments : Json.t list option
    ; command : string
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?arguments:Json.t list
    -> command:string
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module ExecuteCommandRegistrationOptions : sig
  type t =
    { commands : string list
    ; workDoneProgress : bool option
    }

  val create : commands:string list -> ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module FileOperationPatternOptions : sig
  type t = { ignoreCase : bool option }

  val create : ?ignoreCase:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module FileOperationPattern : sig
  type t =
    { glob : string
    ; matches : FileOperationPatternKind.t option
    ; options : FileOperationPatternOptions.t option
    }

  val create
    :  glob:string
    -> ?matches:FileOperationPatternKind.t
    -> ?options:FileOperationPatternOptions.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module FileOperationFilter : sig
  type t =
    { pattern : FileOperationPattern.t
    ; scheme : string option
    }

  val create : pattern:FileOperationPattern.t -> ?scheme:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module FileOperationRegistrationOptions : sig
  type t = { filters : FileOperationFilter.t list }

  val create : filters:FileOperationFilter.t list -> t

  include Json.Jsonable.S with type t := t
end

module FileOperationOptions : sig
  type t =
    { didCreate : FileOperationRegistrationOptions.t option
    ; didDelete : FileOperationRegistrationOptions.t option
    ; didRename : FileOperationRegistrationOptions.t option
    ; willCreate : FileOperationRegistrationOptions.t option
    ; willDelete : FileOperationRegistrationOptions.t option
    ; willRename : FileOperationRegistrationOptions.t option
    }

  val create
    :  ?didCreate:FileOperationRegistrationOptions.t
    -> ?didDelete:FileOperationRegistrationOptions.t
    -> ?didRename:FileOperationRegistrationOptions.t
    -> ?willCreate:FileOperationRegistrationOptions.t
    -> ?willDelete:FileOperationRegistrationOptions.t
    -> ?willRename:FileOperationRegistrationOptions.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module FileRename : sig
  type t =
    { newUri : string
    ; oldUri : string
    }

  val create : newUri:string -> oldUri:string -> t

  include Json.Jsonable.S with type t := t
end

module FoldingRange : sig
  type t =
    { collapsedText : string option
    ; endCharacter : int option
    ; endLine : int
    ; kind : FoldingRangeKind.t option
    ; startCharacter : int option
    ; startLine : int
    }

  val create
    :  ?collapsedText:string
    -> ?endCharacter:int
    -> endLine:int
    -> ?kind:FoldingRangeKind.t
    -> ?startCharacter:int
    -> startLine:int
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module FoldingRangeOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module FoldingRangeParams : sig
  type t =
    { partialResultToken : ProgressToken.t option
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?partialResultToken:ProgressToken.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module FoldingRangeRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; id : string option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?id:string
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module Hover : sig
  type t =
    { contents :
        [ `MarkupContent of MarkupContent.t
        | `MarkedString of MarkedString.t
        | `List of MarkedString.t list
        ]
    ; range : Range.t option
    }

  val create
    :  contents:
         [ `MarkupContent of MarkupContent.t
         | `MarkedString of MarkedString.t
         | `List of MarkedString.t list
         ]
    -> ?range:Range.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module HoverOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module HoverParams : sig
  type t =
    { position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module HoverRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }

  val create : ?documentSelector:DocumentSelector.t -> ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module ImplementationOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module ImplementationParams : sig
  type t =
    { partialResultToken : ProgressToken.t option
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?partialResultToken:ProgressToken.t
    -> position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module ImplementationRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; id : string option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?id:string
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module InitializeError : sig
  type t = { retry : bool }

  val create : retry:bool -> t

  include Json.Jsonable.S with type t := t
end

module InitializeParams : sig
  type clientInfo =
    { name : string
    ; version : string option
    }

  val create_clientInfo : name:string -> ?version:string -> unit -> clientInfo

  type t =
    { capabilities : ClientCapabilities.t
    ; clientInfo : clientInfo option
    ; initializationOptions : Json.t option
    ; locale : string option
    ; processId : int option
    ; rootPath : string option option
    ; rootUri : DocumentUri.t option
    ; trace : TraceValues.t option
    ; workDoneToken : ProgressToken.t option
    ; workspaceFolders : WorkspaceFolder.t list option option
    }

  val create
    :  capabilities:ClientCapabilities.t
    -> ?clientInfo:clientInfo
    -> ?initializationOptions:Json.t
    -> ?locale:string
    -> ?processId:int
    -> ?rootPath:string option
    -> ?rootUri:DocumentUri.t
    -> ?trace:TraceValues.t
    -> ?workDoneToken:ProgressToken.t
    -> ?workspaceFolders:WorkspaceFolder.t list option
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module WorkspaceSymbolOptions : sig
  type t =
    { resolveProvider : bool option
    ; workDoneProgress : bool option
    }

  val create : ?resolveProvider:bool -> ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module WorkspaceFoldersServerCapabilities : sig
  type t =
    { changeNotifications : [ `String of string | `Bool of bool ] option
    ; supported : bool option
    }

  val create
    :  ?changeNotifications:[ `String of string | `Bool of bool ]
    -> ?supported:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module TypeHierarchyRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; id : string option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?id:string
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module TypeHierarchyOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module TypeDefinitionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; id : string option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?id:string
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module TypeDefinitionOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module SaveOptions : sig
  type t = { includeText : bool option }

  val create : ?includeText:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module TextDocumentSyncOptions : sig
  type t =
    { change : TextDocumentSyncKind.t option
    ; openClose : bool option
    ; save : [ `Bool of bool | `SaveOptions of SaveOptions.t ] option
    ; willSave : bool option
    ; willSaveWaitUntil : bool option
    }

  val create
    :  ?change:TextDocumentSyncKind.t
    -> ?openClose:bool
    -> ?save:[ `Bool of bool | `SaveOptions of SaveOptions.t ]
    -> ?willSave:bool
    -> ?willSaveWaitUntil:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SignatureHelpOptions : sig
  type t =
    { retriggerCharacters : string list option
    ; triggerCharacters : string list option
    ; workDoneProgress : bool option
    }

  val create
    :  ?retriggerCharacters:string list
    -> ?triggerCharacters:string list
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SemanticTokensLegend : sig
  type t =
    { tokenModifiers : string list
    ; tokenTypes : string list
    }

  val create : tokenModifiers:string list -> tokenTypes:string list -> t

  include Json.Jsonable.S with type t := t
end

module SemanticTokensRegistrationOptions : sig
  type full = { delta : bool option }

  val create_full : ?delta:bool -> unit -> full

  type t =
    { documentSelector : DocumentSelector.t option
    ; full : [ `Bool of bool | `Full of full ] option
    ; id : string option
    ; legend : SemanticTokensLegend.t
    ; range : bool option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?full:[ `Bool of bool | `Full of full ]
    -> ?id:string
    -> legend:SemanticTokensLegend.t
    -> ?range:bool
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SemanticTokensOptions : sig
  type full = { delta : bool option }

  val create_full : ?delta:bool -> unit -> full

  type t =
    { full : [ `Bool of bool | `Full of full ] option
    ; legend : SemanticTokensLegend.t
    ; range : bool option
    ; workDoneProgress : bool option
    }

  val create
    :  ?full:[ `Bool of bool | `Full of full ]
    -> legend:SemanticTokensLegend.t
    -> ?range:bool
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SelectionRangeRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; id : string option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?id:string
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SelectionRangeOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module RenameOptions : sig
  type t =
    { prepareProvider : bool option
    ; workDoneProgress : bool option
    }

  val create : ?prepareProvider:bool -> ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module ReferenceOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module MonikerRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }

  val create : ?documentSelector:DocumentSelector.t -> ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module MonikerOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module LinkedEditingRangeRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; id : string option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?id:string
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module LinkedEditingRangeOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module InlineValueRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; id : string option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?id:string
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module InlineValueOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module InlineCompletionOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module InlayHintRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; id : string option
    ; resolveProvider : bool option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?id:string
    -> ?resolveProvider:bool
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module InlayHintOptions : sig
  type t =
    { resolveProvider : bool option
    ; workDoneProgress : bool option
    }

  val create : ?resolveProvider:bool -> ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module ServerCapabilities : sig
  type workspace =
    { workspaceFolders : WorkspaceFoldersServerCapabilities.t option
    ; fileOperations : FileOperationOptions.t option
    }

  val create_workspace
    :  ?workspaceFolders:WorkspaceFoldersServerCapabilities.t
    -> ?fileOperations:FileOperationOptions.t
    -> unit
    -> workspace

  type diagnostic = { markupMessageSupport : bool option }

  val create_diagnostic : ?markupMessageSupport:bool -> unit -> diagnostic

  type textDocument = { diagnostic : diagnostic option }

  val create_textDocument : ?diagnostic:diagnostic -> unit -> textDocument

  type t =
    { callHierarchyProvider :
        [ `Bool of bool
        | `CallHierarchyOptions of CallHierarchyOptions.t
        | `CallHierarchyRegistrationOptions of CallHierarchyRegistrationOptions.t
        ]
          option
    ; codeActionProvider :
        [ `Bool of bool | `CodeActionOptions of CodeActionOptions.t ] option
    ; codeLensProvider : CodeLensOptions.t option
    ; colorProvider :
        [ `Bool of bool
        | `DocumentColorOptions of DocumentColorOptions.t
        | `DocumentColorRegistrationOptions of DocumentColorRegistrationOptions.t
        ]
          option
    ; completionProvider : CompletionOptions.t option
    ; declarationProvider :
        [ `Bool of bool
        | `DeclarationOptions of DeclarationOptions.t
        | `DeclarationRegistrationOptions of DeclarationRegistrationOptions.t
        ]
          option
    ; definitionProvider :
        [ `Bool of bool | `DefinitionOptions of DefinitionOptions.t ] option
    ; diagnosticProvider :
        [ `DiagnosticOptions of DiagnosticOptions.t
        | `DiagnosticRegistrationOptions of DiagnosticRegistrationOptions.t
        ]
          option
    ; documentFormattingProvider :
        [ `Bool of bool | `DocumentFormattingOptions of DocumentFormattingOptions.t ]
          option
    ; documentHighlightProvider :
        [ `Bool of bool | `DocumentHighlightOptions of DocumentHighlightOptions.t ] option
    ; documentLinkProvider : DocumentLinkOptions.t option
    ; documentOnTypeFormattingProvider : DocumentOnTypeFormattingOptions.t option
    ; documentRangeFormattingProvider :
        [ `Bool of bool
        | `DocumentRangeFormattingOptions of DocumentRangeFormattingOptions.t
        ]
          option
    ; documentSymbolProvider :
        [ `Bool of bool | `DocumentSymbolOptions of DocumentSymbolOptions.t ] option
    ; executeCommandProvider : ExecuteCommandOptions.t option
    ; experimental : Json.t option
    ; foldingRangeProvider :
        [ `Bool of bool
        | `FoldingRangeOptions of FoldingRangeOptions.t
        | `FoldingRangeRegistrationOptions of FoldingRangeRegistrationOptions.t
        ]
          option
    ; hoverProvider : [ `Bool of bool | `HoverOptions of HoverOptions.t ] option
    ; implementationProvider :
        [ `Bool of bool
        | `ImplementationOptions of ImplementationOptions.t
        | `ImplementationRegistrationOptions of ImplementationRegistrationOptions.t
        ]
          option
    ; inlayHintProvider :
        [ `Bool of bool
        | `InlayHintOptions of InlayHintOptions.t
        | `InlayHintRegistrationOptions of InlayHintRegistrationOptions.t
        ]
          option
    ; inlineCompletionProvider :
        [ `Bool of bool | `InlineCompletionOptions of InlineCompletionOptions.t ] option
    ; inlineValueProvider :
        [ `Bool of bool
        | `InlineValueOptions of InlineValueOptions.t
        | `InlineValueRegistrationOptions of InlineValueRegistrationOptions.t
        ]
          option
    ; linkedEditingRangeProvider :
        [ `Bool of bool
        | `LinkedEditingRangeOptions of LinkedEditingRangeOptions.t
        | `LinkedEditingRangeRegistrationOptions of
          LinkedEditingRangeRegistrationOptions.t
        ]
          option
    ; monikerProvider :
        [ `Bool of bool
        | `MonikerOptions of MonikerOptions.t
        | `MonikerRegistrationOptions of MonikerRegistrationOptions.t
        ]
          option
    ; notebookDocumentSync :
        [ `NotebookDocumentSyncOptions of NotebookDocumentSyncOptions.t
        | `NotebookDocumentSyncRegistrationOptions of
          NotebookDocumentSyncRegistrationOptions.t
        ]
          option
    ; positionEncoding : PositionEncodingKind.t option
    ; referencesProvider :
        [ `Bool of bool | `ReferenceOptions of ReferenceOptions.t ] option
    ; renameProvider : [ `Bool of bool | `RenameOptions of RenameOptions.t ] option
    ; selectionRangeProvider :
        [ `Bool of bool
        | `SelectionRangeOptions of SelectionRangeOptions.t
        | `SelectionRangeRegistrationOptions of SelectionRangeRegistrationOptions.t
        ]
          option
    ; semanticTokensProvider :
        [ `SemanticTokensOptions of SemanticTokensOptions.t
        | `SemanticTokensRegistrationOptions of SemanticTokensRegistrationOptions.t
        ]
          option
    ; signatureHelpProvider : SignatureHelpOptions.t option
    ; textDocument : textDocument option
    ; textDocumentSync :
        [ `TextDocumentSyncOptions of TextDocumentSyncOptions.t
        | `TextDocumentSyncKind of TextDocumentSyncKind.t
        ]
          option
    ; typeDefinitionProvider :
        [ `Bool of bool
        | `TypeDefinitionOptions of TypeDefinitionOptions.t
        | `TypeDefinitionRegistrationOptions of TypeDefinitionRegistrationOptions.t
        ]
          option
    ; typeHierarchyProvider :
        [ `Bool of bool
        | `TypeHierarchyOptions of TypeHierarchyOptions.t
        | `TypeHierarchyRegistrationOptions of TypeHierarchyRegistrationOptions.t
        ]
          option
    ; workspace : workspace option
    ; workspaceSymbolProvider :
        [ `Bool of bool | `WorkspaceSymbolOptions of WorkspaceSymbolOptions.t ] option
    }

  val create
    :  ?callHierarchyProvider:
         [ `Bool of bool
         | `CallHierarchyOptions of CallHierarchyOptions.t
         | `CallHierarchyRegistrationOptions of CallHierarchyRegistrationOptions.t
         ]
    -> ?codeActionProvider:[ `Bool of bool | `CodeActionOptions of CodeActionOptions.t ]
    -> ?codeLensProvider:CodeLensOptions.t
    -> ?colorProvider:
         [ `Bool of bool
         | `DocumentColorOptions of DocumentColorOptions.t
         | `DocumentColorRegistrationOptions of DocumentColorRegistrationOptions.t
         ]
    -> ?completionProvider:CompletionOptions.t
    -> ?declarationProvider:
         [ `Bool of bool
         | `DeclarationOptions of DeclarationOptions.t
         | `DeclarationRegistrationOptions of DeclarationRegistrationOptions.t
         ]
    -> ?definitionProvider:[ `Bool of bool | `DefinitionOptions of DefinitionOptions.t ]
    -> ?diagnosticProvider:
         [ `DiagnosticOptions of DiagnosticOptions.t
         | `DiagnosticRegistrationOptions of DiagnosticRegistrationOptions.t
         ]
    -> ?documentFormattingProvider:
         [ `Bool of bool | `DocumentFormattingOptions of DocumentFormattingOptions.t ]
    -> ?documentHighlightProvider:
         [ `Bool of bool | `DocumentHighlightOptions of DocumentHighlightOptions.t ]
    -> ?documentLinkProvider:DocumentLinkOptions.t
    -> ?documentOnTypeFormattingProvider:DocumentOnTypeFormattingOptions.t
    -> ?documentRangeFormattingProvider:
         [ `Bool of bool
         | `DocumentRangeFormattingOptions of DocumentRangeFormattingOptions.t
         ]
    -> ?documentSymbolProvider:
         [ `Bool of bool | `DocumentSymbolOptions of DocumentSymbolOptions.t ]
    -> ?executeCommandProvider:ExecuteCommandOptions.t
    -> ?experimental:Json.t
    -> ?foldingRangeProvider:
         [ `Bool of bool
         | `FoldingRangeOptions of FoldingRangeOptions.t
         | `FoldingRangeRegistrationOptions of FoldingRangeRegistrationOptions.t
         ]
    -> ?hoverProvider:[ `Bool of bool | `HoverOptions of HoverOptions.t ]
    -> ?implementationProvider:
         [ `Bool of bool
         | `ImplementationOptions of ImplementationOptions.t
         | `ImplementationRegistrationOptions of ImplementationRegistrationOptions.t
         ]
    -> ?inlayHintProvider:
         [ `Bool of bool
         | `InlayHintOptions of InlayHintOptions.t
         | `InlayHintRegistrationOptions of InlayHintRegistrationOptions.t
         ]
    -> ?inlineCompletionProvider:
         [ `Bool of bool | `InlineCompletionOptions of InlineCompletionOptions.t ]
    -> ?inlineValueProvider:
         [ `Bool of bool
         | `InlineValueOptions of InlineValueOptions.t
         | `InlineValueRegistrationOptions of InlineValueRegistrationOptions.t
         ]
    -> ?linkedEditingRangeProvider:
         [ `Bool of bool
         | `LinkedEditingRangeOptions of LinkedEditingRangeOptions.t
         | `LinkedEditingRangeRegistrationOptions of
           LinkedEditingRangeRegistrationOptions.t
         ]
    -> ?monikerProvider:
         [ `Bool of bool
         | `MonikerOptions of MonikerOptions.t
         | `MonikerRegistrationOptions of MonikerRegistrationOptions.t
         ]
    -> ?notebookDocumentSync:
         [ `NotebookDocumentSyncOptions of NotebookDocumentSyncOptions.t
         | `NotebookDocumentSyncRegistrationOptions of
           NotebookDocumentSyncRegistrationOptions.t
         ]
    -> ?positionEncoding:PositionEncodingKind.t
    -> ?referencesProvider:[ `Bool of bool | `ReferenceOptions of ReferenceOptions.t ]
    -> ?renameProvider:[ `Bool of bool | `RenameOptions of RenameOptions.t ]
    -> ?selectionRangeProvider:
         [ `Bool of bool
         | `SelectionRangeOptions of SelectionRangeOptions.t
         | `SelectionRangeRegistrationOptions of SelectionRangeRegistrationOptions.t
         ]
    -> ?semanticTokensProvider:
         [ `SemanticTokensOptions of SemanticTokensOptions.t
         | `SemanticTokensRegistrationOptions of SemanticTokensRegistrationOptions.t
         ]
    -> ?signatureHelpProvider:SignatureHelpOptions.t
    -> ?textDocument:textDocument
    -> ?textDocumentSync:
         [ `TextDocumentSyncOptions of TextDocumentSyncOptions.t
         | `TextDocumentSyncKind of TextDocumentSyncKind.t
         ]
    -> ?typeDefinitionProvider:
         [ `Bool of bool
         | `TypeDefinitionOptions of TypeDefinitionOptions.t
         | `TypeDefinitionRegistrationOptions of TypeDefinitionRegistrationOptions.t
         ]
    -> ?typeHierarchyProvider:
         [ `Bool of bool
         | `TypeHierarchyOptions of TypeHierarchyOptions.t
         | `TypeHierarchyRegistrationOptions of TypeHierarchyRegistrationOptions.t
         ]
    -> ?workspace:workspace
    -> ?workspaceSymbolProvider:
         [ `Bool of bool | `WorkspaceSymbolOptions of WorkspaceSymbolOptions.t ]
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module InitializeResult : sig
  type serverInfo =
    { name : string
    ; version : string option
    }

  val create_serverInfo : name:string -> ?version:string -> unit -> serverInfo

  type t =
    { capabilities : ServerCapabilities.t
    ; serverInfo : serverInfo option
    }

  val create : capabilities:ServerCapabilities.t -> ?serverInfo:serverInfo -> unit -> t

  include Json.Jsonable.S with type t := t
end

module InitializedParams_ : sig
  type clientInfo =
    { name : string
    ; version : string option
    }

  val create_clientInfo : name:string -> ?version:string -> unit -> clientInfo

  type t =
    { capabilities : ClientCapabilities.t
    ; clientInfo : clientInfo option
    ; initializationOptions : Json.t option
    ; locale : string option
    ; processId : int option
    ; rootPath : string option option
    ; rootUri : DocumentUri.t option
    ; trace : TraceValues.t option
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  capabilities:ClientCapabilities.t
    -> ?clientInfo:clientInfo
    -> ?initializationOptions:Json.t
    -> ?locale:string
    -> ?processId:int
    -> ?rootPath:string option
    -> ?rootUri:DocumentUri.t
    -> ?trace:TraceValues.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module InlayHintLabelPart : sig
  type t =
    { command : Command.t option
    ; location : Location.t option
    ; tooltip : [ `String of string | `MarkupContent of MarkupContent.t ] option
    ; value : string
    }

  val create
    :  ?command:Command.t
    -> ?location:Location.t
    -> ?tooltip:[ `String of string | `MarkupContent of MarkupContent.t ]
    -> value:string
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module InlayHint : sig
  type t =
    { data : Json.t option
    ; kind : InlayHintKind.t option
    ; label : [ `String of string | `List of InlayHintLabelPart.t list ]
    ; paddingLeft : bool option
    ; paddingRight : bool option
    ; position : Position.t
    ; textEdits : TextEdit.t list option
    ; tooltip : [ `String of string | `MarkupContent of MarkupContent.t ] option
    }

  val create
    :  ?data:Json.t
    -> ?kind:InlayHintKind.t
    -> label:[ `String of string | `List of InlayHintLabelPart.t list ]
    -> ?paddingLeft:bool
    -> ?paddingRight:bool
    -> position:Position.t
    -> ?textEdits:TextEdit.t list
    -> ?tooltip:[ `String of string | `MarkupContent of MarkupContent.t ]
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module InlayHintParams : sig
  type t =
    { range : Range.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  range:Range.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SelectedCompletionInfo : sig
  type t =
    { range : Range.t
    ; text : string
    }

  val create : range:Range.t -> text:string -> t

  include Json.Jsonable.S with type t := t
end

module InlineCompletionContext : sig
  type t =
    { selectedCompletionInfo : SelectedCompletionInfo.t option
    ; triggerKind : InlineCompletionTriggerKind.t
    }

  val create
    :  ?selectedCompletionInfo:SelectedCompletionInfo.t
    -> triggerKind:InlineCompletionTriggerKind.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module StringValue : sig
  type t = { value : string }

  val create : value:string -> t

  include Json.Jsonable.S with type t := t
end

module InlineCompletionItem : sig
  type t =
    { command : Command.t option
    ; filterText : string option
    ; insertText : [ `String of string | `StringValue of StringValue.t ]
    ; range : Range.t option
    }

  val create
    :  ?command:Command.t
    -> ?filterText:string
    -> insertText:[ `String of string | `StringValue of StringValue.t ]
    -> ?range:Range.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module InlineCompletionList : sig
  type t = { items : InlineCompletionItem.t list }

  val create : items:InlineCompletionItem.t list -> t

  include Json.Jsonable.S with type t := t
end

module InlineCompletionParams : sig
  type t =
    { context : InlineCompletionContext.t
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  context:InlineCompletionContext.t
    -> position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module InlineCompletionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; id : string option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?id:string
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module InlineValueEvaluatableExpression : sig
  type t =
    { expression : string option
    ; range : Range.t
    }

  val create : ?expression:string -> range:Range.t -> unit -> t

  include Json.Jsonable.S with type t := t
end

module InlineValueVariableLookup : sig
  type t =
    { caseSensitiveLookup : bool
    ; range : Range.t
    ; variableName : string option
    }

  val create
    :  caseSensitiveLookup:bool
    -> range:Range.t
    -> ?variableName:string
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module InlineValueText : sig
  type t =
    { range : Range.t
    ; text : string
    }

  val create : range:Range.t -> text:string -> t

  include Json.Jsonable.S with type t := t
end

module InlineValue : sig
  type t =
    [ `InlineValueText of InlineValueText.t
    | `InlineValueVariableLookup of InlineValueVariableLookup.t
    | `InlineValueEvaluatableExpression of InlineValueEvaluatableExpression.t
    ]

  include Json.Jsonable.S with type t := t
end

module InlineValueContext : sig
  type t =
    { frameId : int
    ; stoppedLocation : Range.t
    }

  val create : frameId:int -> stoppedLocation:Range.t -> t

  include Json.Jsonable.S with type t := t
end

module InlineValueParams : sig
  type t =
    { context : InlineValueContext.t
    ; range : Range.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  context:InlineValueContext.t
    -> range:Range.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module LinkedEditingRangeParams : sig
  type t =
    { position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module LinkedEditingRanges : sig
  type t =
    { ranges : Range.t list
    ; wordPattern : string option
    }

  val create : ranges:Range.t list -> ?wordPattern:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module LogMessageParams : sig
  type t =
    { message : string
    ; type_ : MessageType.t
    }

  val create : message:string -> type_:MessageType.t -> t

  include Json.Jsonable.S with type t := t
end

module LogTraceParams : sig
  type t =
    { message : string
    ; verbose : string option
    }

  val create : message:string -> ?verbose:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module MessageActionItem : sig
  type t = { title : string }

  val create : title:string -> t

  include Json.Jsonable.S with type t := t
end

module Moniker : sig
  type t =
    { identifier : string
    ; kind : MonikerKind.t option
    ; scheme : string
    ; unique : UniquenessLevel.t
    }

  val create
    :  identifier:string
    -> ?kind:MonikerKind.t
    -> scheme:string
    -> unique:UniquenessLevel.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module MonikerParams : sig
  type t =
    { partialResultToken : ProgressToken.t option
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?partialResultToken:ProgressToken.t
    -> position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module ParameterInformation : sig
  type t =
    { documentation : [ `String of string | `MarkupContent of MarkupContent.t ] option
    ; label : [ `String of string | `Offset of int * int ]
    }

  val create
    :  ?documentation:[ `String of string | `MarkupContent of MarkupContent.t ]
    -> label:[ `String of string | `Offset of int * int ]
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module PartialResultParams : sig
  type t = { partialResultToken : ProgressToken.t option }

  val create : ?partialResultToken:ProgressToken.t -> unit -> t

  include Json.Jsonable.S with type t := t
end

module PrepareRenameParams : sig
  type t =
    { position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module PreviousResultId : sig
  type t =
    { uri : DocumentUri.t
    ; value : string
    }

  val create : uri:DocumentUri.t -> value:string -> t

  include Json.Jsonable.S with type t := t
end

module PublishDiagnosticsParams : sig
  type t =
    { diagnostics : Diagnostic.t list
    ; uri : DocumentUri.t
    ; version : int option
    }

  val create
    :  diagnostics:Diagnostic.t list
    -> uri:DocumentUri.t
    -> ?version:int
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module ReferenceContext : sig
  type t = { includeDeclaration : bool }

  val create : includeDeclaration:bool -> t

  include Json.Jsonable.S with type t := t
end

module ReferenceParams : sig
  type t =
    { context : ReferenceContext.t
    ; partialResultToken : ProgressToken.t option
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  context:ReferenceContext.t
    -> ?partialResultToken:ProgressToken.t
    -> position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module ReferenceRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }

  val create : ?documentSelector:DocumentSelector.t -> ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module Registration : sig
  type t =
    { id : string
    ; method_ : string
    ; registerOptions : Json.t option
    }

  val create : id:string -> method_:string -> ?registerOptions:Json.t -> unit -> t

  include Json.Jsonable.S with type t := t
end

module RegistrationParams : sig
  type t = { registrations : Registration.t list }

  val create : registrations:Registration.t list -> t

  include Json.Jsonable.S with type t := t
end

module RenameFilesParams : sig
  type t = { files : FileRename.t list }

  val create : files:FileRename.t list -> t

  include Json.Jsonable.S with type t := t
end

module RenameParams : sig
  type t =
    { newName : string
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  newName:string
    -> position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module RenameRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; prepareProvider : bool option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?prepareProvider:bool
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module ResourceOperation : sig
  type t =
    { annotationId : ChangeAnnotationIdentifier.t option
    ; kind : string
    }

  val create : ?annotationId:ChangeAnnotationIdentifier.t -> kind:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module SelectionRange : sig
  type t =
    { parent : t option
    ; range : Range.t
    }

  val create : ?parent:t -> range:Range.t -> unit -> t

  include Json.Jsonable.S with type t := t
end

module SelectionRangeParams : sig
  type t =
    { partialResultToken : ProgressToken.t option
    ; positions : Position.t list
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?partialResultToken:ProgressToken.t
    -> positions:Position.t list
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SemanticTokens : sig
  type t =
    { data : int array
    ; resultId : string option
    }

  val create : data:int array -> ?resultId:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module SemanticTokensEdit : sig
  type t =
    { data : int array option
    ; deleteCount : int
    ; start : int
    }

  val create : ?data:int array -> deleteCount:int -> start:int -> unit -> t

  include Json.Jsonable.S with type t := t
end

module SemanticTokensDelta : sig
  type t =
    { edits : SemanticTokensEdit.t list
    ; resultId : string option
    }

  val create : edits:SemanticTokensEdit.t list -> ?resultId:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module SemanticTokensDeltaParams : sig
  type t =
    { partialResultToken : ProgressToken.t option
    ; previousResultId : string
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?partialResultToken:ProgressToken.t
    -> previousResultId:string
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SemanticTokensDeltaPartialResult : sig
  type t = { edits : SemanticTokensEdit.t list }

  val create : edits:SemanticTokensEdit.t list -> t

  include Json.Jsonable.S with type t := t
end

module SemanticTokensParams : sig
  type t =
    { partialResultToken : ProgressToken.t option
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?partialResultToken:ProgressToken.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SemanticTokensPartialResult : sig
  type t = { data : int array }

  val create : data:int array -> t

  include Json.Jsonable.S with type t := t
end

module SemanticTokensRangeParams : sig
  type t =
    { partialResultToken : ProgressToken.t option
    ; range : Range.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?partialResultToken:ProgressToken.t
    -> range:Range.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SetTraceParams : sig
  type t = { value : TraceValues.t }

  val create : value:TraceValues.t -> t

  include Json.Jsonable.S with type t := t
end

module ShowDocumentParams : sig
  type t =
    { external_ : bool option
    ; selection : Range.t option
    ; takeFocus : bool option
    ; uri : DocumentUri.t
    }

  val create
    :  ?external_:bool
    -> ?selection:Range.t
    -> ?takeFocus:bool
    -> uri:DocumentUri.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module ShowDocumentResult : sig
  type t = { success : bool }

  val create : success:bool -> t

  include Json.Jsonable.S with type t := t
end

module ShowMessageParams : sig
  type t =
    { message : string
    ; type_ : MessageType.t
    }

  val create : message:string -> type_:MessageType.t -> t

  include Json.Jsonable.S with type t := t
end

module ShowMessageRequestParams : sig
  type t =
    { actions : MessageActionItem.t list option
    ; message : string
    ; type_ : MessageType.t
    }

  val create
    :  ?actions:MessageActionItem.t list
    -> message:string
    -> type_:MessageType.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SignatureInformation : sig
  type t =
    { activeParameter : int option option
    ; documentation : [ `String of string | `MarkupContent of MarkupContent.t ] option
    ; label : string
    ; parameters : ParameterInformation.t list option
    }

  val create
    :  ?activeParameter:int option
    -> ?documentation:[ `String of string | `MarkupContent of MarkupContent.t ]
    -> label:string
    -> ?parameters:ParameterInformation.t list
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SignatureHelp : sig
  type t =
    { activeParameter : int option option
    ; activeSignature : int option
    ; signatures : SignatureInformation.t list
    }

  val create
    :  ?activeParameter:int option
    -> ?activeSignature:int
    -> signatures:SignatureInformation.t list
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SignatureHelpContext : sig
  type t =
    { activeSignatureHelp : SignatureHelp.t option
    ; isRetrigger : bool
    ; triggerCharacter : string option
    ; triggerKind : SignatureHelpTriggerKind.t
    }

  val create
    :  ?activeSignatureHelp:SignatureHelp.t
    -> isRetrigger:bool
    -> ?triggerCharacter:string
    -> triggerKind:SignatureHelpTriggerKind.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SignatureHelpParams : sig
  type t =
    { context : SignatureHelpContext.t option
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?context:SignatureHelpContext.t
    -> position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module SignatureHelpRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; retriggerCharacters : string list option
    ; triggerCharacters : string list option
    ; workDoneProgress : bool option
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> ?retriggerCharacters:string list
    -> ?triggerCharacters:string list
    -> ?workDoneProgress:bool
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module StaticRegistrationOptions : sig
  type t = { id : string option }

  val create : ?id:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module SymbolInformation : sig
  type t =
    { containerName : string option
    ; deprecated : bool option
    ; kind : SymbolKind.t
    ; location : Location.t
    ; name : string
    ; tags : SymbolTag.t list option
    }

  val create
    :  ?containerName:string
    -> ?deprecated:bool
    -> kind:SymbolKind.t
    -> location:Location.t
    -> name:string
    -> ?tags:SymbolTag.t list
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module TextDocumentChangeRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; syncKind : TextDocumentSyncKind.t
    }

  val create
    :  ?documentSelector:DocumentSelector.t
    -> syncKind:TextDocumentSyncKind.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module TextDocumentPositionParams : sig
  type t =
    { position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    }

  val create : position:Position.t -> textDocument:TextDocumentIdentifier.t -> t

  include Json.Jsonable.S with type t := t
end

module TextDocumentRegistrationOptions : sig
  type t = { documentSelector : DocumentSelector.t option }

  val create : ?documentSelector:DocumentSelector.t -> unit -> t

  include Json.Jsonable.S with type t := t
end

module TextDocumentSaveRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; includeText : bool option
    }

  val create : ?documentSelector:DocumentSelector.t -> ?includeText:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module TypeDefinitionParams : sig
  type t =
    { partialResultToken : ProgressToken.t option
    ; position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?partialResultToken:ProgressToken.t
    -> position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module TypeHierarchyItem : sig
  type t =
    { data : Json.t option
    ; detail : string option
    ; kind : SymbolKind.t
    ; name : string
    ; range : Range.t
    ; selectionRange : Range.t
    ; tags : SymbolTag.t list option
    ; uri : DocumentUri.t
    }

  val create
    :  ?data:Json.t
    -> ?detail:string
    -> kind:SymbolKind.t
    -> name:string
    -> range:Range.t
    -> selectionRange:Range.t
    -> ?tags:SymbolTag.t list
    -> uri:DocumentUri.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module TypeHierarchyPrepareParams : sig
  type t =
    { position : Position.t
    ; textDocument : TextDocumentIdentifier.t
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  position:Position.t
    -> textDocument:TextDocumentIdentifier.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module TypeHierarchySubtypesParams : sig
  type t =
    { item : TypeHierarchyItem.t
    ; partialResultToken : ProgressToken.t option
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  item:TypeHierarchyItem.t
    -> ?partialResultToken:ProgressToken.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module TypeHierarchySupertypesParams : sig
  type t =
    { item : TypeHierarchyItem.t
    ; partialResultToken : ProgressToken.t option
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  item:TypeHierarchyItem.t
    -> ?partialResultToken:ProgressToken.t
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module Unregistration : sig
  type t =
    { id : string
    ; method_ : string
    }

  val create : id:string -> method_:string -> t

  include Json.Jsonable.S with type t := t
end

module UnregistrationParams : sig
  type t = { unregisterations : Unregistration.t list }

  val create : unregisterations:Unregistration.t list -> t

  include Json.Jsonable.S with type t := t
end

module WillSaveTextDocumentParams : sig
  type t =
    { reason : TextDocumentSaveReason.t
    ; textDocument : TextDocumentIdentifier.t
    }

  val create
    :  reason:TextDocumentSaveReason.t
    -> textDocument:TextDocumentIdentifier.t
    -> t

  include Json.Jsonable.S with type t := t
end

module WorkDoneProgressBegin : sig
  type t =
    { cancellable : bool option
    ; message : string option
    ; percentage : int option
    ; title : string
    }

  val create
    :  ?cancellable:bool
    -> ?message:string
    -> ?percentage:int
    -> title:string
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module WorkDoneProgressCancelParams : sig
  type t = { token : ProgressToken.t }

  val create : token:ProgressToken.t -> t

  include Json.Jsonable.S with type t := t
end

module WorkDoneProgressCreateParams : sig
  type t = { token : ProgressToken.t }

  val create : token:ProgressToken.t -> t

  include Json.Jsonable.S with type t := t
end

module WorkDoneProgressEnd : sig
  type t = { message : string option }

  val create : ?message:string -> unit -> t

  include Json.Jsonable.S with type t := t
end

module WorkDoneProgressOptions : sig
  type t = { workDoneProgress : bool option }

  val create : ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end

module WorkDoneProgressParams : sig
  type t = { workDoneToken : ProgressToken.t option }

  val create : ?workDoneToken:ProgressToken.t -> unit -> t

  include Json.Jsonable.S with type t := t
end

module WorkDoneProgressReport : sig
  type t =
    { cancellable : bool option
    ; message : string option
    ; percentage : int option
    }

  val create : ?cancellable:bool -> ?message:string -> ?percentage:int -> unit -> t

  include Json.Jsonable.S with type t := t
end

module WorkspaceDiagnosticParams : sig
  type t =
    { identifier : string option
    ; partialResultToken : ProgressToken.t option
    ; previousResultIds : PreviousResultId.t list
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?identifier:string
    -> ?partialResultToken:ProgressToken.t
    -> previousResultIds:PreviousResultId.t list
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module WorkspaceUnchangedDocumentDiagnosticReport : sig
  type t =
    { resultId : string
    ; uri : DocumentUri.t
    ; version : int option
    }

  val create : resultId:string -> uri:DocumentUri.t -> ?version:int -> unit -> t

  include Json.Jsonable.S with type t := t
end

module WorkspaceFullDocumentDiagnosticReport : sig
  type t =
    { items : Diagnostic.t list
    ; resultId : string option
    ; uri : DocumentUri.t
    ; version : int option
    }

  val create
    :  items:Diagnostic.t list
    -> ?resultId:string
    -> uri:DocumentUri.t
    -> ?version:int
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module WorkspaceDocumentDiagnosticReport : sig
  type t =
    [ `WorkspaceFullDocumentDiagnosticReport of WorkspaceFullDocumentDiagnosticReport.t
    | `WorkspaceUnchangedDocumentDiagnosticReport of
      WorkspaceUnchangedDocumentDiagnosticReport.t
    ]

  include Json.Jsonable.S with type t := t
end

module WorkspaceDiagnosticReport : sig
  type t = { items : WorkspaceDocumentDiagnosticReport.t list }

  val create : items:WorkspaceDocumentDiagnosticReport.t list -> t

  include Json.Jsonable.S with type t := t
end

module WorkspaceDiagnosticReportPartialResult : sig
  type t = { items : WorkspaceDocumentDiagnosticReport.t list }

  val create : items:WorkspaceDocumentDiagnosticReport.t list -> t

  include Json.Jsonable.S with type t := t
end

module WorkspaceFoldersInitializeParams : sig
  type t = { workspaceFolders : WorkspaceFolder.t list option option }

  val create : ?workspaceFolders:WorkspaceFolder.t list option -> unit -> t

  include Json.Jsonable.S with type t := t
end

module WorkspaceSymbol : sig
  type t =
    { containerName : string option
    ; data : Json.t option
    ; kind : SymbolKind.t
    ; location : Location.t
    ; name : string
    ; tags : SymbolTag.t list option
    }

  val create
    :  ?containerName:string
    -> ?data:Json.t
    -> kind:SymbolKind.t
    -> location:Location.t
    -> name:string
    -> ?tags:SymbolTag.t list
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module WorkspaceSymbolParams : sig
  type t =
    { partialResultToken : ProgressToken.t option
    ; query : string
    ; workDoneToken : ProgressToken.t option
    }

  val create
    :  ?partialResultToken:ProgressToken.t
    -> query:string
    -> ?workDoneToken:ProgressToken.t
    -> unit
    -> t

  include Json.Jsonable.S with type t := t
end

module WorkspaceSymbolRegistrationOptions : sig
  type t =
    { resolveProvider : bool option
    ; workDoneProgress : bool option
    }

  val create : ?resolveProvider:bool -> ?workDoneProgress:bool -> unit -> t

  include Json.Jsonable.S with type t := t
end
(*$*)

module CodeActionResult : sig
  type t = [ `Command of Command.t | `CodeAction of CodeAction.t ] list option

  include Json.Jsonable.S with type t := t
end

module Locations : sig
  type t =
    [ `Location of Location.t list
    | `LocationLink of LocationLink.t list
    ]

  include Json.Jsonable.S with type t := t
end

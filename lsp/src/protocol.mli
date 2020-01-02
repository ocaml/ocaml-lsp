open Import

module Only : sig
  type 'a t =
    | All
    | Only of 'a list
end

module Or_bool : sig
  type 'a t =
    | Bool of bool
    | Value of 'a

  include Yojsonable.S1 with type 'a t := 'a t
end

module Or_string : sig
  type 'a t =
    | String of string
    | Value of 'a

  include Yojsonable.S1 with type 'a t := 'a t
end

module Void : Yojsonable.S

type documentUri = Uri.t

val documentUri_of_yojson : json -> documentUri

val yojson_of_documentUri : documentUri -> json

module Position : sig
  type t =
    { line : int
    ; character : int
    }

  include Yojsonable.S with type t := t
end

module Range : sig
  type t =
    { start_ : Position.t
    ; end_ : Position.t
    }

  include Yojsonable.S with type t := t
end

module Command : sig
  type t =
    { title : string
    ; command : string
    }

  include Yojsonable.S with type t := t
end

module MarkupKind : sig
  type t =
    | Plaintext
    | Markdown

  include Yojsonable.S with type t := t
end

module MarkupContent : sig
  type t =
    { value : string
    ; kind : MarkupKind.t
    }

  include Yojsonable.S with type t := t
end

module Location : sig
  type t =
    { uri : Uri.t
    ; range : Range.t
    }

  include Yojsonable.S with type t := t
end

module LocationLink : sig
  type t =
    { originSelectionRange : Range.t option
    ; targetUri : documentUri
    ; targetrange : Range.t
    ; targetSelectionRange : Range.t
    }

  include Yojsonable.S with type t := t
end

module Locations : sig
  type t =
    | Location of Location.t
    | Locations of Location.t list
    | Location_links of LocationLink.t list
end

module TextDocumentIdentifier : sig
  type t = { uri : documentUri }

  include Yojsonable.S with type t := t
end

module VersionedTextDocumentIdentifier : sig
  type t =
    { uri : documentUri
    ; version : int
    }

  include Yojsonable.S with type t := t
end

module TextDocumentItem : sig
  type t =
    { uri : documentUri
    ; languageId : string
    ; version : int
    ; text : string
    }

  include Yojsonable.S with type t := t
end

module DidOpen : sig
  type params = didOpenTextDocumentParams

  and didOpenTextDocumentParams = { textDocument : TextDocumentItem.t }

  val params_of_yojson : json -> params

  val didOpenTextDocumentParams_of_yojson : json -> params

  val yojson_of_params : params -> json

  val yojson_of_didOpenTextDocumentParams : params -> json
end

module TextDocumentContentChangeEvent : sig
  type t =
    { range : Range.t option
    ; rangeLength : int option
    ; text : string
    }

  include Yojsonable.S with type t := t
end

module DidChangeTextDocumentParams : sig
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; contentChanges : TextDocumentContentChangeEvent.t list
    }

  include Yojsonable.S with type t := t
end

module TextDocumentPositionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }

  include Yojsonable.S with type t := t
end

module DocumentHighlight : sig
  type kind =
    | Text
    | Read
    | Write

  val yojson_of_kind : kind -> json

  val kind_of_yojson : json -> kind

  type t =
    { range : Range.t
    ; kind : kind option
    }

  include Yojsonable.S with type t := t
end

module TextEdit : sig
  type t =
    { range : Range.t
    ; newText : string
    }

  include Yojsonable.S with type t := t
end

module TextDocumentEdit : sig
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; edits : TextEdit.t list
    }

  include Yojsonable.S with type t := t
end

module CreateFileOptions : sig
  type t =
    { overwrite : bool option
    ; ignoreIfExists : bool option
    }
end

module CreateFile : sig
  type t =
    { uri : documentUri
    ; options : CreateFileOptions.t option
    }

  include Yojsonable.S with type t := t
end

module RenameFileOptions : sig
  type t =
    { overwrite : bool option
    ; ignoreIfExists : bool option
    }

  include Yojsonable.S with type t := t
end

module RenameFile : sig
  type t =
    { oldUri : documentUri
    ; newUri : documentUri
    ; options : RenameFileOptions.t option
    }

  include Yojsonable.S with type t := t
end

module DeleteFileOptions : sig
  type t =
    { recursive : bool option
    ; ignoreIfNotExists : bool option
    }

  include Yojsonable.S with type t := t
end

module DeleteFile : sig
  type t =
    { uri : documentUri
    ; options : DeleteFileOptions.t option
    }

  include Yojsonable.S with type t := t
end

module WorkspaceEdit : sig
  module DocumentChange : sig
    type t =
      | TextDocumentEdit of TextDocumentEdit.t
      | CreateFile of CreateFile.t
      | RenameFile of RenameFile.t
      | DeleteFile of DeleteFile.t

    val yojson_of_t : t -> json
  end

  type t =
    { changes : (Uri.t * TextEdit.t list) list
    ; documentChanges : DocumentChange.t list
    }

  val yojson_of_t : t -> json

  val empty : t

  val make :
       documentChanges:bool
    -> uri:documentUri
    -> version:int
    -> edits:TextEdit.t list
    -> t
end

module PublishDiagnostics : sig
  type diagnosticCode =
    | IntCode of int
    | StringCode of string
    | NoCode

  val yojson_of_diagnosticCode : diagnosticCode -> json

  val diagnosticCode_of_yojson : json -> diagnosticCode

  type diagnosticSeverity =
    | Error
    | Warning
    | Information
    | Hint

  val yojson_of_diagnosticSeverity : diagnosticSeverity -> json

  val diagnosticSeverity_of_yojson : json -> diagnosticSeverity

  type params = publishDiagnosticsParams

  and publishDiagnosticsParams =
    { uri : documentUri
    ; diagnostics : diagnostic list
    }

  and diagnostic =
    { range : Range.t
    ; severity : diagnosticSeverity option
    ; code : diagnosticCode
    ; source : string option
    ; message : string
    ; relatedInformation : diagnosticRelatedInformation list
    ; relatedLocations : relatedLocation list
    }

  and diagnosticRelatedInformation =
    { relatedLocation : Location.t
    ; relatedMessage : string
    }

  and relatedLocation = diagnosticRelatedInformation

  val params_of_yojson : json -> params

  val publishDiagnosticsParams_of_yojson : json -> params

  val diagnostic_of_yojson : json -> diagnostic

  val diagnosticRelatedInformation_of_yojson : json -> relatedLocation

  val relatedLocation_of_yojson : json -> relatedLocation

  val yojson_of_params : params -> json

  val yojson_of_publishDiagnosticsParams : params -> json

  val yojson_of_diagnostic : diagnostic -> json

  val yojson_of_diagnosticRelatedInformation : relatedLocation -> json

  val yojson_of_relatedLocation : relatedLocation -> json
end

module Completion : sig
  type completionTriggerKind =
    | Invoked
    | TriggerCharacter
    | TriggerForIncompleteCompletions

  val yojson_of_completionTriggerKind : completionTriggerKind -> json

  val completionTriggerKind_of_yojson : json -> completionTriggerKind

  type completionItemKind =
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

  val yojson_of_completionItemKind : completionItemKind -> json

  val completionItemKind_of_yojson : json -> completionItemKind

  type insertTextFormat =
    | PlainText
    | SnippetFormat

  val yojson_of_insertTextFormat : insertTextFormat -> json

  val insertTextFormat_of_yojson : json -> insertTextFormat

  type params = completionParams

  and completionParams =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; context : completionContext option
    }

  and completionContext =
    { triggerKind : completionTriggerKind
    ; triggerCharacter : string option
    }

  and result = completionList

  and completionList =
    { isIncomplete : bool
    ; items : completionItem list
    }

  and completionItem =
    { label : string
    ; kind : completionItemKind option
    ; detail : string option
    ; documentation : string option
    ; sortText : string option
    ; filterText : string option
    ; insertText : string option
    ; insertTextFormat : insertTextFormat option
    ; textEdit : TextEdit.t option
    ; additionalTextEdits : TextEdit.t list
    ; commitCharacters : string list
    ; data : json option
    }

  val params_of_yojson : json -> params

  val completionParams_of_yojson : json -> params

  val completionContext_of_yojson : json -> completionContext

  val result_of_yojson : json -> result

  val completionList_of_yojson : json -> result

  val completionItem_of_yojson : json -> completionItem

  val yojson_of_params : params -> json

  val yojson_of_completionParams : params -> json

  val yojson_of_completionContext : completionContext -> json

  val yojson_of_result : result -> json

  val yojson_of_completionList : result -> json

  val yojson_of_completionItem : completionItem -> json
end

module Hover : sig
  type params = TextDocumentPositionParams.t

  and result = hoverResult option

  and hoverResult =
    { contents : MarkupContent.t
    ; range : Range.t option
    }

  val params_of_yojson : json -> params

  val result_of_yojson : json -> result

  val hoverResult_of_yojson : json -> hoverResult

  val yojson_of_params : params -> json

  val yojson_of_result : result -> json

  val yojson_of_hoverResult : hoverResult -> json
end

module SignatureHelpOptions : sig
  type t = { triggerCharacters : string list }

  include Yojsonable.S with type t := t
end

module ParameterInformation : sig
  module Label : sig
    type t =
      | Substring of string
      | Range of int * int

    include Yojsonable.S with type t := t
  end

  type t =
    { label : Label.t
    ; documentation : MarkupContent.t Or_string.t
    }

  include Yojsonable.S with type t := t
end

module SignatureInformation : sig
  type t =
    { label : string
    ; documentation : string option
    ; parameters : ParameterInformation.t list
    }

  include Yojsonable.S with type t := t
end

module SignatureHelp : sig
  type t =
    { signatures : SignatureInformation.t list
    ; activeSignature : int option
    ; activeParameter : int option
    }

  include Yojsonable.S with type t := t
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
    | Other of string

  include Yojsonable.S with type t := t
end

module CodeActionOptions : sig
  type t = { codeActionsKinds : CodeActionKind.t list }

  include Yojsonable.S with type t := t
end

module CodeActionLiteralSupport : sig
  type codeActionKind = { valueSet : CodeActionKind.t list }

  val codeActionKind_of_yojson : json -> codeActionKind

  val yojson_of_codeActionKind : codeActionKind -> json

  type t = { codeActionKind : codeActionKind }

  include Yojsonable.S with type t := t
end

module WorkspaceFolder : sig
  type t =
    { uri : documentUri
    ; name : string
    }

  include Yojsonable.S with type t := t
end

module Initialize : sig
  module Trace : sig
    type t =
      | Off
      | Messages
      | Verbose

    include Yojsonable.S with type t := t
  end

  module TextDocumentSyncKind : sig
    type t =
      | NoSync
      | FullSync
      | IncrementalSync

    include Yojsonable.S with type t := t
  end

  module Synchronization : sig
    type t =
      { willSave : bool
      ; willSaveWaitUntil : bool
      ; didSave : bool
      }

    include Yojsonable.S with type t := t
  end

  type completionItem = { snippetSupport : bool }

  val completionItem_of_yojson : json -> completionItem

  val yojson_of_completionItem : completionItem -> json

  val completionItem_empty : completionItem

  module Completion : sig
    type t = { completionItem : completionItem }

    include Yojsonable.S with type t := t

    val empty : t
  end

  module Hover : sig
    type t = { contentFormat : MarkupKind.t list }

    val empty : t

    include Yojsonable.S with type t := t
  end

  module CodeAction : sig
    type t =
      { codeActionLiteralSupport : CodeActionLiteralSupport.t option
      ; dynamicRegistration : bool option
      ; isPreferredSupport : bool option
      }

    include Yojsonable.S with type t := t

    val empty : t
  end

  module DocumentSymbol : sig
    type t = { hierarchicalDocumentSymbolSupport : bool }

    include Yojsonable.S with type t := t

    val empty : t
  end

  module TextDocumentClientCapabilities : sig
    type t =
      { synchronization : Synchronization.t
      ; completion : Completion.t
      ; documentSymbol : DocumentSymbol.t
      ; hover : Hover.t
      ; codeAction : CodeAction.t
      }

    include Yojsonable.S with type t := t

    val empty : t
  end

  type workspaceEdit = { documentChanges : bool }

  val workspaceEdit_of_yojson : json -> workspaceEdit

  val yojson_of_workspaceEdit : workspaceEdit -> json

  val workspaceEdit_empty : workspaceEdit

  type workspaceClientCapabilities =
    { applyEdit : bool
    ; workspaceEdit : workspaceEdit
    }

  val workspaceClientCapabilities_of_yojson :
    json -> workspaceClientCapabilities

  val yojson_of_workspaceClientCapabilities :
    workspaceClientCapabilities -> json

  type foldingRangeClientCapabilities =
    { rangeLimit : int option
    ; lineFoldingOnly : bool
    }

  val foldingRangeClientCapabilities_of_yojson :
    json -> foldingRangeClientCapabilities

  val yojson_of_foldingRangeClientCapabilities :
    foldingRangeClientCapabilities -> json

  val foldingRangeClientCapabilities_empty : foldingRangeClientCapabilities

  type client_capabilities =
    { workspace : workspaceClientCapabilities
    ; textDocument : TextDocumentClientCapabilities.t
    ; foldingRange : foldingRangeClientCapabilities
    }

  val client_capabilities_of_yojson : json -> client_capabilities

  val yojson_of_client_capabilities : client_capabilities -> json

  val client_capabilities_empty : client_capabilities

  type params =
    { processId : int option
    ; rootPath : string option
    ; rootUri : documentUri option
    ; client_capabilities : client_capabilities
    ; trace : Trace.t
    ; workspaceFolders : WorkspaceFolder.t list
    }

  and result = { server_capabilities : server_capabilities }

  and errorData = { retry : bool }

  and server_capabilities =
    { textDocumentSync : textDocumentSyncOptions
    ; hoverProvider : bool
    ; completionProvider : completionOptions option
    ; signatureHelpProvider : SignatureHelpOptions.t option
    ; definitionProvider : bool
    ; typeDefinitionProvider : bool
    ; referencesProvider : bool
    ; documentHighlightProvider : bool
    ; documentSymbolProvider : bool
    ; workspaceSymbolProvider : bool
    ; codeActionProvider : CodeActionOptions.t Or_bool.t
    ; codeLensProvider : codeLensOptions option
    ; documentFormattingProvider : bool
    ; documentRangeFormattingProvider : bool
    ; documentOnTypeFormattingProvider : documentOnTypeFormattingOptions option
    ; renameProvider : bool
    ; documentLinkProvider : documentLinkOptions option
    ; executeCommandProvider : executeCommandOptions option
    ; typeCoverageProvider : bool
    ; foldingRangeProvider : Void.t Or_bool.t
    }

  and completionOptions =
    { resolveProvider : bool
    ; triggerCharacters : string list
    }

  and codeLensOptions = { codelens_resolveProvider : bool }

  and documentOnTypeFormattingOptions =
    { firstTriggerCharacter : string
    ; moreTriggerCharacter : string list
    }

  and documentLinkOptions = { doclink_resolveProvider : bool }

  and executeCommandOptions = { commands : string list }

  and textDocumentSyncOptions =
    { openClose : bool
    ; change : TextDocumentSyncKind.t
    ; willSave : bool
    ; willSaveWaitUntil : bool
    ; didSave : saveOptions option
    }

  and saveOptions = { includeText : bool }

  val params_of_yojson : json -> params

  val result_of_yojson : json -> result

  val errorData_of_yojson : json -> errorData

  val server_capabilities_of_yojson : json -> server_capabilities

  val completionOptions_of_yojson : json -> completionOptions

  val codeLensOptions_of_yojson : json -> codeLensOptions

  val documentOnTypeFormattingOptions_of_yojson :
    json -> documentOnTypeFormattingOptions

  val documentLinkOptions_of_yojson : json -> documentLinkOptions

  val executeCommandOptions_of_yojson : json -> executeCommandOptions

  val textDocumentSyncOptions_of_yojson : json -> textDocumentSyncOptions

  val saveOptions_of_yojson : json -> saveOptions

  val yojson_of_params : params -> json

  val yojson_of_result : result -> json

  val yojson_of_errorData : errorData -> json

  val yojson_of_server_capabilities : server_capabilities -> json

  val yojson_of_completionOptions : completionOptions -> json

  val yojson_of_codeLensOptions : codeLensOptions -> json

  val yojson_of_documentOnTypeFormattingOptions :
    documentOnTypeFormattingOptions -> json

  val yojson_of_documentLinkOptions : documentLinkOptions -> json

  val yojson_of_executeCommandOptions : executeCommandOptions -> json

  val yojson_of_textDocumentSyncOptions : textDocumentSyncOptions -> json

  val yojson_of_saveOptions : saveOptions -> json
end

module Definition : sig
  type params = TextDocumentPositionParams.t

  and result = Locations.t option

  val params_of_yojson : json -> params

  val result_of_yojson : json -> result

  val yojson_of_params : params -> json

  val yojson_of_result : result -> json
end

module TypeDefinition : sig
  type params = TextDocumentPositionParams.t

  and result = Location.t list

  val params_of_yojson : json -> params

  val result_of_yojson : json -> result

  val yojson_of_params : params -> json

  val yojson_of_result : result -> json
end

module References : sig
  type params =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; context : referenceContext
    }

  and referenceContext = { includeDeclaration : bool }

  and result = Location.t list

  val params_of_yojson : json -> params

  val referenceContext_of_yojson : json -> referenceContext

  val result_of_yojson : json -> result

  val yojson_of_params : params -> json

  val yojson_of_referenceContext : referenceContext -> json

  val yojson_of_result : result -> json
end

module TextDocumentHighlight : sig
  type params = TextDocumentPositionParams.t

  and result = DocumentHighlight.t list

  val params_of_yojson : json -> params

  val result_of_yojson : json -> result

  val yojson_of_params : params -> json

  val yojson_of_result : result -> json
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

  include Yojsonable.S with type t := t
end

module SymbolInformation : sig
  type t =
    { name : string
    ; kind : SymbolKind.t
    ; deprecated : bool option
    ; location : Location.t
    ; containerName : string option
    }

  include Yojsonable.S with type t := t
end

module DocumentSymbol : sig
  type t =
    { name : string
    ; detail : string option
    ; kind : SymbolKind.t
    ; deprecated : bool
    ; range : Range.t
    ; selectionRange : Range.t
    ; children : t list
    }

  include Yojsonable.S with type t := t
end

module TextDocumentDocumentSymbol : sig
  type params = { textDocument : TextDocumentIdentifier.t }

  val params_of_yojson : json -> params

  val yojson_of_params : params -> json

  type result =
    | DocumentSymbol of DocumentSymbol.t list
    | SymbolInformation of SymbolInformation.t list

  val yojson_of_result : result -> json
end

module CodeLens : sig
  type params = { textDocument : TextDocumentIdentifier.t }

  and result = item list

  and item =
    { range : Range.t
    ; command : Command.t option
    }

  val params_of_yojson : json -> params

  val result_of_yojson : json -> result

  val item_of_yojson : json -> item

  val yojson_of_params : params -> json

  val yojson_of_result : result -> json

  val yojson_of_item : item -> json
end

module Rename : sig
  type params =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; newName : string
    }

  val params_of_yojson : json -> params

  val yojson_of_params : params -> json

  type result = WorkspaceEdit.t

  val yojson_of_result : result -> json
end

module DebugEcho : sig
  type params = { message : string }

  and result = params

  val params_of_yojson : json -> result

  val result_of_yojson : json -> result

  val yojson_of_params : result -> json

  val yojson_of_result : result -> json
end

module DebugTextDocumentGet : sig
  type params = TextDocumentPositionParams.t

  val params_of_yojson : json -> params

  val yojson_of_params : params -> json

  type result = string option

  val yojson_of_result : result -> json
end

module FoldingRange : sig
  module Kind : sig
    type t =
      | Comment
      | Imports
      | Region

    include Yojsonable.S with type t := t
  end

  type t =
    { startLine : int
    ; startCharacter : int option
    ; endLine : int
    ; endCharacter : int option
    ; kind : Kind.t option
    }

  include Yojsonable.S with type t := t

  type params = { textDocument : TextDocumentIdentifier.t }

  val params_of_yojson : json -> params

  val yojson_of_params : params -> json

  type result = t list

  val yojson_of_result : t list -> json
end

module CodeActionContext : sig
  type t =
    { diagnostics : PublishDiagnostics.diagnostic list
    ; only : CodeActionKind.t Only.t
    }

  include Yojsonable.S with type t := t
end

module CodeActionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; context : CodeActionContext.t
    }

  include Yojsonable.S with type t := t
end

module CodeAction : sig
  type t =
    { title : string
    ; kind : CodeActionKind.t option
    ; diagnostics : PublishDiagnostics.diagnostic list
    ; edit : WorkspaceEdit.t option
    ; command : Command.t option
    }

  val yojson_of_t : t -> json

  type result = (Command.t, t) Either.t list

  val yojson_of_result : result -> json
end

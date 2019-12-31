open Import

module Either : sig
  type ('left, 'right) t =
    | Left of 'left
    | Right of 'right
end

module Or_bool : sig
  type 'a t =
    | Bool of bool
    | Value of 'a

  val t_of_yojson : (json -> 'a) -> json -> 'a t

  val yojson_of_t : ('a -> json) -> 'a t -> json
end

module Or_string : sig
  type 'a t =
    | String of string
    | Value of 'a

  val t_of_yojson : (json -> 'a) -> json -> 'a t

  val yojson_of_t : ('a -> json) -> 'a t -> json
end

module Void : sig
  type t

  val t_of_yojson : json -> 'a

  val yojson_of_t : t -> 'a
end

module Id : sig
  type t = (string, int) Either.t

  val yojson_of_t : t -> json

  val t_of_yojson : json -> t
end

type documentUri = Uri.t

val documentUri_of_yojson : json -> documentUri

val yojson_of_documentUri : documentUri -> json

type zero_based_int = int

val zero_based_int_of_yojson : json -> zero_based_int

val yojson_of_zero_based_int : zero_based_int -> json

type position =
  { line : zero_based_int
  ; character : zero_based_int
  }

val position_of_yojson : json -> position

val yojson_of_position : position -> json

module Range : sig
  type t =
    { start_ : position
    ; end_ : position
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module Command : sig
  type t =
    { title : string
    ; command : string
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module MarkupKind : sig
  type t =
    | Plaintext
    | Markdown

  val yojson_of_t : t -> json

  val t_of_yojson : json -> t
end

module MarkupContent : sig
  type t =
    { value : string
    ; kind : MarkupKind.t
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module Location : sig
  type t =
    { uri : Uri.t
    ; range : Range.t
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module LocationLink : sig
  type t =
    { originSelectionRange : Range.t option
    ; targetUri : documentUri
    ; targetrange : Range.t
    ; targetSelectionRange : Range.t
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module Locations : sig
  type t =
    | Location of Location.t
    | Locations of Location.t list
    | Location_links of LocationLink.t list
end

module TextDocumentIdentifier : sig
  type t = { uri : documentUri }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module VersionedTextDocumentIdentifier : sig
  type t =
    { uri : documentUri
    ; version : zero_based_int
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module TextDocumentItem : sig
  type t =
    { uri : documentUri
    ; languageId : string
    ; version : zero_based_int
    ; text : string
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module DidOpen : sig
  type params = didOpenTextDocumentParams

  and didOpenTextDocumentParams = { textDocument : TextDocumentItem.t }

  val params_of_yojson : json -> params

  val didOpenTextDocumentParams_of_yojson : json -> params

  val yojson_of_params : params -> json

  val yojson_of_didOpenTextDocumentParams : params -> json
end

module DidChange : sig
  type params = didChangeTextDocumentParams

  and didChangeTextDocumentParams =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; contentChanges : textDocumentContentChangeEvent list
    }

  and textDocumentContentChangeEvent =
    { range : Range.t option
    ; rangeLength : zero_based_int option
    ; text : string
    }

  val params_of_yojson : json -> params

  val didChangeTextDocumentParams_of_yojson : json -> params

  val textDocumentContentChangeEvent_of_yojson :
    json -> textDocumentContentChangeEvent

  val yojson_of_params : params -> json

  val yojson_of_didChangeTextDocumentParams : params -> json

  val yojson_of_textDocumentContentChangeEvent :
    textDocumentContentChangeEvent -> json
end

module TextDocumentPositionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : position
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
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

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module TextEdit : sig
  type t =
    { range : Range.t
    ; newText : string
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module TextDocumentEdit : sig
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; edits : TextEdit.t list
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module WorkspaceEdit : sig
  type changes = (Uri.t * TextEdit.t list) list

  val yojson_of_changes : changes -> json

  type documentChanges = TextDocumentEdit.t list

  val yojson_of_documentChanges : documentChanges -> json

  type t =
    { changes : changes option
    ; documentChanges : documentChanges option
    }

  val yojson_of_t : t -> json

  val empty : t

  val make :
       documentChanges:bool
    -> uri:documentUri
    -> version:zero_based_int
    -> edits:TextEdit.t list
    -> t
end

module PublishDiagnostics : sig
  type diagnosticCode =
    | IntCode of zero_based_int
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

  val int_of_completionItemKind : completionItemKind -> zero_based_int

  val yojson_of_completionItemKind : completionItemKind -> json

  val completionItemKind_of_int_opt :
    zero_based_int -> completionItemKind option

  val completionItemKind_of_yojson : json -> completionItemKind

  type insertTextFormat =
    | PlainText
    | SnippetFormat

  val int_of_insertFormat : insertTextFormat -> zero_based_int

  val yojson_of_insertTextFormat : insertTextFormat -> json

  val insertFormat_of_int_opt : zero_based_int -> insertTextFormat option

  val insertTextFormat_of_yojson : json -> insertTextFormat

  type params = completionParams

  and completionParams =
    { textDocument : TextDocumentIdentifier.t
    ; position : position
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
    ; additionalTextEdits : TextEdit.t list option
    ; commitCharacters : string list option
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

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module ParameterInformation : sig
  module Label : sig
    type t =
      | Substring of string
      | Range of zero_based_int * zero_based_int

    val t_of_yojson : json -> t

    val yojson_of_t : t -> json
  end

  type t =
    { label : Label.t
    ; documentation : MarkupContent.t Or_string.t
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module SignatureInformation : sig
  type t =
    { label : string
    ; documentation : string option
    ; parameters : ParameterInformation.t list
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module SignatureHelp : sig
  type t =
    { signatures : SignatureInformation.t list
    ; activeSignature : zero_based_int option
    ; activeParameter : zero_based_int option
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
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

  val to_string : t -> string

  val yojson_of_t : t -> json

  val t_of_yojson : json -> t
end

module CodeActionOptions : sig
  type t = { codeActionsKinds : CodeActionKind.t list }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module CodeActionLiteralSupport : sig
  type codeActionKind = { valueSet : CodeActionKind.t list }

  val codeActionKind_of_yojson : json -> codeActionKind

  val yojson_of_codeActionKind : codeActionKind -> json

  type t = { codeActionKind : codeActionKind }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module Initialize : sig
  type trace =
    | Off
    | Messages
    | Verbose

  val yojson_of_trace : trace -> json

  val trace_of_yojson : json -> trace

  type textDocumentSyncKind =
    | NoSync
    | FullSync
    | IncrementalSync

  val yojson_of_textDocumentSyncKind : textDocumentSyncKind -> json

  val textDocumentSyncKind_of_yojson : json -> textDocumentSyncKind

  type synchronization =
    { willSave : bool
    ; willSaveWaitUntil : bool
    ; didSave : bool
    }

  val synchronization_of_yojson : json -> synchronization

  val yojson_of_synchronization : synchronization -> json

  val synchronization_empty : synchronization

  type completionItem = { snippetSupport : bool }

  val completionItem_of_yojson : json -> completionItem

  val yojson_of_completionItem : completionItem -> json

  val completionItem_empty : completionItem

  type completion = { completionItem : completionItem }

  val completion_of_yojson : json -> completion

  val yojson_of_completion : completion -> json

  val completion_empty : completion

  type hover = { contentFormat : MarkupKind.t list }

  val hover_of_yojson : json -> hover

  val yojson_of_hover : hover -> json

  val hover_empty : hover

  type codeAction =
    { codeActionLiteralSupport : CodeActionLiteralSupport.t option
    ; dynamicRegistration : bool option
    ; isPreferredSupport : bool option
    }

  val codeAction_of_yojson : json -> codeAction

  val yojson_of_codeAction : codeAction -> json

  val codeAction_empty : codeAction

  type documentSymbol = { hierarchicalDocumentSymbolSupport : bool }

  val documentSymbol_of_yojson : json -> documentSymbol

  val yojson_of_documentSymbol : documentSymbol -> json

  val documentSymbol_empty : documentSymbol

  type textDocumentClientCapabilities =
    { synchronization : synchronization
    ; completion : completion
    ; documentSymbol : documentSymbol
    ; hover : hover
    ; codeAction : codeAction
    }

  val textDocumentClientCapabilities_of_yojson :
    json -> textDocumentClientCapabilities

  val yojson_of_textDocumentClientCapabilities :
    textDocumentClientCapabilities -> json

  val textDocumentClientCapabilities_empty : textDocumentClientCapabilities

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
    { rangeLimit : zero_based_int option
    ; lineFoldingOnly : bool
    }

  val foldingRangeClientCapabilities_of_yojson :
    json -> foldingRangeClientCapabilities

  val yojson_of_foldingRangeClientCapabilities :
    foldingRangeClientCapabilities -> json

  val foldingRangeClientCapabilities_empty : foldingRangeClientCapabilities

  type client_capabilities =
    { workspace : workspaceClientCapabilities
    ; textDocument : textDocumentClientCapabilities
    ; foldingRange : foldingRangeClientCapabilities
    }

  val client_capabilities_of_yojson : json -> client_capabilities

  val yojson_of_client_capabilities : client_capabilities -> json

  val client_capabilities_empty : client_capabilities

  type params =
    { processId : zero_based_int option
    ; rootPath : string option
    ; rootUri : documentUri option
    ; client_capabilities : client_capabilities
    ; trace : trace
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
    ; change : textDocumentSyncKind
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
    ; position : position
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

  val yojson_of_t : t -> json

  val t_of_yojson : json -> t
end

module SymbolInformation : sig
  type t =
    { name : string
    ; kind : SymbolKind.t
    ; deprecated : bool option
    ; location : Location.t
    ; containerName : string option
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
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

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
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
    ; position : position
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

    val yojson_of_t : t -> json

    val t_of_yojson : json -> t
  end

  type t =
    { startLine : zero_based_int
    ; startCharacter : zero_based_int option
    ; endLine : zero_based_int
    ; endCharacter : zero_based_int option
    ; kind : Kind.t option
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json

  type params = { textDocument : TextDocumentIdentifier.t }

  val params_of_yojson : json -> params

  val yojson_of_params : params -> json

  type result = t list

  val yojson_of_result : t list -> json
end

module CodeActionContext : sig
  type t =
    { diagnostics : PublishDiagnostics.diagnostic list
    ; only : CodeActionKind.t list option
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module CodeActionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; context : CodeActionContext.t
    }

  val t_of_yojson : json -> t

  val yojson_of_t : t -> json
end

module CodeAction : sig
  type t =
    { title : string
    ; kind : CodeActionKind.t option
    ; diagnostics : PublishDiagnostics.diagnostic list option
    ; edit : WorkspaceEdit.t option
    ; command : Command.t option
    }

  val yojson_of_t : t -> json

  type result = (Command.t, t) Either.t list option

  val yojson_of_result : result -> json
end

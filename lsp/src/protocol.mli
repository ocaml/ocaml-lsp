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
end

module Or_string : sig
  type 'a t =
    | String of string
    | Value of 'a
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
end

module Range : sig
  type t =
    { start_ : Position.t
    ; end_ : Position.t
    }
end

module Command : sig
  type t =
    { title : string
    ; command : string
    ; arguments : json list option
    }
end

module MarkupKind : sig
  type t =
    | Plaintext
    | Markdown
end

module MarkupContent : sig
  type t =
    { value : string
    ; kind : MarkupKind.t
    }
end

module Location : sig
  type t =
    { uri : Uri.t
    ; range : Range.t
    }
end

module LocationLink : sig
  type t =
    { originSelectionRange : Range.t option
    ; targetUri : documentUri
    ; targetrange : Range.t
    ; targetSelectionRange : Range.t
    }
end

module Locations : sig
  type t =
    | Location of Location.t
    | Locations of Location.t list
    | Location_links of LocationLink.t list
end

module TextDocumentIdentifier : sig
  type t = { uri : documentUri }
end

module VersionedTextDocumentIdentifier : sig
  type t =
    { uri : documentUri
    ; version : int
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

module TextDocumentItem : sig
  type t =
    { uri : documentUri
    ; languageId : string
    ; version : int
    ; text : string
    }
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

  type t =
    { range : Range.t
    ; kind : kind option
    }
end

module TextEdit : sig
  type t =
    { range : Range.t
    ; newText : string
    }
end

module TextDocumentEdit : sig
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; edits : TextEdit.t list
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
    { uri : documentUri
    ; options : CreateFileOptions.t option
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
    { oldUri : documentUri
    ; newUri : documentUri
    ; options : RenameFileOptions.t option
    }
end

module DeleteFileOptions : sig
  type t =
    { recursive : bool option
    ; ignoreIfNotExists : bool option
    }
end

module DeleteFile : sig
  type t =
    { uri : documentUri
    ; options : DeleteFileOptions.t option
    }
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
end

module ParameterInformation : sig
  module Label : sig
    type t =
      | Substring of string
      | Range of int * int
  end

  type t =
    { label : Label.t
    ; documentation : MarkupContent.t Or_string.t
    }
end

module SignatureInformation : sig
  type t =
    { label : string
    ; documentation : string option
    ; parameters : ParameterInformation.t list
    }
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
end

module CodeActionOptions : sig
  type t = { codeActionsKinds : CodeActionKind.t list }
end

module CodeActionLiteralSupport : sig
  type codeActionKind = { valueSet : CodeActionKind.t list }

  type t = { codeActionKind : codeActionKind }
end

module WorkspaceFolder : sig
  type t =
    { uri : documentUri
    ; name : string
    }
end

module Initialize : sig
  module Trace : sig
    type t =
      | Off
      | Messages
      | Verbose
  end

  module TextDocumentSyncKind : sig
    type t =
      | NoSync
      | FullSync
      | IncrementalSync
  end

  module Synchronization : sig
    type t =
      { willSave : bool
      ; willSaveWaitUntil : bool
      ; didSave : bool
      }
  end

  module CompletionItem : sig
    type t = { snippetSupport : bool }

    val empty : t
  end

  module Completion : sig
    type t = { completionItem : CompletionItem.t }

    val empty : t
  end

  module Hover : sig
    type t = { contentFormat : MarkupKind.t list }

    val empty : t
  end

  module CodeAction : sig
    type t =
      { codeActionLiteralSupport : CodeActionLiteralSupport.t option
      ; dynamicRegistration : bool option
      ; isPreferredSupport : bool option
      }

    val empty : t
  end

  module DocumentSymbol : sig
    type t = { hierarchicalDocumentSymbolSupport : bool }

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

    val empty : t
  end

  module WorkspaceEdit : sig
    module ResourceOperationKind : sig
      type t =
        | Create
        | Rename
        | Delete
    end

    module FailureHandlingKind : sig
      type t =
        | Abort
        | Transactional
        | TextOnlyTransactional
        | Undo
    end

    type t =
      { documentChanges : bool
      ; resourceOperations : ResourceOperationKind.t list option
      ; failureHandlingKind : FailureHandlingKind.t option
      }

    val empty : t
  end

  module WorkspaceClientCapabilities : sig
    module Symbol : sig
      type t = { valueSet : SymbolKind.t list }
    end

    type t =
      { applyEdit : bool
      ; workspaceEdit : WorkspaceEdit.t
      ; symbol : Symbol.t
      ; workspaceFolders : bool
      ; configuration : bool
      }
  end

  module FoldingRangeClientCapabilities : sig
    type t =
      { rangeLimit : int option
      ; lineFoldingOnly : bool
      }

    val empty : t
  end

  module ClientCapabilities : sig
    type t =
      { workspace : WorkspaceClientCapabilities.t
      ; textDocument : TextDocumentClientCapabilities.t
      ; foldingRange : FoldingRangeClientCapabilities.t
      }

    val empty : t
  end

  module Params : sig
    type t =
      { processId : int option
      ; rootPath : string option
      ; rootUri : documentUri option
      ; capabilities : ClientCapabilities.t
      ; trace : Trace.t
      ; workspaceFolders : WorkspaceFolder.t list
      ; initializationOptions : json option
      }

    include Yojsonable.S with type t := t
  end

  module TextDocumentSyncOptions : sig
    type saveOptions = { includeText : bool }

    type t =
      { openClose : bool
      ; change : TextDocumentSyncKind.t
      ; willSave : bool
      ; willSaveWaitUntil : bool
      ; didSave : saveOptions option
      }
  end

  module CompletionOptions : sig
    type t =
      { resolveProvider : bool
      ; triggerCharacters : string list
      }
  end

  module CodeLensOptions : sig
    type t = { resolveProvider : bool }
  end

  module DocumentOnTypeFormattingOptions : sig
    type t =
      { firstTriggerCharacter : string
      ; moreTriggerCharacter : string list
      }
  end

  module DocumentLinkOptions : sig
    type t = { doclink_resolveProvider : bool }
  end

  module ExecuteCommandOptions : sig
    type t = { commands : string list }
  end

  module ServerCapabilities : sig
    type t =
      { textDocumentSync : TextDocumentSyncOptions.t
      ; hoverProvider : bool
      ; completionProvider : CompletionOptions.t option
      ; signatureHelpProvider : SignatureHelpOptions.t option
      ; definitionProvider : bool
      ; typeDefinitionProvider : bool
      ; referencesProvider : bool
      ; documentHighlightProvider : bool
      ; documentSymbolProvider : bool
      ; workspaceSymbolProvider : bool
      ; codeActionProvider : CodeActionOptions.t Or_bool.t
      ; codeLensProvider : CodeLensOptions.t option
      ; documentFormattingProvider : bool
      ; documentRangeFormattingProvider : bool
      ; documentOnTypeFormattingProvider :
          DocumentOnTypeFormattingOptions.t option
      ; renameProvider : bool
      ; documentLinkProvider : DocumentLinkOptions.t option
      ; executeCommandProvider : ExecuteCommandOptions.t option
      ; typeCoverageProvider : bool
      ; foldingRangeProvider : Void.t Or_bool.t
      }
  end

  module Result : sig
    type t = { capabilities : ServerCapabilities.t }

    include Yojsonable.S with type t := t
  end
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

module SymbolInformation : sig
  type t =
    { name : string
    ; kind : SymbolKind.t
    ; deprecated : bool option
    ; location : Location.t
    ; containerName : string option
    }
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
  end

  type t =
    { startLine : int
    ; startCharacter : int option
    ; endLine : int
    ; endCharacter : int option
    ; kind : Kind.t option
    }

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

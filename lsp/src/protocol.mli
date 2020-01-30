open Import

module Only : sig
  type 'a t =
    | All
    | Only of 'a list

  include Json.Jsonable.S1 with type 'a t := 'a t
end

module Or_bool : sig
  type 'a t =
    | Bool of bool
    | Value of 'a

  include Json.Jsonable.S1 with type 'a t := 'a t
end

module Or_string : sig
  type 'a t =
    | String of string
    | Value of 'a
end

module Void : Json.Jsonable.S

module DocumentFilter : sig
  type t =
    { language : string option
    ; scheme : string option
    ; pattern : string
    }

  include Json.Jsonable.S with type t := t
end

module Unregistration : sig
  type t =
    { id : string
    ; method_ : string
    }

  include Json.Jsonable.S with type t := t

  module Params : sig
    type nonrec t = { unregistrations : t list }

    include Json.Jsonable.S with type t := t
  end
end

type documentUri = Uri.t

val documentUri_of_yojson : Json.t -> documentUri

val yojson_of_documentUri : documentUri -> Json.t

module Position : sig
  type t =
    { line : int
    ; character : int
    }

  include Json.Jsonable.S with type t := t
end

module Range : sig
  type t =
    { start_ : Position.t
    ; end_ : Position.t
    }

  include Json.Jsonable.S with type t := t
end

module Command : sig
  type t =
    { title : string
    ; command : string
    ; arguments : Json.t list option
    }

  include Json.Jsonable.S with type t := t
end

module MarkupKind : sig
  type t =
    | Plaintext
    | Markdown

  include Json.Jsonable.S with type t := t
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

  include Json.Jsonable.S with type t := t
end

module Message : sig
  module Type : sig
    type t =
      | Error
      | Warning
      | Info
      | Log
  end

  module ActionItem : sig
    type t = { title : string }
  end
end

module ShowMessage : sig
  module Params : sig
    type t =
      { type_ : Message.Type.t
      ; message : string
      }

    include Json.Jsonable.S with type t := t
  end

  module Request : sig
    type t =
      { type_ : Message.Type.t
      ; message : string
      ; actions : Message.ActionItem.t list
      }

    include Json.Jsonable.S with type t := t
  end
end

module Configuration : sig
  module Item : sig
    type t =
      { scopeUri : documentUri option
      ; section : string option
      }
  end

  module Params : sig
    type t = { items : Item.t list }

    include Json.Jsonable.S with type t := t
  end
end

module TextDocumentIdentifier : sig
  type t = { uri : documentUri }

  include Json.Jsonable.S with type t := t
end

module VersionedTextDocumentIdentifier : sig
  type t =
    { uri : documentUri
    ; version : int
    }
end

module TextDocumentSaveReason : sig
  type t =
    | Manual
    | AfterDelay
    | FocusOut
end

module WillSaveTextDocumentParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; reason : TextDocumentSaveReason.t
    }

  include Json.Jsonable.S with type t := t
end

module DidSaveTextDocumentParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; text : string option
    }

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

  val params_of_yojson : Json.t -> params

  val didOpenTextDocumentParams_of_yojson : Json.t -> params

  val yojson_of_params : params -> Json.t

  val yojson_of_didOpenTextDocumentParams : params -> Json.t
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

  include Json.Jsonable.S with type t := t
end

module TextDocumentPositionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }

  include Json.Jsonable.S with type t := t
end

module PrepareRename : sig
  module Range : sig
    type t =
      { range : Range.t
      ; placeholder : string option
      }
  end

  module Result : sig
    type nonrec t = Range.t option

    include Json.Jsonable.S with type t := t
  end
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

  include Json.Jsonable.S with type t := t
end

module WillSaveWaitUntilTextDocument : sig
  module Result : sig
    type t = TextEdit.t list

    include Json.Jsonable.S with type t := t
  end
end

module FormattingOptions : sig
  type t =
    { tabSize : int
    ; insertSpaces : bool
    ; trimTrailingWhitespace : bool option
    ; insertFinalNewline : bool option
    ; trimFinalNewlines : bool option
    }
end

module DocumentFormattingParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; options : FormattingOptions.t
    }

  include Json.Jsonable.S with type t := t
end

module DocumentLink : sig
  type t =
    { range : Range.t
    ; target : documentUri option
    ; data : Json.t option
    }

  include Json.Jsonable.S with type t := t

  module Params : sig
    type t = { textDocument : TextDocumentIdentifier.t }

    include Json.Jsonable.S with type t := t
  end

  module Result : sig
    type nonrec t = t list

    include Json.Jsonable.S with type t := t
  end
end

module DocumentOnTypeFormattingParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; ch : string
    ; options : FormattingOptions.t
    }

  include Json.Jsonable.S with type t := t
end

module TextDocumentOnTypeFormatting : sig
  module Result : sig
    type t = TextEdit.t list

    include Json.Jsonable.S with type t := t
  end
end

module TextDocumentFormatting : sig
  module Result : sig
    type t = TextEdit.t list

    include Json.Jsonable.S with type t := t
  end
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

    val yojson_of_t : t -> Json.t
  end

  type t =
    { changes : (Uri.t * TextEdit.t list) list
    ; documentChanges : DocumentChange.t list
    }

  val yojson_of_t : t -> Json.t

  val empty : t

  val make :
       documentChanges:bool
    -> uri:documentUri
    -> version:int
    -> edits:TextEdit.t list
    -> t
end

module Registration : sig
  type t =
    { id : string
    ; method_ : string
    ; registerOptions : Json.t option
    }

  module Params : sig
    type nonrec t = { registrations : t list }

    include Json.Jsonable.S with type t := t
  end
end

module ApplyWorkspaceEdit : sig
  module Params : sig
    type t =
      { label : string option
      ; edit : WorkspaceEdit.t list
      }

    include Json.Jsonable.S with type t := t
  end

  module Response : sig
    type t =
      { applied : bool
      ; failureReason : string option
      }

    include Json.Jsonable.S with type t := t
  end
end

module PublishDiagnostics : sig
  type diagnosticCode =
    | IntCode of int
    | StringCode of string
    | NoCode

  val yojson_of_diagnosticCode : diagnosticCode -> Json.t

  val diagnosticCode_of_yojson : Json.t -> diagnosticCode

  type diagnosticSeverity =
    | Error
    | Warning
    | Information
    | Hint

  val yojson_of_diagnosticSeverity : diagnosticSeverity -> Json.t

  val diagnosticSeverity_of_yojson : Json.t -> diagnosticSeverity

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

  val params_of_yojson : Json.t -> params

  val publishDiagnosticsParams_of_yojson : Json.t -> params

  val diagnostic_of_yojson : Json.t -> diagnostic

  val diagnosticRelatedInformation_of_yojson : Json.t -> relatedLocation

  val relatedLocation_of_yojson : Json.t -> relatedLocation

  val yojson_of_params : params -> Json.t

  val yojson_of_publishDiagnosticsParams : params -> Json.t

  val yojson_of_diagnostic : diagnostic -> Json.t

  val yojson_of_diagnosticRelatedInformation : relatedLocation -> Json.t

  val yojson_of_relatedLocation : relatedLocation -> Json.t
end

module Hover : sig
  type params = TextDocumentPositionParams.t

  and result = hoverResult option

  and hoverResult =
    { contents : MarkupContent.t
    ; range : Range.t option
    }

  val params_of_yojson : Json.t -> params

  val result_of_yojson : Json.t -> result

  val hoverResult_of_yojson : Json.t -> hoverResult

  val yojson_of_params : params -> Json.t

  val yojson_of_result : result -> Json.t

  val yojson_of_hoverResult : hoverResult -> Json.t
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

  include Json.Jsonable.S with type t := t
end

module WorkspaceFolder : sig
  type t =
    { uri : documentUri
    ; name : string
    }

  include Json.Jsonable.S with type t := t
end

module WorkspaceFoldersChangeEvent : sig
  type t =
    { added : WorkspaceFolder.t list
    ; removed : WorkspaceFolder.t list
    }
end

module DidChangeConfiguration : sig
  module Params : sig
    type t = { settings : Json.t }

    include Json.Jsonable.S with type t := t
  end
end

module DidChangeWorkspaceFolders : sig
  module Params : sig
    type t = { event : WorkspaceFoldersChangeEvent.t }

    include Json.Jsonable.S with type t := t
  end
end

module Definition : sig
  type params = TextDocumentPositionParams.t

  and result = Locations.t option

  val params_of_yojson : Json.t -> params

  val result_of_yojson : Json.t -> result

  val yojson_of_params : params -> Json.t

  val yojson_of_result : result -> Json.t
end

module TypeDefinition : sig
  type params = TextDocumentPositionParams.t

  and result = Location.t list

  val params_of_yojson : Json.t -> params

  val result_of_yojson : Json.t -> result

  val yojson_of_params : params -> Json.t

  val yojson_of_result : result -> Json.t
end

module References : sig
  type params =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; context : referenceContext
    }

  and referenceContext = { includeDeclaration : bool }

  and result = Location.t list

  val params_of_yojson : Json.t -> params

  val referenceContext_of_yojson : Json.t -> referenceContext

  val result_of_yojson : Json.t -> result

  val yojson_of_params : params -> Json.t

  val yojson_of_referenceContext : referenceContext -> Json.t

  val yojson_of_result : result -> Json.t
end

module TextDocumentHighlight : sig
  type params = TextDocumentPositionParams.t

  and result = DocumentHighlight.t list

  val params_of_yojson : Json.t -> params

  val result_of_yojson : Json.t -> result

  val yojson_of_params : params -> Json.t

  val yojson_of_result : result -> Json.t
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

  val params_of_yojson : Json.t -> params

  val yojson_of_params : params -> Json.t

  type result =
    | DocumentSymbol of DocumentSymbol.t list
    | SymbolInformation of SymbolInformation.t list

  val yojson_of_result : result -> Json.t
end

module WorkspaceSymbol : sig
  module Params : sig
    type t = { query : string }

    include Json.Jsonable.S with type t := t
  end

  module Result : sig
    type t = SymbolInformation.t list

    include Json.Jsonable.S with type t := t
  end
end

module Color : sig
  type t =
    { red : int
    ; green : int
    ; blue : int
    ; alpha : int
    }

  module Information : sig
    type nonrec t =
      { color : t
      ; range : Range.t
      }
  end
end

module ColorPresentation : sig
  type t =
    { label : string
    ; textEdit : TextEdit.t option
    ; additionalTextEdits : TextEdit.t list
    }

  include Json.Jsonable.S with type t := t

  module Params : sig
    type t =
      { textDocument : TextDocumentIdentifier.t
      ; color : Color.t
      ; range : Range.t
      }

    include Json.Jsonable.S with type t := t
  end

  module Result : sig
    type nonrec t = t list

    include Json.Jsonable.S with type t := t
  end
end

module DocumentColor : sig
  module Params : sig
    type t = { textDocument : TextDocumentIdentifier.t }

    include Json.Jsonable.S with type t := t
  end

  module Result : sig
    type t = Color.Information.t list

    include Json.Jsonable.S with type t := t
  end
end

module CodeLens : sig
  type t =
    { range : Range.t
    ; command : Command.t option
    ; data : Json.t option
    }

  include Json.Jsonable.S with type t := t

  module Params : sig
    type t = { textDocument : TextDocumentIdentifier.t }

    include Json.Jsonable.S with type t := t
  end

  module Result : sig
    type nonrec t = t list

    include Json.Jsonable.S with type t := t
  end
end

module Rename : sig
  type params =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; newName : string
    }

  val params_of_yojson : Json.t -> params

  val yojson_of_params : params -> Json.t

  type result = WorkspaceEdit.t

  val yojson_of_result : result -> Json.t
end

module DebugEcho : sig
  type params = { message : string }

  and result = params

  val params_of_yojson : Json.t -> result

  val result_of_yojson : Json.t -> result

  val yojson_of_params : result -> Json.t

  val yojson_of_result : result -> Json.t
end

module DebugTextDocumentGet : sig
  type params = TextDocumentPositionParams.t

  val params_of_yojson : Json.t -> params

  val yojson_of_params : params -> Json.t

  type result = string option

  val yojson_of_result : result -> Json.t
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

  val params_of_yojson : Json.t -> params

  val yojson_of_params : params -> Json.t

  type result = t list

  val yojson_of_result : t list -> Json.t
end

module SelectionRange : sig
  module Params : sig
    type t =
      { textDocument : TextDocumentIdentifier.t
      ; positions : Position.t list
      }

    include Json.Jsonable.S with type t := t
  end

  type t =
    { range : Range.t
    ; parent : t option
    }

  include Json.Jsonable.S with type t := t
end

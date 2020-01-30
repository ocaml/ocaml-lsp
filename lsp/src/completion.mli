open Import
open Protocol

module ItemTag : sig
  type t = Deprecated
end

type completionTriggerKind =
  | Invoked
  | TriggerCharacter
  | TriggerForIncompleteCompletions

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

type insertTextFormat =
  | PlainText
  | SnippetFormat

type completionItem =
  { label : string
  ; kind : completionItemKind option
  ; detail : string option
  ; documentation : string option
  ; deprecated : bool
  ; preselect : bool option
  ; sortText : string option
  ; filterText : string option
  ; insertText : string option
  ; insertTextFormat : insertTextFormat option
  ; textEdit : TextEdit.t option
  ; additionalTextEdits : TextEdit.t list
  ; commitCharacters : string list
  ; tags : ItemTag.t list
  ; data : Json.t option
  }

type completionContext =
  { triggerKind : completionTriggerKind
  ; triggerCharacter : string option
  }

type completionParams =
  { textDocument : TextDocumentIdentifier.t
  ; position : Position.t
  ; context : completionContext option
  }

type completionList =
  { isIncomplete : bool
  ; items : completionItem list
  }

type result = completionList

type params = completionParams

val params_of_yojson : Json.t -> params

val result_of_yojson : Json.t -> result

val yojson_of_params : params -> Json.t

val yojson_of_result : result -> Json.t

val yojson_of_completionItem : completionItem -> Json.t

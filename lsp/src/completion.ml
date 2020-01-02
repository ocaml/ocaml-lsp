open Import
open Protocol

type completionTriggerKind =
  | Invoked (* 1 *)
  | TriggerCharacter (* 2 *)
  | TriggerForIncompleteCompletions

(* 3 *)

let yojson_of_completionTriggerKind = function
  | Invoked -> `Int 1
  | TriggerCharacter -> `Int 2
  | TriggerForIncompleteCompletions -> `Int 3

let completionTriggerKind_of_yojson = function
  | `Int 1 -> Invoked
  | `Int 2 -> TriggerCharacter
  | `Int 3 -> TriggerForIncompleteCompletions
  | v ->
    yojson_error "invalid completion.triggerKind, should be equal to 1, 2 or 3"
      v

type completionItemKind =
  | Text (* 1 *)
  | Method (* 2 *)
  | Function (* 3 *)
  | Constructor (* 4 *)
  | Field (* 5 *)
  | Variable (* 6 *)
  | Class (* 7 *)
  | Interface (* 8 *)
  | Module (* 9 *)
  | Property (* 10 *)
  | Unit (* 11 *)
  | Value (* 12 *)
  | Enum (* 13 *)
  | Keyword (* 14 *)
  | Snippet (* 15 *)
  | Color (* 16 *)
  | File (* 17 *)
  | Reference (* 18 *)
  | Folder (* 19 *)
  | EnumMember (* 20 *)
  | Constant (* 21 *)
  | Struct (* 22 *)
  | Event (* 23 *)
  | Operator (* 24 *)
  | TypeParameter

(* 25 *)

(** Once we get better PPX support we can use [@@deriving enum]. Keep in sync
    with completionItemKind_of_int_opt. *)
let int_of_completionItemKind = function
  | Text -> 1
  | Method -> 2
  | Function -> 3
  | Constructor -> 4
  | Field -> 5
  | Variable -> 6
  | Class -> 7
  | Interface -> 8
  | Module -> 9
  | Property -> 10
  | Unit -> 11
  | Value -> 12
  | Enum -> 13
  | Keyword -> 14
  | Snippet -> 15
  | Color -> 16
  | File -> 17
  | Reference -> 18
  | Folder -> 19
  | EnumMember -> 20
  | Constant -> 21
  | Struct -> 22
  | Event -> 23
  | Operator -> 24
  | TypeParameter -> 25

let yojson_of_completionItemKind v = `Int (int_of_completionItemKind v)

(** Once we get better PPX support we can use [@@deriving enum]. Keep in sync
    with int_of_completionItemKind. *)
let completionItemKind_of_int_opt = function
  | 1 -> Some Text
  | 2 -> Some Method
  | 3 -> Some Function
  | 4 -> Some Constructor
  | 5 -> Some Field
  | 6 -> Some Variable
  | 7 -> Some Class
  | 8 -> Some Interface
  | 9 -> Some Module
  | 10 -> Some Property
  | 11 -> Some Unit
  | 12 -> Some Value
  | 13 -> Some Enum
  | 14 -> Some Keyword
  | 15 -> Some Snippet
  | 16 -> Some Color
  | 17 -> Some File
  | 18 -> Some Reference
  | 19 -> Some Folder
  | 20 -> Some EnumMember
  | 21 -> Some Constant
  | 22 -> Some Struct
  | 23 -> Some Event
  | 24 -> Some Operator
  | 25 -> Some TypeParameter
  | _ -> None

let completionItemKind_of_yojson = function
  | `Int v -> (
    match completionItemKind_of_int_opt v with
    | Some v -> v
    | None ->
      yojson_error "completion.kind expected to be between 1 and 25" (`Int v) )
  | node -> yojson_error "completion.kind expected to be between 1 and 25" node

(** Keep this in sync with `int_of_completionItemKind`. *)
type insertTextFormat =
  | PlainText (* 1 *)
  (* the insertText/textEdits are just plain strings *)
  | SnippetFormat

(* 2 *)
(* wire: just "Snippet" *)

(** Once we get better PPX support we can use [@@deriving enum]. Keep in sync
    with insertFormat_of_int_opt. *)
let int_of_insertFormat = function
  | PlainText -> 1
  | SnippetFormat -> 2

let yojson_of_insertTextFormat v = `Int (int_of_insertFormat v)

(** Once we get better PPX support we can use [@@deriving enum]. Keep in sync
    with int_of_insertFormat. *)
let insertFormat_of_int_opt = function
  | 1 -> Some PlainText
  | 2 -> Some SnippetFormat
  | _ -> None

let insertTextFormat_of_yojson = function
  | `Int v -> (
    match insertFormat_of_int_opt v with
    | Some v -> v
    | None -> yojson_error "insertTextFormat expected to be 1 or 2" (`Int v) )
  | node -> yojson_error "insertTextFormat expected to be 1 or 2" node

type params = completionParams

and completionParams =
  { textDocument : TextDocumentIdentifier.t
  ; (* the text document *)
    position : Position.t
  ; (* the position inside the text document *)
    context : completionContext option [@yojson.option]
  }
[@@yojson.allow_extra_fields]

and completionContext =
  { triggerKind : completionTriggerKind
  ; triggerCharacter : string option [@yojson.option]
  }
[@@yojson.allow_extra_fields]

and result = completionList

(* wire: can also be 'completionItem list' *)
and completionList =
  { isIncomplete : bool
  ; (* further typing should result in recomputing *)
    items : completionItem list
  }
[@@yojson.allow_extra_fields]

and completionItem =
  { label : string
  ; (* the label in the UI *)
    kind : completionItemKind option [@yojson.option]
  ; (* tells editor which icon to use *)
    detail : string option [@yojson.option]
  ; documentation : string option [@yojson.option]
  ; (* human-readable doc-comment *)
    sortText : string option [@yojson.option]
  ; (* used for sorting; if absent, uses label *)
    filterText : string option [@yojson.option]
  ; (* used for filtering; if absent, uses label *)
    insertText : string option [@yojson.option]
  ; (* used for inserting; if absent, uses label *)
    insertTextFormat : insertTextFormat option [@yojson.option]
  ; textEdit : TextEdit.t option [@yojson.option]
  ; additionalTextEdits : TextEdit.t list
        [@default []]
        [@yojson_drop_default ( = )]
        (* command: Command.t option [@default None]; (1* if present, is
           executed after completion *1) *)
        (* data: Hh_json.json option [@default None]; *)
  ; commitCharacters : string list [@default []] [@yojson_drop_default ( = )]
  ; data : json option [@yojson.option]
  }
[@@yojson.allow_extra_fields] [@@deriving_inline yojson]

let _ = fun (_ : params) -> ()

let _ = fun (_ : completionParams) -> ()

let _ = fun (_ : completionContext) -> ()

let _ = fun (_ : result) -> ()

let _ = fun (_ : completionList) -> ()

let _ = fun (_ : completionItem) -> ()

let rec params_of_yojson =
  ( let _tp_loc = "lsp/src/protocol.ml.Completion.params" in
    fun t -> completionParams_of_yojson t
    : Ppx_yojson_conv_lib.Yojson.Safe.t -> params )

and completionParams_of_yojson =
  ( let _tp_loc = "lsp/src/protocol.ml.Completion.completionParams" in
    function
    | `Assoc field_yojsons as yojson -> (
      let textDocument_field = ref None
      and position_field = ref None
      and context_field = ref None
      and duplicates = ref []
      and extra = ref [] in
      let rec iter = function
        | (field_name, _field_yojson) :: tail ->
          ( match field_name with
          | "textDocument" -> (
            match Ppx_yojson_conv_lib.( ! ) textDocument_field with
            | None ->
              let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
              textDocument_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | "position" -> (
            match Ppx_yojson_conv_lib.( ! ) position_field with
            | None ->
              let fvalue = Position.t_of_yojson _field_yojson in
              position_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | "context" -> (
            match Ppx_yojson_conv_lib.( ! ) context_field with
            | None ->
              let fvalue = completionContext_of_yojson _field_yojson in
              context_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | _ -> () );
          iter tail
        | [] -> ()
      in
      iter field_yojsons;
      match Ppx_yojson_conv_lib.( ! ) duplicates with
      | _ :: _ ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
          (Ppx_yojson_conv_lib.( ! ) duplicates)
          yojson
      | [] -> (
        match Ppx_yojson_conv_lib.( ! ) extra with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) extra)
            yojson
        | [] -> (
          match
            ( Ppx_yojson_conv_lib.( ! ) textDocument_field
            , Ppx_yojson_conv_lib.( ! ) position_field
            , Ppx_yojson_conv_lib.( ! ) context_field )
          with
          | Some textDocument_value, Some position_value, context_value ->
            { textDocument = textDocument_value
            ; position = position_value
            ; context = context_value
            }
          | _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
              _tp_loc yojson
              [ ( Ppx_yojson_conv_lib.poly_equal
                    (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                    None
                , "textDocument" )
              ; ( Ppx_yojson_conv_lib.poly_equal
                    (Ppx_yojson_conv_lib.( ! ) position_field)
                    None
                , "position" )
              ] ) ) )
    | _ as yojson ->
      Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
        yojson
    : Ppx_yojson_conv_lib.Yojson.Safe.t -> completionParams )

and completionContext_of_yojson =
  ( let _tp_loc = "lsp/src/protocol.ml.Completion.completionContext" in
    function
    | `Assoc field_yojsons as yojson -> (
      let triggerKind_field = ref None
      and triggerCharacter_field = ref None
      and duplicates = ref []
      and extra = ref [] in
      let rec iter = function
        | (field_name, _field_yojson) :: tail ->
          ( match field_name with
          | "triggerKind" -> (
            match Ppx_yojson_conv_lib.( ! ) triggerKind_field with
            | None ->
              let fvalue = completionTriggerKind_of_yojson _field_yojson in
              triggerKind_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | "triggerCharacter" -> (
            match Ppx_yojson_conv_lib.( ! ) triggerCharacter_field with
            | None ->
              let fvalue = string_of_yojson _field_yojson in
              triggerCharacter_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | _ -> () );
          iter tail
        | [] -> ()
      in
      iter field_yojsons;
      match Ppx_yojson_conv_lib.( ! ) duplicates with
      | _ :: _ ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
          (Ppx_yojson_conv_lib.( ! ) duplicates)
          yojson
      | [] -> (
        match Ppx_yojson_conv_lib.( ! ) extra with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) extra)
            yojson
        | [] -> (
          match
            ( Ppx_yojson_conv_lib.( ! ) triggerKind_field
            , Ppx_yojson_conv_lib.( ! ) triggerCharacter_field )
          with
          | Some triggerKind_value, triggerCharacter_value ->
            { triggerKind = triggerKind_value
            ; triggerCharacter = triggerCharacter_value
            }
          | _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
              _tp_loc yojson
              [ ( Ppx_yojson_conv_lib.poly_equal
                    (Ppx_yojson_conv_lib.( ! ) triggerKind_field)
                    None
                , "triggerKind" )
              ] ) ) )
    | _ as yojson ->
      Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
        yojson
    : Ppx_yojson_conv_lib.Yojson.Safe.t -> completionContext )

and result_of_yojson =
  ( let _tp_loc = "lsp/src/protocol.ml.Completion.result" in
    fun t -> completionList_of_yojson t
    : Ppx_yojson_conv_lib.Yojson.Safe.t -> result )

and completionList_of_yojson =
  ( let _tp_loc = "lsp/src/protocol.ml.Completion.completionList" in
    function
    | `Assoc field_yojsons as yojson -> (
      let isIncomplete_field = ref None
      and items_field = ref None
      and duplicates = ref []
      and extra = ref [] in
      let rec iter = function
        | (field_name, _field_yojson) :: tail ->
          ( match field_name with
          | "isIncomplete" -> (
            match Ppx_yojson_conv_lib.( ! ) isIncomplete_field with
            | None ->
              let fvalue = bool_of_yojson _field_yojson in
              isIncomplete_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | "items" -> (
            match Ppx_yojson_conv_lib.( ! ) items_field with
            | None ->
              let fvalue =
                list_of_yojson completionItem_of_yojson _field_yojson
              in
              items_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | _ -> () );
          iter tail
        | [] -> ()
      in
      iter field_yojsons;
      match Ppx_yojson_conv_lib.( ! ) duplicates with
      | _ :: _ ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
          (Ppx_yojson_conv_lib.( ! ) duplicates)
          yojson
      | [] -> (
        match Ppx_yojson_conv_lib.( ! ) extra with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) extra)
            yojson
        | [] -> (
          match
            ( Ppx_yojson_conv_lib.( ! ) isIncomplete_field
            , Ppx_yojson_conv_lib.( ! ) items_field )
          with
          | Some isIncomplete_value, Some items_value ->
            { isIncomplete = isIncomplete_value; items = items_value }
          | _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
              _tp_loc yojson
              [ ( Ppx_yojson_conv_lib.poly_equal
                    (Ppx_yojson_conv_lib.( ! ) isIncomplete_field)
                    None
                , "isIncomplete" )
              ; ( Ppx_yojson_conv_lib.poly_equal
                    (Ppx_yojson_conv_lib.( ! ) items_field)
                    None
                , "items" )
              ] ) ) )
    | _ as yojson ->
      Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
        yojson
    : Ppx_yojson_conv_lib.Yojson.Safe.t -> completionList )

and completionItem_of_yojson =
  ( let _tp_loc = "lsp/src/protocol.ml.Completion.completionItem" in
    function
    | `Assoc field_yojsons as yojson -> (
      let label_field = ref None
      and kind_field = ref None
      and detail_field = ref None
      and documentation_field = ref None
      and sortText_field = ref None
      and filterText_field = ref None
      and insertText_field = ref None
      and insertTextFormat_field = ref None
      and textEdit_field = ref None
      and additionalTextEdits_field = ref None
      and commitCharacters_field = ref None
      and data_field = ref None
      and duplicates = ref []
      and extra = ref [] in
      let rec iter = function
        | (field_name, _field_yojson) :: tail ->
          ( match field_name with
          | "label" -> (
            match Ppx_yojson_conv_lib.( ! ) label_field with
            | None ->
              let fvalue = string_of_yojson _field_yojson in
              label_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | "kind" -> (
            match Ppx_yojson_conv_lib.( ! ) kind_field with
            | None ->
              let fvalue = completionItemKind_of_yojson _field_yojson in
              kind_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | "detail" -> (
            match Ppx_yojson_conv_lib.( ! ) detail_field with
            | None ->
              let fvalue = string_of_yojson _field_yojson in
              detail_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | "documentation" -> (
            match Ppx_yojson_conv_lib.( ! ) documentation_field with
            | None ->
              let fvalue = string_of_yojson _field_yojson in
              documentation_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | "sortText" -> (
            match Ppx_yojson_conv_lib.( ! ) sortText_field with
            | None ->
              let fvalue = string_of_yojson _field_yojson in
              sortText_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | "filterText" -> (
            match Ppx_yojson_conv_lib.( ! ) filterText_field with
            | None ->
              let fvalue = string_of_yojson _field_yojson in
              filterText_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | "insertText" -> (
            match Ppx_yojson_conv_lib.( ! ) insertText_field with
            | None ->
              let fvalue = string_of_yojson _field_yojson in
              insertText_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | "insertTextFormat" -> (
            match Ppx_yojson_conv_lib.( ! ) insertTextFormat_field with
            | None ->
              let fvalue = insertTextFormat_of_yojson _field_yojson in
              insertTextFormat_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | "textEdit" -> (
            match Ppx_yojson_conv_lib.( ! ) textEdit_field with
            | None ->
              let fvalue = TextEdit.t_of_yojson _field_yojson in
              textEdit_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | "additionalTextEdits" -> (
            match Ppx_yojson_conv_lib.( ! ) additionalTextEdits_field with
            | None ->
              let fvalue = list_of_yojson TextEdit.t_of_yojson _field_yojson in
              additionalTextEdits_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | "commitCharacters" -> (
            match Ppx_yojson_conv_lib.( ! ) commitCharacters_field with
            | None ->
              let fvalue = list_of_yojson string_of_yojson _field_yojson in
              commitCharacters_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | "data" -> (
            match Ppx_yojson_conv_lib.( ! ) data_field with
            | None ->
              let fvalue = json_of_yojson _field_yojson in
              data_field := Some fvalue
            | Some _ ->
              duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
          | _ -> () );
          iter tail
        | [] -> ()
      in
      iter field_yojsons;
      match Ppx_yojson_conv_lib.( ! ) duplicates with
      | _ :: _ ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
          (Ppx_yojson_conv_lib.( ! ) duplicates)
          yojson
      | [] -> (
        match Ppx_yojson_conv_lib.( ! ) extra with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) extra)
            yojson
        | [] -> (
          match
            ( Ppx_yojson_conv_lib.( ! ) label_field
            , Ppx_yojson_conv_lib.( ! ) kind_field
            , Ppx_yojson_conv_lib.( ! ) detail_field
            , Ppx_yojson_conv_lib.( ! ) documentation_field
            , Ppx_yojson_conv_lib.( ! ) sortText_field
            , Ppx_yojson_conv_lib.( ! ) filterText_field
            , Ppx_yojson_conv_lib.( ! ) insertText_field
            , Ppx_yojson_conv_lib.( ! ) insertTextFormat_field
            , Ppx_yojson_conv_lib.( ! ) textEdit_field
            , Ppx_yojson_conv_lib.( ! ) additionalTextEdits_field
            , Ppx_yojson_conv_lib.( ! ) commitCharacters_field
            , Ppx_yojson_conv_lib.( ! ) data_field )
          with
          | ( Some label_value
            , kind_value
            , detail_value
            , documentation_value
            , sortText_value
            , filterText_value
            , insertText_value
            , insertTextFormat_value
            , textEdit_value
            , additionalTextEdits_value
            , commitCharacters_value
            , data_value ) ->
            { label = label_value
            ; kind = kind_value
            ; detail = detail_value
            ; documentation = documentation_value
            ; sortText = sortText_value
            ; filterText = filterText_value
            ; insertText = insertText_value
            ; insertTextFormat = insertTextFormat_value
            ; textEdit = textEdit_value
            ; additionalTextEdits =
                ( match additionalTextEdits_value with
                | None -> []
                | Some v -> v )
            ; commitCharacters =
                ( match commitCharacters_value with
                | None -> []
                | Some v -> v )
            ; data = data_value
            }
          | _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
              _tp_loc yojson
              [ ( Ppx_yojson_conv_lib.poly_equal
                    (Ppx_yojson_conv_lib.( ! ) label_field)
                    None
                , "label" )
              ] ) ) )
    | _ as yojson ->
      Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
        yojson
    : Ppx_yojson_conv_lib.Yojson.Safe.t -> completionItem )

let _ = params_of_yojson

and _ = completionParams_of_yojson

and _ = completionContext_of_yojson

and _ = result_of_yojson

and _ = completionList_of_yojson

and _ = completionItem_of_yojson

let rec yojson_of_params =
  ( fun v -> yojson_of_completionParams v
    : params -> Ppx_yojson_conv_lib.Yojson.Safe.t )

and yojson_of_completionParams =
  ( function
    | { textDocument = v_textDocument
      ; position = v_position
      ; context = v_context
      } ->
      let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
      let bnds =
        match v_context with
        | None -> bnds
        | Some v ->
          let arg = yojson_of_completionContext v in
          let bnd = ("context", arg) in
          bnd :: bnds
      in
      let bnds =
        let arg = Position.yojson_of_t v_position in
        ("position", arg) :: bnds
      in
      let bnds =
        let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
        ("textDocument", arg) :: bnds
      in
      `Assoc bnds
    : completionParams -> Ppx_yojson_conv_lib.Yojson.Safe.t )

and yojson_of_completionContext =
  ( function
    | { triggerKind = v_triggerKind; triggerCharacter = v_triggerCharacter } ->
      let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
      let bnds =
        match v_triggerCharacter with
        | None -> bnds
        | Some v ->
          let arg = yojson_of_string v in
          let bnd = ("triggerCharacter", arg) in
          bnd :: bnds
      in
      let bnds =
        let arg = yojson_of_completionTriggerKind v_triggerKind in
        ("triggerKind", arg) :: bnds
      in
      `Assoc bnds
    : completionContext -> Ppx_yojson_conv_lib.Yojson.Safe.t )

and yojson_of_result =
  ( fun v -> yojson_of_completionList v
    : result -> Ppx_yojson_conv_lib.Yojson.Safe.t )

and yojson_of_completionList =
  ( function
    | { isIncomplete = v_isIncomplete; items = v_items } ->
      let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
      let bnds =
        let arg = yojson_of_list yojson_of_completionItem v_items in
        ("items", arg) :: bnds
      in
      let bnds =
        let arg = yojson_of_bool v_isIncomplete in
        ("isIncomplete", arg) :: bnds
      in
      `Assoc bnds
    : completionList -> Ppx_yojson_conv_lib.Yojson.Safe.t )

and yojson_of_completionItem =
  ( function
    | { label = v_label
      ; kind = v_kind
      ; detail = v_detail
      ; documentation = v_documentation
      ; sortText = v_sortText
      ; filterText = v_filterText
      ; insertText = v_insertText
      ; insertTextFormat = v_insertTextFormat
      ; textEdit = v_textEdit
      ; additionalTextEdits = v_additionalTextEdits
      ; commitCharacters = v_commitCharacters
      ; data = v_data
      } ->
      let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
      let bnds =
        match v_data with
        | None -> bnds
        | Some v ->
          let arg = yojson_of_json v in
          let bnd = ("data", arg) in
          bnd :: bnds
      in
      let bnds =
        if [] = v_commitCharacters then
          bnds
        else
          let arg = (yojson_of_list yojson_of_string) v_commitCharacters in
          let bnd = ("commitCharacters", arg) in
          bnd :: bnds
      in
      let bnds =
        if [] = v_additionalTextEdits then
          bnds
        else
          let arg =
            (yojson_of_list TextEdit.yojson_of_t) v_additionalTextEdits
          in
          let bnd = ("additionalTextEdits", arg) in
          bnd :: bnds
      in
      let bnds =
        match v_textEdit with
        | None -> bnds
        | Some v ->
          let arg = TextEdit.yojson_of_t v in
          let bnd = ("textEdit", arg) in
          bnd :: bnds
      in
      let bnds =
        match v_insertTextFormat with
        | None -> bnds
        | Some v ->
          let arg = yojson_of_insertTextFormat v in
          let bnd = ("insertTextFormat", arg) in
          bnd :: bnds
      in
      let bnds =
        match v_insertText with
        | None -> bnds
        | Some v ->
          let arg = yojson_of_string v in
          let bnd = ("insertText", arg) in
          bnd :: bnds
      in
      let bnds =
        match v_filterText with
        | None -> bnds
        | Some v ->
          let arg = yojson_of_string v in
          let bnd = ("filterText", arg) in
          bnd :: bnds
      in
      let bnds =
        match v_sortText with
        | None -> bnds
        | Some v ->
          let arg = yojson_of_string v in
          let bnd = ("sortText", arg) in
          bnd :: bnds
      in
      let bnds =
        match v_documentation with
        | None -> bnds
        | Some v ->
          let arg = yojson_of_string v in
          let bnd = ("documentation", arg) in
          bnd :: bnds
      in
      let bnds =
        match v_detail with
        | None -> bnds
        | Some v ->
          let arg = yojson_of_string v in
          let bnd = ("detail", arg) in
          bnd :: bnds
      in
      let bnds =
        match v_kind with
        | None -> bnds
        | Some v ->
          let arg = yojson_of_completionItemKind v in
          let bnd = ("kind", arg) in
          bnd :: bnds
      in
      let bnds =
        let arg = yojson_of_string v_label in
        ("label", arg) :: bnds
      in
      `Assoc bnds
    : completionItem -> Ppx_yojson_conv_lib.Yojson.Safe.t )

let _ = yojson_of_params

and _ = yojson_of_completionParams

and _ = yojson_of_completionContext

and _ = yojson_of_result

and _ = yojson_of_completionList

and _ = yojson_of_completionItem

[@@@end]

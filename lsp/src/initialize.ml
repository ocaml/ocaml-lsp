open Import
open Protocol

module SignatureHelpOptions = struct
  type t = { triggerCharacters : string list [@default []] }
  [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.SignatureHelpOptions.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let triggerCharacters_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "triggerCharacters" -> (
              match Ppx_yojson_conv_lib.( ! ) triggerCharacters_field with
              | None ->
                let fvalue = list_of_yojson string_of_yojson _field_yojson in
                triggerCharacters_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ ->
              if
                Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
              then
                extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
              else
                () );
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
          | [] ->
            let triggerCharacters_value =
              Ppx_yojson_conv_lib.( ! ) triggerCharacters_field
            in
            { triggerCharacters =
                ( match triggerCharacters_value with
                | None -> []
                | Some v -> v )
            } ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { triggerCharacters = v_triggerCharacters } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_list yojson_of_string v_triggerCharacters in
          ("triggerCharacters", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module CodeActionOptions = struct
  type t = { codeActionsKinds : CodeAction.Kind.t list [@default []] }
  [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.CodeActionOptions.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let codeActionsKinds_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "codeActionsKinds" -> (
              match Ppx_yojson_conv_lib.( ! ) codeActionsKinds_field with
              | None ->
                let fvalue =
                  list_of_yojson CodeAction.Kind.t_of_yojson _field_yojson
                in
                codeActionsKinds_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ ->
              if
                Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
              then
                extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
              else
                () );
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
          | [] ->
            let codeActionsKinds_value =
              Ppx_yojson_conv_lib.( ! ) codeActionsKinds_field
            in
            { codeActionsKinds =
                ( match codeActionsKinds_value with
                | None -> []
                | Some v -> v )
            } ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { codeActionsKinds = v_codeActionsKinds } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg =
            yojson_of_list CodeAction.Kind.yojson_of_t v_codeActionsKinds
          in
          ("codeActionsKinds", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module CodeActionLiteralSupport = struct
  type codeActionKind = { valueSet : CodeAction.Kind.t list }
  [@@deriving_inline yojson]

  let _ = fun (_ : codeActionKind) -> ()

  let codeActionKind_of_yojson =
    ( let _tp_loc =
        "lsp/src/initialize.ml.CodeActionLiteralSupport.codeActionKind"
      in
      function
      | `Assoc field_yojsons as yojson -> (
        let valueSet_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "valueSet" -> (
              match Ppx_yojson_conv_lib.( ! ) valueSet_field with
              | None ->
                let fvalue =
                  list_of_yojson CodeAction.Kind.t_of_yojson _field_yojson
                in
                valueSet_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ ->
              if
                Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
              then
                extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
              else
                () );
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
            match Ppx_yojson_conv_lib.( ! ) valueSet_field with
            | Some valueSet_value -> { valueSet = valueSet_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) valueSet_field)
                      None
                  , "valueSet" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> codeActionKind )

  let _ = codeActionKind_of_yojson

  let yojson_of_codeActionKind =
    ( function
      | { valueSet = v_valueSet } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_list CodeAction.Kind.yojson_of_t v_valueSet in
          ("valueSet", arg) :: bnds
        in
        `Assoc bnds
      : codeActionKind -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_codeActionKind

  [@@@end]

  type t = { codeActionKind : codeActionKind } [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.CodeActionLiteralSupport.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let codeActionKind_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "codeActionKind" -> (
              match Ppx_yojson_conv_lib.( ! ) codeActionKind_field with
              | None ->
                let fvalue = codeActionKind_of_yojson _field_yojson in
                codeActionKind_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ ->
              if
                Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
              then
                extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
              else
                () );
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
            match Ppx_yojson_conv_lib.( ! ) codeActionKind_field with
            | Some codeActionKind_value ->
              { codeActionKind = codeActionKind_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) codeActionKind_field)
                      None
                  , "codeActionKind" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { codeActionKind = v_codeActionKind } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_codeActionKind v_codeActionKind in
          ("codeActionKind", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module Trace = struct
  type t =
    | Off
    | Messages
    | Verbose

  let yojson_of_t = function
    | Off -> `String "off"
    | Messages -> `String "messages"
    | Verbose -> `String "verbose"

  let t_of_yojson = function
    | `String "off" -> Off
    | `String "messages" -> Messages
    | `String "verbose" -> Verbose
    | node -> Json.error "invalid trace" node
end

module TextDocumentSyncKind = struct
  type t =
    | NoSync
    | FullSync
    | IncrementalSync

  let yojson_of_t = function
    | NoSync -> `Int 0
    | FullSync -> `Int 1
    | IncrementalSync -> `Int 2

  let t_of_yojson = function
    | `Int 0 -> NoSync
    | `Int 1 -> FullSync
    | `Int 2 -> IncrementalSync
    | node -> Json.error "invalid textDocumentSyncKind" node
end

module Synchronization = struct
  (* synchronization capabilities say what messages the client is capable
   * of sending, should be be so asked by the server.
   * We use the "can_" prefix for OCaml naming reasons; it's absent in LSP *)

  type t =
    { willSave : bool [@default false]
    ; (* client can send textDocument/willSave *)
      willSaveWaitUntil : bool [@default false]
    ; (* textDoc.../willSaveWaitUntil *)
      didSave : bool [@default false] (* textDocument/didSave *)
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.Synchronization.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let willSave_field = ref None
        and willSaveWaitUntil_field = ref None
        and didSave_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "willSave" -> (
              match Ppx_yojson_conv_lib.( ! ) willSave_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                willSave_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "willSaveWaitUntil" -> (
              match Ppx_yojson_conv_lib.( ! ) willSaveWaitUntil_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                willSaveWaitUntil_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "didSave" -> (
              match Ppx_yojson_conv_lib.( ! ) didSave_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                didSave_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
          | [] ->
            let willSave_value, willSaveWaitUntil_value, didSave_value =
              ( Ppx_yojson_conv_lib.( ! ) willSave_field
              , Ppx_yojson_conv_lib.( ! ) willSaveWaitUntil_field
              , Ppx_yojson_conv_lib.( ! ) didSave_field )
            in
            { willSave =
                ( match willSave_value with
                | None -> false
                | Some v -> v )
            ; willSaveWaitUntil =
                ( match willSaveWaitUntil_value with
                | None -> false
                | Some v -> v )
            ; didSave =
                ( match didSave_value with
                | None -> false
                | Some v -> v )
            } ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { willSave = v_willSave
        ; willSaveWaitUntil = v_willSaveWaitUntil
        ; didSave = v_didSave
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_bool v_didSave in
          ("didSave", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_willSaveWaitUntil in
          ("willSaveWaitUntil", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_willSave in
          ("willSave", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let empty = { willSave = true; willSaveWaitUntil = true; didSave = true }
end

module CompletionItem = struct
  module TagSupport = struct
    type t = { valueSet : Completion.ItemTag.t list }
    [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

    let _ = fun (_ : t) -> ()

    let t_of_yojson =
      ( let _tp_loc = "lsp/src/initialize.ml.CompletionItem.TagSupport.t" in
        function
        | `Assoc field_yojsons as yojson -> (
          let valueSet_field = ref None
          and duplicates = ref []
          and extra = ref [] in
          let rec iter = function
            | (field_name, _field_yojson) :: tail ->
              ( match field_name with
              | "valueSet" -> (
                match Ppx_yojson_conv_lib.( ! ) valueSet_field with
                | None ->
                  let fvalue =
                    list_of_yojson Completion.ItemTag.t_of_yojson _field_yojson
                  in
                  valueSet_field := Some fvalue
                | Some _ ->
                  duplicates :=
                    field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
              | _ -> () );
              iter tail
            | [] -> ()
          in
          iter field_yojsons;
          match Ppx_yojson_conv_lib.( ! ) duplicates with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
              _tp_loc
              (Ppx_yojson_conv_lib.( ! ) duplicates)
              yojson
          | [] -> (
            match Ppx_yojson_conv_lib.( ! ) extra with
            | _ :: _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
                (Ppx_yojson_conv_lib.( ! ) extra)
                yojson
            | [] -> (
              match Ppx_yojson_conv_lib.( ! ) valueSet_field with
              | Some valueSet_value -> { valueSet = valueSet_value }
              | _ ->
                Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                  _tp_loc yojson
                  [ ( Ppx_yojson_conv_lib.poly_equal
                        (Ppx_yojson_conv_lib.( ! ) valueSet_field)
                        None
                    , "valueSet" )
                  ] ) ) )
        | _ as yojson ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
            yojson
        : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

    let _ = t_of_yojson

    let yojson_of_t =
      ( function
        | { valueSet = v_valueSet } ->
          let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
          let bnds =
            let arg =
              yojson_of_list Completion.ItemTag.yojson_of_t v_valueSet
            in
            ("valueSet", arg) :: bnds
          in
          `Assoc bnds
        : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

    let _ = yojson_of_t

    [@@@end]

    let default = { valueSet = [] }
  end

  type t =
    { snippetSupport : bool
          [@default false] (* client can do snippets as insert text *)
    ; commitCharactersSupport : bool [@default false]
    ; documentationFormat : MarkupKind.t list [@default []]
    ; deprecatedSupport : bool [@default false]
    ; preselectSupport : bool [@default false]
    ; tagSupport : TagSupport.t [@default TagSupport.default]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.CompletionItem.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let snippetSupport_field = ref None
        and commitCharactersSupport_field = ref None
        and documentationFormat_field = ref None
        and deprecatedSupport_field = ref None
        and preselectSupport_field = ref None
        and tagSupport_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "snippetSupport" -> (
              match Ppx_yojson_conv_lib.( ! ) snippetSupport_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                snippetSupport_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "commitCharactersSupport" -> (
              match Ppx_yojson_conv_lib.( ! ) commitCharactersSupport_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                commitCharactersSupport_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "documentationFormat" -> (
              match Ppx_yojson_conv_lib.( ! ) documentationFormat_field with
              | None ->
                let fvalue =
                  list_of_yojson MarkupKind.t_of_yojson _field_yojson
                in
                documentationFormat_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "deprecatedSupport" -> (
              match Ppx_yojson_conv_lib.( ! ) deprecatedSupport_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                deprecatedSupport_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "preselectSupport" -> (
              match Ppx_yojson_conv_lib.( ! ) preselectSupport_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                preselectSupport_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "tagSupport" -> (
              match Ppx_yojson_conv_lib.( ! ) tagSupport_field with
              | None ->
                let fvalue = TagSupport.t_of_yojson _field_yojson in
                tagSupport_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
          | [] ->
            let ( snippetSupport_value
                , commitCharactersSupport_value
                , documentationFormat_value
                , deprecatedSupport_value
                , preselectSupport_value
                , tagSupport_value ) =
              ( Ppx_yojson_conv_lib.( ! ) snippetSupport_field
              , Ppx_yojson_conv_lib.( ! ) commitCharactersSupport_field
              , Ppx_yojson_conv_lib.( ! ) documentationFormat_field
              , Ppx_yojson_conv_lib.( ! ) deprecatedSupport_field
              , Ppx_yojson_conv_lib.( ! ) preselectSupport_field
              , Ppx_yojson_conv_lib.( ! ) tagSupport_field )
            in
            { snippetSupport =
                ( match snippetSupport_value with
                | None -> false
                | Some v -> v )
            ; commitCharactersSupport =
                ( match commitCharactersSupport_value with
                | None -> false
                | Some v -> v )
            ; documentationFormat =
                ( match documentationFormat_value with
                | None -> []
                | Some v -> v )
            ; deprecatedSupport =
                ( match deprecatedSupport_value with
                | None -> false
                | Some v -> v )
            ; preselectSupport =
                ( match preselectSupport_value with
                | None -> false
                | Some v -> v )
            ; tagSupport =
                ( match tagSupport_value with
                | None -> TagSupport.default
                | Some v -> v )
            } ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { snippetSupport = v_snippetSupport
        ; commitCharactersSupport = v_commitCharactersSupport
        ; documentationFormat = v_documentationFormat
        ; deprecatedSupport = v_deprecatedSupport
        ; preselectSupport = v_preselectSupport
        ; tagSupport = v_tagSupport
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = TagSupport.yojson_of_t v_tagSupport in
          ("tagSupport", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_preselectSupport in
          ("preselectSupport", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_deprecatedSupport in
          ("deprecatedSupport", arg) :: bnds
        in
        let bnds =
          let arg =
            yojson_of_list MarkupKind.yojson_of_t v_documentationFormat
          in
          ("documentationFormat", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_commitCharactersSupport in
          ("commitCharactersSupport", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_snippetSupport in
          ("snippetSupport", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let empty =
    { snippetSupport = false
    ; commitCharactersSupport = false
    ; documentationFormat = []
    ; deprecatedSupport = false
    ; preselectSupport = false
    ; tagSupport = TagSupport.default
    }
end

module Completion = struct
  type t = { completionItem : CompletionItem.t [@default CompletionItem.empty] }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.Completion.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let completionItem_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "completionItem" -> (
              match Ppx_yojson_conv_lib.( ! ) completionItem_field with
              | None ->
                let fvalue = CompletionItem.t_of_yojson _field_yojson in
                completionItem_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
          | [] ->
            let completionItem_value =
              Ppx_yojson_conv_lib.( ! ) completionItem_field
            in
            { completionItem =
                ( match completionItem_value with
                | None -> CompletionItem.empty
                | Some v -> v )
            } ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { completionItem = v_completionItem } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = CompletionItem.yojson_of_t v_completionItem in
          ("completionItem", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let empty = { completionItem = CompletionItem.empty }
end

module Hover = struct
  type t = { contentFormat : MarkupKind.t list [@default [ Plaintext ]] }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.Hover.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let contentFormat_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "contentFormat" -> (
              match Ppx_yojson_conv_lib.( ! ) contentFormat_field with
              | None ->
                let fvalue =
                  list_of_yojson MarkupKind.t_of_yojson _field_yojson
                in
                contentFormat_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
          | [] ->
            let contentFormat_value =
              Ppx_yojson_conv_lib.( ! ) contentFormat_field
            in
            { contentFormat =
                ( match contentFormat_value with
                | None -> [ Plaintext ]
                | Some v -> v )
            } ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { contentFormat = v_contentFormat } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_list MarkupKind.yojson_of_t v_contentFormat in
          ("contentFormat", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let empty = { contentFormat = [ Plaintext ] }
end

module CodeAction = struct
  type t =
    { codeActionLiteralSupport : CodeActionLiteralSupport.t option
          [@yojson.option]
    ; dynamicRegistration : bool option [@yojson.option]
    ; isPreferredSupport : bool option [@yojson.option]
    }
  [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.CodeAction.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let codeActionLiteralSupport_field = ref None
        and dynamicRegistration_field = ref None
        and isPreferredSupport_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "codeActionLiteralSupport" -> (
              match
                Ppx_yojson_conv_lib.( ! ) codeActionLiteralSupport_field
              with
              | None ->
                let fvalue =
                  CodeActionLiteralSupport.t_of_yojson _field_yojson
                in
                codeActionLiteralSupport_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "dynamicRegistration" -> (
              match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                dynamicRegistration_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "isPreferredSupport" -> (
              match Ppx_yojson_conv_lib.( ! ) isPreferredSupport_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                isPreferredSupport_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ ->
              if
                Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
              then
                extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
              else
                () );
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
          | [] ->
            let ( codeActionLiteralSupport_value
                , dynamicRegistration_value
                , isPreferredSupport_value ) =
              ( Ppx_yojson_conv_lib.( ! ) codeActionLiteralSupport_field
              , Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
              , Ppx_yojson_conv_lib.( ! ) isPreferredSupport_field )
            in
            { codeActionLiteralSupport = codeActionLiteralSupport_value
            ; dynamicRegistration = dynamicRegistration_value
            ; isPreferredSupport = isPreferredSupport_value
            } ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { codeActionLiteralSupport = v_codeActionLiteralSupport
        ; dynamicRegistration = v_dynamicRegistration
        ; isPreferredSupport = v_isPreferredSupport
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          match v_isPreferredSupport with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_bool v in
            let bnd = ("isPreferredSupport", arg) in
            bnd :: bnds
        in
        let bnds =
          match v_dynamicRegistration with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_bool v in
            let bnd = ("dynamicRegistration", arg) in
            bnd :: bnds
        in
        let bnds =
          match v_codeActionLiteralSupport with
          | None -> bnds
          | Some v ->
            let arg = CodeActionLiteralSupport.yojson_of_t v in
            let bnd = ("codeActionLiteralSupport", arg) in
            bnd :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let empty =
    { codeActionLiteralSupport = None
    ; dynamicRegistration = None
    ; isPreferredSupport = None
    }
end

module DocumentSymbol = struct
  type t = { hierarchicalDocumentSymbolSupport : bool [@default false] }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.DocumentSymbol.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let hierarchicalDocumentSymbolSupport_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "hierarchicalDocumentSymbolSupport" -> (
              match
                Ppx_yojson_conv_lib.( ! )
                  hierarchicalDocumentSymbolSupport_field
              with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                hierarchicalDocumentSymbolSupport_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
          | [] ->
            let hierarchicalDocumentSymbolSupport_value =
              Ppx_yojson_conv_lib.( ! ) hierarchicalDocumentSymbolSupport_field
            in
            { hierarchicalDocumentSymbolSupport =
                ( match hierarchicalDocumentSymbolSupport_value with
                | None -> false
                | Some v -> v )
            } ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { hierarchicalDocumentSymbolSupport =
            v_hierarchicalDocumentSymbolSupport
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_bool v_hierarchicalDocumentSymbolSupport in
          ("hierarchicalDocumentSymbolSupport", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let empty = { hierarchicalDocumentSymbolSupport = false }
end

module PublishDiagnosticsClientCapabilities = struct
  type tagSupport = { valueSet : Diagnostics.Tag.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : tagSupport) -> ()

  let tagSupport_of_yojson =
    ( let _tp_loc =
        "lsp/src/initialize.ml.PublishDiagnosticsClientCapabilities.tagSupport"
      in
      function
      | `Assoc field_yojsons as yojson -> (
        let valueSet_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "valueSet" -> (
              match Ppx_yojson_conv_lib.( ! ) valueSet_field with
              | None ->
                let fvalue =
                  list_of_yojson Diagnostics.Tag.t_of_yojson _field_yojson
                in
                valueSet_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
            match Ppx_yojson_conv_lib.( ! ) valueSet_field with
            | Some valueSet_value -> { valueSet = valueSet_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) valueSet_field)
                      None
                  , "valueSet" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> tagSupport )

  let _ = tagSupport_of_yojson

  let yojson_of_tagSupport =
    ( function
      | { valueSet = v_valueSet } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_list Diagnostics.Tag.yojson_of_t v_valueSet in
          ("valueSet", arg) :: bnds
        in
        `Assoc bnds
      : tagSupport -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_tagSupport

  [@@@end]

  type t =
    { relatedInformation : bool [@default false]
    ; tagSupport : tagSupport option [@yojson.option]
    ; versionSupport : bool [@default false]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc =
        "lsp/src/initialize.ml.PublishDiagnosticsClientCapabilities.t"
      in
      function
      | `Assoc field_yojsons as yojson -> (
        let relatedInformation_field = ref None
        and tagSupport_field = ref None
        and versionSupport_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "relatedInformation" -> (
              match Ppx_yojson_conv_lib.( ! ) relatedInformation_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                relatedInformation_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "tagSupport" -> (
              match Ppx_yojson_conv_lib.( ! ) tagSupport_field with
              | None ->
                let fvalue = tagSupport_of_yojson _field_yojson in
                tagSupport_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "versionSupport" -> (
              match Ppx_yojson_conv_lib.( ! ) versionSupport_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                versionSupport_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
          | [] ->
            let relatedInformation_value, tagSupport_value, versionSupport_value
                =
              ( Ppx_yojson_conv_lib.( ! ) relatedInformation_field
              , Ppx_yojson_conv_lib.( ! ) tagSupport_field
              , Ppx_yojson_conv_lib.( ! ) versionSupport_field )
            in
            { relatedInformation =
                ( match relatedInformation_value with
                | None -> false
                | Some v -> v )
            ; tagSupport = tagSupport_value
            ; versionSupport =
                ( match versionSupport_value with
                | None -> false
                | Some v -> v )
            } ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { relatedInformation = v_relatedInformation
        ; tagSupport = v_tagSupport
        ; versionSupport = v_versionSupport
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_bool v_versionSupport in
          ("versionSupport", arg) :: bnds
        in
        let bnds =
          match v_tagSupport with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_tagSupport v in
            let bnd = ("tagSupport", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_relatedInformation in
          ("relatedInformation", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let empty =
    { relatedInformation = false; tagSupport = None; versionSupport = false }
end

module FoldingRangeClientCapabilities = struct
  type t =
    { rangeLimit : int option
    ; lineFoldingOnly : bool [@default false]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.FoldingRangeClientCapabilities.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let rangeLimit_field = ref None
        and lineFoldingOnly_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "rangeLimit" -> (
              match Ppx_yojson_conv_lib.( ! ) rangeLimit_field with
              | None ->
                let fvalue = option_of_yojson int_of_yojson _field_yojson in
                rangeLimit_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "lineFoldingOnly" -> (
              match Ppx_yojson_conv_lib.( ! ) lineFoldingOnly_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                lineFoldingOnly_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
              ( Ppx_yojson_conv_lib.( ! ) rangeLimit_field
              , Ppx_yojson_conv_lib.( ! ) lineFoldingOnly_field )
            with
            | Some rangeLimit_value, lineFoldingOnly_value ->
              { rangeLimit = rangeLimit_value
              ; lineFoldingOnly =
                  ( match lineFoldingOnly_value with
                  | None -> false
                  | Some v -> v )
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) rangeLimit_field)
                      None
                  , "rangeLimit" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { rangeLimit = v_rangeLimit; lineFoldingOnly = v_lineFoldingOnly } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_bool v_lineFoldingOnly in
          ("lineFoldingOnly", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_option yojson_of_int v_rangeLimit in
          ("rangeLimit", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let empty = { rangeLimit = None; lineFoldingOnly = false }
end

module SignatureHelpClientCapabilities = struct
  type parameterInformation = { labelOffsetSupport : bool [@default false] }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : parameterInformation) -> ()

  let parameterInformation_of_yojson =
    ( let _tp_loc =
        "lsp/src/initialize.ml.SignatureHelpClientCapabilities.parameterInformation"
      in
      function
      | `Assoc field_yojsons as yojson -> (
        let labelOffsetSupport_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "labelOffsetSupport" -> (
              match Ppx_yojson_conv_lib.( ! ) labelOffsetSupport_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                labelOffsetSupport_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
          | [] ->
            let labelOffsetSupport_value =
              Ppx_yojson_conv_lib.( ! ) labelOffsetSupport_field
            in
            { labelOffsetSupport =
                ( match labelOffsetSupport_value with
                | None -> false
                | Some v -> v )
            } ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> parameterInformation )

  let _ = parameterInformation_of_yojson

  let yojson_of_parameterInformation =
    ( function
      | { labelOffsetSupport = v_labelOffsetSupport } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_bool v_labelOffsetSupport in
          ("labelOffsetSupport", arg) :: bnds
        in
        `Assoc bnds
      : parameterInformation -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_parameterInformation

  [@@@end]

  let parameterInformation_empty = { labelOffsetSupport = false }

  type signatureInformation =
    { documentationFormat : MarkupKind.t list [@default []]
    ; parameterInformation : parameterInformation
          [@default parameterInformation_empty]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : signatureInformation) -> ()

  
let signatureInformation_of_yojson =
  (let _tp_loc =
     "lsp/src/initialize.ml.SignatureHelpClientCapabilities.signatureInformation" in
   function
   | `Assoc field_yojsons as yojson ->
       let documentationFormat_field = ref None
       and parameterInformation_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "documentationFormat" ->
                   (match Ppx_yojson_conv_lib.(!) documentationFormat_field
                    with
                    | None ->
                        let fvalue =
                          list_of_yojson MarkupKind.t_of_yojson _field_yojson in
                        documentationFormat_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "parameterInformation" ->
                   (match Ppx_yojson_conv_lib.(!) parameterInformation_field
                    with
                    | None ->
                        let fvalue =
                          parameterInformation_of_yojson _field_yojson in
                        parameterInformation_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | _ -> ());
              iter tail)
         | [] -> () in
       (iter field_yojsons;
        (match Ppx_yojson_conv_lib.(!) duplicates with
         | _::_ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
               _tp_loc (Ppx_yojson_conv_lib.(!) duplicates) yojson
         | [] ->
             (match Ppx_yojson_conv_lib.(!) extra with
              | _::_ ->
                  Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
                    _tp_loc (Ppx_yojson_conv_lib.(!) extra) yojson
              | [] ->
                  let (documentationFormat_value, parameterInformation_value)
                    =
                    ((Ppx_yojson_conv_lib.(!) documentationFormat_field),
                      (Ppx_yojson_conv_lib.(!) parameterInformation_field)) in
                  {
                    documentationFormat =
                      ((match documentationFormat_value with
                        | None -> []
                        | Some v -> v));
                    parameterInformation =
                      ((match parameterInformation_value with
                        | None -> parameterInformation_empty
                        | Some v -> v))
                  })))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> signatureInformation)

  let _ = signatureInformation_of_yojson

  let yojson_of_signatureInformation =
    ( function
      | { documentationFormat = v_documentationFormat
        ; parameterInformation = v_parameterInformation
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_parameterInformation v_parameterInformation in
          ("parameterInformation", arg) :: bnds
        in
        let bnds =
          let arg =
            yojson_of_list MarkupKind.yojson_of_t v_documentationFormat
          in
          ("documentationFormat", arg) :: bnds
        in
        `Assoc bnds
      : signatureInformation -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_signatureInformation

  [@@@end]

  let signatureInformation_empty =
    { documentationFormat = []
    ; parameterInformation = parameterInformation_empty
    }

  type t =
    { dynamicRegistration : bool [@default false]
    ; signatureInformation : signatureInformation
          [@default signatureInformation_empty]
    ; contextSupport : bool [@default false]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.SignatureHelpClientCapabilities.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let dynamicRegistration_field = ref None
        and signatureInformation_field = ref None
        and contextSupport_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "dynamicRegistration" -> (
              match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                dynamicRegistration_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "signatureInformation" -> (
              match Ppx_yojson_conv_lib.( ! ) signatureInformation_field with
              | None ->
                let fvalue = signatureInformation_of_yojson _field_yojson in
                signatureInformation_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "contextSupport" -> (
              match Ppx_yojson_conv_lib.( ! ) contextSupport_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                contextSupport_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
          | [] ->
            let ( dynamicRegistration_value
                , signatureInformation_value
                , contextSupport_value ) =
              ( Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
              , Ppx_yojson_conv_lib.( ! ) signatureInformation_field
              , Ppx_yojson_conv_lib.( ! ) contextSupport_field )
            in
            { dynamicRegistration =
                ( match dynamicRegistration_value with
                | None -> false
                | Some v -> v )
            ; signatureInformation =
                ( match signatureInformation_value with
                | None -> signatureInformation_empty
                | Some v -> v )
            ; contextSupport =
                ( match contextSupport_value with
                | None -> false
                | Some v -> v )
            } ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { dynamicRegistration = v_dynamicRegistration
        ; signatureInformation = v_signatureInformation
        ; contextSupport = v_contextSupport
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_bool v_contextSupport in
          ("contextSupport", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_signatureInformation v_signatureInformation in
          ("signatureInformation", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_dynamicRegistration in
          ("dynamicRegistration", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let empty =
    { dynamicRegistration = false
    ; signatureInformation = signatureInformation_empty
    ; contextSupport = false
    }
end

module TextDocumentClientCapabilities = struct
  type t =
    { synchronization : Synchronization.t [@default Synchronization.empty]
    ; completion : Completion.t [@default Completion.empty]
    ; documentSymbol : DocumentSymbol.t [@default DocumentSymbol.empty]
    ; hover : Hover.t
          [@default Hover.empty] (* omitted: dynamic-registration fields *)
    ; codeAction : CodeAction.t [@default CodeAction.empty]
    ; publishDiagnostics : PublishDiagnosticsClientCapabilities.t
          [@default PublishDiagnosticsClientCapabilities.empty]
    ; foldingRange : FoldingRangeClientCapabilities.t
          [@default FoldingRangeClientCapabilities.empty]
    ; signatureHelp : SignatureHelpClientCapabilities.t
          [@default SignatureHelpClientCapabilities.empty]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.TextDocumentClientCapabilities.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let synchronization_field = ref None
        and completion_field = ref None
        and documentSymbol_field = ref None
        and hover_field = ref None
        and codeAction_field = ref None
        and publishDiagnostics_field = ref None
        and foldingRange_field = ref None
        and signatureHelp_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "synchronization" -> (
              match Ppx_yojson_conv_lib.( ! ) synchronization_field with
              | None ->
                let fvalue = Synchronization.t_of_yojson _field_yojson in
                synchronization_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "completion" -> (
              match Ppx_yojson_conv_lib.( ! ) completion_field with
              | None ->
                let fvalue = Completion.t_of_yojson _field_yojson in
                completion_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "documentSymbol" -> (
              match Ppx_yojson_conv_lib.( ! ) documentSymbol_field with
              | None ->
                let fvalue = DocumentSymbol.t_of_yojson _field_yojson in
                documentSymbol_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "hover" -> (
              match Ppx_yojson_conv_lib.( ! ) hover_field with
              | None ->
                let fvalue = Hover.t_of_yojson _field_yojson in
                hover_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "codeAction" -> (
              match Ppx_yojson_conv_lib.( ! ) codeAction_field with
              | None ->
                let fvalue = CodeAction.t_of_yojson _field_yojson in
                codeAction_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "publishDiagnostics" -> (
              match Ppx_yojson_conv_lib.( ! ) publishDiagnostics_field with
              | None ->
                let fvalue =
                  PublishDiagnosticsClientCapabilities.t_of_yojson _field_yojson
                in
                publishDiagnostics_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "foldingRange" -> (
              match Ppx_yojson_conv_lib.( ! ) foldingRange_field with
              | None ->
                let fvalue =
                  FoldingRangeClientCapabilities.t_of_yojson _field_yojson
                in
                foldingRange_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "signatureHelp" -> (
              match Ppx_yojson_conv_lib.( ! ) signatureHelp_field with
              | None ->
                let fvalue =
                  SignatureHelpClientCapabilities.t_of_yojson _field_yojson
                in
                signatureHelp_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
          | [] ->
            let ( synchronization_value
                , completion_value
                , documentSymbol_value
                , hover_value
                , codeAction_value
                , publishDiagnostics_value
                , foldingRange_value
                , signatureHelp_value ) =
              ( Ppx_yojson_conv_lib.( ! ) synchronization_field
              , Ppx_yojson_conv_lib.( ! ) completion_field
              , Ppx_yojson_conv_lib.( ! ) documentSymbol_field
              , Ppx_yojson_conv_lib.( ! ) hover_field
              , Ppx_yojson_conv_lib.( ! ) codeAction_field
              , Ppx_yojson_conv_lib.( ! ) publishDiagnostics_field
              , Ppx_yojson_conv_lib.( ! ) foldingRange_field
              , Ppx_yojson_conv_lib.( ! ) signatureHelp_field )
            in
            { synchronization =
                ( match synchronization_value with
                | None -> Synchronization.empty
                | Some v -> v )
            ; completion =
                ( match completion_value with
                | None -> Completion.empty
                | Some v -> v )
            ; documentSymbol =
                ( match documentSymbol_value with
                | None -> DocumentSymbol.empty
                | Some v -> v )
            ; hover =
                ( match hover_value with
                | None -> Hover.empty
                | Some v -> v )
            ; codeAction =
                ( match codeAction_value with
                | None -> CodeAction.empty
                | Some v -> v )
            ; publishDiagnostics =
                ( match publishDiagnostics_value with
                | None -> PublishDiagnosticsClientCapabilities.empty
                | Some v -> v )
            ; foldingRange =
                ( match foldingRange_value with
                | None -> FoldingRangeClientCapabilities.empty
                | Some v -> v )
            ; signatureHelp =
                ( match signatureHelp_value with
                | None -> SignatureHelpClientCapabilities.empty
                | Some v -> v )
            } ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { synchronization = v_synchronization
        ; completion = v_completion
        ; documentSymbol = v_documentSymbol
        ; hover = v_hover
        ; codeAction = v_codeAction
        ; publishDiagnostics = v_publishDiagnostics
        ; foldingRange = v_foldingRange
        ; signatureHelp = v_signatureHelp
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg =
            SignatureHelpClientCapabilities.yojson_of_t v_signatureHelp
          in
          ("signatureHelp", arg) :: bnds
        in
        let bnds =
          let arg = FoldingRangeClientCapabilities.yojson_of_t v_foldingRange in
          ("foldingRange", arg) :: bnds
        in
        let bnds =
          let arg =
            PublishDiagnosticsClientCapabilities.yojson_of_t
              v_publishDiagnostics
          in
          ("publishDiagnostics", arg) :: bnds
        in
        let bnds =
          let arg = CodeAction.yojson_of_t v_codeAction in
          ("codeAction", arg) :: bnds
        in
        let bnds =
          let arg = Hover.yojson_of_t v_hover in
          ("hover", arg) :: bnds
        in
        let bnds =
          let arg = DocumentSymbol.yojson_of_t v_documentSymbol in
          ("documentSymbol", arg) :: bnds
        in
        let bnds =
          let arg = Completion.yojson_of_t v_completion in
          ("completion", arg) :: bnds
        in
        let bnds =
          let arg = Synchronization.yojson_of_t v_synchronization in
          ("synchronization", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let empty =
    { completion = Completion.empty
    ; synchronization = Synchronization.empty
    ; hover = Hover.empty
    ; documentSymbol = DocumentSymbol.empty
    ; codeAction = CodeAction.empty
    ; publishDiagnostics = PublishDiagnosticsClientCapabilities.empty
    ; foldingRange = FoldingRangeClientCapabilities.empty
    ; signatureHelp = SignatureHelpClientCapabilities.empty
    }
end

module WorkspaceEdit = struct
  module ResourceOperationKind = struct
    type t =
      | Create
      | Rename
      | Delete

    let t_of_yojson = function
      | `String "create" -> Create
      | `String "rename" -> Rename
      | `String "delete" -> Delete
      | json -> Json.error "resource operation kind" json

    let yojson_of_t = function
      | Create -> `String "create"
      | Rename -> `String "rename"
      | Delete -> `String "delete"
  end

  module FailureHandlingKind = struct
    type t =
      | Abort
      | Transactional
      | TextOnlyTransactional
      | Undo

    let t_of_yojson = function
      | `String "abort" -> Abort
      | `String "transactional" -> Transactional
      | `String "textOnlyTransactional" -> TextOnlyTransactional
      | `String "undo" -> Undo
      | json -> Json.error "failure handling kind" json

    let yojson_of_t = function
      | Abort -> `String "abort"
      | Transactional -> `String "transactional"
      | TextOnlyTransactional -> `String "textOnlyTransactional"
      | Undo -> `String "undo"
  end

  type t =
    { documentChanges : bool [@default false]
          (** client supports versioned doc changes *)
    ; resourceOperations : ResourceOperationKind.t list option [@yojson.option]
    ; failureHandlingKind : FailureHandlingKind.t option [@yojson.option]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.WorkspaceEdit.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let documentChanges_field = ref None
        and resourceOperations_field = ref None
        and failureHandlingKind_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "documentChanges" -> (
              match Ppx_yojson_conv_lib.( ! ) documentChanges_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                documentChanges_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "resourceOperations" -> (
              match Ppx_yojson_conv_lib.( ! ) resourceOperations_field with
              | None ->
                let fvalue =
                  list_of_yojson ResourceOperationKind.t_of_yojson _field_yojson
                in
                resourceOperations_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "failureHandlingKind" -> (
              match Ppx_yojson_conv_lib.( ! ) failureHandlingKind_field with
              | None ->
                let fvalue = FailureHandlingKind.t_of_yojson _field_yojson in
                failureHandlingKind_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
          | [] ->
            let ( documentChanges_value
                , resourceOperations_value
                , failureHandlingKind_value ) =
              ( Ppx_yojson_conv_lib.( ! ) documentChanges_field
              , Ppx_yojson_conv_lib.( ! ) resourceOperations_field
              , Ppx_yojson_conv_lib.( ! ) failureHandlingKind_field )
            in
            { documentChanges =
                ( match documentChanges_value with
                | None -> false
                | Some v -> v )
            ; resourceOperations = resourceOperations_value
            ; failureHandlingKind = failureHandlingKind_value
            } ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { documentChanges = v_documentChanges
        ; resourceOperations = v_resourceOperations
        ; failureHandlingKind = v_failureHandlingKind
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          match v_failureHandlingKind with
          | None -> bnds
          | Some v ->
            let arg = FailureHandlingKind.yojson_of_t v in
            let bnd = ("failureHandlingKind", arg) in
            bnd :: bnds
        in
        let bnds =
          match v_resourceOperations with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_list ResourceOperationKind.yojson_of_t v in
            let bnd = ("resourceOperations", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_documentChanges in
          ("documentChanges", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let empty =
    { documentChanges = false
    ; resourceOperations = None
    ; failureHandlingKind = None
    }
end

module WorkspaceClientCapabilities = struct
  module Symbol = struct
    let default_set : SymbolKind.t list =
      [ File
      ; Module
      ; Namespace
      ; Package
      ; Class
      ; Method
      ; Property
      ; Field
      ; Constructor
      ; Enum
      ; Interface
      ; Function
      ; Variable
      ; Constant
      ; String
      ; Number
      ; Boolean
      ]

    type t = { valueSet : SymbolKind.t list [@default default_set] }
    [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

    let _ = fun (_ : t) -> ()

    let t_of_yojson =
      ( let _tp_loc =
          "lsp/src/initialize.ml.WorkspaceClientCapabilities.Symbol.t"
        in
        function
        | `Assoc field_yojsons as yojson -> (
          let valueSet_field = ref None
          and duplicates = ref []
          and extra = ref [] in
          let rec iter = function
            | (field_name, _field_yojson) :: tail ->
              ( match field_name with
              | "valueSet" -> (
                match Ppx_yojson_conv_lib.( ! ) valueSet_field with
                | None ->
                  let fvalue =
                    list_of_yojson SymbolKind.t_of_yojson _field_yojson
                  in
                  valueSet_field := Some fvalue
                | Some _ ->
                  duplicates :=
                    field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
              | _ -> () );
              iter tail
            | [] -> ()
          in
          iter field_yojsons;
          match Ppx_yojson_conv_lib.( ! ) duplicates with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
              _tp_loc
              (Ppx_yojson_conv_lib.( ! ) duplicates)
              yojson
          | [] -> (
            match Ppx_yojson_conv_lib.( ! ) extra with
            | _ :: _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
                (Ppx_yojson_conv_lib.( ! ) extra)
                yojson
            | [] ->
              let valueSet_value = Ppx_yojson_conv_lib.( ! ) valueSet_field in
              { valueSet =
                  ( match valueSet_value with
                  | None -> default_set
                  | Some v -> v )
              } ) )
        | _ as yojson ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
            yojson
        : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

    let _ = t_of_yojson

    let yojson_of_t =
      ( function
        | { valueSet = v_valueSet } ->
          let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
          let bnds =
            let arg = yojson_of_list SymbolKind.yojson_of_t v_valueSet in
            ("valueSet", arg) :: bnds
          in
          `Assoc bnds
        : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

    let _ = yojson_of_t

    [@@@end]

    let default = { valueSet = default_set }
  end

  type t =
    { applyEdit : bool [@default false]
    ; workspaceEdit : WorkspaceEdit.t [@default WorkspaceEdit.empty]
          (** omitted: dynamic-registration fields *)
    ; symbol : Symbol.t [@default Symbol.default]
    ; workspaceFolders : bool [@default false]
    ; configuration : bool [@default false]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.WorkspaceClientCapabilities.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let applyEdit_field = ref None
        and workspaceEdit_field = ref None
        and symbol_field = ref None
        and workspaceFolders_field = ref None
        and configuration_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "applyEdit" -> (
              match Ppx_yojson_conv_lib.( ! ) applyEdit_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                applyEdit_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "workspaceEdit" -> (
              match Ppx_yojson_conv_lib.( ! ) workspaceEdit_field with
              | None ->
                let fvalue = WorkspaceEdit.t_of_yojson _field_yojson in
                workspaceEdit_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "symbol" -> (
              match Ppx_yojson_conv_lib.( ! ) symbol_field with
              | None ->
                let fvalue = Symbol.t_of_yojson _field_yojson in
                symbol_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "workspaceFolders" -> (
              match Ppx_yojson_conv_lib.( ! ) workspaceFolders_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                workspaceFolders_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "configuration" -> (
              match Ppx_yojson_conv_lib.( ! ) configuration_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                configuration_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
          | [] ->
            let ( applyEdit_value
                , workspaceEdit_value
                , symbol_value
                , workspaceFolders_value
                , configuration_value ) =
              ( Ppx_yojson_conv_lib.( ! ) applyEdit_field
              , Ppx_yojson_conv_lib.( ! ) workspaceEdit_field
              , Ppx_yojson_conv_lib.( ! ) symbol_field
              , Ppx_yojson_conv_lib.( ! ) workspaceFolders_field
              , Ppx_yojson_conv_lib.( ! ) configuration_field )
            in
            { applyEdit =
                ( match applyEdit_value with
                | None -> false
                | Some v -> v )
            ; workspaceEdit =
                ( match workspaceEdit_value with
                | None -> WorkspaceEdit.empty
                | Some v -> v )
            ; symbol =
                ( match symbol_value with
                | None -> Symbol.default
                | Some v -> v )
            ; workspaceFolders =
                ( match workspaceFolders_value with
                | None -> false
                | Some v -> v )
            ; configuration =
                ( match configuration_value with
                | None -> false
                | Some v -> v )
            } ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { applyEdit = v_applyEdit
        ; workspaceEdit = v_workspaceEdit
        ; symbol = v_symbol
        ; workspaceFolders = v_workspaceFolders
        ; configuration = v_configuration
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_bool v_configuration in
          ("configuration", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_workspaceFolders in
          ("workspaceFolders", arg) :: bnds
        in
        let bnds =
          let arg = Symbol.yojson_of_t v_symbol in
          ("symbol", arg) :: bnds
        in
        let bnds =
          let arg = WorkspaceEdit.yojson_of_t v_workspaceEdit in
          ("workspaceEdit", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_applyEdit in
          ("applyEdit", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let empty =
    { applyEdit = false
    ; workspaceEdit = WorkspaceEdit.empty
    ; symbol = Symbol.default
    ; configuration = false
    ; workspaceFolders = false
    }
end

module ClientCapabilities = struct
  type t =
    { workspace : WorkspaceClientCapabilities.t
          [@default WorkspaceClientCapabilities.empty]
    ; textDocument : TextDocumentClientCapabilities.t
          [@default TextDocumentClientCapabilities.empty]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.ClientCapabilities.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let workspace_field = ref None
        and textDocument_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "workspace" -> (
              match Ppx_yojson_conv_lib.( ! ) workspace_field with
              | None ->
                let fvalue =
                  WorkspaceClientCapabilities.t_of_yojson _field_yojson
                in
                workspace_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "textDocument" -> (
              match Ppx_yojson_conv_lib.( ! ) textDocument_field with
              | None ->
                let fvalue =
                  TextDocumentClientCapabilities.t_of_yojson _field_yojson
                in
                textDocument_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
          | [] ->
            let workspace_value, textDocument_value =
              ( Ppx_yojson_conv_lib.( ! ) workspace_field
              , Ppx_yojson_conv_lib.( ! ) textDocument_field )
            in
            { workspace =
                ( match workspace_value with
                | None -> WorkspaceClientCapabilities.empty
                | Some v -> v )
            ; textDocument =
                ( match textDocument_value with
                | None -> TextDocumentClientCapabilities.empty
                | Some v -> v )
            } ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { workspace = v_workspace; textDocument = v_textDocument } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = TextDocumentClientCapabilities.yojson_of_t v_textDocument in
          ("textDocument", arg) :: bnds
        in
        let bnds =
          let arg = WorkspaceClientCapabilities.yojson_of_t v_workspace in
          ("workspace", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let empty =
    { workspace = WorkspaceClientCapabilities.empty
    ; textDocument = TextDocumentClientCapabilities.empty
    }
end

module Info = struct
  type t =
    { name : string
    ; version : string option [@yojson.option]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.Info.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let name_field = ref None
        and version_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "name" -> (
              match Ppx_yojson_conv_lib.( ! ) name_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                name_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "version" -> (
              match Ppx_yojson_conv_lib.( ! ) version_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                version_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
              ( Ppx_yojson_conv_lib.( ! ) name_field
              , Ppx_yojson_conv_lib.( ! ) version_field )
            with
            | Some name_value, version_value ->
              { name = name_value; version = version_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) name_field)
                      None
                  , "name" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { name = v_name; version = v_version } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          match v_version with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_string v in
            let bnd = ("version", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_name in
          ("name", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module Params = struct
  type t =
    { processId : int option [@default None] [@yojson_drop_default ( = )]
    ; (* pid of parent process *)
      rootPath : string option [@default None] [@yojson_drop_default ( = )]
    ; (* deprecated *)
      rootUri : documentUri option [@default None]
    ; (* the root URI of the workspace *)
      capabilities : ClientCapabilities.t [@default ClientCapabilities.empty]
    ; trace : Trace.t
          [@default Trace.Off] (* the initial trace setting, default="off" *)
    ; workspaceFolders : WorkspaceFolder.t list [@default []]
    ; initializationOptions : Json.t option [@yojson.option]
    ; clientInfo : Info.t option [@yojson.option]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.Params.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let processId_field = ref None
        and rootPath_field = ref None
        and rootUri_field = ref None
        and capabilities_field = ref None
        and trace_field = ref None
        and workspaceFolders_field = ref None
        and initializationOptions_field = ref None
        and clientInfo_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "processId" -> (
              match Ppx_yojson_conv_lib.( ! ) processId_field with
              | None ->
                let fvalue = option_of_yojson int_of_yojson _field_yojson in
                processId_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "rootPath" -> (
              match Ppx_yojson_conv_lib.( ! ) rootPath_field with
              | None ->
                let fvalue = option_of_yojson string_of_yojson _field_yojson in
                rootPath_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "rootUri" -> (
              match Ppx_yojson_conv_lib.( ! ) rootUri_field with
              | None ->
                let fvalue =
                  option_of_yojson documentUri_of_yojson _field_yojson
                in
                rootUri_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "capabilities" -> (
              match Ppx_yojson_conv_lib.( ! ) capabilities_field with
              | None ->
                let fvalue = ClientCapabilities.t_of_yojson _field_yojson in
                capabilities_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "trace" -> (
              match Ppx_yojson_conv_lib.( ! ) trace_field with
              | None ->
                let fvalue = Trace.t_of_yojson _field_yojson in
                trace_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "workspaceFolders" -> (
              match Ppx_yojson_conv_lib.( ! ) workspaceFolders_field with
              | None ->
                let fvalue =
                  list_of_yojson WorkspaceFolder.t_of_yojson _field_yojson
                in
                workspaceFolders_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "initializationOptions" -> (
              match Ppx_yojson_conv_lib.( ! ) initializationOptions_field with
              | None ->
                let fvalue = Json.t_of_yojson _field_yojson in
                initializationOptions_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "clientInfo" -> (
              match Ppx_yojson_conv_lib.( ! ) clientInfo_field with
              | None ->
                let fvalue = Info.t_of_yojson _field_yojson in
                clientInfo_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
          | [] ->
            let ( processId_value
                , rootPath_value
                , rootUri_value
                , capabilities_value
                , trace_value
                , workspaceFolders_value
                , initializationOptions_value
                , clientInfo_value ) =
              ( Ppx_yojson_conv_lib.( ! ) processId_field
              , Ppx_yojson_conv_lib.( ! ) rootPath_field
              , Ppx_yojson_conv_lib.( ! ) rootUri_field
              , Ppx_yojson_conv_lib.( ! ) capabilities_field
              , Ppx_yojson_conv_lib.( ! ) trace_field
              , Ppx_yojson_conv_lib.( ! ) workspaceFolders_field
              , Ppx_yojson_conv_lib.( ! ) initializationOptions_field
              , Ppx_yojson_conv_lib.( ! ) clientInfo_field )
            in
            { processId =
                ( match processId_value with
                | None -> None
                | Some v -> v )
            ; rootPath =
                ( match rootPath_value with
                | None -> None
                | Some v -> v )
            ; rootUri =
                ( match rootUri_value with
                | None -> None
                | Some v -> v )
            ; capabilities =
                ( match capabilities_value with
                | None -> ClientCapabilities.empty
                | Some v -> v )
            ; trace =
                ( match trace_value with
                | None -> Trace.Off
                | Some v -> v )
            ; workspaceFolders =
                ( match workspaceFolders_value with
                | None -> []
                | Some v -> v )
            ; initializationOptions = initializationOptions_value
            ; clientInfo = clientInfo_value
            } ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { processId = v_processId
        ; rootPath = v_rootPath
        ; rootUri = v_rootUri
        ; capabilities = v_capabilities
        ; trace = v_trace
        ; workspaceFolders = v_workspaceFolders
        ; initializationOptions = v_initializationOptions
        ; clientInfo = v_clientInfo
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          match v_clientInfo with
          | None -> bnds
          | Some v ->
            let arg = Info.yojson_of_t v in
            let bnd = ("clientInfo", arg) in
            bnd :: bnds
        in
        let bnds =
          match v_initializationOptions with
          | None -> bnds
          | Some v ->
            let arg = Json.yojson_of_t v in
            let bnd = ("initializationOptions", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg =
            yojson_of_list WorkspaceFolder.yojson_of_t v_workspaceFolders
          in
          ("workspaceFolders", arg) :: bnds
        in
        let bnds =
          let arg = Trace.yojson_of_t v_trace in
          ("trace", arg) :: bnds
        in
        let bnds =
          let arg = ClientCapabilities.yojson_of_t v_capabilities in
          ("capabilities", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_option yojson_of_documentUri v_rootUri in
          ("rootUri", arg) :: bnds
        in
        let bnds =
          if None = v_rootPath then
            bnds
          else
            let arg = (yojson_of_option yojson_of_string) v_rootPath in
            let bnd = ("rootPath", arg) in
            bnd :: bnds
        in
        let bnds =
          if None = v_processId then
            bnds
          else
            let arg = (yojson_of_option yojson_of_int) v_processId in
            let bnd = ("processId", arg) in
            bnd :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let create ?processId ?rootPath ?rootUri
      ?(capabilities = ClientCapabilities.empty) ?(trace = Trace.Off)
      ?(workspaceFolders = []) ?initializationOptions ?clientInfo () =
    { processId
    ; rootPath
    ; rootUri
    ; capabilities
    ; trace
    ; workspaceFolders
    ; initializationOptions
    ; clientInfo
    }
end

module CompletionOptions = struct
  type t =
    { resolveProvider : bool
    ; (* server resolves extra info on demand *)
      triggerCharacters : string list (* wire "triggerCharacters" *)
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.CompletionOptions.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let resolveProvider_field = ref None
        and triggerCharacters_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "resolveProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) resolveProvider_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                resolveProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "triggerCharacters" -> (
              match Ppx_yojson_conv_lib.( ! ) triggerCharacters_field with
              | None ->
                let fvalue = list_of_yojson string_of_yojson _field_yojson in
                triggerCharacters_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
              ( Ppx_yojson_conv_lib.( ! ) resolveProvider_field
              , Ppx_yojson_conv_lib.( ! ) triggerCharacters_field )
            with
            | Some resolveProvider_value, Some triggerCharacters_value ->
              { resolveProvider = resolveProvider_value
              ; triggerCharacters = triggerCharacters_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) resolveProvider_field)
                      None
                  , "resolveProvider" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) triggerCharacters_field)
                      None
                  , "triggerCharacters" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { resolveProvider = v_resolveProvider
        ; triggerCharacters = v_triggerCharacters
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_list yojson_of_string v_triggerCharacters in
          ("triggerCharacters", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_resolveProvider in
          ("resolveProvider", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module CodeLensOptions = struct
  type t = { resolveProvider : bool (* wire "resolveProvider" *) }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.CodeLensOptions.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let resolveProvider_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "resolveProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) resolveProvider_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                resolveProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
            match Ppx_yojson_conv_lib.( ! ) resolveProvider_field with
            | Some resolveProvider_value ->
              { resolveProvider = resolveProvider_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) resolveProvider_field)
                      None
                  , "resolveProvider" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { resolveProvider = v_resolveProvider } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_bool v_resolveProvider in
          ("resolveProvider", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module DocumentLinkOptions = struct
  type t = { doclink_resolveProvider : bool (* wire "resolveProvider" *) }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.DocumentLinkOptions.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let doclink_resolveProvider_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "doclink_resolveProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) doclink_resolveProvider_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                doclink_resolveProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
            match Ppx_yojson_conv_lib.( ! ) doclink_resolveProvider_field with
            | Some doclink_resolveProvider_value ->
              { doclink_resolveProvider = doclink_resolveProvider_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) doclink_resolveProvider_field)
                      None
                  , "doclink_resolveProvider" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { doclink_resolveProvider = v_doclink_resolveProvider } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_bool v_doclink_resolveProvider in
          ("doclink_resolveProvider", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module DocumentOnTypeFormattingOptions = struct
  type t =
    { firstTriggerCharacter : string
    ; (* e.g. "}" *)
      moreTriggerCharacter : string list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.DocumentOnTypeFormattingOptions.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let firstTriggerCharacter_field = ref None
        and moreTriggerCharacter_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "firstTriggerCharacter" -> (
              match Ppx_yojson_conv_lib.( ! ) firstTriggerCharacter_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                firstTriggerCharacter_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "moreTriggerCharacter" -> (
              match Ppx_yojson_conv_lib.( ! ) moreTriggerCharacter_field with
              | None ->
                let fvalue = list_of_yojson string_of_yojson _field_yojson in
                moreTriggerCharacter_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
              ( Ppx_yojson_conv_lib.( ! ) firstTriggerCharacter_field
              , Ppx_yojson_conv_lib.( ! ) moreTriggerCharacter_field )
            with
            | Some firstTriggerCharacter_value, Some moreTriggerCharacter_value
              ->
              { firstTriggerCharacter = firstTriggerCharacter_value
              ; moreTriggerCharacter = moreTriggerCharacter_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) firstTriggerCharacter_field)
                      None
                  , "firstTriggerCharacter" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) moreTriggerCharacter_field)
                      None
                  , "moreTriggerCharacter" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { firstTriggerCharacter = v_firstTriggerCharacter
        ; moreTriggerCharacter = v_moreTriggerCharacter
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_list yojson_of_string v_moreTriggerCharacter in
          ("moreTriggerCharacter", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_firstTriggerCharacter in
          ("firstTriggerCharacter", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module ExecuteCommandOptions = struct
  type t =
    { commands : string list (* the commands to be executed on the server *) }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.ExecuteCommandOptions.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let commands_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "commands" -> (
              match Ppx_yojson_conv_lib.( ! ) commands_field with
              | None ->
                let fvalue = list_of_yojson string_of_yojson _field_yojson in
                commands_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
            match Ppx_yojson_conv_lib.( ! ) commands_field with
            | Some commands_value -> { commands = commands_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) commands_field)
                      None
                  , "commands" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { commands = v_commands } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_list yojson_of_string v_commands in
          ("commands", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module TextDocumentSyncOptions = struct
  type saveOptions =
    { includeText : bool (* the client should include content on save *) }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : saveOptions) -> ()

  let saveOptions_of_yojson =
    ( let _tp_loc =
        "lsp/src/initialize.ml.TextDocumentSyncOptions.saveOptions"
      in
      function
      | `Assoc field_yojsons as yojson -> (
        let includeText_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "includeText" -> (
              match Ppx_yojson_conv_lib.( ! ) includeText_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                includeText_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
            match Ppx_yojson_conv_lib.( ! ) includeText_field with
            | Some includeText_value -> { includeText = includeText_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) includeText_field)
                      None
                  , "includeText" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> saveOptions )

  let _ = saveOptions_of_yojson

  let yojson_of_saveOptions =
    ( function
      | { includeText = v_includeText } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_bool v_includeText in
          ("includeText", arg) :: bnds
        in
        `Assoc bnds
      : saveOptions -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_saveOptions

  [@@@end]

  type t =
    { openClose : bool
    ; (* textDocument/didOpen+didClose *)
      change : TextDocumentSyncKind.t
    ; willSave : bool
    ; (* textDocument/willSave *)
      willSaveWaitUntil : bool
    ; (* textDoc.../willSaveWaitUntil *)
      didSave : saveOptions option [@yojson.option] (* textDocument/didSave *)
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.TextDocumentSyncOptions.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let openClose_field = ref None
        and change_field = ref None
        and willSave_field = ref None
        and willSaveWaitUntil_field = ref None
        and didSave_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "openClose" -> (
              match Ppx_yojson_conv_lib.( ! ) openClose_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                openClose_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "change" -> (
              match Ppx_yojson_conv_lib.( ! ) change_field with
              | None ->
                let fvalue = TextDocumentSyncKind.t_of_yojson _field_yojson in
                change_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "willSave" -> (
              match Ppx_yojson_conv_lib.( ! ) willSave_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                willSave_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "willSaveWaitUntil" -> (
              match Ppx_yojson_conv_lib.( ! ) willSaveWaitUntil_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                willSaveWaitUntil_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "didSave" -> (
              match Ppx_yojson_conv_lib.( ! ) didSave_field with
              | None ->
                let fvalue = saveOptions_of_yojson _field_yojson in
                didSave_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
              ( Ppx_yojson_conv_lib.( ! ) openClose_field
              , Ppx_yojson_conv_lib.( ! ) change_field
              , Ppx_yojson_conv_lib.( ! ) willSave_field
              , Ppx_yojson_conv_lib.( ! ) willSaveWaitUntil_field
              , Ppx_yojson_conv_lib.( ! ) didSave_field )
            with
            | ( Some openClose_value
              , Some change_value
              , Some willSave_value
              , Some willSaveWaitUntil_value
              , didSave_value ) ->
              { openClose = openClose_value
              ; change = change_value
              ; willSave = willSave_value
              ; willSaveWaitUntil = willSaveWaitUntil_value
              ; didSave = didSave_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) openClose_field)
                      None
                  , "openClose" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) change_field)
                      None
                  , "change" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) willSave_field)
                      None
                  , "willSave" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) willSaveWaitUntil_field)
                      None
                  , "willSaveWaitUntil" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { openClose = v_openClose
        ; change = v_change
        ; willSave = v_willSave
        ; willSaveWaitUntil = v_willSaveWaitUntil
        ; didSave = v_didSave
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          match v_didSave with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_saveOptions v in
            let bnd = ("didSave", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_willSaveWaitUntil in
          ("willSaveWaitUntil", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_willSave in
          ("willSave", arg) :: bnds
        in
        let bnds =
          let arg = TextDocumentSyncKind.yojson_of_t v_change in
          ("change", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_openClose in
          ("openClose", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let default =
    { openClose = false
    ; change = TextDocumentSyncKind.NoSync
    ; willSave = false
    ; willSaveWaitUntil = false
    ; didSave = None
    }
end

module ServerCapabilities = struct
  type t =
    { textDocumentSync : TextDocumentSyncOptions.t
    ; (* how to sync *)
      hoverProvider : bool
    ; completionProvider : CompletionOptions.t option [@yojson.option]
    ; signatureHelpProvider : SignatureHelpOptions.t option [@yojson.option]
    ; definitionProvider : bool
    ; typeDefinitionProvider : bool
    ; referencesProvider : bool
    ; documentHighlightProvider : bool
    ; documentSymbolProvider : bool
    ; (* ie. document outline *)
      workspaceSymbolProvider : bool
    ; (* ie. find-symbol-in-project *)
      codeActionProvider : CodeActionOptions.t Or_bool.t
    ; codeLensProvider : CodeLensOptions.t option [@yojson.option]
    ; documentFormattingProvider : bool
    ; documentRangeFormattingProvider : bool
    ; documentOnTypeFormattingProvider :
        DocumentOnTypeFormattingOptions.t option
          [@yojson.option]
    ; renameProvider : bool
    ; documentLinkProvider : DocumentLinkOptions.t option [@yojson.option]
    ; executeCommandProvider : ExecuteCommandOptions.t option [@yojson.option]
    ; typeCoverageProvider : bool
    ; foldingRangeProvider : Void.t Or_bool.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.ServerCapabilities.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let textDocumentSync_field = ref None
        and hoverProvider_field = ref None
        and completionProvider_field = ref None
        and signatureHelpProvider_field = ref None
        and definitionProvider_field = ref None
        and typeDefinitionProvider_field = ref None
        and referencesProvider_field = ref None
        and documentHighlightProvider_field = ref None
        and documentSymbolProvider_field = ref None
        and workspaceSymbolProvider_field = ref None
        and codeActionProvider_field = ref None
        and codeLensProvider_field = ref None
        and documentFormattingProvider_field = ref None
        and documentRangeFormattingProvider_field = ref None
        and documentOnTypeFormattingProvider_field = ref None
        and renameProvider_field = ref None
        and documentLinkProvider_field = ref None
        and executeCommandProvider_field = ref None
        and typeCoverageProvider_field = ref None
        and foldingRangeProvider_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "textDocumentSync" -> (
              match Ppx_yojson_conv_lib.( ! ) textDocumentSync_field with
              | None ->
                let fvalue =
                  TextDocumentSyncOptions.t_of_yojson _field_yojson
                in
                textDocumentSync_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "hoverProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) hoverProvider_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                hoverProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "completionProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) completionProvider_field with
              | None ->
                let fvalue = CompletionOptions.t_of_yojson _field_yojson in
                completionProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "signatureHelpProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) signatureHelpProvider_field with
              | None ->
                let fvalue = SignatureHelpOptions.t_of_yojson _field_yojson in
                signatureHelpProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "definitionProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) definitionProvider_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                definitionProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "typeDefinitionProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) typeDefinitionProvider_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                typeDefinitionProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "referencesProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) referencesProvider_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                referencesProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "documentHighlightProvider" -> (
              match
                Ppx_yojson_conv_lib.( ! ) documentHighlightProvider_field
              with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                documentHighlightProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "documentSymbolProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) documentSymbolProvider_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                documentSymbolProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "workspaceSymbolProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) workspaceSymbolProvider_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                workspaceSymbolProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "codeActionProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) codeActionProvider_field with
              | None ->
                let fvalue =
                  Or_bool.t_of_yojson CodeActionOptions.t_of_yojson
                    _field_yojson
                in
                codeActionProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "codeLensProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) codeLensProvider_field with
              | None ->
                let fvalue = CodeLensOptions.t_of_yojson _field_yojson in
                codeLensProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "documentFormattingProvider" -> (
              match
                Ppx_yojson_conv_lib.( ! ) documentFormattingProvider_field
              with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                documentFormattingProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "documentRangeFormattingProvider" -> (
              match
                Ppx_yojson_conv_lib.( ! ) documentRangeFormattingProvider_field
              with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                documentRangeFormattingProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "documentOnTypeFormattingProvider" -> (
              match
                Ppx_yojson_conv_lib.( ! ) documentOnTypeFormattingProvider_field
              with
              | None ->
                let fvalue =
                  DocumentOnTypeFormattingOptions.t_of_yojson _field_yojson
                in
                documentOnTypeFormattingProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "renameProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) renameProvider_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                renameProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "documentLinkProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) documentLinkProvider_field with
              | None ->
                let fvalue = DocumentLinkOptions.t_of_yojson _field_yojson in
                documentLinkProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "executeCommandProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) executeCommandProvider_field with
              | None ->
                let fvalue = ExecuteCommandOptions.t_of_yojson _field_yojson in
                executeCommandProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "typeCoverageProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) typeCoverageProvider_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                typeCoverageProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "foldingRangeProvider" -> (
              match Ppx_yojson_conv_lib.( ! ) foldingRangeProvider_field with
              | None ->
                let fvalue =
                  Or_bool.t_of_yojson Void.t_of_yojson _field_yojson
                in
                foldingRangeProvider_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
              ( Ppx_yojson_conv_lib.( ! ) textDocumentSync_field
              , Ppx_yojson_conv_lib.( ! ) hoverProvider_field
              , Ppx_yojson_conv_lib.( ! ) completionProvider_field
              , Ppx_yojson_conv_lib.( ! ) signatureHelpProvider_field
              , Ppx_yojson_conv_lib.( ! ) definitionProvider_field
              , Ppx_yojson_conv_lib.( ! ) typeDefinitionProvider_field
              , Ppx_yojson_conv_lib.( ! ) referencesProvider_field
              , Ppx_yojson_conv_lib.( ! ) documentHighlightProvider_field
              , Ppx_yojson_conv_lib.( ! ) documentSymbolProvider_field
              , Ppx_yojson_conv_lib.( ! ) workspaceSymbolProvider_field
              , Ppx_yojson_conv_lib.( ! ) codeActionProvider_field
              , Ppx_yojson_conv_lib.( ! ) codeLensProvider_field
              , Ppx_yojson_conv_lib.( ! ) documentFormattingProvider_field
              , Ppx_yojson_conv_lib.( ! ) documentRangeFormattingProvider_field
              , Ppx_yojson_conv_lib.( ! ) documentOnTypeFormattingProvider_field
              , Ppx_yojson_conv_lib.( ! ) renameProvider_field
              , Ppx_yojson_conv_lib.( ! ) documentLinkProvider_field
              , Ppx_yojson_conv_lib.( ! ) executeCommandProvider_field
              , Ppx_yojson_conv_lib.( ! ) typeCoverageProvider_field
              , Ppx_yojson_conv_lib.( ! ) foldingRangeProvider_field )
            with
            | ( Some textDocumentSync_value
              , Some hoverProvider_value
              , completionProvider_value
              , signatureHelpProvider_value
              , Some definitionProvider_value
              , Some typeDefinitionProvider_value
              , Some referencesProvider_value
              , Some documentHighlightProvider_value
              , Some documentSymbolProvider_value
              , Some workspaceSymbolProvider_value
              , Some codeActionProvider_value
              , codeLensProvider_value
              , Some documentFormattingProvider_value
              , Some documentRangeFormattingProvider_value
              , documentOnTypeFormattingProvider_value
              , Some renameProvider_value
              , documentLinkProvider_value
              , executeCommandProvider_value
              , Some typeCoverageProvider_value
              , Some foldingRangeProvider_value ) ->
              { textDocumentSync = textDocumentSync_value
              ; hoverProvider = hoverProvider_value
              ; completionProvider = completionProvider_value
              ; signatureHelpProvider = signatureHelpProvider_value
              ; definitionProvider = definitionProvider_value
              ; typeDefinitionProvider = typeDefinitionProvider_value
              ; referencesProvider = referencesProvider_value
              ; documentHighlightProvider = documentHighlightProvider_value
              ; documentSymbolProvider = documentSymbolProvider_value
              ; workspaceSymbolProvider = workspaceSymbolProvider_value
              ; codeActionProvider = codeActionProvider_value
              ; codeLensProvider = codeLensProvider_value
              ; documentFormattingProvider = documentFormattingProvider_value
              ; documentRangeFormattingProvider =
                  documentRangeFormattingProvider_value
              ; documentOnTypeFormattingProvider =
                  documentOnTypeFormattingProvider_value
              ; renameProvider = renameProvider_value
              ; documentLinkProvider = documentLinkProvider_value
              ; executeCommandProvider = executeCommandProvider_value
              ; typeCoverageProvider = typeCoverageProvider_value
              ; foldingRangeProvider = foldingRangeProvider_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) textDocumentSync_field)
                      None
                  , "textDocumentSync" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) hoverProvider_field)
                      None
                  , "hoverProvider" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) definitionProvider_field)
                      None
                  , "definitionProvider" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) typeDefinitionProvider_field)
                      None
                  , "typeDefinitionProvider" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) referencesProvider_field)
                      None
                  , "referencesProvider" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! )
                         documentHighlightProvider_field)
                      None
                  , "documentHighlightProvider" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) documentSymbolProvider_field)
                      None
                  , "documentSymbolProvider" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) workspaceSymbolProvider_field)
                      None
                  , "workspaceSymbolProvider" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) codeActionProvider_field)
                      None
                  , "codeActionProvider" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! )
                         documentFormattingProvider_field)
                      None
                  , "documentFormattingProvider" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! )
                         documentRangeFormattingProvider_field)
                      None
                  , "documentRangeFormattingProvider" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) renameProvider_field)
                      None
                  , "renameProvider" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) typeCoverageProvider_field)
                      None
                  , "typeCoverageProvider" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) foldingRangeProvider_field)
                      None
                  , "foldingRangeProvider" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { textDocumentSync = v_textDocumentSync
        ; hoverProvider = v_hoverProvider
        ; completionProvider = v_completionProvider
        ; signatureHelpProvider = v_signatureHelpProvider
        ; definitionProvider = v_definitionProvider
        ; typeDefinitionProvider = v_typeDefinitionProvider
        ; referencesProvider = v_referencesProvider
        ; documentHighlightProvider = v_documentHighlightProvider
        ; documentSymbolProvider = v_documentSymbolProvider
        ; workspaceSymbolProvider = v_workspaceSymbolProvider
        ; codeActionProvider = v_codeActionProvider
        ; codeLensProvider = v_codeLensProvider
        ; documentFormattingProvider = v_documentFormattingProvider
        ; documentRangeFormattingProvider = v_documentRangeFormattingProvider
        ; documentOnTypeFormattingProvider = v_documentOnTypeFormattingProvider
        ; renameProvider = v_renameProvider
        ; documentLinkProvider = v_documentLinkProvider
        ; executeCommandProvider = v_executeCommandProvider
        ; typeCoverageProvider = v_typeCoverageProvider
        ; foldingRangeProvider = v_foldingRangeProvider
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg =
            Or_bool.yojson_of_t Void.yojson_of_t v_foldingRangeProvider
          in
          ("foldingRangeProvider", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_typeCoverageProvider in
          ("typeCoverageProvider", arg) :: bnds
        in
        let bnds =
          match v_executeCommandProvider with
          | None -> bnds
          | Some v ->
            let arg = ExecuteCommandOptions.yojson_of_t v in
            let bnd = ("executeCommandProvider", arg) in
            bnd :: bnds
        in
        let bnds =
          match v_documentLinkProvider with
          | None -> bnds
          | Some v ->
            let arg = DocumentLinkOptions.yojson_of_t v in
            let bnd = ("documentLinkProvider", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_renameProvider in
          ("renameProvider", arg) :: bnds
        in
        let bnds =
          match v_documentOnTypeFormattingProvider with
          | None -> bnds
          | Some v ->
            let arg = DocumentOnTypeFormattingOptions.yojson_of_t v in
            let bnd = ("documentOnTypeFormattingProvider", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_documentRangeFormattingProvider in
          ("documentRangeFormattingProvider", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_documentFormattingProvider in
          ("documentFormattingProvider", arg) :: bnds
        in
        let bnds =
          match v_codeLensProvider with
          | None -> bnds
          | Some v ->
            let arg = CodeLensOptions.yojson_of_t v in
            let bnd = ("codeLensProvider", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg =
            Or_bool.yojson_of_t CodeActionOptions.yojson_of_t
              v_codeActionProvider
          in
          ("codeActionProvider", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_workspaceSymbolProvider in
          ("workspaceSymbolProvider", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_documentSymbolProvider in
          ("documentSymbolProvider", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_documentHighlightProvider in
          ("documentHighlightProvider", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_referencesProvider in
          ("referencesProvider", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_typeDefinitionProvider in
          ("typeDefinitionProvider", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_definitionProvider in
          ("definitionProvider", arg) :: bnds
        in
        let bnds =
          match v_signatureHelpProvider with
          | None -> bnds
          | Some v ->
            let arg = SignatureHelpOptions.yojson_of_t v in
            let bnd = ("signatureHelpProvider", arg) in
            bnd :: bnds
        in
        let bnds =
          match v_completionProvider with
          | None -> bnds
          | Some v ->
            let arg = CompletionOptions.yojson_of_t v in
            let bnd = ("completionProvider", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_hoverProvider in
          ("hoverProvider", arg) :: bnds
        in
        let bnds =
          let arg = TextDocumentSyncOptions.yojson_of_t v_textDocumentSync in
          ("textDocumentSync", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let default =
    { textDocumentSync = TextDocumentSyncOptions.default
    ; hoverProvider = false
    ; completionProvider = None
    ; signatureHelpProvider = None
    ; definitionProvider = false
    ; typeDefinitionProvider = false
    ; referencesProvider = false
    ; documentHighlightProvider = false
    ; documentSymbolProvider = false
    ; workspaceSymbolProvider = false
    ; codeActionProvider = Bool false
    ; codeLensProvider = None
    ; documentFormattingProvider = false
    ; documentRangeFormattingProvider = false
    ; documentOnTypeFormattingProvider = None
    ; renameProvider = false
    ; documentLinkProvider = None
    ; executeCommandProvider = None
    ; typeCoverageProvider = false
    ; foldingRangeProvider = Bool false
    }
end

module Result = struct
  type t =
    { capabilities : ServerCapabilities.t
    ; serverInfo : Info.t option [@yojson.option]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.Result.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let capabilities_field = ref None
        and serverInfo_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "capabilities" -> (
              match Ppx_yojson_conv_lib.( ! ) capabilities_field with
              | None ->
                let fvalue = ServerCapabilities.t_of_yojson _field_yojson in
                capabilities_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "serverInfo" -> (
              match Ppx_yojson_conv_lib.( ! ) serverInfo_field with
              | None ->
                let fvalue = Info.t_of_yojson _field_yojson in
                serverInfo_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
              ( Ppx_yojson_conv_lib.( ! ) capabilities_field
              , Ppx_yojson_conv_lib.( ! ) serverInfo_field )
            with
            | Some capabilities_value, serverInfo_value ->
              { capabilities = capabilities_value
              ; serverInfo = serverInfo_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) capabilities_field)
                      None
                  , "capabilities" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { capabilities = v_capabilities; serverInfo = v_serverInfo } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          match v_serverInfo with
          | None -> bnds
          | Some v ->
            let arg = Info.yojson_of_t v in
            let bnd = ("serverInfo", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = ServerCapabilities.yojson_of_t v_capabilities in
          ("capabilities", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module ErrorData = struct
  type t = { retry : bool (* should client retry the initialize request *) }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/initialize.ml.ErrorData.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let retry_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "retry" -> (
              match Ppx_yojson_conv_lib.( ! ) retry_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                retry_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
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
            match Ppx_yojson_conv_lib.( ! ) retry_field with
            | Some retry_value -> { retry = retry_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) retry_field)
                      None
                  , "retry" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { retry = v_retry } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_bool v_retry in
          ("retry", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

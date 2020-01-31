open Import
open Protocol

module Kind = struct
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

  let to_string = function
    | Empty -> ""
    | QuickFix -> "quickfix"
    | Refactor -> "refactor"
    | RefactorExtract -> "refactor.extract"
    | RefactorInline -> "refactor.inline"
    | RefactorRewrite -> "refactor.rewrite"
    | Source -> "source"
    | SourceOrganizeImports -> "source.organizeImports"
    | Other s -> s

  let yojson_of_t t = `String (to_string t)

  let t_of_yojson = function
    | `String s -> (
      match s with
      | "" -> Empty
      | "quickfix" -> QuickFix
      | "refactor" -> Refactor
      | "refactor.extract" -> RefactorExtract
      | "refactor.inline" -> RefactorInline
      | "refactor.rewrite" -> RefactorRewrite
      | "source" -> Source
      | "source.organizeImports" -> SourceOrganizeImports
      | s -> Other s )
    | j -> Json.error "Invalid code action" j
end

module Context = struct
  type t =
    { diagnostics : PublishDiagnostics.diagnostic list
    ; only : Kind.t Only.t [@default Only.All] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/codeAction.ml.Context.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let diagnostics_field = ref None
        and only_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "diagnostics" -> (
              match Ppx_yojson_conv_lib.( ! ) diagnostics_field with
              | None ->
                let fvalue =
                  list_of_yojson PublishDiagnostics.diagnostic_of_yojson
                    _field_yojson
                in
                diagnostics_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "only" -> (
              match Ppx_yojson_conv_lib.( ! ) only_field with
              | None ->
                let fvalue = Only.t_of_yojson Kind.t_of_yojson _field_yojson in
                only_field := Some fvalue
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
            match
              ( Ppx_yojson_conv_lib.( ! ) diagnostics_field
              , Ppx_yojson_conv_lib.( ! ) only_field )
            with
            | Some diagnostics_value, only_value ->
              { diagnostics = diagnostics_value
              ; only =
                  ( match only_value with
                  | None -> Only.All
                  | Some v -> v )
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) diagnostics_field)
                      None
                  , "diagnostics" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { diagnostics = v_diagnostics; only = v_only } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          if Only.All = v_only then
            bnds
          else
            let arg = (Only.yojson_of_t Kind.yojson_of_t) v_only in
            let bnd = ("only", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg =
            yojson_of_list PublishDiagnostics.yojson_of_diagnostic v_diagnostics
          in
          ("diagnostics", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module Params = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; context : Context.t
    }
  [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/codeAction.ml.Params.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let textDocument_field = ref None
        and range_field = ref None
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
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "range" -> (
              match Ppx_yojson_conv_lib.( ! ) range_field with
              | None ->
                let fvalue = Range.t_of_yojson _field_yojson in
                range_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "context" -> (
              match Ppx_yojson_conv_lib.( ! ) context_field with
              | None ->
                let fvalue = Context.t_of_yojson _field_yojson in
                context_field := Some fvalue
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
            match
              ( Ppx_yojson_conv_lib.( ! ) textDocument_field
              , Ppx_yojson_conv_lib.( ! ) range_field
              , Ppx_yojson_conv_lib.( ! ) context_field )
            with
            | Some textDocument_value, Some range_value, Some context_value ->
              { textDocument = textDocument_value
              ; range = range_value
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
                      (Ppx_yojson_conv_lib.( ! ) range_field)
                      None
                  , "range" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) context_field)
                      None
                  , "context" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { textDocument = v_textDocument; range = v_range; context = v_context }
        ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = Context.yojson_of_t v_context in
          ("context", arg) :: bnds
        in
        let bnds =
          let arg = Range.yojson_of_t v_range in
          ("range", arg) :: bnds
        in
        let bnds =
          let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
          ("textDocument", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

type t =
  { title : string
  ; kind : Kind.t option [@yojson.option]
  ; diagnostics : PublishDiagnostics.diagnostic list
        [@default []] [@yojson_drop_default ( = )]
  ; edit : WorkspaceEdit.t option [@yojson.option]
  ; command : Command.t option [@yojson.option]
  ; isPreferred : bool [@default false]
  }
[@@deriving_inline yojson_of]

let _ = fun (_ : t) -> ()

let yojson_of_t =
  (function
   | { title = v_title; kind = v_kind; diagnostics = v_diagnostics;
       edit = v_edit; command = v_command; isPreferred = v_isPreferred } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_isPreferred in ("isPreferred", arg) ::
           bnds in
       let bnds =
         match v_command with
         | None -> bnds
         | Some v ->
             let arg = Command.yojson_of_t v in
             let bnd = ("command", arg) in bnd :: bnds in
       let bnds =
         match v_edit with
         | None -> bnds
         | Some v ->
             let arg = WorkspaceEdit.yojson_of_t v in
             let bnd = ("edit", arg) in bnd :: bnds in
       let bnds =
         if [] = v_diagnostics
         then bnds
         else
           (let arg =
              (yojson_of_list PublishDiagnostics.yojson_of_diagnostic)
                v_diagnostics in
            let bnd = ("diagnostics", arg) in bnd :: bnds) in
       let bnds =
         match v_kind with
         | None -> bnds
         | Some v ->
             let arg = Kind.yojson_of_t v in
             let bnd = ("kind", arg) in bnd :: bnds in
       let bnds =
         let arg = yojson_of_string v_title in ("title", arg) :: bnds in
       `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

let _ = yojson_of_t

[@@@end]

type result = (Command.t, t) Either.t list

let yojson_of_result (elems : result) : Yojson.Safe.t =
  `List
    (List.map elems ~f:(function
      | Either.Right r -> yojson_of_t r
      | Left l -> Command.yojson_of_t l))

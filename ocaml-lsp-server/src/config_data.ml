open Import
open Import.Json.Conv

module InlayHints = struct
  type t =
    { hint_pattern_variables : bool [@key "hintPatternVariables"] [@default false]
    ; hint_let_bindings : bool [@key "hintLetBindings"] [@default false]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "ocaml-lsp-server/src/config_data.ml.InlayHints.t" in
     function
     | `Assoc field_yojsons as yojson ->
       let hint_pattern_variables_field = ref Ppx_yojson_conv_lib.Option.None
       and hint_let_bindings_field = ref Ppx_yojson_conv_lib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
            | "hintPatternVariables" ->
              (match Ppx_yojson_conv_lib.( ! ) hint_pattern_variables_field with
               | Ppx_yojson_conv_lib.Option.None ->
                 let fvalue = bool_of_yojson _field_yojson in
                 hint_pattern_variables_field := Ppx_yojson_conv_lib.Option.Some fvalue
               | Ppx_yojson_conv_lib.Option.Some _ ->
                 duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
            | "hintLetBindings" ->
              (match Ppx_yojson_conv_lib.( ! ) hint_let_bindings_field with
               | Ppx_yojson_conv_lib.Option.None ->
                 let fvalue = bool_of_yojson _field_yojson in
                 hint_let_bindings_field := Ppx_yojson_conv_lib.Option.Some fvalue
               | Ppx_yojson_conv_lib.Option.Some _ ->
                 duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
            | _ -> ());
           iter tail
         | [] -> ()
       in
       iter field_yojsons;
       (match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
            _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] ->
          (match Ppx_yojson_conv_lib.( ! ) extra with
           | _ :: _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
               _tp_loc
               (Ppx_yojson_conv_lib.( ! ) extra)
               yojson
           | [] ->
             let hint_pattern_variables_value, hint_let_bindings_value =
               ( Ppx_yojson_conv_lib.( ! ) hint_pattern_variables_field
               , Ppx_yojson_conv_lib.( ! ) hint_let_bindings_field )
             in
             { hint_pattern_variables =
                 (match hint_pattern_variables_value with
                  | Ppx_yojson_conv_lib.Option.None -> false
                  | Ppx_yojson_conv_lib.Option.Some v -> v)
             ; hint_let_bindings =
                 (match hint_let_bindings_value with
                  | Ppx_yojson_conv_lib.Option.None -> false
                  | Ppx_yojson_conv_lib.Option.Some v -> v)
             }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc yojson
     : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
  ;;

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { hint_pattern_variables = v_hint_pattern_variables
       ; hint_let_bindings = v_hint_let_bindings
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_hint_let_bindings in
         ("hintLetBindings", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_bool v_hint_pattern_variables in
         ("hintPatternVariables", arg) :: bnds
       in
       `Assoc bnds
     : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
  ;;

  let _ = yojson_of_t

  [@@@end]
end

module Lens = struct
  type t = { enable : bool [@default true] }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "ocaml-lsp-server/src/config_data.ml.Lens.t" in
     function
     | `Assoc field_yojsons as yojson ->
       let enable_field = ref Ppx_yojson_conv_lib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
            | "enable" ->
              (match Ppx_yojson_conv_lib.( ! ) enable_field with
               | Ppx_yojson_conv_lib.Option.None ->
                 let fvalue = bool_of_yojson _field_yojson in
                 enable_field := Ppx_yojson_conv_lib.Option.Some fvalue
               | Ppx_yojson_conv_lib.Option.Some _ ->
                 duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
            | _ -> ());
           iter tail
         | [] -> ()
       in
       iter field_yojsons;
       (match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
            _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] ->
          (match Ppx_yojson_conv_lib.( ! ) extra with
           | _ :: _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
               _tp_loc
               (Ppx_yojson_conv_lib.( ! ) extra)
               yojson
           | [] ->
             let enable_value = Ppx_yojson_conv_lib.( ! ) enable_field in
             { enable =
                 (match enable_value with
                  | Ppx_yojson_conv_lib.Option.None -> true
                  | Ppx_yojson_conv_lib.Option.Some v -> v)
             }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc yojson
     : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
  ;;

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { enable = v_enable } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_enable in
         ("enable", arg) :: bnds
       in
       `Assoc bnds
     : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
  ;;

  let _ = yojson_of_t

  [@@@end]
end

module ExtendedHover = struct
  type t = { enable : bool [@default false] }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "ocaml-lsp-server/src/config_data.ml.ExtendedHover.t" in
     function
     | `Assoc field_yojsons as yojson ->
       let enable_field = ref Ppx_yojson_conv_lib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
            | "enable" ->
              (match Ppx_yojson_conv_lib.( ! ) enable_field with
               | Ppx_yojson_conv_lib.Option.None ->
                 let fvalue = bool_of_yojson _field_yojson in
                 enable_field := Ppx_yojson_conv_lib.Option.Some fvalue
               | Ppx_yojson_conv_lib.Option.Some _ ->
                 duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
            | _ -> ());
           iter tail
         | [] -> ()
       in
       iter field_yojsons;
       (match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
            _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] ->
          (match Ppx_yojson_conv_lib.( ! ) extra with
           | _ :: _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
               _tp_loc
               (Ppx_yojson_conv_lib.( ! ) extra)
               yojson
           | [] ->
             let enable_value = Ppx_yojson_conv_lib.( ! ) enable_field in
             { enable =
                 (match enable_value with
                  | Ppx_yojson_conv_lib.Option.None -> false
                  | Ppx_yojson_conv_lib.Option.Some v -> v)
             }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc yojson
     : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
  ;;

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { enable = v_enable } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_enable in
         ("enable", arg) :: bnds
       in
       `Assoc bnds
     : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
  ;;

  let _ = yojson_of_t

  [@@@end]
end

module DuneDiagnostics = struct
  type t = { enable : bool [@default true] }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "ocaml-lsp-server/src/config_data.ml.DuneDiagnostics.t" in
     function
     | `Assoc field_yojsons as yojson ->
       let enable_field = ref Ppx_yojson_conv_lib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
            | "enable" ->
              (match Ppx_yojson_conv_lib.( ! ) enable_field with
               | Ppx_yojson_conv_lib.Option.None ->
                 let fvalue = bool_of_yojson _field_yojson in
                 enable_field := Ppx_yojson_conv_lib.Option.Some fvalue
               | Ppx_yojson_conv_lib.Option.Some _ ->
                 duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
            | _ -> ());
           iter tail
         | [] -> ()
       in
       iter field_yojsons;
       (match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
            _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] ->
          (match Ppx_yojson_conv_lib.( ! ) extra with
           | _ :: _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
               _tp_loc
               (Ppx_yojson_conv_lib.( ! ) extra)
               yojson
           | [] ->
             let enable_value = Ppx_yojson_conv_lib.( ! ) enable_field in
             { enable =
                 (match enable_value with
                  | Ppx_yojson_conv_lib.Option.None -> true
                  | Ppx_yojson_conv_lib.Option.Some v -> v)
             }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc yojson
     : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
  ;;

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { enable = v_enable } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_enable in
         ("enable", arg) :: bnds
       in
       `Assoc bnds
     : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
  ;;

  let _ = yojson_of_t

  [@@@end]
end

module SyntaxDocumentation = struct
  type t = { enable : bool [@default false] }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "ocaml-lsp-server/src/config_data.ml.SyntaxDocumentation.t" in
     function
     | `Assoc field_yojsons as yojson ->
       let enable_field = ref Ppx_yojson_conv_lib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
            | "enable" ->
              (match Ppx_yojson_conv_lib.( ! ) enable_field with
               | Ppx_yojson_conv_lib.Option.None ->
                 let fvalue = bool_of_yojson _field_yojson in
                 enable_field := Ppx_yojson_conv_lib.Option.Some fvalue
               | Ppx_yojson_conv_lib.Option.Some _ ->
                 duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
            | _ -> ());
           iter tail
         | [] -> ()
       in
       iter field_yojsons;
       (match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
            _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] ->
          (match Ppx_yojson_conv_lib.( ! ) extra with
           | _ :: _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
               _tp_loc
               (Ppx_yojson_conv_lib.( ! ) extra)
               yojson
           | [] ->
             let enable_value = Ppx_yojson_conv_lib.( ! ) enable_field in
             { enable =
                 (match enable_value with
                  | Ppx_yojson_conv_lib.Option.None -> false
                  | Ppx_yojson_conv_lib.Option.Some v -> v)
             }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc yojson
     : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
  ;;

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { enable = v_enable } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_enable in
         ("enable", arg) :: bnds
       in
       `Assoc bnds
     : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
  ;;

  let _ = yojson_of_t

  [@@@end]
end

type t =
  { codelens : Lens.t Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
  ; extended_hover : ExtendedHover.t Json.Nullable_option.t
       [@key "extendedHover"] [@default None] [@yojson_drop_default ( = )]
  ; inlay_hints : InlayHints.t Json.Nullable_option.t
       [@key "inlayHints"] [@default None] [@yojson_drop_default ( = )]
  ; dune_diagnostics : DuneDiagnostics.t Json.Nullable_option.t
       [@key "duneDiagnostics"] [@default None] [@yojson_drop_default ( = )]
  ; syntax_documentation : SyntaxDocumentation.t Json.Nullable_option.t
       [@key "syntaxDocumentation"] [@default None] [@yojson_drop_default ( = )]
  }
[@@deriving_inline yojson] [@@yojson.allow_extra_fields]

let _ = fun (_ : t) -> ()

let t_of_yojson =
  (let _tp_loc = "ocaml-lsp-server/src/config_data.ml.t" in
   function
   | `Assoc field_yojsons as yojson ->
     let codelens_field = ref Ppx_yojson_conv_lib.Option.None
     and extended_hover_field = ref Ppx_yojson_conv_lib.Option.None
     and inlay_hints_field = ref Ppx_yojson_conv_lib.Option.None
     and dune_diagnostics_field = ref Ppx_yojson_conv_lib.Option.None
     and syntax_documentation_field = ref Ppx_yojson_conv_lib.Option.None
     and duplicates = ref []
     and extra = ref [] in
     let rec iter = function
       | (field_name, _field_yojson) :: tail ->
         (match field_name with
          | "codelens" ->
            (match Ppx_yojson_conv_lib.( ! ) codelens_field with
             | Ppx_yojson_conv_lib.Option.None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson Lens.t_of_yojson _field_yojson
               in
               codelens_field := Ppx_yojson_conv_lib.Option.Some fvalue
             | Ppx_yojson_conv_lib.Option.Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
          | "extendedHover" ->
            (match Ppx_yojson_conv_lib.( ! ) extended_hover_field with
             | Ppx_yojson_conv_lib.Option.None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson ExtendedHover.t_of_yojson _field_yojson
               in
               extended_hover_field := Ppx_yojson_conv_lib.Option.Some fvalue
             | Ppx_yojson_conv_lib.Option.Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
          | "syntaxDocumentation" ->
            (match Ppx_yojson_conv_lib.( ! ) syntax_documentation_field with
             | Ppx_yojson_conv_lib.Option.None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   SyntaxDocumentation.t_of_yojson
                   _field_yojson
               in
               syntax_documentation_field := Ppx_yojson_conv_lib.Option.Some fvalue
             | Ppx_yojson_conv_lib.Option.Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
          | "inlayHints" ->
            (match Ppx_yojson_conv_lib.( ! ) inlay_hints_field with
             | Ppx_yojson_conv_lib.Option.None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson InlayHints.t_of_yojson _field_yojson
               in
               inlay_hints_field := Ppx_yojson_conv_lib.Option.Some fvalue
             | Ppx_yojson_conv_lib.Option.Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
          | "duneDiagnostics" ->
            (match Ppx_yojson_conv_lib.( ! ) dune_diagnostics_field with
             | Ppx_yojson_conv_lib.Option.None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   DuneDiagnostics.t_of_yojson
                   _field_yojson
               in
               dune_diagnostics_field := Ppx_yojson_conv_lib.Option.Some fvalue
             | Ppx_yojson_conv_lib.Option.Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
          | _ -> ());
         iter tail
       | [] -> ()
     in
     iter field_yojsons;
     (match Ppx_yojson_conv_lib.( ! ) duplicates with
      | _ :: _ ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
          _tp_loc
          (Ppx_yojson_conv_lib.( ! ) duplicates)
          yojson
      | [] ->
        (match Ppx_yojson_conv_lib.( ! ) extra with
         | _ :: _ ->
           Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
             _tp_loc
             (Ppx_yojson_conv_lib.( ! ) extra)
             yojson
         | [] ->
           let ( codelens_value
               , extended_hover_value
               , inlay_hints_value
               , dune_diagnostics_value
               , syntax_documentation_value )
             =
             ( Ppx_yojson_conv_lib.( ! ) codelens_field
             , Ppx_yojson_conv_lib.( ! ) extended_hover_field
             , Ppx_yojson_conv_lib.( ! ) inlay_hints_field
             , Ppx_yojson_conv_lib.( ! ) dune_diagnostics_field
             , Ppx_yojson_conv_lib.( ! ) syntax_documentation_field )
           in
           { codelens =
               (match codelens_value with
                | Ppx_yojson_conv_lib.Option.None -> None
                | Ppx_yojson_conv_lib.Option.Some v -> v)
           ; extended_hover =
               (match extended_hover_value with
                | Ppx_yojson_conv_lib.Option.None -> None
                | Ppx_yojson_conv_lib.Option.Some v -> v)
           ; inlay_hints =
               (match inlay_hints_value with
                | Ppx_yojson_conv_lib.Option.None -> None
                | Ppx_yojson_conv_lib.Option.Some v -> v)
           ; dune_diagnostics =
               (match dune_diagnostics_value with
                | Ppx_yojson_conv_lib.Option.None -> None
                | Ppx_yojson_conv_lib.Option.Some v -> v)
           ; syntax_documentation =
               (match syntax_documentation_value with
                | Ppx_yojson_conv_lib.Option.None -> None
                | Ppx_yojson_conv_lib.Option.Some v -> v)
           }))
   | _ as yojson ->
     Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc yojson
   : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
;;

let _ = t_of_yojson

let yojson_of_t =
  (function
   | { codelens = v_codelens
     ; extended_hover = v_extended_hover
     ; inlay_hints = v_inlay_hints
     ; dune_diagnostics = v_dune_diagnostics
     ; syntax_documentation = v_syntax_documentation
     } ->
     let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
     let bnds =
       if None = v_dune_diagnostics
       then bnds
       else (
         let arg =
           (Json.Nullable_option.yojson_of_t DuneDiagnostics.yojson_of_t)
             v_dune_diagnostics
         in
         let bnd = "duneDiagnostics", arg in
         bnd :: bnds)
     in
     let bnds =
       if None = v_inlay_hints
       then bnds
       else (
         let arg =
           (Json.Nullable_option.yojson_of_t InlayHints.yojson_of_t) v_inlay_hints
         in
         let bnd = "inlayHints", arg in
         bnd :: bnds)
     in
     let bnds =
       if None = v_syntax_documentation
       then bnds
       else (
         let arg =
           (Json.Nullable_option.yojson_of_t SyntaxDocumentation.yojson_of_t)
             v_syntax_documentation
         in
         let bnd = "syntaxDocumentation", arg in
         bnd :: bnds)
     in
     let bnds =
       if None = v_extended_hover
       then bnds
       else (
         let arg =
           (Json.Nullable_option.yojson_of_t ExtendedHover.yojson_of_t) v_extended_hover
         in
         let bnd = "extendedHover", arg in
         bnd :: bnds)
     in
     let bnds =
       if None = v_codelens
       then bnds
       else (
         let arg = (Json.Nullable_option.yojson_of_t Lens.yojson_of_t) v_codelens in
         let bnd = "codelens", arg in
         bnd :: bnds)
     in
     `Assoc bnds
   : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
;;

let _ = yojson_of_t

[@@@end]

let default =
  { codelens = Some { enable = false }
  ; extended_hover = Some { enable = false }
  ; inlay_hints = Some { hint_pattern_variables = false; hint_let_bindings = false }
  ; dune_diagnostics = Some { enable = true }
  ; syntax_documentation = Some { enable = false }
  }
;;

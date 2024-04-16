open Import
open Import.Json.Conv

module InlayHints = struct
  type t =
    { hint_pattern_variables : bool
          [@key "hintPatternVariables"] [@default false]
    ; hint_let_bindings : bool [@key "hintLetBindings"] [@default false]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "ocaml-lsp-server/src/config_data.ml.InlayHints.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let hint_pattern_variables_field = ref Ppx_yojson_conv_lib.Option.None
       and hint_let_bindings_field = ref Ppx_yojson_conv_lib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "hintPatternVariables" -> (
             match Ppx_yojson_conv_lib.( ! ) hint_pattern_variables_field with
             | Ppx_yojson_conv_lib.Option.None ->
               let fvalue = bool_of_yojson _field_yojson in
               hint_pattern_variables_field :=
                 Ppx_yojson_conv_lib.Option.Some fvalue
             | Ppx_yojson_conv_lib.Option.Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "hintLetBindings" -> (
             match Ppx_yojson_conv_lib.( ! ) hint_let_bindings_field with
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
       match Ppx_yojson_conv_lib.( ! ) duplicates with
       | _ :: _ ->
         Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
           _tp_loc
           (Ppx_yojson_conv_lib.( ! ) duplicates)
           yojson
       | [] -> (
         match Ppx_yojson_conv_lib.( ! ) extra with
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
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom
         _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

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
     | `Assoc field_yojsons as yojson -> (
       let enable_field = ref Ppx_yojson_conv_lib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "enable" -> (
             match Ppx_yojson_conv_lib.( ! ) enable_field with
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
       match Ppx_yojson_conv_lib.( ! ) duplicates with
       | _ :: _ ->
         Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
           _tp_loc
           (Ppx_yojson_conv_lib.( ! ) duplicates)
           yojson
       | [] -> (
         match Ppx_yojson_conv_lib.( ! ) extra with
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
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom
         _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

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
     | `Assoc field_yojsons as yojson -> (
       let enable_field = ref Ppx_yojson_conv_lib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "enable" -> (
             match Ppx_yojson_conv_lib.( ! ) enable_field with
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
       match Ppx_yojson_conv_lib.( ! ) duplicates with
       | _ :: _ ->
         Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
           _tp_loc
           (Ppx_yojson_conv_lib.( ! ) duplicates)
           yojson
       | [] -> (
         match Ppx_yojson_conv_lib.( ! ) extra with
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
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom
         _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

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
     | `Assoc field_yojsons as yojson -> (
       let enable_field = ref Ppx_yojson_conv_lib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "enable" -> (
             match Ppx_yojson_conv_lib.( ! ) enable_field with
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
       match Ppx_yojson_conv_lib.( ! ) duplicates with
       | _ :: _ ->
         Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
           _tp_loc
           (Ppx_yojson_conv_lib.( ! ) duplicates)
           yojson
       | [] -> (
         match Ppx_yojson_conv_lib.( ! ) extra with
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
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom
         _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

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

  let _ = yojson_of_t

  [@@@end]
end

module DuneContext = struct
  type selected =
    | Default
    | Custom of string

  type t = { value : selected [@default Default] }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let to_string = function
    | Default -> "default"
    | Custom str -> str

  let of_string = function
    | "default" -> Default
    | str -> Custom str

  let to_dyn t = Dyn.string (to_string t)

  let equal a b =
    match (a, b) with
    | Default, Default -> true
    | Custom str1, Custom str2 when String.equal str1 str2 -> true
    | Default, Custom _ | Custom _, Default | Custom _, Custom _ -> false

  let t_of_yojson =
    (let _tp_loc = "ocaml-lsp-server/src/config_data.ml.DuneContext.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let value_field = ref Ppx_yojson_conv_lib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "value" -> (
             match Ppx_yojson_conv_lib.( ! ) value_field with
             | Ppx_yojson_conv_lib.Option.None ->
               let fvalue = string_of_yojson _field_yojson in
               value_field := Ppx_yojson_conv_lib.Option.Some fvalue
             | Ppx_yojson_conv_lib.Option.Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
             _tp_loc
             (Ppx_yojson_conv_lib.( ! ) extra)
             yojson
         | [] ->
           let value_value = Ppx_yojson_conv_lib.( ! ) value_field in
           { value =
               (match value_value with
               | Ppx_yojson_conv_lib.Option.None
               | Ppx_yojson_conv_lib.Option.Some "default" -> Default
               | Ppx_yojson_conv_lib.Option.Some v -> Custom v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom
         _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { value = v_value } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg =
           match v_value with
           | Default -> "default"
           | Custom ctxt -> ctxt
         in
         ("value", yojson_of_string arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]
end

type t =
  { codelens : Lens.t Json.Nullable_option.t
        [@default None] [@yojson_drop_default ( = )]
  ; extended_hover : ExtendedHover.t Json.Nullable_option.t
        [@key "extendedHover"] [@default None] [@yojson_drop_default ( = )]
  ; inlay_hints : InlayHints.t Json.Nullable_option.t
        [@key "inlayHints"] [@default None] [@yojson_drop_default ( = )]
  ; dune_diagnostics : DuneDiagnostics.t Json.Nullable_option.t
        [@key "duneDiagnostics"] [@default None] [@yojson_drop_default ( = )]
  ; dune_context : DuneContext.t Json.Nullable_option.t
        [@key "duneContext"] [@default None] [@yojson_drop_default ( = )]
  }
[@@deriving_inline yojson] [@@yojson.allow_extra_fields]

let _ = fun (_ : t) -> ()

let t_of_yojson =
  (let _tp_loc = "ocaml-lsp-server/src/config_data.ml.t" in
   function
   | `Assoc field_yojsons as yojson -> (
     let codelens_field = ref Ppx_yojson_conv_lib.Option.None
     and extended_hover_field = ref Ppx_yojson_conv_lib.Option.None
     and inlay_hints_field = ref Ppx_yojson_conv_lib.Option.None
     and dune_diagnostics_field = ref Ppx_yojson_conv_lib.Option.None
     and dune_context_field = ref Ppx_yojson_conv_lib.Option.None
     and duplicates = ref []
     and extra = ref [] in
     let rec iter = function
       | (field_name, _field_yojson) :: tail ->
         (match field_name with
         | "codelens" -> (
           match Ppx_yojson_conv_lib.( ! ) codelens_field with
           | Ppx_yojson_conv_lib.Option.None ->
             let fvalue =
               Json.Nullable_option.t_of_yojson Lens.t_of_yojson _field_yojson
             in
             codelens_field := Ppx_yojson_conv_lib.Option.Some fvalue
           | Ppx_yojson_conv_lib.Option.Some _ ->
             duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
         | "extendedHover" -> (
           match Ppx_yojson_conv_lib.( ! ) extended_hover_field with
           | Ppx_yojson_conv_lib.Option.None ->
             let fvalue =
               Json.Nullable_option.t_of_yojson
                 ExtendedHover.t_of_yojson
                 _field_yojson
             in
             extended_hover_field := Ppx_yojson_conv_lib.Option.Some fvalue
           | Ppx_yojson_conv_lib.Option.Some _ ->
             duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
         | "inlayHints" -> (
           match Ppx_yojson_conv_lib.( ! ) inlay_hints_field with
           | Ppx_yojson_conv_lib.Option.None ->
             let fvalue =
               Json.Nullable_option.t_of_yojson
                 InlayHints.t_of_yojson
                 _field_yojson
             in
             inlay_hints_field := Ppx_yojson_conv_lib.Option.Some fvalue
           | Ppx_yojson_conv_lib.Option.Some _ ->
             duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
         | "duneDiagnostics" -> (
           match Ppx_yojson_conv_lib.( ! ) dune_diagnostics_field with
           | Ppx_yojson_conv_lib.Option.None ->
             let fvalue =
               Json.Nullable_option.t_of_yojson
                 DuneDiagnostics.t_of_yojson
                 _field_yojson
             in
             dune_diagnostics_field := Ppx_yojson_conv_lib.Option.Some fvalue
           | Ppx_yojson_conv_lib.Option.Some _ ->
             duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
         | "duneContext" -> (
           match Ppx_yojson_conv_lib.( ! ) dune_context_field with
           | Ppx_yojson_conv_lib.Option.None ->
             let fvalue =
               Json.Nullable_option.t_of_yojson
                 DuneContext.t_of_yojson
                 _field_yojson
             in
             dune_context_field := Ppx_yojson_conv_lib.Option.Some fvalue
           | Ppx_yojson_conv_lib.Option.Some _ ->
             duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
         | _ -> ());
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
         Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
           _tp_loc
           (Ppx_yojson_conv_lib.( ! ) extra)
           yojson
       | [] ->
         let ( codelens_value
             , extended_hover_value
             , inlay_hints_value
             , dune_diagnostics_value
             , dune_context_value ) =
           ( Ppx_yojson_conv_lib.( ! ) codelens_field
           , Ppx_yojson_conv_lib.( ! ) extended_hover_field
           , Ppx_yojson_conv_lib.( ! ) inlay_hints_field
           , Ppx_yojson_conv_lib.( ! ) dune_diagnostics_field
           , Ppx_yojson_conv_lib.( ! ) dune_context_field )
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
         ; dune_context =
             (match dune_context_value with
             | Ppx_yojson_conv_lib.Option.None -> None
             | Ppx_yojson_conv_lib.Option.Some v -> v)
         }))
   | _ as yojson ->
     Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom
       _tp_loc
       yojson
    : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

let _ = t_of_yojson

let yojson_of_t =
  (function
   | { codelens = v_codelens
     ; extended_hover = v_extended_hover
     ; inlay_hints = v_inlay_hints
     ; dune_diagnostics = v_dune_diagnostics
     ; dune_context = v_dune_context
     } ->
     let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
     let bnds =
       if None = v_dune_diagnostics then bnds
       else
         let arg =
           (Json.Nullable_option.yojson_of_t DuneDiagnostics.yojson_of_t)
             v_dune_diagnostics
         in
         let bnd = ("duneDiagnostics", arg) in
         bnd :: bnds
     in
     let bnds =
       if None = v_inlay_hints then bnds
       else
         let arg =
           (Json.Nullable_option.yojson_of_t InlayHints.yojson_of_t)
             v_inlay_hints
         in
         let bnd = ("inlayHints", arg) in
         bnd :: bnds
     in
     let bnds =
       if None = v_extended_hover then bnds
       else
         let arg =
           (Json.Nullable_option.yojson_of_t ExtendedHover.yojson_of_t)
             v_extended_hover
         in
         let bnd = ("extendedHover", arg) in
         bnd :: bnds
     in
     let bnds =
       if None = v_codelens then bnds
       else
         let arg =
           (Json.Nullable_option.yojson_of_t Lens.yojson_of_t) v_codelens
         in
         let bnd = ("codelens", arg) in
         bnd :: bnds
     in
     let bnds =
       if None = v_dune_context then bnds
       else
         let arg =
           (Json.Nullable_option.yojson_of_t DuneContext.yojson_of_t)
             v_dune_context
         in
         let bnd = ("duneContext", arg) in
         bnd :: bnds
     in
     `Assoc bnds
    : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

let _ = yojson_of_t

[@@@end]

let default =
  { codelens = Some { enable = false }
  ; extended_hover = Some { enable = false }
  ; inlay_hints =
      Some { hint_pattern_variables = false; hint_let_bindings = false }
  ; dune_diagnostics = Some { enable = true }
  ; dune_context = Some { value = Default }
  }

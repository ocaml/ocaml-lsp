open Import
module List = Merlin_utils.Std.List
module String = Merlin_utils.Std.String
module Misc_utils = Merlin_analysis.Misc_utils
module Type_utils = Merlin_analysis.Type_utils

open struct
  open Ocaml_typing
  module Predef = Predef
  module Btype = Btype
end

type parameter_info =
  { label : Asttypes.arg_label
  ; param_start : int
  ; param_end : int
  ; argument : Typedtree.expression option
  }

type application_signature =
  { function_name : string option
  ; function_position : Msource.position
  ; signature : string
  ; parameters : parameter_info list
  ; active_param : int option
  }

(* extract a properly parenthesized identifier from (expression_desc (Texp_ident
   (Longident))) *)
let extract_ident (exp_desc : Typedtree.expression_desc) =
  let rec longident ppf : Longident.t -> unit = function
    | Lident s -> Format.fprintf ppf "%s" (Misc_utils.parenthesize_name s)
    | Ldot (p, s) ->
      Format.fprintf ppf "%a.%s" longident p (Misc_utils.parenthesize_name s)
    | Lapply (p1, p2) -> Format.fprintf ppf "%a(%a)" longident p1 longident p2
  in
  match exp_desc with
  | Texp_ident (_, { txt = li; _ }, _) ->
    let ppf, to_string = Format.to_string () in
    longident ppf li;
    Some (to_string ())
  | _ -> None
;;

(* Type variables shared across arguments should all be printed with the same
   name. [Printtyp.type_scheme] ensure that a name is unique within a given
   type, but not across different invocations. [reset] followed by calls to
   [mark_loops] and [type_sch] provide that *)
let pp_type env ppf ty =
  let module Printtyp = Type_utils.Printtyp in
  Printtyp.wrap_printing_env env ~verbosity:(Lvl 0) (fun () ->
    Printtyp.shared_type_scheme ppf ty)
;;

let rec type_is_arrow ty =
  match Types.get_desc ty with
  | Tarrow _ -> true
  | Tlink ty -> type_is_arrow ty
  | Tpoly (ty, _) -> type_is_arrow ty
  | _ -> false
;;

(* surround function types in parentheses *)
let pp_parameter_type env ppf ty =
  if type_is_arrow ty
  then Format.fprintf ppf "(%a)" (pp_type env) ty
  else pp_type env ppf ty
;;

(* print parameter labels and types *)
let pp_parameter env label ppf ty =
  match (label : Asttypes.arg_label) with
  | Nolabel -> pp_parameter_type env ppf ty
  | Labelled l -> Format.fprintf ppf "%s:%a" l (pp_parameter_type env) ty
  | Optional l ->
    (* unwrap option for optional labels the same way as
       [Raw_compat.labels_of_application] *)
    let unwrap_option ty =
      match Types.get_desc ty with
      | Types.Tconstr (path, [ ty ], _) when Path.same path Predef.path_option -> ty
      | _ -> ty
    in
    Format.fprintf ppf "?%s:%a" l (pp_parameter_type env) (unwrap_option ty)
;;

(* record buffer offsets to be able to underline parameter types *)
let print_parameter_offset ?arg:argument ppf buffer env label ty =
  let param_start = Buffer.length buffer in
  Format.fprintf ppf "%a%!" (pp_parameter env label) ty;
  let param_end = Buffer.length buffer in
  Format.pp_print_string ppf " -> ";
  Format.pp_print_flush ppf ();
  { label; param_start; param_end; argument }
;;

let separate_function_signature ~args (e : Typedtree.expression) =
  Type_utils.Printtyp.reset ();
  let buffer = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buffer in
  let rec separate ?(i = 0) ?(parameters = []) args ty =
    match args, Types.get_desc ty with
    | (_l, arg) :: args, Tarrow (label, ty1, ty2, _) ->
      let parameter = print_parameter_offset ppf buffer e.exp_env label ty1 ?arg in
      separate args ty2 ~i:(succ i) ~parameters:(parameter :: parameters)
    | [], Tarrow (label, ty1, ty2, _) ->
      let parameter = print_parameter_offset ppf buffer e.exp_env label ty1 in
      separate args ty2 ~i:(succ i) ~parameters:(parameter :: parameters)
    (* end of function type, print remaining type without recording offsets *)
    | _ ->
      Format.fprintf ppf "%a%!" (pp_type e.exp_env) ty;
      { function_name = extract_ident e.exp_desc
      ; function_position = `Offset e.exp_loc.loc_end.pos_cnum
      ; signature = Buffer.contents buffer
      ; parameters = List.rev parameters
      ; active_param = None
      }
  in
  separate args e.exp_type
;;

let active_parameter_by_arg ~arg params =
  let find_by_arg = function
    | { argument = Some a; _ } when a == arg -> true
    | _ -> false
  in
  try Some (List.index params ~f:find_by_arg) with
  | Not_found -> None
;;

let active_parameter_by_prefix ~prefix params =
  let common = function
    | Asttypes.Nolabel -> Some 0
    | l when String.is_prefixed ~by:"~" prefix || String.is_prefixed ~by:"?" prefix ->
      Some (String.common_prefix_len (Btype.prefixed_label_name l) prefix)
    | _ -> None
  in
  let rec find_by_prefix ?(i = 0) ?longest_len ?longest_i = function
    | [] -> longest_i
    | p :: ps ->
      (match common p.label, longest_len with
       | Some common_len, Some longest_len when common_len > longest_len ->
         find_by_prefix ps ~i:(succ i) ~longest_len:common_len ~longest_i:i
       | Some common_len, None ->
         find_by_prefix ps ~i:(succ i) ~longest_len:common_len ~longest_i:i
       | _ -> find_by_prefix ps ~i:(succ i) ?longest_len ?longest_i)
  in
  find_by_prefix params
;;

let is_arrow t =
  match Types.get_desc t with
  | Tarrow _ -> true
  | _ -> false
;;

let application_signature ~prefix = function
  (* provide signature information for applied functions *)
  | (_, Browse_raw.Expression arg)
    :: (_, Expression { exp_desc = Texp_apply (({ exp_type; _ } as e), args); _ })
    :: _
    when is_arrow exp_type ->
    let result = separate_function_signature e ~args in
    let active_param = active_parameter_by_arg ~arg result.parameters in
    let active_param =
      match active_param with
      | Some _ as ap -> ap
      | None -> active_parameter_by_prefix ~prefix result.parameters
    in
    Some { result with active_param }
  (* provide signature information directly after an unapplied function-type
     value *)
  | (_, Expression ({ exp_type; _ } as e)) :: _ when is_arrow exp_type ->
    let result = separate_function_signature e ~args:[] in
    let active_param = active_parameter_by_prefix ~prefix result.parameters in
    Some { result with active_param }
  | _ -> None
;;

let format_doc ~markdown ~doc =
  `MarkupContent
    (if markdown
     then (
       let value =
         match Doc_to_md.translate doc with
         | Raw d -> sprintf "(** %s *)" d
         | Markdown d -> d
       in
       { MarkupContent.value; kind = MarkupKind.Markdown })
     else { MarkupContent.value = doc; kind = MarkupKind.PlainText })
;;

let run (state : State.t) { SignatureHelpParams.textDocument = { uri }; position; _ } =
  let open Fiber.O in
  let doc =
    let store = state.store in
    Document_store.get store uri
  in
  let pos = Position.logical position in
  let prefix =
    (* The value of [short_path] doesn't make a difference to the final result
       because labels cannot include dots. However, a true value is slightly
       faster for getting the prefix. *)
    Compl.prefix_of_position (Document.source doc) pos ~short_path:true
  in
  (* TODO use merlin resources efficiently and do everything in 1 thread *)
  match Document.kind doc with
  | `Other ->
    let help = SignatureHelp.create ~signatures:[] () in
    Fiber.return help
  | `Merlin merlin ->
    let* application_signature =
      let* inside_comment = Check_for_comments.position_in_comment ~position ~merlin in
      match inside_comment with
      | true -> Fiber.return None
      | false ->
        Document.Merlin.with_pipeline_exn ~name:"signature-help" merlin (fun pipeline ->
          let typer = Mpipeline.typer_result pipeline in
          let pos = Mpipeline.get_lexing_pos pipeline pos in
          let node = Mtyper.node_at typer pos in
          application_signature node ~prefix)
    in
    (match application_signature with
     | None ->
       let help = SignatureHelp.create ~signatures:[] () in
       Fiber.return help
     | Some application_signature ->
       let prefix =
         let fun_name = Option.value ~default:"_" application_signature.function_name in
         sprintf "%s : " fun_name
       in
       let offset = String.length prefix in
       let+ doc =
         Document.Merlin.doc_comment
           ~name:"signature help-position"
           merlin
           application_signature.function_position
       in
       let info =
         let parameters =
           List.map application_signature.parameters ~f:(fun (p : parameter_info) ->
             let label = `Offset (offset + p.param_start, offset + p.param_end) in
             ParameterInformation.create ~label ())
         in
         let documentation =
           let open Option.O in
           let+ doc in
           let markdown =
             ClientCapabilities.markdown_support
               (State.client_capabilities state)
               ~field:(fun td ->
                 let* sh = td.signatureHelp in
                 let+ si = sh.signatureInformation in
                 si.documentationFormat)
           in
           format_doc ~markdown ~doc
         in
         let label = prefix ^ application_signature.signature in
         SignatureInformation.create ~label ?documentation ~parameters ()
       in
       SignatureHelp.create
         ~signatures:[ info ]
         ~activeSignature:0
         ?activeParameter:(Some application_signature.active_param)
         ())
;;

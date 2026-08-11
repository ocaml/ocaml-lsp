open Import
module Env_lookup = Merlin_analysis.Env_lookup
module Locate = Merlin_analysis.Locate

(** Split an odoc reference into its kind (if any) and its path, e.g.
    [value:foo] returns [(Some "value", "foo")], [bar] returns [(None, "bar")]. *)
let split reference =
  let split separator component =
    match String.rsplit2 component ~on:separator with
    | Some (kind, name) -> Some kind, name
    | None -> None, component
  in
  let kind, path = split ':' reference in
  let components = String.split path ~on:'.' in
  let kind, components =
    match List.rev components with
    | [] -> kind, components
    | last :: rest ->
      let of_component, last = split '-' last in
      Option.first_some kind of_component, List.rev (last :: rest)
  in
  kind, String.concat components ~sep:"."
;;

let path reference = snd (split reference)

(** Say which namespaces to search: the one its kind names, or all of them. *)
let namespaces_of_kind kind : Env_lookup.Namespace.inferred_basic list =
  match kind with
  | Some "type" -> [ `Type ]
  | Some ("val" | "value") -> [ `Vals ]
  | Some "module" -> [ `Mod ]
  | Some ("modtype" | "module-type") -> [ `Modtype ]
  | Some "constructor" -> [ `Constr ]
  | Some "field" -> [ `Labels ]
  (* An unqualified reference, or one whose kind has no namespace of its own
     such as [exception] or [method]. *)
  | Some _ | None -> [ `Type; `Vals; `Mod; `Modtype; `Constr; `Labels ]
;;

(** Find the appropriate environment in which to resolve references. *)
let signature_env (node : Browse_raw.node) =
  match node with
  | Structure structure -> Some structure.str_final_env
  | Signature signature -> Some signature.sig_final_env
  | Module_expr { mod_desc = Tmod_structure structure; _ } -> Some structure.str_final_env
  | Module_type { mty_desc = Tmty_signature signature; _ } -> Some signature.sig_final_env
  | _ -> None
;;

let enclosing_signature_env local_defs pos =
  Mbrowse.enclosing pos [ Mbrowse.of_typedtree local_defs ]
  |> List.find_map ~f:(fun (_, node) -> signature_env node)
;;

(** [DocumentLink.target] and a markdown link are both plain URIs, so a position
    within the file goes in the fragment, as [#L<line>,<column>]. *)
let target ~uri file (position : Lexing.position) =
  let open Option.O in
  let+ { Position.line; character } = Position.of_lexical_position position in
  let uri = Option.value_map file ~default:uri ~f:Uri.of_path in
  Uri.of_string (sprintf "%s#L%d,%d" (Uri.to_string uri) (line + 1) (character + 1))
;;

let resolve pipeline ~uri ~position reference =
  let open Option.O in
  let pos = Mpipeline.get_lexing_pos pipeline (Position.logical position) in
  let local_defs = Mtyper.get_typedtree (Mpipeline.typer_result pipeline) in
  let* env = enclosing_signature_env local_defs pos in
  let kind, path = split reference in
  let config =
    { Locate.mconfig = Mpipeline.final_config pipeline
    ; ml_or_mli = `Smart
    ; traverse_aliases = true
    }
  in
  let namespaces = namespaces_of_kind kind in
  match Locate.from_string ~config ~env ~local_defs ~pos ~namespaces path with
  | `Found { Locate.file; location; _ } -> target ~uri (Some file) location.loc_start
  (* Merlin cannot always place a reference: it may name something from a unit
     that has not been built, a builtin, or nothing at all. *)
  | `At_origin
  | `Builtin _
  | `File_not_found _
  | `Missing_labels_namespace
  | `Not_found _
  | `Not_in_env _ -> None
;;

(** Collect every reference a comment mentions, so that a caller holding a
    pipeline can settle them all at once. *)
let rec of_inline_elements
          acc
          (elements : Odoc_parser.Ast.inline_element Odoc_parser.Loc.with_location list)
  =
  List.fold elements ~init:acc ~f:(fun acc { Odoc_parser.Loc.value; location = _ } ->
    match value with
    | `Reference (_, reference, content) ->
      of_inline_elements (reference.Odoc_parser.Loc.value :: acc) content
    | `Styled (_, content) | `Link (_, content) -> of_inline_elements acc content
    | `Space _ | `Word _ | `Code_span _ | `Raw_markup _ | `Math_span _ -> acc)
;;

let rec of_nestable_block_elements
          acc
          (elements :
            Odoc_parser.Ast.nestable_block_element Odoc_parser.Loc.with_location list)
  =
  List.fold elements ~init:acc ~f:(fun acc { Odoc_parser.Loc.value; location = _ } ->
    match value with
    | `Paragraph inlines -> of_inline_elements acc inlines
    | `List (_, _, items) -> List.fold items ~init:acc ~f:of_nestable_block_elements
    | `Table ((grid, _), _) ->
      List.fold grid ~init:acc ~f:(fun acc row ->
        List.fold row ~init:acc ~f:(fun acc (cell, _) ->
          of_nestable_block_elements acc cell))
    | `Code_block { output = Some output; _ } -> of_nestable_block_elements acc output
    | `Code_block _ | `Verbatim _ | `Modules _ | `Math_block _ -> acc)
;;

let of_tag acc (tag : Odoc_parser.Ast.tag) =
  match tag with
  | `Deprecated content
  | `Return content
  | `See (_, _, content)
  | `Param (_, content)
  | `Raise (_, content)
  | `Before (_, content) -> of_nestable_block_elements acc content
  | `Author _ | `Since _ | `Version _ | `Canonical _ | `Inline | `Open | `Closed | `Hidden
    -> acc
;;

let of_comment text =
  Odoc_parser.parse_comment ~location:Lexing.dummy_pos ~text
  |> Odoc_parser.ast
  |> List.fold ~init:[] ~f:(fun acc element ->
    match element.Odoc_parser.Loc.value with
    | `Heading (_, _, inlines) -> of_inline_elements acc inlines
    | `Tag v -> of_tag acc v
    | #Odoc_parser.Ast.nestable_block_element as value ->
      of_nestable_block_elements acc [ { element with Odoc_parser.Loc.value } ])
  |> List.dedup_and_sort ~compare:String.compare
;;

let resolve_all pipeline ~uri ~position text =
  of_comment text
  |> List.filter_map ~f:(fun reference ->
    let open Option.O in
    let+ target = resolve pipeline ~uri ~position reference in
    reference, target)
;;

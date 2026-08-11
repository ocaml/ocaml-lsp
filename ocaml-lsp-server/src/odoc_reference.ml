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

open Import

let capability = ("handleWrappingAstNode", `Bool true)

let meth = "ocamllsp/wrappingAstNode"

module Request_params = struct
  type t =
    { text_document_uri : Uri.t
    ; cursor_position : Position.t
    }

  let params_schema =
    `Assoc
      [ ("uri", `String "<DocumentUri>"); ("position", `String "<Position>") ]

  let of_jsonrpc_params params : t option =
    match params with
    | `Assoc [ ("uri", uri); ("position", position) ] ->
      let text_document_uri = Uri.t_of_yojson uri in
      let cursor_position = Position.t_of_yojson position in
      Some { text_document_uri; cursor_position }
    | _ -> None

  let of_jsonrpc_params_exn params : t =
    let params_spec = { Custom_request.params_schema; of_jsonrpc_params } in
    Custom_request.of_jsonrpc_params_exn params_spec params
end

let on_request ~params state =
  let { Request_params.text_document_uri; cursor_position } =
    Request_params.of_jsonrpc_params_exn params
  in
  let doc = Document_store.get state.State.store text_document_uri in
  let pos = Position.logical cursor_position in
  let open Fiber.O in
  let+ node =
    Document.with_pipeline_exn doc (fun pipeline ->
        let typer = Mpipeline.typer_result pipeline in
        let pos = Mpipeline.get_lexing_pos pipeline pos in
        let enclosing_nodes (* from smallest node to largest *) =
          Mbrowse.enclosing pos
            [ Mbrowse.of_typedtree (Mtyper.get_typedtree typer) ]
        in
        let loc_of_structure_item { Typedtree.str_loc; _ } = str_loc in
        let find_fst_structure_item_or_structure = function
          | _, Browse_raw.Structure_item (str_item, _) ->
            Some (loc_of_structure_item str_item)
          | _, Structure { str_items; _ } -> (
            match str_items with
            | [] -> None
            | hd :: rest ->
              let last = List.last rest |> Option.value ~default:hd in
              let { Loc.loc_start; _ } = loc_of_structure_item hd in
              let { Loc.loc_end; _ } = loc_of_structure_item last in
              Some { Loc.loc_start; loc_end; loc_ghost = false })
          | _ -> None
        in
        List.find_map enclosing_nodes ~f:find_fst_structure_item_or_structure)
  in
  match node with
  | None -> `Null
  | Some loc -> Range.of_loc loc |> Range.yojson_of_t

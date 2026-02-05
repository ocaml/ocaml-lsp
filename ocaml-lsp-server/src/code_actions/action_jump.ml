open Import
open Fiber.O
open Stdune

let command_name = "ocamllsp/merlin-jump-to-target"

let rename_target target =
  if String.starts_with ~prefix:"match-" target
  then String.sub target ~pos:6 ~len:(String.length target - 6)
  else target
;;

let available (capabilities : ShowDocumentClientCapabilities.t option) =
  match capabilities with
  | Some { support } -> support
  | None -> false
;;

let error message =
  Jsonrpc.Response.Error.raise
  @@ Jsonrpc.Response.Error.make
       ~code:Jsonrpc.Response.Error.Code.InvalidParams
       ~message
       ()
;;

let command_run server (params : ExecuteCommandParams.t) =
  let uri, range =
    match params.arguments with
    | Some [ json_uri; json_range ] ->
      let uri = DocumentUri.t_of_yojson json_uri in
      let range = Range.t_of_yojson json_range in
      uri, range
    | None | Some _ -> error "takes a URI and a range as input"
  in
  let+ { ShowDocumentResult.success } =
    let req = ShowDocumentParams.create ~uri ~selection:range ~takeFocus:true () in
    Server.request server (Server_request.ShowDocumentRequest req)
  in
  if not success
  then (
    let uri = Uri.to_string uri in
    Format.eprintf "failed to open %s@." uri);
  `Null
;;

(* Dispatch the jump request to Merlin and get the result *)
let get_all_possible_jump_targets ~merlin ~position =
  Document.Merlin.with_pipeline_exn merlin (fun pipeline ->
    let position = Mpipeline.get_lexing_pos pipeline position in
    let typedtree = Mpipeline.typer_result pipeline |> Mtyper.get_typedtree in
    Merlin_analysis.Jump.get_all typedtree position)
;;

let code_actions
      (doc : Document.t)
      (params : CodeActionParams.t)
      (capabilities : ShowDocumentClientCapabilities.t option)
  =
  match Document.kind doc with
  | `Merlin merlin when available capabilities ->
    let position = Position.logical params.range.start in
    let+ res = get_all_possible_jump_targets ~merlin ~position in
    List.filter_map res ~f:(fun (target, lexing_pos) ->
      let open Option.O in
      let+ position = Position.of_lexical_position lexing_pos in
      let uri = Document.uri doc in
      let range = { Range.start = position; end_ = position } in
      let title = sprintf "%s jump" (String.capitalize_ascii (rename_target target)) in
      let command =
        let arguments = [ DocumentUri.yojson_of_t uri; Range.yojson_of_t range ] in
        Command.create ~title ~command:command_name ~arguments ()
      in
      CodeAction.create
        ~title
        ~kind:(CodeActionKind.Other (sprintf "merlin-jump-%s" (rename_target target)))
        ~command
        ())
  | _ -> Fiber.return []
;;

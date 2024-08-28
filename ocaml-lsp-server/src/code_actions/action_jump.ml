open Import
open Fiber.O
open Stdune

let command_name = "ocamllsp/merlin-jump-to-target"

let targets =
  [ "fun"; "match"; "let"; "module"; "module-type"; "match-next-case"; "match-prev-case" ]
;;

let available (capabilities : ShowDocumentClientCapabilities.t option) =
  match capabilities with
  | None | Some { support = false } -> false
  | Some { support = true } -> true
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
let process_jump_request ~merlin ~position ~target =
  let* results =
    Document.Merlin.with_pipeline_exn merlin (fun pipeline ->
      let pposition = Position.logical position in
      let query = Query_protocol.Jump (target, pposition) in
      Query_commands.dispatch pipeline query)
  in
  match results with
  | `Error _ -> Fiber.return None
  | `Found pos -> Fiber.return (Some pos)
;;

let code_actions
  (doc : Document.t)
  (params : CodeActionParams.t)
  (capabilities : ShowDocumentClientCapabilities.t option)
  =
  match Document.kind doc with
  | `Other -> Fiber.return []
  | `Merlin merlin ->
    (match available capabilities with
     | false -> Fiber.return []
     | true ->
       let* actions_fiber_list =
         Fiber.parallel_map targets ~f:(fun target ->
           let* res = process_jump_request ~merlin ~position:params.range.start ~target in
           match res with
           | None -> Fiber.return None
           | Some pos ->
             (match Position.of_lexical_position pos with
              | None -> Fiber.return None
              | Some position ->
                let uri = Document.uri doc in
                let range = { Range.start = position; end_ = position } in
                let title = sprintf "Jump to %s" target in
                let command =
                  let arguments =
                    [ DocumentUri.yojson_of_t uri; Range.yojson_of_t range ]
                  in
                  Command.create ~title ~command:command_name ~arguments ()
                in
                Fiber.return
                  (Some
                     (CodeAction.create
                        ~title
                        ~kind:(CodeActionKind.Other "merlin-jump")
                        ~command
                        ()))))
       in
       Fiber.return (List.filter_opt actions_fiber_list))
;;

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
          Merlin_analysis.Signature_help.application_signature node ~prefix ~cursor:pos)
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
           List.map
             application_signature.parameters
             ~f:(fun (p : Merlin_analysis.Signature_help.parameter_info) ->
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

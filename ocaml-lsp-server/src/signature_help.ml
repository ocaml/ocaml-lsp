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

let dispatch merlin position =
  Document.Merlin.with_pipeline_exn ~name:"signature-help" merlin (fun pipeline ->
    let params : Query_protocol.signature_help =
      { position
      ; trigger_kind = None
      ; active_signature_help = None
      ; is_retrigger = false
      }
    in
    let query = Query_protocol.Signature_help params in
    Query_commands.dispatch pipeline query)
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
  match Document.kind doc with
  | `Other -> Fiber.return (SignatureHelp.create ~signatures:[] ())
  | `Merlin merlin ->
    let position = Position.logical position in
    let* signature_help_result = dispatch merlin position in
    (match signature_help_result with
     | None -> Fiber.return (SignatureHelp.create ~signatures:[] ())
     | Some signature_help ->
       let+ doc =
         Document.Merlin.doc_comment ~name:"signature help-position" merlin position
       in
       let info =
         let parameters =
           List.map
             signature_help.parameters
             ~f:(fun (p : Query_protocol.signature_help_param) ->
               let label = `Offset (p.label_start, p.label_end) in
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
         SignatureInformation.create
           ~label:signature_help.label
           ?documentation
           ~parameters
           ()
       in
       SignatureHelp.create
         ~signatures:[ info ]
         ~activeSignature:signature_help.active_signature
         ?activeParameter:(Some (Some signature_help.active_param))
         ())
;;

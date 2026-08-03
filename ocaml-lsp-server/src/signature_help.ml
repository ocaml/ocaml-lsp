open Import
module Lexer_raw = Ocaml_preprocess.Lexer_raw
module Misc_utils = Merlin_analysis.Misc_utils
module Parser_raw = Ocaml_preprocess.Parser_raw
module Type_utils = Merlin_analysis.Type_utils

open struct
  open Ocaml_typing
  module Predef = Predef
  module Btype = Btype
end

(* Merlin may retain the nearest application after parsing has moved on to
   another expression. Lexing this slice avoids treating separators in comments
   or strings as boundaries. *)
let contains_application_boundary source ~from ~to_ =
  let source_length = String.length source in
  let from = max 0 (min from source_length) in
  let lexbuf =
    let source =
      let to_ = max 0 (min to_ source_length) in
      String.prefix source to_
    in
    Lexing.from_string source
  in
  let state = Lexer_raw.make (Lexer_raw.keywords []) in
  let rec loop ~last_was_boundary = function
    | Lexer_raw.Fail _ -> false
    | Return Parser_raw.EOF -> last_was_boundary
    | Refill k -> loop ~last_was_boundary (k ())
    | Return token ->
      let is_boundary =
        match token with
        | Parser_raw.ELSE | MINUSGREATER | SEMI | SEMISEMI -> true
        | _ -> false
      in
      if is_boundary && Lexing.lexeme_start lexbuf >= from
      then true
      else
        loop
          ~last_was_boundary:is_boundary
          (Lexer_raw.token_without_comments state lexbuf)
  in
  loop ~last_was_boundary:false (Lexer_raw.token_without_comments state lexbuf)
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
          match
            Merlin_analysis.Signature_help.application_signature node ~prefix ~cursor:pos
          with
          | None -> None
          | Some signature ->
            let function_position =
              Mpipeline.get_lexing_pos pipeline signature.function_position
            in
            let application_end, has_unassigned_parameter =
              List.fold_left
                signature.parameters
                ~init:(function_position.pos_cnum, false)
                ~f:(fun (application_end, has_unassigned_parameter) parameter ->
                  match parameter.argument with
                  | Omitted _ -> application_end, true
                  | Arg argument ->
                    ( max application_end argument.exp_loc.loc_end.pos_cnum
                    , has_unassigned_parameter || argument.exp_loc.loc_ghost ))
            in
            let source = Msource.text (Mpipeline.input_source pipeline) in
            (* Error recovery can make Merlin select an application after the
               cursor. A completed application can also remain selected after
               inserting whitespace, despite having no parameter left to edit. *)
            if
              pos.pos_cnum < function_position.pos_cnum
              || (Option.is_none signature.active_param && not has_unassigned_parameter)
              || contains_application_boundary
                   source
                   ~from:application_end
                   ~to_:pos.pos_cnum
            then None
            else Some signature)
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
       let supports_parameter_label_offsets =
         Capabilities.signature_label_offset_support (State.client_capabilities state)
       in
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
               let label =
                 if supports_parameter_label_offsets
                 then `Offset (offset + p.param_start, offset + p.param_end)
                 else
                   `String
                     (String.sub
                        application_signature.signature
                        ~pos:p.param_start
                        ~len:(p.param_end - p.param_start))
               in
               ParameterInformation.create ~label ())
         in
         let documentation =
           let open Option.O in
           let+ doc in
           let markdown =
             Capabilities.supports_markdown
               (Capabilities.signature_documentation_format
                  (State.client_capabilities state))
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

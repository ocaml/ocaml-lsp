let protocol_version = "1"

(* The server first emits [(version VERSION)]. Requests are
   [(indent PATH LINE SOURCE (SYNTAX ...))], and responses are either
   [(ok INDENTATION)] or [(error MESSAGE)]. Messages use canonical S-expressions. *)

let configuration path =
  let directory = Filename.dirname path in
  (* Plugins from [load=] execute arbitrary code and are intentionally unsupported. *)
  let config, syntaxes, (_ : [ `Mod of string | `Pkg of string ] list) =
    IndentConfig.local_default ~path:directory ()
  in
  config, syntaxes
;;

let configure_syntax syntaxes =
  Approx_lexer.disable_extensions ();
  List.iter
    (fun syntax ->
       try Approx_lexer.enable_extension syntax with
       | IndentExtend.Syntax_not_found name ->
         Format.eprintf "ocp-indent-rpc: unknown syntax extension %S@." name)
    syntaxes
;;

let indentation ~path ~line ~source ~syntaxes =
  if line < 1 then invalid_arg "line numbers start at 1";
  let config, configured_syntaxes = configuration path in
  configure_syntax (configured_syntaxes @ syntaxes);
  let output =
    { IndentPrinter.debug = false
    ; config
    ; in_lines = (fun current -> current = line)
    ; adaptive = true
    ; indent_empty = true
    ; kind = Numeric (fun indent result -> indent :: result)
    }
  in
  match IndentPrinter.proceed output (Nstream.of_string source) IndentBlock.empty [] with
  | [ indent ] -> indent
  | [] -> invalid_arg "line is outside the document"
  | _ :: _ :: _ -> failwith "ocp-indent returned more than one indentation"
;;

let handle = function
  | Csexp.List [ Atom "indent"; Atom path; Atom line; Atom source; List syntaxes ] ->
    let syntaxes =
      List.map
        (function
          | Csexp.Atom syntax -> syntax
          | List _ -> invalid_arg "syntax extension must be an atom")
        syntaxes
    in
    let indent = indentation ~path ~line:(int_of_string line) ~source ~syntaxes in
    Csexp.List [ Atom "ok"; Atom (string_of_int indent) ]
  | _ -> invalid_arg "invalid request"
;;

let send sexp =
  Csexp.to_channel stdout sexp;
  flush stdout
;;

let rec loop () =
  match Csexp.input_opt stdin with
  | Ok None -> ()
  | Error message -> send (List [ Atom "error"; Atom ("invalid csexp: " ^ message) ])
  | Ok (Some request) ->
    let response =
      try handle request with
      | exn -> List [ Atom "error"; Atom (Printexc.to_string exn) ]
    in
    send response;
    loop ()
;;

let () =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  send (List [ Atom "version"; Atom protocol_version ]);
  loop ()
;;

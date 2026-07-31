module Sexp = struct
  type t =
    | Atom of string
    | List of t list
end

module Csexp = Csexp.Make (Sexp)
open Sexp

let protocol_version = "1"

(* The server first emits [(version VERSION)]. Requests are
   [(indent PATH LINE SOURCE (SYNTAX ...))], and responses are either
   [(ok INDENTATION)] or [(error MESSAGE)]. Messages use canonical S-expressions. *)

let send sexp =
  Csexp.to_channel stdout sexp;
  flush stdout
;;

let loaded_modules = Hashtbl.create 8

let load_dynlink entries =
  let fresh =
    List.filter
      (function
        | `Pkg _ -> true
        | `Mod path -> not (Hashtbl.mem loaded_modules path))
      entries
  in
  IndentLoader.load fresh;
  List.iter
    (function
      | `Pkg _ -> ()
      | `Mod path -> Hashtbl.replace loaded_modules path ())
    fresh
;;

type configuration =
  { config : IndentConfig.t
  ; syntaxes : string list
  }

(* Configuration discovery touches the filesystem, so do it only on the first
   request from a directory and share configurations that resolve to the same file. *)
let configurations_by_file : (string option, configuration) Hashtbl.t = Hashtbl.create 8
let configurations_by_directory : (string, configuration) Hashtbl.t = Hashtbl.create 8

let configuration path =
  let directory = Filename.dirname path in
  match Hashtbl.find_opt configurations_by_directory directory with
  | Some configuration -> configuration
  | None ->
    let config_file = IndentConfig.find_conf_file directory in
    let configuration =
      match Hashtbl.find_opt configurations_by_file config_file with
      | Some configuration -> configuration
      | None ->
        let config, syntaxes, dynlink = IndentConfig.local_default ~path:directory () in
        load_dynlink dynlink;
        let configuration = { config; syntaxes } in
        Hashtbl.add configurations_by_file config_file configuration;
        configuration
    in
    Hashtbl.add configurations_by_directory directory configuration;
    configuration
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
  let { config; syntaxes = configured_syntaxes } = configuration path in
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
  | List [ Atom "indent"; Atom path; Atom line; Atom source; List syntaxes ] ->
    let syntaxes =
      List.map
        (function
          | Atom syntax -> syntax
          | List _ -> invalid_arg "syntax extension must be an atom")
        syntaxes
    in
    let indent = indentation ~path ~line:(int_of_string line) ~source ~syntaxes in
    List [ Atom "ok"; Atom (string_of_int indent) ]
  | _ -> invalid_arg "invalid request"
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

open Import

let capability = ("handleSwitchImplIntf", `Bool true)

let meth = "ocamllsp/switchImplIntf"

(** See the spec for 'ocamllsp/switchImplIntf' *)
let switch (state : State.t) (param : DocumentUri.t) :
    (Json.t, Jsonrpc.Response.Error.t) result =
  let file_uri = Uri.t_of_yojson (`String param) in
  let filepath = Uri.to_path file_uri in
  let ml, mli, re, rei, mll, mly = ("ml", "mli", "re", "rei", "mll", "mly") in
  let open Result.O in
  let+ doc = Document_store.get state.store file_uri in
  let extensions_to_switch_to =
    match Document.syntax doc with
    | Ocaml -> (
      match Document.kind doc with
      | Intf -> [ ml; mly; mll; re ]
      | Impl -> [ mli; mly; mll; rei ] )
    | Reason -> (
      match Document.kind doc with
      | Intf -> [ re; ml ]
      | Impl -> [ rei; mli ] )
    | Ocamllex -> [ mli; rei ]
    | Menhir -> [ mli; rei ]
  in
  let path_without_extension = Filename.remove_extension filepath ^ "." in
  let find_switch (exts : string list) =
    List.filter_map exts ~f:(fun ext ->
        let file_to_switch_to = path_without_extension ^ ext in
        Option.some_if (Sys.file_exists file_to_switch_to) file_to_switch_to)
  in
  let to_switch_to =
    match find_switch extensions_to_switch_to with
    | [] ->
      let main_switch_to_candidate_ext = List.hd extensions_to_switch_to in
      let main_switch_to_candidate_path =
        path_without_extension ^ main_switch_to_candidate_ext
      in
      [ main_switch_to_candidate_path ]
    | to_switch_to -> to_switch_to
  in
  let to_switch_to_json_array =
    List.map to_switch_to ~f:(fun s -> `String (Uri.to_string @@ Uri.of_path s))
  in
  `List to_switch_to_json_array

let on_request ~(params : Json.t option) state =
  Fiber.return
    ( match params with
    | Some (`String (file_uri : DocumentUri.t)) ->
      let open Result.O in
      let+ res = switch state file_uri in
      (res, state)
    | Some _
    | None ->
      Error
        (Jsonrpc.Response.Error.make ~code:InvalidRequest
           ~message:"ocamllsp/switchImplIntf must receive param : DocumentUri.t"
           ()) )

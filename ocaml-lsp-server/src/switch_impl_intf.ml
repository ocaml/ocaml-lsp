open Import

let capability = ("handleSwitchImplIntf", `Bool true)

let meth = "ocamllsp/switchImplIntf"

let scheme_path_of_uri s =
  let uri_regex_1 =
    Str.regexp "\\([a-z]+\\)://\\(.+\\)"
    (* handles URI: scheme://path as in file:///path/to/test.ml or
       file:///c:/test.ml *)
  in
  let uri_regex_2 =
    Str.regexp "\\([a-z]+\\):\\(.+\\)"
    (* handles URI: scheme:path as in untitled:/path/to/test.ml or
       untitled:/c:/test.ml *)
  in
  let scheme_path_of_uri () = (Str.matched_group 1 s, Str.matched_group 2 s) in
  if Str.string_match uri_regex_1 s 0 then
    Some (scheme_path_of_uri ())
  else if Str.string_match uri_regex_2 s 0 then
    Some (scheme_path_of_uri ())
  else
    None

(** see the spec for [ocamllsp/switchImplIntf] *)
let switch (param : DocumentUri.t) : (Json.t, Jsonrpc.Response.Error.t) result =
  let incorrect_uri =
    Jsonrpc.Response.Error.make ~code:Jsonrpc.Response.Error.Code.InvalidParams
      ~message:"provided file URI (param) doesn't follow URI spec" ()
  in
  let open Result.O in
  let+ fpath =
    match scheme_path_of_uri param with
    | Some ("file", _path) -> Ok (Uri.t_of_yojson (`String param) |> Uri.to_path)
    | Some (_scheme, path) -> Ok path
    | None -> Error incorrect_uri
  in
  let fname = Filename.basename fpath in
  let ml, mli, re, rei, mll, mly = ("ml", "mli", "re", "rei", "mll", "mly") in
  let exts_to_switch_to =
    match Document.Syntax.of_fname fname with
    | Ocaml -> (
      match Document.Kind.of_fname fname with
      | Intf -> [ ml; mly; mll; re ]
      | Impl -> [ mli; mly; mll; rei ] )
    | Reason -> (
      match Document.Kind.of_fname fname with
      | Intf -> [ re; ml ]
      | Impl -> [ rei; mli ] )
    | Ocamllex -> [ mli; rei ]
    | Menhir -> [ mli; rei ]
  in
  let fpath_w_ext ext = Filename.remove_extension fpath ^ "." ^ ext in
  let find_switch exts =
    List.filter_map exts ~f:(fun ext ->
        let file_to_switch_to = fpath_w_ext ext in
        Option.some_if (Sys.file_exists file_to_switch_to) file_to_switch_to)
  in
  let files_to_switch_to =
    match find_switch exts_to_switch_to with
    | [] ->
      let switch_to_ext = List.hd exts_to_switch_to in
      let switch_to_fpath = fpath_w_ext switch_to_ext in
      [ switch_to_fpath ]
    | to_switch_to -> to_switch_to
  in
  Json.yojson_of_list
    (fun fpath -> Uri.of_path fpath |> Uri.to_string |> fun s -> `String s)
    files_to_switch_to

let on_request ~(params : Json.t option) state =
  Fiber.return
    ( match params with
    | Some (`String (file_uri : DocumentUri.t)) ->
      let open Result.O in
      let+ res = switch file_uri in
      (res, state)
    | Some _
    | None ->
      Error
        (Jsonrpc.Response.Error.make ~code:InvalidRequest
           ~message:"ocamllsp/switchImplIntf must receive param : DocumentUri.t"
           ()) )

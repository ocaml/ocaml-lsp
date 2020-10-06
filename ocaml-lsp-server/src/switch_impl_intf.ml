open Import

let capability = ("handleSwitchImplIntf", `Bool true)

let meth = "ocamllsp/switchImplIntf"

(** see the spec for [ocamllsp/switchImplIntf] *)
let switch (param : DocumentUri.t) : (Json.t, Jsonrpc.Response.Error.t) result =
  let fpath =
    match String.split ~on:':' param with
    | [ scheme; path ] ->
      if scheme = "file" then
        Uri.t_of_yojson (`String param) |> Uri.to_path
      else
        path
    | _ -> failwith "provided file URI (param) doesn't follow URI spec"
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
  Ok
    (Json.yojson_of_list
       (fun fpath -> Uri.of_path fpath |> Uri.to_string |> fun s -> `String s)
       files_to_switch_to)

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

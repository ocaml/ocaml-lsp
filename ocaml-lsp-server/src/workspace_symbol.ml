open Import

let cmt_sign (cmt_infos : Cmt_format.cmt_infos) =
  match cmt_infos.cmt_annots with
  | Implementation { Typedtree.str_type = sign; _ }
  | Interface { Typedtree.sig_type = sign; _ }
  | Packed (sign, _) ->
    sign
  | _ -> []

let loc_of_sig_item (signature_item : Types.signature_item) =
  match signature_item with
  | Sig_value (_, descr, _) -> descr.val_loc
  | Sig_type (_, descr, _, _) -> descr.type_loc
  | Sig_typext (_, descr, _, _) -> descr.ext_loc
  | Sig_module (_, _, descr, _, _) -> descr.md_loc
  | Sig_modtype (_, descr, _) -> descr.mtd_loc
  | Sig_class (_, descr, _, _) -> descr.cty_loc
  | Sig_class_type (_, descr, _, _) -> descr.clty_loc

let id_of_sig_item (signature_item : Types.signature_item) =
  match signature_item with
  | Sig_value (id, _, _)
  | Sig_type (id, _, _, _)
  | Sig_typext (id, _, _, _)
  | Sig_module (id, _, _, _, _)
  | Sig_modtype (id, _, _)
  | Sig_class (id, _, _, _)
  | Sig_class_type (id, _, _, _) ->
    id

let kind_of_sig_item (signature_item : Types.signature_item) : SymbolKind.t =
  match signature_item with
  | Sig_value _ -> Function
  | Sig_type _ -> String
  (* | Types.Sig_typext (_, _, Types.Text_exception, _) -> Constructor *)
  | Sig_typext _ -> Constructor
  | Sig_module _ -> Module
  | Sig_modtype _ -> Module
  | Sig_class _ -> Class
  | Sig_class_type _ -> Class

let symbol_of_signature_item ~sourcepath (signature_item : Types.signature_item)
    =
  let name =
    let id = id_of_sig_item signature_item in
    Ident.name id
  in
  let kind = kind_of_sig_item signature_item in
  let location =
    let range =
      let loc = loc_of_sig_item signature_item in
      Range.of_loc loc
    in
    let uri = sourcepath |> Uri.of_path in
    Location.create ~uri ~range
  in
  SymbolInformation.create ~name ~kind ~location ()

let is_directory dir =
  try Sys.is_directory dir with
  | Sys_error _ -> false

type error = Build_dir_not_found of string

let find_build_dir ({ name; uri } : WorkspaceFolder.t) =
  let build_dir = Filename.concat (Uri.to_path uri) "_build/default" in
  if is_directory build_dir then
    Ok build_dir
  else
    Error (Build_dir_not_found name)

type cm_file =
  | Cmt of string
  | Cmti of string

let string_of_cm cm =
  match cm with
  | Cmt f
  | Cmti f ->
    f

let symbols_from_cm_file root_uri cm_file =
  let cmt =
    let filename = string_of_cm cm_file in
    Cmt_format.read_cmt filename
  in
  match cmt.cmt_sourcefile with
  | None -> []
  | Some sourcefile -> (
    match Filename.extension sourcefile with
    | ".ml"
    | ".mli" ->
      let sourcepath = Filename.concat root_uri sourcefile in
      let signatures = cmt_sign cmt in
      let filename_from_path path = Filename.basename path in
      List.filter_map signatures ~f:(fun signature_item ->
          let pos_fname =
            let loc = loc_of_sig_item signature_item in
            filename_from_path loc.loc_start.pos_fname
          in
          let filename = filename_from_path sourcefile in
          (* Remove includes as the loc points to the original file *)
          if pos_fname = filename then
            Some (symbol_of_signature_item ~sourcepath signature_item)
          else
            None)
    | _ -> [])

let find_cm_files dir =
  let choose_file f1 f2 =
    match (f1, f2) with
    | (Cmt _ as f), _
    | _, (Cmt _ as f) ->
      f
    | (Cmti _ as f), Cmti _ -> f
  in
  (* TODO we could get into a symlink loop here so we should we be careful *)
  let rec loop acc dir =
    let contents = Sys.readdir dir in
    Array.fold_left contents ~init:acc ~f:(fun acc fname ->
        let path = Filename.concat dir fname in
        if is_directory path then
          loop acc path
        else
          match String.rsplit2 ~on:'.' path with
          | Some (path_without_ext, "cmt") ->
            String.Map.set acc path_without_ext (Cmt path)
          | Some (path_without_ext, "cmti") -> (
            let current_file = String.Map.find acc path_without_ext in
            let cmi_file = Cmti path in
            match current_file with
            | None -> String.Map.set acc path_without_ext cmi_file
            | Some current_file ->
              String.Map.set acc path_without_ext
                (choose_file current_file cmi_file))
          | _ -> acc)
  in
  loop String.Map.empty dir |> String.Map.values

let run ({ query; _ } : WorkspaceSymbolParams.t)
    (workspace_folders : WorkspaceFolder.t list) =
  let filter =
    match query with
    | "" -> fun x -> x
    | query ->
      let re = Re.str query |> Re.compile in
      List.filter ~f:(fun (symbol : SymbolInformation.t) ->
          Re.execp re symbol.name)
  in
  List.map workspace_folders ~f:(fun (workspace_folder : WorkspaceFolder.t) ->
      let open Result.O in
      let* build_dir = find_build_dir workspace_folder in
      let all_symbols =
        let cm_files = find_cm_files build_dir in
        let path =
          let uri = workspace_folder.uri in
          Uri.to_path uri
        in
        List.concat_map ~f:(symbols_from_cm_file path) cm_files
      in
      Ok (filter all_symbols))

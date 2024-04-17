open Import

let capability = ("handleDuneContexts", `Bool true)

let meth = "ocamllsp/duneContexts"

let on_request () =
  match Bin.which "dune" with
  | None ->
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make
         ~code:InternalError
         ~message:"dune binary not found"
         ())
  | Some dune -> (
    let describe =
      "DUNE_CONFIG__GLOBAL_LOCK=disabled " ^ Fpath.to_string dune
      ^ " describe contexts"
    in
    let temp_file = Filename.temp_file "req_dune_contexts" ".txt" in
    let command = Printf.sprintf "%s > %s" describe temp_file in
    let error ~data =
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make
           ~code:InternalError
           ~message:"Execution of `dune describe contexts` failed"
           ~data
           ())
    in

    try
      let exit_status = Sys.command command in
      if exit_status = 0 then (
        let ic = open_in temp_file in
        let rec read_lines acc =
          try
            let line = input_line ic in
            read_lines (line :: acc)
          with End_of_file -> List.rev acc
        in
        let lines = read_lines [] in
        close_in ic;
        Sys.remove temp_file;
        Json.yojson_of_list (fun line -> `String line) lines)
      else error ~data:(`Assoc [ ("exitStatus", `Int exit_status) ])
    with
    | Sys_error msg -> error ~data:(`Assoc [ ("systemError", `String msg) ])
    | Failure msg -> error ~data:(`Assoc [ ("Failure", `String msg) ])
    | _ -> error ~data:(`String "Unknown error"))

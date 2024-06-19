open Import

let capability = ("handleMerlinCallCompatible", `Bool true)

let meth = "ocamllsp/merlinCallCompatible"

module Request_params = struct
  type t =
    { uri : Uri.t
    ; result_as_sexp : bool
    ; command : string
    ; args : string list
    }

  let expected =
    `Assoc
      [ ("uri", `String "<DocumentUri>")
      ; ("resultAsSexp?", `String "<true | false>")
      ; ("command", `String "<MerlinCommand>")
      ; ("args?", `String "<string | bool | float | int | intLit>[] | Object")
      ]

  let as_sexp_of_yojson params =
    match List.assoc_opt "resultAsSexp" params with
    | Some (`Bool value) -> value
    | _ ->
      (* If the parameter is incorrectly formatted or missing, it is assumed that
         the result is not requested in the form of Sexp *)
      false

  let command_of_yojson params =
    match List.assoc_opt "command" params with
    | Some (`String command_name) -> Some command_name
    | _ ->
      (* If the parameter is incorrectly formatted or missing, we refuse to build
         the parameter, [command] is mandatory. *)
      None

  let uri_of_yojson params =
    match List.assoc_opt "uri" params with
    | Some uri -> Some (Uri.t_of_yojson uri)
    | _ ->
      (* If the parameter is incorrectly formatted or missing, we refuse to build
         the parameter, [uri] is mandatory. *)
      None

  let stringish_of_yojson =
    (* The function is relatively optimistic and attempts to treat literal data as
       strings of characters. *)
    function
    | `String s -> Some s
    | `Bool b -> Some (string_of_bool b)
    | `Float f -> Some (string_of_float f)
    | `Int i -> Some (string_of_int i)
    | `Intlit i -> Some i
    | _ -> None

  let args_of_yojson_list args =
    let open Option.O in
    let+ args =
      List.fold_left
        ~f:(fun acc x ->
          let* acc in
          let+ x = stringish_of_yojson x in
          x :: acc)
        ~init:(Some [])
        args
    in
    List.rev args

  let args_of_yojson_assoc args =
    let open Option.O in
    let+ args =
      List.fold_left
        ~f:(fun acc (key, value) ->
          let key = "-" ^ key in
          let* acc in
          let+ x = stringish_of_yojson value in
          x :: key :: acc)
        ~init:(Some [])
        args
    in
    List.rev args

  let args_of_yojson params =
    match List.assoc_opt "args" params with
    | Some (`List args) -> args_of_yojson_list args
    | Some (`Assoc args) -> args_of_yojson_assoc args
    | _ ->
      (* If args is not a list or is absent, it should fail. *)
      None

  let t_of_yojson = function
    | `Assoc params ->
      let result_as_sexp = as_sexp_of_yojson params in
      let open Option.O in
      let* command = command_of_yojson params in
      let* args = args_of_yojson params in
      let* uri = uri_of_yojson params in
      Some { result_as_sexp; command; args; uri }
    | _ -> None
end

let raise_invalid_params ?data ~message () =
  let open Jsonrpc.Response.Error in
  raise @@ make ?data ~code:Code.InvalidParams ~message ()

let from_structured_json_exn = function
  | None -> raise_invalid_params ~message:"Expected params but received none" ()
  | Some params -> (
    match Request_params.t_of_yojson params with
    | Some params -> params
    | None ->
      let data =
        `Assoc
          [ ("expectedParams", Request_params.expected)
          ; ("receivedParams", (params :> Json.t))
          ]
      in
      raise_invalid_params ~data ~message:"Unexpected params format" ())

let with_pipeline state uri specs raw_args cmd_args f =
  let doc = Document_store.get state.State.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return `Null
  | `Merlin merlin ->
    let open Fiber.O in
    let* config = Document.Merlin.mconfig merlin in
    let specs = List.map ~f:snd specs in
    let config, args =
      Mconfig.parse_arguments
        ~wd:(Sys.getcwd ())
        ~warning:ignore
        specs
        raw_args
        config
        cmd_args
    in
    Document.Merlin.with_configurable_pipeline_exn ~config merlin (f args)

let perform_query action params pipeline =
  let action () = action pipeline params in
  let class_, output =
    match action () with
    | result -> ("return", result)
    | exception Failure message -> ("failure", `String message)
    | exception exn ->
      let message = Printexc.to_string exn in
      ("exception", `String message)
  in
  `Assoc [ ("class", `String class_); ("value", output) ]

let on_request ~params state =
  Fiber.of_thunk (fun () ->
      let Request_params.{ result_as_sexp; command; args; uri } =
        from_structured_json_exn params
      in
      match
        Merlin_commands.New_commands.(find_command command all_commands)
      with
      | Merlin_commands.New_commands.Command (_name, _doc, specs, params, action)
        ->
        let open Fiber.O in
        let+ json =
          with_pipeline state uri specs args params @@ perform_query action
        in
        let result =
          if result_as_sexp then
            Merlin_utils.(json |> Sexp.of_json |> Sexp.to_string)
          else json |> Yojson.Basic.to_string
        in
        `Assoc
          [ ("resultAsSexp", `Bool result_as_sexp); ("result", `String result) ]
      | exception Not_found ->
        let data = `Assoc [ ("command", `String command) ] in
        raise_invalid_params ~data ~message:"Unexpected command name" ())

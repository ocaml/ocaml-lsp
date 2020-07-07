open Import
open Json.Conv

module Id = struct
  type t = (string, int) Either.t

  let yojson_of_t = function
    | Either.Left s -> `String s
    | Right i -> `Int i

  let t_of_yojson = function
    | `String s -> Either.Left s
    | `Int i -> Right i
    | json -> Json.error "Id.t" json

  let to_dyn x = Dyn.Encoder.opaque x

  let hash x = Hashtbl.hash x

  let equal = ( = )
end

module Constant = struct
  let jsonrpc = "jsonrpc"

  let jsonrpcv = "2.0"

  let id = "id"

  let method_ = "method"

  let params = "params"

  let result = "result"

  let error = "error"
end

module Notify = struct
  type t =
    | Stop
    | Continue
end

module Message = struct
  type 'id t =
    { id : 'id
    ; method_ : string
    ; params : Json.t option
    }

  let create ?params ~id ~method_ () = { id; method_; params }

  let yojson_of_t add_id { id; method_; params } =
    let json =
      [ (Constant.method_, `String method_)
      ; (Constant.jsonrpc, `String Constant.jsonrpcv)
      ]
    in
    let json =
      match params with
      | None -> json
      | Some params -> (Constant.params, params) :: json
    in
    let json =
      match add_id id with
      | None -> json
      | Some id -> (Constant.id, id) :: json
    in
    `Assoc json

  let either_of_yojson json =
    match json with
    | `Assoc fields ->
      let method_ =
        Json.field_exn fields Constant.method_ Json.Conv.string_of_yojson
      in
      let params = Json.field fields Constant.params (fun x -> x) in
      let id = Json.field fields Constant.id Id.t_of_yojson in
      let jsonrpc =
        Json.field_exn fields Constant.jsonrpc Json.Conv.string_of_yojson
      in
      if jsonrpc = Constant.jsonrpcv then
        { method_; params; id }
      else
        Json.error "invalid version" json
    | _ -> Json.error "invalid request" json

  let read_json_params f v =
    match f v with
    | r -> Ok r
    | exception Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (Failure msg, _)
      ->
      Error msg

  let require_params json =
    match json with
    | None -> Error "params are required"
    | Some params -> Ok params

  let params t f =
    let open Result.O in
    require_params t.params >>= read_json_params f

  let yojson_of_either t : Json.t = yojson_of_t (Option.map ~f:Id.yojson_of_t) t

  type request = Id.t t

  type notification = unit t

  type either = Id.t option t

  let yojson_of_notification = yojson_of_t (fun () -> None)

  let yojson_of_request (t : request) : Json.t =
    yojson_of_t (fun id -> Some (Id.yojson_of_t id)) t
end

module Response = struct
  module Error = struct
    module Code = struct
      type t =
        | ParseError
        | InvalidRequest
        | MethodNotFound
        | InvalidParams
        | InternalError
        | ServerErrorStart
        | ServerErrorEnd
        | ServerNotInitialized
        | UnknownErrorCode
        | RequestCancelled
        | ContentModified

      let of_int = function
        | -32700 -> Some ParseError
        | -32600 -> Some InvalidRequest
        | -32601 -> Some MethodNotFound
        | -32602 -> Some InvalidParams
        | -32603 -> Some InternalError
        | -32099 -> Some ServerErrorStart
        | -32000 -> Some ServerErrorEnd
        | -32002 -> Some ServerNotInitialized
        | -32001 -> Some UnknownErrorCode
        | -32800 -> Some RequestCancelled
        | -32801 -> Some ContentModified
        | _ -> None

      let to_int = function
        | ParseError -> -32700
        | InvalidRequest -> -32600
        | MethodNotFound -> -32601
        | InvalidParams -> -32602
        | InternalError -> -32603
        | ServerErrorStart -> -32099
        | ServerErrorEnd -> -32000
        | ServerNotInitialized -> -32002
        | UnknownErrorCode -> -32001
        | RequestCancelled -> -32800
        | ContentModified -> -32801

      let t_of_yojson json =
        match json with
        | `Int i -> (
          match of_int i with
          | None -> Json.error "unknown code" json
          | Some i -> i )
        | _ -> Json.error "invalid code" json

      let yojson_of_t t = `Int (to_int t)
    end

    type t =
      { code : Code.t
      ; message : string
      ; data : Json.t option [@yojson.option]
      }
    [@@deriving_inline yojson]

    let _ = fun (_ : t) -> ()

    let t_of_yojson =
      ( let _tp_loc = "lsp/src/jsonrpc.ml.Response.Error.t" in
        function
        | `Assoc field_yojsons as yojson -> (
          let code_field = ref None
          and message_field = ref None
          and data_field = ref None
          and duplicates = ref []
          and extra = ref [] in
          let rec iter = function
            | (field_name, _field_yojson) :: tail ->
              ( match field_name with
              | "code" -> (
                match Ppx_yojson_conv_lib.( ! ) code_field with
                | None ->
                  let fvalue = Code.t_of_yojson _field_yojson in
                  code_field := Some fvalue
                | Some _ ->
                  duplicates :=
                    field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
              | "message" -> (
                match Ppx_yojson_conv_lib.( ! ) message_field with
                | None ->
                  let fvalue = string_of_yojson _field_yojson in
                  message_field := Some fvalue
                | Some _ ->
                  duplicates :=
                    field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
              | "data" -> (
                match Ppx_yojson_conv_lib.( ! ) data_field with
                | None ->
                  let fvalue = Json.t_of_yojson _field_yojson in
                  data_field := Some fvalue
                | Some _ ->
                  duplicates :=
                    field_name :: Ppx_yojson_conv_lib.( ! ) duplicates )
              | _ ->
                if
                  Ppx_yojson_conv_lib.( ! )
                    Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
                then
                  extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
                else
                  () );
              iter tail
            | [] -> ()
          in
          iter field_yojsons;
          match Ppx_yojson_conv_lib.( ! ) duplicates with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
              _tp_loc
              (Ppx_yojson_conv_lib.( ! ) duplicates)
              yojson
          | [] -> (
            match Ppx_yojson_conv_lib.( ! ) extra with
            | _ :: _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
                (Ppx_yojson_conv_lib.( ! ) extra)
                yojson
            | [] -> (
              match
                ( Ppx_yojson_conv_lib.( ! ) code_field
                , Ppx_yojson_conv_lib.( ! ) message_field
                , Ppx_yojson_conv_lib.( ! ) data_field )
              with
              | Some code_value, Some message_value, data_value ->
                { code = code_value
                ; message = message_value
                ; data = data_value
                }
              | _ ->
                Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                  _tp_loc yojson
                  [ ( Ppx_yojson_conv_lib.poly_equal
                        (Ppx_yojson_conv_lib.( ! ) code_field)
                        None
                    , "code" )
                  ; ( Ppx_yojson_conv_lib.poly_equal
                        (Ppx_yojson_conv_lib.( ! ) message_field)
                        None
                    , "message" )
                  ] ) ) )
        | _ as yojson ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
            yojson
        : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

    let _ = t_of_yojson

    let yojson_of_t =
      ( function
        | { code = v_code; message = v_message; data = v_data } ->
          let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
          let bnds =
            match v_data with
            | None -> bnds
            | Some v ->
              let arg = Json.yojson_of_t v in
              let bnd = ("data", arg) in
              bnd :: bnds
          in
          let bnds =
            let arg = yojson_of_string v_message in
            ("message", arg) :: bnds
          in
          let bnds =
            let arg = Code.yojson_of_t v_code in
            ("code", arg) :: bnds
          in
          `Assoc bnds
        : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

    let _ = yojson_of_t

    [@@@end]

    exception E of t

    let raise t = raise (E t)

    let make ?data ~code ~message () = { data; code; message }

    let of_exn exn =
      let message = Printexc.to_string exn in
      make ~code:InternalError ~message ()
  end

  type t =
    { id : Id.t
    ; result : (Json.t, Error.t) Result.t
    }

  let yojson_of_t { id; result } =
    let result =
      match result with
      | Ok json -> (Constant.result, json)
      | Error e -> (Constant.error, Error.yojson_of_t e)
    in
    `Assoc
      [ (Constant.id, Id.yojson_of_t id)
      ; (Constant.jsonrpc, `String Constant.jsonrpcv)
      ; result
      ]

  let t_of_yojson json =
    match json with
    | `Assoc fields -> (
      let id = Json.field_exn fields Constant.id Id.t_of_yojson in
      let jsonrpc =
        Json.field_exn fields Constant.jsonrpc Json.Conv.string_of_yojson
      in
      if jsonrpc <> Constant.jsonrpcv then
        Json.error "Invalid response" json
      else
        match Json.field fields Constant.result (fun x -> x) with
        | Some res -> { id; result = Ok res }
        | None ->
          let result =
            Error (Json.field_exn fields Constant.error Error.t_of_yojson)
          in
          { id; result } )
    | _ -> Json.error "Jsonrpc.Result.t" json

  let make ~id ~result = { id; result }

  let ok id result = make ~id ~result:(Ok result)

  let error id error = make ~id ~result:(Error error)
end

type packet =
  | Message of Id.t option Message.t
  | Response of Response.t

let yojson_of_packet = function
  | Message r -> Message.yojson_of_either r
  | Response r -> Response.yojson_of_t r

module Session (Chan : sig
  type t

  val send : t -> packet -> unit Fiber.t

  val recv : t -> packet option Fiber.t

  val close : t -> unit Fiber.t
end) =
struct
  type 'state t =
    { chan : Chan.t
    ; on_request : ('state, Id.t) context -> (Response.t * 'state) Fiber.t
    ; on_notification : ('state, unit) context -> (Notify.t * 'state) Fiber.t
    ; pending : (Id.t, Response.t Fiber.Ivar.t) Table.t
    ; stop_requested : unit Fiber.Ivar.t
    ; stopped : unit Fiber.Ivar.t
    ; name : string
    ; mutable running : bool
    ; mutable tick : int
    ; mutable state : 'state
    }

  and ('a, 'id) context = 'a t * 'id Message.t

  module Context = struct
    type nonrec ('a, 'id) t = ('a, 'id) context

    let message = snd

    let session = fst

    let state t = (session t).state
  end

  let log t = Log.log ~section:t.name

  let response_of_result id = function
    | Ok (x, _) -> x
    | Error exns ->
      let data : Json.t =
        `List
          (List.map
             ~f:(fun e -> e |> Exn_with_backtrace.to_dyn |> Json.of_dyn)
             exns)
      in
      let error =
        Response.Error.make ~code:InternalError ~data
          ~message:"uncaught exception" ()
      in
      Response.error id error

  let on_request_fail ctx : (Response.t * _) Fiber.t =
    let req : Message.request = Context.message ctx in
    let state = Context.state ctx in
    let error =
      Response.Error.make ~code:InternalError ~message:"not implemented" ()
    in
    Fiber.return (Response.error req.id error, state)

  let state t = t.state

  let stop t =
    log t (fun () -> Log.msg "requesting shutdown" []);
    Fiber.Ivar.fill t.stop_requested ()

  let stopped t = Fiber.Ivar.read t.stopped

  let close t =
    let open Fiber.O in
    let* () = Chan.close t.chan in
    Fiber.Ivar.fill t.stopped ()

  let run t =
    let stop_requested = Fiber.Ivar.read t.stop_requested in
    let open Fiber.O in
    let on_notification (r : unit Message.t) =
      let+ res = Fiber.collect_errors (fun () -> t.on_notification (t, r)) in
      match res with
      | Ok (next, state) ->
        t.state <- state;
        next
      | Error errors ->
        Format.eprintf
          "Uncaught error when handling notification:@.%a@.Error:@.%s@." Json.pp
          (Message.yojson_of_notification r)
          (Dyn.to_string (Dyn.Encoder.list Exn_with_backtrace.to_dyn errors));
        Notify.Continue
    in
    let on_request (r : Id.t Message.t) =
      let* resp = Fiber.collect_errors (fun () -> t.on_request (t, r)) in
      let jsonrpc_resp = response_of_result r.id resp in
      log t (fun () ->
          Log.msg "sending response"
            [ ("response", Response.yojson_of_t jsonrpc_resp) ]);
      let+ () = Chan.send t.chan (Response jsonrpc_resp) in
      ( match resp with
      | Ok (_, state) -> t.state <- state
      | Error _ -> () );
      Notify.Continue
    in
    let rec loop () =
      t.tick <- t.tick + 1;
      log t (fun () -> Log.msg "new tick" [ ("tick", `Int t.tick) ]);
      let* res =
        Fiber.fork_and_race
          (fun () -> Chan.recv t.chan)
          (fun () -> stop_requested)
      in
      match res with
      | Either.Right () ->
        log t (fun () -> Log.msg "shutdown granted" []);
        Chan.close t.chan
      | Left None -> Fiber.Ivar.fill t.stop_requested ()
      | Left (Some packet) -> (
        let* next_step =
          match packet with
          | Message r -> on_message r
          | Response r ->
            let+ () = on_response r in
            Notify.Continue
        in
        match next_step with
        | Notify.Continue -> loop ()
        | Stop -> Fiber.return () )
    and on_message (r : _ Message.t) =
      log t (fun () ->
          let what =
            match r.id with
            | None -> "notification"
            | Some _ -> "request"
          in
          Log.msg ("received " ^ what) [ ("r", Message.yojson_of_either r) ]);
      match r.id with
      | None -> on_notification { r with id = () }
      | Some id -> on_request { r with id }
    and on_response r =
      let log (what : string) =
        log t (fun () ->
            Log.msg ("response " ^ what) [ ("r", Response.yojson_of_t r) ])
      in
      match Table.find t.pending r.id with
      | None ->
        log "dropped";
        Fiber.return ()
      | Some ivar ->
        log "acknowledged";
        Table.remove t.pending r.id;
        Fiber.Ivar.fill ivar r
    in
    t.running <- true;
    let* () = loop () in
    close t

  let on_notification_fail ctx =
    let state = Context.state ctx in
    Fiber.return (Notify.Continue, state)

  let create ?(on_request = on_request_fail)
      ?(on_notification = on_notification_fail) ~name chan state =
    { chan
    ; on_request
    ; on_notification
    ; pending = Table.create (module Id) 10
    ; stop_requested = Fiber.Ivar.create ()
    ; stopped = Fiber.Ivar.create ()
    ; name
    ; running = false
    ; tick = 0
    ; state
    }

  let notification t (req : Message.notification) =
    if not t.running then Code_error.raise "jsonrpc must be running" [];
    let req = { req with Message.id = None } in
    Chan.send t.chan (Message req)

  let request t (req : Message.request) =
    if not t.running then Code_error.raise "jsonrpc must be running" [];
    let open Fiber.O in
    let* () =
      let req = { req with Message.id = Some req.id } in
      Chan.send t.chan (Message req)
    in
    let ivar = Fiber.Ivar.create () in
    Table.add_exn t.pending req.id ivar;
    Fiber.Ivar.read ivar
end

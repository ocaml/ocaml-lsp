open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Infer_intf

let infer_intf ?(uri = Helpers.uri) client =
  Test.custom_request client Req.meth (`List [ DocumentUri.yojson_of_t uri ])
;;

let%expect_test "an interface document raises an internal error" =
  let uri = DocumentUri.of_path "test.mli" in
  let source = "val x : int" in
  let request client =
    let* result = Fiber.collect_errors (fun () -> infer_intf ~uri client) in
    match result with
    | Error
        [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ] ->
      let data = Option.value_exn error.data in
      let exn = Yojson.Safe.Util.(data |> member "exn" |> to_string) in
      Printf.printf
        "code: %s\nexception: %s\n"
        (Jsonrpc.Response.Error.Code.to_string error.code)
        exn;
      Fiber.return ()
    | Error errors -> Fiber.reraise_all errors
    | Ok response ->
      Test.print_result response;
      Fiber.return ()
  in
  Helpers.test ~uri ~language_id:"ocaml.interface" source request;
  [%expect
    {|
    code: InternalError
    exception: Failure("expected an implementation document, got an interface instead")
    |}]
;;

let%expect_test "can infer module interfaces" =
  let source =
    {ocaml|type t = Foo of int | Bar of bool

let f (x : t) = x
|ocaml}
  in
  let req client =
    let* response = infer_intf client in
    let () = Yojson.Safe.Util.to_string response |> print_string in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    type t = Foo of int | Bar of bool
    val f : t -> t |}]
;;

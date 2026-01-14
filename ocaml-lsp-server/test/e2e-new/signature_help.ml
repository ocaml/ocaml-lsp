open Test.Import

let print_signature_help (signature_help : SignatureHelp.t) =
  (* let e : SignatureInformation.t = List.hd signature_help.signatures in *)
  signature_help
  |> SignatureHelp.yojson_of_t
  |> Yojson.Safe.pretty_to_string ~std:false
  |> print_endline
;;

let signature_help client position =
  Client.request
    client
    (SignatureHelp
       { context = None
       ; position
       ; textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri
       ; workDoneToken = None
       })
;;

let%expect_test "signature help formats the signature" =
  let source =
    {ocaml|
let f ~(d : unit -> unit -> unit -> unit -> unit ->  unit -> unit -> unit -> unit -> unit -> unit  -> unit -> unit) ~t = ignore f
let _ : int = f 
|ocaml}
  in
  let position = Position.create ~line:2 ~character:16 in
  let req client =
    let* resp : SignatureHelp.t = signature_help client position in
    let () = print_signature_help resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "activeSignature": 0,
      "signatures": [
        {
          "label": "f : d:\n  (unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit) ->\nt:'a ->\nunit",
          "parameters": [ { "label": [ 4, 135 ] }, { "label": [ 138, 143 ] } ]
        }
      ]
    }
    |}]
;;

let%expect_test "signature help formats the signature" =
  let source =
    {ocaml|
let i () = 234 in
let f a b c d e ~f g h ~i = 1 + a + b + c + d + e + f + g + h + (i ()) in
let _ : int = f 3 5
|ocaml}
  in
  let position = Position.create ~line:3 ~character:17 in
  let req client =
    let* resp : SignatureHelp.t = signature_help client position in
    let () = print_signature_help resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "activeParameter": 0,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "f : int ->\nint ->\nint ->\nint ->\nint ->\nf:int ->\nint ->\nint ->\ni:(unit -> int) ->\nint",
          "parameters": [
            { "label": [ 4, 7 ] },
            { "label": [ 10, 14 ] },
            { "label": [ 17, 21 ] },
            { "label": [ 24, 28 ] },
            { "label": [ 31, 35 ] },
            { "label": [ 38, 44 ] },
            { "label": [ 47, 51 ] },
            { "label": [ 54, 58 ] },
            { "label": [ 61, 77 ] }
          ]
        }
      ]
    }
    |}]
;;

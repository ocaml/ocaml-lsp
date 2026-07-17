open Test.Import

let folding_range client =
  let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
  Client.request
    client
    (TextDocumentFoldingRange (FoldingRangeParams.create ~textDocument ()))
;;

let print_folding_ranges = Test.print_option_list ~none:"null" FoldingRange.yojson_of_t

let test source =
  let req client =
    let* response = folding_range client in
    print_folding_ranges response;
    Fiber.return ()
  in
  Helpers.test source req
;;

let%expect_test "returns folding ranges for `let`" =
  let source =
    {folding_range|let a =
  let b = 1
  in
  let c =
    "foo"
  in
  ()|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 4,
        "endLine": 6,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 9,
        "endLine": 4,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 3
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for open expressions" =
  let source =
    {folding_range|open struct
  let u =
    ()
end|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 3,
        "endLine": 3,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 3,
        "endLine": 3,
        "kind": "region",
        "startCharacter": 5,
        "startLine": 0
      },
      {
        "endCharacter": 6,
        "endLine": 2,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 1
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for Pexp_apply expressions" =
  let source =
    {folding_range|Stdlib.print_endline "one
two
three"|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 6,
        "endLine": 2,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for Pexp_letop" =
  let source =
    {folding_range|let () =
  let+ outline =
    Stdlib.print_endline "one";
    Stdlib.print_endline "two";
in
let symbol_info_of_outline_item =
  Stdlib.print_endline "one";
  Stdlib.print_endline "two";|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 29,
        "endLine": 7,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 29,
        "endLine": 7,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 1
      },
      {
        "endCharacter": 29,
        "endLine": 7,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 5
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for Pexp_newtype" =
  let source =
    {folding_range|let magic_of_kind : type a . a ast_kind -> string =
  let () =
    Stdlib.print_endline "one";
    Stdlib.print_endline "two"
  in ()|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 7,
        "endLine": 4,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 30,
        "endLine": 3,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 1
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for type_extension" =
  let source =
    {folding_range|type t +=
  | A
  | B

module type Type = sig
  type t +=
    | A
    | B
end|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 5,
        "endLine": 2,
        "kind": "region",
        "startCharacter": 5,
        "startLine": 0
      },
      {
        "endCharacter": 3,
        "endLine": 8,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 4
      },
      {
        "endCharacter": 3,
        "endLine": 8,
        "kind": "region",
        "startCharacter": 19,
        "startLine": 4
      },
      {
        "endCharacter": 7,
        "endLine": 7,
        "kind": "region",
        "startCharacter": 7,
        "startLine": 5
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for match expressions" =
  let source =
    {folding_range|match
  Some
    "large expr"
with
| None ->
  ()
| Some _ ->
  print_endline "foo";
  print_endline "bar"|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 21,
        "endLine": 8,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 4,
        "endLine": 5,
        "kind": "region",
        "startCharacter": 6,
        "startLine": 4
      },
      {
        "endCharacter": 21,
        "endLine": 8,
        "kind": "region",
        "startCharacter": 8,
        "startLine": 6
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for records" =
  let source =
    {folding_range|type r = {
  a: string;
  b: int
}

let f
      { a;
        b } =
  { a = a;
    b = b + 1 }|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 1,
        "endLine": 3,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 15,
        "endLine": 9,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 5
      },
      {
        "endCharacter": 11,
        "endLine": 7,
        "kind": "region",
        "startCharacter": 6,
        "startLine": 6
      },
      {
        "endCharacter": 15,
        "endLine": 9,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 8
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for classes" =
  let source =
    {folding_range|class foobar =
  let a =
    let () = Stdlib.print_endline "" in
    let () = Stdlib.print_endline "" in
    let () = Stdlib.print_endline "" in
    ()
  in
  object
    method add x y =
      let z =
        let a = 5 in
        let b = 6 in
        let () =
          let () = Stdlib.print_endline "" in
          let () = Stdlib.print_endline "" in
          let () = Stdlib.print_endline "" in
          ()
        in
        a + b
      in
      x + y + z
  end|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 5,
        "endLine": 21,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 5,
        "endLine": 21,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 1
      },
      {
        "endCharacter": 6,
        "endLine": 5,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 1
      },
      {
        "endCharacter": 5,
        "endLine": 21,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 7
      },
      {
        "endCharacter": 15,
        "endLine": 20,
        "kind": "region",
        "startCharacter": 4,
        "startLine": 8
      },
      {
        "endCharacter": 13,
        "endLine": 18,
        "kind": "region",
        "startCharacter": 6,
        "startLine": 9
      },
      {
        "endCharacter": 12,
        "endLine": 16,
        "kind": "region",
        "startCharacter": 8,
        "startLine": 12
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges Pexp_while" =
  let source =
    {folding_range|while true do
  Stdlib.print_endline "one";
  Stdlib.print_endline "two";
done|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 4,
        "endLine": 3,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges Pexp_for" =
  let source =
    {folding_range|for j = 0 to Array.length index - 1 do
  Stdlib.print_endline "one";
  Stdlib.print_endline "two";
done;|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 4,
        "endLine": 3,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges Pexp_object" =
  let source =
    {folding_range|object
  method print =
    Stdlib.print_enline "one";
    Stdlib.print_enline "two";
end|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 3,
        "endLine": 4,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 30,
        "endLine": 3,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 1
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges Pexp_pack" =
  let source =
    {folding_range|(module Set.Make (struct
  type t = s
  let compare = cmp
  let print =
    Stdlib.print_endline "one";
    Stdlib.print_endline "two"
end))|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 5,
        "endLine": 6,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 3,
        "endLine": 6,
        "kind": "region",
        "startCharacter": 18,
        "startLine": 0
      },
      {
        "endCharacter": 30,
        "endLine": 5,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 3
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges Pexp_letmodule" =
  let source =
    {folding_range|let module W = Set.Make (struct
  type t = s

  let compare = cmp

  let print =
    Stdlib.print_endline "one";
    Stdlib.print_endline "two"
end)|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 4,
        "endLine": 8,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 4,
        "endLine": 8,
        "kind": "region",
        "startCharacter": 4,
        "startLine": 0
      },
      {
        "endCharacter": 3,
        "endLine": 8,
        "kind": "region",
        "startCharacter": 25,
        "startLine": 0
      },
      {
        "endCharacter": 30,
        "endLine": 7,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 5
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for value_description" =
  let source =
    {folding_range|module Type : sig
  val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
    ?params:(core_type * (variance * injectivity)) list ->
    ?cstrs:(core_type * core_type * loc) list ->
    ?kind:type_kind -> ?priv:private_flag -> ?manifest:core_type -> str ->
    type_declaration
end|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 3,
        "endLine": 6,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 3,
        "endLine": 6,
        "kind": "region",
        "startCharacter": 14,
        "startLine": 0
      },
      {
        "endCharacter": 20,
        "endLine": 5,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 1
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for Pstr_extension" =
  let source =
    {folding_range|[%%expect{|
  module type Module =
    sig
      module N : sig type t end
      type t
    end
  |}]|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 5,
        "endLine": 6,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      }
    ]
    |}]
;;

let%expect_test "traverses Pexp_lazy nodes" =
  let source =
    {folding_range|let res =
  lazy
    (let () =
       Stdlib.print_endline "one";
       Stdlib.print_endline "two"
     in
     ())
in|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 8,
        "endLine": 6,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 33,
        "endLine": 4,
        "kind": "region",
        "startCharacter": 5,
        "startLine": 2
      }
    ]
    |}]
;;

let%expect_test "traverses Pexp_letexception nodes" =
  let source =
    {folding_range|let decode_map str =
  let exception Shortcut of error_message in
  let () =
    Stdlib.print_endline "one";
    Stdlib.print_endline "two"
  in
  ()
in|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 4,
        "endLine": 6,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 30,
        "endLine": 4,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 2
      }
    ]
    |}]
;;

let%expect_test "traverses Pexp_sequence nodes" =
  let source =
    {folding_range|let a =
  Stdlib.print_endline "";
  let b =
    5 + 3 in
  b|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 3,
        "endLine": 4,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 9,
        "endLine": 3,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 2
      }
    ]
    |}]
;;

let%expect_test "supports if/else" =
  let source =
    {folding_range|let fn a =
  if a == true then
    let () =
      Stdlib.print_endline "";
      Stdlib.print_endline ""
    in
    let () =
      Stdlib.print_endline "";
      Stdlib.print_endline ""
    in
    ()
  else
    let () =
      Stdlib.print_endline "";
      Stdlib.print_endline ""
    in
    let () =
      Stdlib.print_endline "";
      Stdlib.print_endline ""
    in
    ()|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 6,
        "endLine": 20,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 6,
        "endLine": 10,
        "kind": "region",
        "startCharacter": 5,
        "startLine": 1
      },
      {
        "endCharacter": 29,
        "endLine": 4,
        "kind": "region",
        "startCharacter": 4,
        "startLine": 2
      },
      {
        "endCharacter": 29,
        "endLine": 8,
        "kind": "region",
        "startCharacter": 4,
        "startLine": 6
      },
      {
        "endCharacter": 29,
        "endLine": 14,
        "kind": "region",
        "startCharacter": 4,
        "startLine": 12
      },
      {
        "endCharacter": 29,
        "endLine": 18,
        "kind": "region",
        "startCharacter": 4,
        "startLine": 16
      }
    ]
    |}]
;;

let%expect_test "supports return type annotation" =
  let source =
    {folding_range|let fn a b : int =
  let result =
    Stdlib.print_endline "";
    a + b
  in
  result|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 8,
        "endLine": 5,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 9,
        "endLine": 3,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 1
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for try/with" =
  let source =
    {folding_range|let result =
  try
    let () =
      Stdlib.print_endline "";
      Stdlib.print_endline ""
    in
    Some 6
  with
  | Not_found ->
    let () =
      Stdlib.print_endline "";
      Stdlib.print_endline ""
    in
    None|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 8,
        "endLine": 13,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 8,
        "endLine": 13,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 1
      },
      {
        "endCharacter": 29,
        "endLine": 4,
        "kind": "region",
        "startCharacter": 4,
        "startLine": 2
      },
      {
        "endCharacter": 8,
        "endLine": 13,
        "kind": "region",
        "startCharacter": 13,
        "startLine": 8
      },
      {
        "endCharacter": 29,
        "endLine": 11,
        "kind": "region",
        "startCharacter": 4,
        "startLine": 9
      }
    ]
    |}]
;;

let%expect_test "traverses Pexp_construct" =
  let source =
    {folding_range|let a =
  Some
    (let () =
       Stdlib.print_endline "";
       Stdlib.print_endline "";
       Stdlib.print_endline ""
     in
     5)|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 7,
        "endLine": 7,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 30,
        "endLine": 5,
        "kind": "region",
        "startCharacter": 5,
        "startLine": 2
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for modules" =
  let source =
    {folding_range|module type X = sig
  type t =
    | A
    | B
    | C
end

module X = struct
  module type T = sig
    module T1 : sig
      type t =
        | A
        | B
        | C
    end
    type t
    val uri : t -> Uri.t
    val dispose : t -> unit
  end

  module Y = struct
    let bar a =
      match a with
      | None ->
        Stdlib.print_endline "";
        Stdlib.print_endline "";
        Stdlib.print_endline ""
      | Some b ->
        Stdlib.print_endline b;
        Stdlib.print_endline b;
        Stdlib.print_endline b
  end

  let foo () =
    let x =
      let y = 5 in
      let z = 3 in
      let open Stdlib in
      let () =
        let () = print_endline "" in
        let () = print_endline "" in
        let () = print_endline "" in
        ()
      in
      let open Promise.Syntax in
      let%lwt result =
        let a = 5 in
        Promise.resolve a
      in
      let+ result_letop =
        let a = 5 in
        Promise.resolve a
      in
      z + y
    in
    x + 3
end|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 3,
        "endLine": 5,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 3,
        "endLine": 5,
        "kind": "region",
        "startCharacter": 16,
        "startLine": 0
      },
      {
        "endCharacter": 7,
        "endLine": 4,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 1
      },
      {
        "endCharacter": 3,
        "endLine": 56,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 7
      },
      {
        "endCharacter": 3,
        "endLine": 56,
        "kind": "region",
        "startCharacter": 11,
        "startLine": 7
      },
      {
        "endCharacter": 5,
        "endLine": 18,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 8
      },
      {
        "endCharacter": 5,
        "endLine": 18,
        "kind": "region",
        "startCharacter": 18,
        "startLine": 8
      },
      {
        "endCharacter": 7,
        "endLine": 14,
        "kind": "region",
        "startCharacter": 4,
        "startLine": 9
      },
      {
        "endCharacter": 7,
        "endLine": 14,
        "kind": "region",
        "startCharacter": 16,
        "startLine": 9
      },
      {
        "endCharacter": 11,
        "endLine": 13,
        "kind": "region",
        "startCharacter": 6,
        "startLine": 10
      },
      {
        "endCharacter": 5,
        "endLine": 31,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 20
      },
      {
        "endCharacter": 5,
        "endLine": 31,
        "kind": "region",
        "startCharacter": 13,
        "startLine": 20
      },
      {
        "endCharacter": 30,
        "endLine": 30,
        "kind": "region",
        "startCharacter": 4,
        "startLine": 21
      },
      {
        "endCharacter": 30,
        "endLine": 30,
        "kind": "region",
        "startCharacter": 6,
        "startLine": 22
      },
      {
        "endCharacter": 31,
        "endLine": 26,
        "kind": "region",
        "startCharacter": 12,
        "startLine": 23
      },
      {
        "endCharacter": 30,
        "endLine": 30,
        "kind": "region",
        "startCharacter": 14,
        "startLine": 27
      },
      {
        "endCharacter": 9,
        "endLine": 55,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 33
      },
      {
        "endCharacter": 11,
        "endLine": 53,
        "kind": "region",
        "startCharacter": 4,
        "startLine": 34
      },
      {
        "endCharacter": 10,
        "endLine": 42,
        "kind": "region",
        "startCharacter": 6,
        "startLine": 38
      },
      {
        "endCharacter": 25,
        "endLine": 47,
        "kind": "region",
        "startCharacter": 6,
        "startLine": 45
      },
      {
        "endCharacter": 11,
        "endLine": 53,
        "kind": "region",
        "startCharacter": 6,
        "startLine": 49
      }
    ]
    |}]
;;

let%expect_test "traverses Pstr_extension structure item" =
  let source =
    {folding_range|let%expect_test "test from jsonrpc_test.ml" =
  let a =
    let b = 5 in
    6 + 5
  in
  Stdlib.print_endline (string_of_int 5)|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 40,
        "endLine": 5,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 40,
        "endLine": 5,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 9,
        "endLine": 3,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 1
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for class_type" =
  let source =
    {folding_range|class type foo_t =
object
  inherit castable
  method foo: string
end;;|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 3,
        "endLine": 4,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 3,
        "endLine": 4,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 1
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for class_type_field" =
  let source =
    {folding_range|module type Type = sig
  class reload_generic :
    object
      method select_operation :
           Cmm.operation
        -> Cmm.expression list
        -> Debuginfo.t
        -> Mach.operation * Cmm.expression list
    end
end|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 3,
        "endLine": 9,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 3,
        "endLine": 9,
        "kind": "region",
        "startCharacter": 19,
        "startLine": 0
      },
      {
        "endCharacter": 7,
        "endLine": 8,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 1
      },
      {
        "endCharacter": 7,
        "endLine": 8,
        "kind": "region",
        "startCharacter": 4,
        "startLine": 2
      },
      {
        "endCharacter": 47,
        "endLine": 7,
        "kind": "region",
        "startCharacter": 6,
        "startLine": 3
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for class_description" =
  let source =
    {folding_range|module type T = sig
  class cse_generic :
    object
    end
end|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 3,
        "endLine": 4,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 3,
        "endLine": 4,
        "kind": "region",
        "startCharacter": 16,
        "startLine": 0
      },
      {
        "endCharacter": 7,
        "endLine": 3,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 1
      },
      {
        "endCharacter": 7,
        "endLine": 3,
        "kind": "region",
        "startCharacter": 4,
        "startLine": 2
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for class_expr" =
  let source =
    {folding_range|class x =
  object
    val x = 3
    val virtual x : t
    val! mutable x = 3
  end|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 5,
        "endLine": 5,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 5,
        "endLine": 5,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 1
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for class_type (2)" =
  let source =
    {folding_range|class type t =
  object
    val x : t
    val mutable x : t
  end|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 5,
        "endLine": 4,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 5,
        "endLine": 4,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 1
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for Pmod_functor and Pmod_structure" =
  let source =
    {folding_range|module M =
  functor (M : S) ->
    (val x)
    (struct
      type t = int
      let x =
        Stdlib.print_endline "one";
        Stdlib.print_endline "two";
    end)|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 8,
        "endLine": 8,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 8,
        "endLine": 8,
        "kind": "region",
        "startCharacter": 10,
        "startLine": 1
      },
      {
        "endCharacter": 7,
        "endLine": 8,
        "kind": "region",
        "startCharacter": 5,
        "startLine": 3
      },
      {
        "endCharacter": 35,
        "endLine": 7,
        "kind": "region",
        "startCharacter": 6,
        "startLine": 5
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for Pmty_functor and Pmty_signature" =
  let source =
    {folding_range|module type S =
  functor (M : S) (_ : module type of M) ->
    sig
      type t =
        | A
        | B
    end|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 7,
        "endLine": 6,
        "kind": "region",
        "startCharacter": 0,
        "startLine": 0
      },
      {
        "endCharacter": 7,
        "endLine": 6,
        "kind": "region",
        "startCharacter": 10,
        "startLine": 1
      },
      {
        "endCharacter": 7,
        "endLine": 6,
        "kind": "region",
        "startCharacter": 18,
        "startLine": 1
      },
      {
        "endCharacter": 7,
        "endLine": 6,
        "kind": "region",
        "startCharacter": 4,
        "startLine": 2
      },
      {
        "endCharacter": 11,
        "endLine": 5,
        "kind": "region",
        "startCharacter": 6,
        "startLine": 3
      }
    ]
    |}]
;;

let%expect_test "returns folding ranges for Pexp_ifthenelse" =
  let source =
    {folding_range|if tool_name = "ocamldep" then
  if is_self_reference ~input_name ~loc lid then
    {type_decl with ptype_manifest = None}
  else {type_decl with ptype_manifest = Some manifest}
else
  let x =
    let () = Stdlib.print_endline "one" in
    let () = Stdlib.print_endline "two" in
    ()
  in
  let y =
    let () = Stdlib.print_endline "one" in
    let () = Stdlib.print_endline "two" in
    ()
  in
  ()|folding_range}
  in
  test source;
  [%expect
    {|
    [
      {
        "endCharacter": 54,
        "endLine": 3,
        "kind": "region",
        "startCharacter": 3,
        "startLine": 0
      },
      {
        "endCharacter": 42,
        "endLine": 2,
        "kind": "region",
        "startCharacter": 5,
        "startLine": 1
      },
      {
        "endCharacter": 6,
        "endLine": 8,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 5
      },
      {
        "endCharacter": 6,
        "endLine": 13,
        "kind": "region",
        "startCharacter": 2,
        "startLine": 10
      }
    ]
    |}]
;;

open Test.Import

module Util = struct
  let call_type_enclosing ?(verbosity = 0) ?range_end client position index =
    let uri = DocumentUri.of_path "test.ml" in
    let text_document = TextDocumentIdentifier.create ~uri in
    let params =
      `Assoc
        ([ ("textDocument", TextDocumentIdentifier.yojson_of_t text_document)
         ; ("position", Position.yojson_of_t position)
         ; ("index", `Int index)
         ; ("verbosity", `Int verbosity)
         ]
        @
        match range_end with
        | None -> []
        | Some x -> [ ("rangeEnd", Position.yojson_of_t x) ])
    in
    let params = Some (Jsonrpc.Structured.t_of_yojson params) in
    let req =
      Lsp.Client_request.UnknownRequest
        { meth = "ocamllsp/typeEnclosing"; params }
    in
    Client.request client req

  let print_type_enclosing result =
    result |> Yojson.Safe.pretty_to_string ~std:false |> print_endline

  let test ?range_end ~verbosity ~index ~line ~character source =
    let position = Position.create ~line ~character in
    let range_end =
      Option.map
        ~f:(fun (line, character) -> Position.create ~line ~character)
        range_end
    in
    let request client =
      let open Fiber.O in
      let+ response =
        call_type_enclosing ~verbosity ?range_end client position index
      in
      print_type_enclosing response
    in
    Helpers.test source request
end

let%expect_test {|
  The cursor is positioned on [x].

  We expect to have the type [string] and no other enclosings
  than the first one ([00:04-00:05]), because [x] is not
  nested into an other expression.
|}
    =
  let source = "let x = string_of_int 2002" in
  let line = 0
  and character = 4
  and verbosity = 0
  and index = 0 in
  Util.test ~verbosity ~index ~line ~character source;
  [%expect
    {|
    {
      "index": 0,
      "enclosings": [
        {
          "end": { "character": 5, "line": 0 },
          "start": { "character": 4, "line": 0 }
        }
      ],
      "type": "string"
    } |}]

let%expect_test {|
  The cursor is positioned on [2002].

  We expect to have the type [int] and to have two enclosings:
  0. [00:22 - 00:26], the [2002] expr
  1. [00:08 - 00:26]. the [string_of_int 2002] expr

|}
    =
  let source = "let x = string_of_int 2002" in
  let line = 0
  and character = 23
  and verbosity = 0
  and index = 0 in
  Util.test ~verbosity ~index ~line ~character source;
  [%expect
    {|
    {
      "index": 0,
      "enclosings": [
        {
          "end": { "character": 26, "line": 0 },
          "start": { "character": 22, "line": 0 }
        },
        {
          "end": { "character": 26, "line": 0 },
          "start": { "character": 8, "line": 0 }
        }
      ],
      "type": "int"
    } |}]

let%expect_test {|
  The cursor is still positioned on [2002] but we ask for
  the index [1] (the second enclosing).

  We expect still have our two enclosings but now, we are targeting the
  second-one, so the expected type is [string]:
  0. [00:22 - 00:26], the [2002] expr
  1. [00:08 - 00:26]. the [string_of_int 2002] expr

|}
    =
  let source = "let x = string_of_int 2002" in
  let line = 0
  and character = 23
  and verbosity = 0
  and index = 1 in
  Util.test ~verbosity ~line ~character ~index source;
  [%expect
    {|
    {
      "index": 1,
      "enclosings": [
        {
          "end": { "character": 26, "line": 0 },
          "start": { "character": 22, "line": 0 }
        },
        {
          "end": { "character": 26, "line": 0 },
          "start": { "character": 8, "line": 0 }
        }
      ],
      "type": "string"
    } |}]

let%expect_test {|
  First, let's locate on [A.z], we expect the type [t], but we
  will increase the verbosity in order to get the fuill expansion of
  [type t]. And we will have 3 enclosings:
  0 : [16:06 - 16:07], the [z] expr.
  1 : [02:11 - 17:03], the [struct ... end] expr.
  2 : [02:00 - 17:03], the [module A] expr.
|}
    =
  let source =
    {|type a = Foo | Bar

module A = struct
  let f () = 10
  let g = Bar
  let h x = x

  module B = struct
    type b = Baz

    let x = (Baz, 10)
    let y = (Bar, Foo)
  end

  type t = { a : string; b : float }

  let z = { a = "Hello"; b = 1.0 }
end|}
  in
  let line = 16
  and character = 6
  and verbosity = 1
  and index = 0 in
  Util.test ~verbosity ~index ~line ~character source;
  [%expect
    {|
    {
      "index": 0,
      "enclosings": [
        {
          "end": { "character": 7, "line": 16 },
          "start": { "character": 6, "line": 16 }
        },
        {
          "end": { "character": 3, "line": 17 },
          "start": { "character": 11, "line": 2 }
        },
        {
          "end": { "character": 3, "line": 17 },
          "start": { "character": 0, "line": 2 }
        }
      ],
      "type": "type t = { a : string; b : float; }"
    } |}]

let%expect_test {|
  Now, let's use our enclosing to jump to the index [2], in order
  to get the type of [module A], our enclosings will no change.
  0 : [16:06 - 16:07], the [z] expr.
  1 : [02:11 - 17:03], the [struct ... end] expr.
  2 : [02:00 - 17:03], the [module A] expr.
|}
    =
  let source =
    {|type a = Foo | Bar

module A = struct
  let f () = 10
  let g = Bar
  let h x = x

  module B = struct
    type b = Baz

    let x = (Baz, 10)
    let y = (Bar, Foo)
  end

  type t = { a : string; b : float }

  let z = { a = "Hello"; b = 1.0 }
end|}
  in
  let line = 16
  and character = 6
  and verbosity = 0
  and index = 2 in
  Util.test ~verbosity ~line ~character ~index source;
  [%expect
    {|
    {
      "index": 2,
      "enclosings": [
        {
          "end": { "character": 7, "line": 16 },
          "start": { "character": 6, "line": 16 }
        },
        {
          "end": { "character": 3, "line": 17 },
          "start": { "character": 11, "line": 2 }
        },
        {
          "end": { "character": 3, "line": 17 },
          "start": { "character": 0, "line": 2 }
        }
      ],
      "type": "sig\n  val f : unit -> int\n  val g : a\n  val h : 'a -> 'a\n  module B : sig type b = Baz val x : b * int val y : a * a end\n  type t = { a : string; b : float; }\n  val z : t\nend"
    } |}]

let%expect_test {|
  Now, let's jump on the [10] inside of [A.B.x]. We expect
  to have the type [int]. And we get a huge list of enclosings!
  0. [10:18 - 10:20] the [10] expr.
  1. [10:12 - 10:21] the [Baz, 10] expr.
  2. [07:13 - 12:05] the [struct .. end] (of [module B])
  3. [02:11 - 17:03] the [struct .. end] (of [module A])
  4. [02:00 - 17:03], the [module A] expr.
|}
    =
  let source =
    {|type a = Foo | Bar

module A = struct
  let f () = 10
  let g = Bar
  let h x = x

  module B = struct
    type b = Baz

    let x = (Baz, 10)
    let y = (Bar, Foo)
  end

  type t = { a : string; b : float }

  let z = { a = "Hello"; b = 1.0 }
end|}
  in
  let line = 10
  and character = 18
  and verbosity = 0
  and index = 0 in
  Util.test ~verbosity ~index ~line ~character source;
  [%expect
    {|
    {
      "index": 0,
      "enclosings": [
        {
          "end": { "character": 20, "line": 10 },
          "start": { "character": 18, "line": 10 }
        },
        {
          "end": { "character": 21, "line": 10 },
          "start": { "character": 12, "line": 10 }
        },
        {
          "end": { "character": 5, "line": 12 },
          "start": { "character": 13, "line": 7 }
        },
        {
          "end": { "character": 5, "line": 12 },
          "start": { "character": 2, "line": 7 }
        },
        {
          "end": { "character": 3, "line": 17 },
          "start": { "character": 11, "line": 2 }
        },
        {
          "end": { "character": 3, "line": 17 },
          "start": { "character": 0, "line": 2 }
        }
      ],
      "type": "int"
    } |}]

let%expect_test {|
  Now, let's jump on the [10] inside of [A.B.x] and ask for index [1].
  We expect to have the type [b * int]. And we keep our list of enclosings!
  0. [10:18 - 10:20] the [10] expr.
  1. [10:12 - 10:21] the [Baz, 10] expr.
  2. [07:13 - 12:05] the [struct .. end] (of [module B])
  3. [02:11 - 17:03] the [struct .. end] (of [module A])
  4. [02:00 - 17:03], the [module A] expr.
|}
    =
  let source =
    {|type a = Foo | Bar

module A = struct
  let f () = 10
  let g = Bar
  let h x = x

  module B = struct
    type b = Baz

    let x = (Baz, 10)
    let y = (Bar, Foo)
  end

  type t = { a : string; b : float }

  let z = { a = "Hello"; b = 1.0 }
end|}
  in
  let line = 10
  and character = 18
  and verbosity = 0
  and index = 1 in
  Util.test ~verbosity ~index ~line ~character source;
  [%expect
    {|
    {
      "index": 1,
      "enclosings": [
        {
          "end": { "character": 20, "line": 10 },
          "start": { "character": 18, "line": 10 }
        },
        {
          "end": { "character": 21, "line": 10 },
          "start": { "character": 12, "line": 10 }
        },
        {
          "end": { "character": 5, "line": 12 },
          "start": { "character": 13, "line": 7 }
        },
        {
          "end": { "character": 5, "line": 12 },
          "start": { "character": 2, "line": 7 }
        },
        {
          "end": { "character": 3, "line": 17 },
          "start": { "character": 11, "line": 2 }
        },
        {
          "end": { "character": 3, "line": 17 },
          "start": { "character": 0, "line": 2 }
        }
      ],
      "type": "b * int"
    } |}]

let%expect_test {|
  Now, the list is a little bit to large and we just want enclosings
  that start at the [struct ... end] attached to the module B.
  We use a [range_end] argument.

  0. [07:13 - 12:05] the [struct .. end] (of [module B])
  1. [02:11 - 17:03] the [struct .. end] (of [module A])
  2. [02:00 - 17:03], the [module A] expr.
|}
    =
  let source =
    {|type a = Foo | Bar

module A = struct
  let f () = 10
  let g = Bar
  let h x = x

  module B = struct
    type b = Baz

    let x = (Baz, 10)
    let y = (Bar, Foo)
  end

  type t = { a : string; b : float }

  let z = { a = "Hello"; b = 1.0 }
end|}
  in
  let line = 10
  and character = 18
  and verbosity = 0
  and index = 0
  and range_end = (7, 17) in
  Util.test ~verbosity ~index ~range_end ~line ~character source;
  [%expect
    {|
    {
      "index": 0,
      "enclosings": [
        {
          "end": { "character": 5, "line": 12 },
          "start": { "character": 13, "line": 7 }
        },
        {
          "end": { "character": 5, "line": 12 },
          "start": { "character": 2, "line": 7 }
        },
        {
          "end": { "character": 3, "line": 17 },
          "start": { "character": 11, "line": 2 }
        },
        {
          "end": { "character": 3, "line": 17 },
          "start": { "character": 0, "line": 2 }
        }
      ],
      "type": "sig type b = Baz val x : b * int val y : a * a end"
    } |}]

let%expect_test {|
  Now, the list is a little bit to large and we just want enclosings
  that start at the [struct ... end] attached to the module B.
  We use a [range_end] argument and we can couple it with [index],
  [2] for example, we get the type of [module A].

  0. [07:13 - 12:05] the [struct .. end] (of [module B])
  1. [02:11 - 17:03] the [struct .. end] (of [module A])
  2. [02:00 - 17:03], the [module A] expr.
|}
    =
  let source =
    {|type a = Foo | Bar

module A = struct
  let f () = 10
  let g = Bar
  let h x = x

  module B = struct
    type b = Baz

    let x = (Baz, 10)
    let y = (Bar, Foo)
  end

  type t = { a : string; b : float }

  let z = { a = "Hello"; b = 1.0 }
end|}
  in
  let line = 10
  and character = 18
  and range_end = (7, 17)
  and verbosity = 0
  and index = 2 in
  Util.test ~verbosity ~range_end ~index ~line ~character source;
  [%expect
    {|
    {
      "index": 2,
      "enclosings": [
        {
          "end": { "character": 5, "line": 12 },
          "start": { "character": 13, "line": 7 }
        },
        {
          "end": { "character": 5, "line": 12 },
          "start": { "character": 2, "line": 7 }
        },
        {
          "end": { "character": 3, "line": 17 },
          "start": { "character": 11, "line": 2 }
        },
        {
          "end": { "character": 3, "line": 17 },
          "start": { "character": 0, "line": 2 }
        }
      ],
      "type": "sig\n  val f : unit -> int\n  val g : a\n  val h : 'a -> 'a\n  module B : sig type b = Baz val x : b * int val y : a * a end\n  type t = { a : string; b : float; }\n  val z : t\nend"
    } |}]
